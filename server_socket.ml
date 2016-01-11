(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following 
  conditions are met:
  
  1. Redistributions of source code must retain the above copyright 
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above 
  copyright notice, this list of conditions and the following disclaimer 
  in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the University of California, San Diego, nor 
  the names of its contributors may be used to endorse or promote 
  products derived from this software without specific prior 
  written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
*)


(** Server for tracking summary-file locations and assigning work
    to worker processes. Single-threaded for simplicity.
    TODO: separate the message handler from the 
    connection/concurrency management?
*)


open Cil
open Unix
open Messages
open Fstructs
open Readcalls
open Callg
open Scc_cg
open Gc_stats
open Stdutil
open Strutil
open Scp

module A = Alias
module RP = Race_reports
module Dis = Distributed
module L = Logging
module Stat = Mystats
module Th = Threads
module DC = Default_cache
module FC = Filecache



(***** Parameters ******)

let serverPort = ref 13790

let listenQueueLen = 96

let cgDir = ref ""

(** Server config file. Contains port numbers, etc. *)
let serverConfig = ref "server.cfg"

(** Client config file. Identifies alias analysis and thread functions *)
let clientConfig = ref "client.cfg"

let restart = ref false

let gen_num = ref 0

let pruneUnreachable = ref false

let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-p", Arg.Set_int serverPort, "server port");
   ("-r", Arg.Set restart, "clear logs and restart");
   ("-u", Arg.Set pruneUnreachable, "prune functions not reachable from roots");
   ("-sc", Arg.Set_string serverConfig, "use given server config file");
   ("-cc", Arg.Set_string clientConfig, "use given client config file")]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname [opts]\n"


(***** Parse Settings ***)


let warn_timerID = ref Timeout.nilTimer


let initServerSettings settings =
  let servSet = Config.getGroup settings "SERVER" in
  Config.iter 
    (fun fieldName value ->
       let informError () = 
         L.logError "Corrupt entry in server settings:";
         L.logError (fieldName ^ ":" ^ value)
       in
       try match fieldName with
         "GEN_FILE" -> gen_num := Gen_num.getNum value
       | "PORT" -> serverPort := (int_of_string value)
       | _ -> informError ()
       with e ->
         L.logError ("initServerSettings: " ^ (Printexc.to_string e));
         informError ();
         raise e
    ) servSet;
  warn_timerID := Timeout.newTimerID ()
    
    
let initSettings () = begin
  Cilinfos.reloadRanges !cgDir;
  let servSet = Config.initSettings !serverConfig in
  Dis.init servSet !cgDir;
  initServerSettings servSet;
  let clientSet = Config.initSettings !clientConfig in
  DC.makeLCaches (!cgDir);
  A.initSettings clientSet !cgDir;
  Th.initSettings clientSet;
  Entry_points.initSettings clientSet;

end

(****** Error logging, termination, misc... ******)

let log_error s =
  L.logError s

exception Serv_done

let quit exit_num =
  Stat.print Pervasives.stdout "STATS:\n";
  printStatistics ();
  L.flushStatus ();         
  exit exit_num

    

(***** Server state *****)

(* Summary Directories *)

(* Map fkey -> network address + path + filename *)
let (sumTable : (fKey, (sockaddr * string * string)) Hashtbl.t) 
     = Hashtbl.create 237
  
(* Map from fkey -> ast/definition file *)
let (astTable : (fKey, string) Hashtbl.t) = Hashtbl.create 237

let initASTTable cg =
  FMap.iter 
    (fun fkey cgNode ->
       Hashtbl.add astTable fkey 
         (SH.merge strings cgNode.defFile)
    ) cg


(* Race Warnings *)

let (warnLocks : (warnKey, message) Hashtbl.t)  = Hashtbl.create 237
let numWarnSites = ref (-1)
let numWarners = ref 0
let numWarnDone = ref 0
  

let warnWork = Queue.create ()  (* TODO: have server assign warning pairs *)

let initWarnWork () =
  Queue.add 1 warnWork

let warnings = new RP.raceReports ()  (** Table of warnings *)

(* Wait at most another X seconds for rest of warnings to 
   come in before we assume that the rest of the warning generators died  *)
let warn_timeout = 600.0

let finishWarnings () =
  Timeout.cancel !warn_timerID;
  warnings#printWarnings;
  warnings#serialize (Filename.concat !cgDir "warnings.dat");           
  warnings#saveToXML (Filename.concat !cgDir "warnings.xml");
  raise Serv_done (* Assume warnings are the last thing we need, so exit *)
    

(* Handle ctrl-c by flushing existing warnings *)
let handleCtrlC signal =
  if signal == Sys.sigint then
    (*
    (L.logError "We get signal! (writing out warnings and quitting";
     finishWarnings ())
    *)
    (L.logError "Got sigint (quitting w/out writing warnings)")
  else
    (L.logError "We get signal (other than sigint in handleCtrlC)! Ignored";
     ())
      
let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle handleCtrlC)



(***** Client handler ******)

let recvBuff = Buffer.create 2048

let sockaddr_eq a b =
  match a, b with
    ADDR_UNIX s1, ADDR_UNIX s2 ->
      s1 = s2
  | ADDR_INET (ia1, _), ADDR_INET (ia2, _) ->
      ia1 = ia2
  | _ -> false


let addPath (addr, u, p) =
  let newP = addString p in
  let newU = addString u in
  (addr, newU, newP)


(************************************************************
      Check if an SCC is ready to be analyzed 
************************************************************)

type sccStat =
    SccDone 
  | SccNotReady
  | SccReady of scc

(* SCC Worklist *)

module OrderedSCC = 
struct
  type t = scc
  let compare x y = x.scc_num - y.scc_num
end

module SQ = Queueset.Make(OrderedSCC)
  
let sccWork = SQ.create () (** worklist of sccs that are not yet done *)

let sccRequests = Queue.create () (** queue of clients waiting for work *)

let hasSCCWork = ref true (** true if not waiting on dependencies *)

let sccsTotal = ref 0   (* progress tracking *)
let sccsDone = ref 0


(** Prune what's not needed to analyze thread roots *)
let pruneCG cg sccCG : sccGraph =
  (* Need everything reachable from thread roots, and need everything
     reachable from the thread creator! *)
  let rooter = new Entry_points.rootGetter cg !cgDir in
  let roots = rooter#getRootKeys () in
  let reachable, _ = Callg.getReachableFunctions cg roots in
  (* Compute new scc graph with only the reachable functions *)
  let newSCCCG = Scc_cg.pruneUnreached sccCG reachable in
  L.logStatus "Pruned nodes unreachable from thread roots/creators";
  L.logStatus ("Prev # SCCs: " ^ (string_of_int (mapSize sccCG IntMap.fold)) ^
                 "\t New: " ^ (string_of_int (mapSize newSCCCG IntMap.fold)));
  L.flushStatus ();
  newSCCCG


(** Initialize the scc worklist *)
let initSCCWork sccCG =
  let rec dfsSCCCG curSCC visited =
    if (IntSet.mem curSCC.scc_num visited) then
      visited
    else
      let newVisited = 
        IntSet.add curSCC.scc_num visited in
      let finalVis = IntSet.fold 
        (fun neighK vis ->
           try
             let neighS = IntMap.find neighK sccCG in
             dfsSCCCG neighS vis
           with Not_found ->
             vis
        ) curSCC.scc_callees newVisited in
      SQ.addOnce curSCC sccWork;
      finalVis
  in
  SQ.clear sccWork;
  let _ = IntMap.fold
    (fun sccK scc visited ->
       incr sccsTotal;           (* also count num sccs *)
       dfsSCCCG scc visited;
    ) sccCG IntSet.empty in
  ()



(** Keep the right count of completed SCCs to see if the barrier is reached 
    (before warning checks can be done) *)
let noteSccDone () =
  incr sccsDone;
  L.logStatus (">>> PROGRESS " ^ (string_of_int !sccsDone) ^ "/" ^
                 (string_of_int !sccsTotal) ^ " SCCs DONE!\n");
  L.flushStatus ()


(** Make persistent record of complete SCCs so server can resume on restart *)
let recordSccDone scc =
  Dis.recordSccDone scc.scc_num;
  noteSccDone ()


(** True if all the functions in the scc are actually done *)
let sccFuncsDone (scc) =
  FSet.for_all
    (fun fkey ->
       if (Hashtbl.mem sumTable fkey) then true
       else match Dis.isFunDone fkey with
         Some ((a, u, p) :: tl) ->
           Hashtbl.replace sumTable fkey (addPath (a, u, p));
           true
       | _ -> false
    ) scc.scc_nodes

(** Do the actual search for the next ready SCC *)
let findSCCWork () =
  (* Damn this is ugly *)
  let checked = ref IntSet.empty in
  let rec check scc =
    if (Dis.isSccDone scc.scc_num) then
      (noteSccDone ();
       loop ())
    else if (sccFuncsDone scc) then
      (recordSccDone scc;
       loop ())
    else if (Dis.neighSCCSNotDone scc) then
      (* can't operate on this one yet *)
      (SQ.addOnce scc sccWork;
       loop ())
    else
      (* found one that's ready! *) 
      SccReady scc
  and loop () =
    if (SQ.is_empty sccWork) then
      if (!sccsDone == !sccsTotal) then
        SccDone
      else
        SccNotReady
    else
      let curSCC = SQ.pop sccWork in
      if (IntSet.mem curSCC.scc_num !checked) then
        (SQ.addOnce curSCC sccWork;
         SccNotReady)
      else
        (checked := IntSet.add curSCC.scc_num !checked;
         check curSCC)
  in loop ()    



(************************************************************
         MAIN EVENT DISPATCH 
************************************************************)

(** Reply w/ given msg, using sock_out. Close sock_out afterwards. *)
let sendAndClose (sock_in, sock_out) msg =
  (try writeMessage sock_out msg
   with e -> L.logError "sendAndClose: couldn't send, just closing"
  );
  close_out_noerr sock_out  


(** Dish-out work (SCCs) for everyone in the sccRequests queue *)
let processSccRequests () =
  while (!hasSCCWork &&
           not (Queue.is_empty sccRequests)) do
    (match findSCCWork () with 
       SccReady (scc) ->
         let dest = Queue.take sccRequests in
         sendAndClose dest (MSCCReady scc)
     | SccNotReady ->
         hasSCCWork := false
     | SccDone ->
         Queue.iter 
           (fun dest ->
              sendAndClose dest MDone
           ) sccRequests;
         Queue.clear sccRequests;
         L.logStatus "Finished all SCCs";
         L.flushStatus ();
    )
  done

(** Read request from sock_in, process request and reply to sock_out. 
    On exit, sock_out should be closed. *)
let recvAndReply (sock_in, sock_out) =
  try
    let msg = readMessage sock_in in
    match msg with
      MInit ->
        sendAndClose (sock_in, sock_out) (MInitReply !gen_num)

    | MReqSCCWork ->
        Queue.add (sock_in, sock_out) sccRequests;
        Stat.time "procReq" processSccRequests ()

    | MNotiSCCDone (scc, user, srcAddr, sumList)->
        List.iter 
          (fun (fkey, path) ->
             (* save entry to disk *)
             let src = addPath (srcAddr, user, path) in
             Dis.recordFunDone fkey [src];
             Hashtbl.replace sumTable fkey src;
          ) sumList;
        sendAndClose (sock_in, sock_out) MSuccess;
        recordSccDone scc;
        hasSCCWork := true;
        Stat.time "procReq" processSccRequests ()

    | MReqSum fkeys ->
        let results = Hashtbl.create 17 in
        let addToResults fkey (srcAddr, srcUser, srcPath) =
          try
            let oldList = Hashtbl.find results (srcUser, srcAddr) in
            Hashtbl.replace results (srcUser, srcAddr) 
              ((fkey, srcPath) :: oldList)
          with Not_found ->
            Hashtbl.add results (srcUser, srcAddr) [(fkey, srcPath)]
        in
        List.iter 
          (fun fkey ->             
             try
               let src = Hashtbl.find sumTable fkey in
               addToResults fkey src
             with Not_found ->
               (* make server check disk *)
               match Dis.isFunDone fkey with
                 Some (aup :: tl) ->
                   let aup = addPath aup in
                   Hashtbl.replace sumTable fkey aup;
                   addToResults fkey aup
               | _ -> ()
          ) fkeys;
        sendAndClose (sock_in, sock_out) (MReplySum results)


    | MReqData (fname, destPath) ->
        (* TODO have dest do the scp w/ server's username *)
        let reply = 
          try
            (* assume fname is rooted at callgraph file's directory *)
            (* TODO FIX if needed
               let src = Filename.concat !cgDir fname in 
               let destString = makeAddrPath addr destPath in
            
            match doScp src destString with
              (_, WEXITED i) when i == 0 ->
                MSuccess
            | _ ->
                MFail
            *)
            MFail
          with Not_found ->
            MFail
        in
        sendAndClose (sock_in, sock_out) reply


    | MWarnBarrier i -> 
        numWarnSites := i;
        sendAndClose (sock_in, sock_out) MSuccess;
        incr numWarners

    | MLockWarn k ->
        let reply = 
          try
            (* either locked or done *)
            Hashtbl.find warnLocks k
          with Not_found ->
            (* not locked and not marked as done -- in memory *)
            if (false) then (* TODO check disk log of warnings! *)
              (Hashtbl.add warnLocks k MDone;
               MDone) (* it's actually all done *)

            else 
              (Hashtbl.add warnLocks k MLocked;
               MSuccess) (* locked and ready to operate *)
        in
        sendAndClose (sock_in, sock_out) reply;

    | MUnlockWarn k ->
        (try 
           let oldValue = Hashtbl.find warnLocks k  in
           if (oldValue != MLocked) then
             log_error "server warn: unlocking an unlocked entry"
           ;
           Hashtbl.replace warnLocks k MDone;
         with Not_found ->
           log_error "server warn: unlocking an unlocked entry";
           Hashtbl.replace warnLocks k MDone;
        );
        sendAndClose (sock_in, sock_out) MSuccess


    | MNotiRace newWarnings ->
        (* need to be careful later... need to know they can join! *)
        warnings#joinReports (new RP.raceReports ~initial:newWarnings ());
        incr numWarnDone;
        sendAndClose (sock_in, sock_out) MSuccess;
        L.logStatus ("Received warnings from worker. Status: " ^ 
                       (string_of_int !numWarnDone) ^ "/" ^ 
                       (string_of_int !numWarners));
        L.flushStatus ();

        (* TODO: have notifier send list of [fkey pairs] that were part
           of these partial reports, checkpoint + record to disk also! *)

        (* Check if barrier has been reached by all *)
        if (!numWarnDone >= !numWarners) then (finishWarnings ()) 
        else (Timeout.set !warn_timerID warn_timeout 
                (fun () -> 
                   log_error ("Timeout: Not waiting for " ^ 
                                 (string_of_int (!numWarners - !numWarnDone)) ^
                                 " more results");
                   finishWarnings ()) )
    | _ ->
        log_error "server receive an unexpected request";
        sendAndClose (sock_in, sock_out) MFail

  with 
    Serv_done ->
      raise Serv_done
  | e ->
      log_error ("exception in recvAndReply " ^ (Printexc.to_string e));
      sendAndClose (sock_in, sock_out) MFail



(************************************************************
        Main server listen/accept loop 
************************************************************)

let serv listenSock () =
  bind listenSock (ADDR_INET (inet_addr_any, !serverPort));
  while (true) do
    listen listenSock listenQueueLen;
    
    let (clientSock, clientAddr) = accept listenSock in
    (* single-threaded version *)
    let (sock_in,sock_out) = 
      (in_channel_of_descr clientSock, out_channel_of_descr clientSock) in
    (try
       recvAndReply (sock_in, sock_out);
     with Unix.Unix_error (e, _, _) ->
       log_error ("child thread died - " ^ 
                    (error_message e))
    )
  done

let startServer () = 
  let listenSock = socket PF_INET SOCK_STREAM 0 in (* TODO use ipv6 instead? *)
  setsockopt listenSock SO_REUSEADDR true;
  (try serv listenSock ()
   with 
     Unix.Unix_error (EADDRINUSE, _, _) -> 
       (Timeout.retry 
          (serv listenSock) 
          (fun () -> log_error "Tried to serv several times -- failed")
          2 1.0)
   | Unix.Unix_error (e, _, _) -> 
       log_error ("server main thread: failed - " ^ (error_message e));
   | Serv_done -> ()
  );
  close listenSock
    
    
let writeMyIP () = 
  let host = gethostbyname (gethostname ()) in
  let addr = host.h_addr_list.(0) in
  let ip_string = (string_of_inet_addr addr) in
  L.logStatus ("Server ip is: " ^ ip_string);
  let outF = open_out "server_ip.txt" in
  output_string outF ip_string;
  close_out outF


let main () =
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" || !serverConfig = "" || !clientConfig ="") then begin
      Arg.usage argSpecs usageMsg;
      exit 1
    end else begin
      Cil.initCIL ();
      setGCConfig ();

      writeMyIP ();

      initSettings ();
      
      if (!restart) then begin
        L.logStatus "trying to clear logs";
        L.flushStatus ();
        Dis.clearState !gen_num;
      end;
      
      L.logStatus "Callgraph, etc.";
      L.logStatus "-----";
      L.flushStatus ();

      let cgFile = Dumpcalls.getCallsFile !cgDir in
      let cg = readCalls cgFile in
      let sccCG = getSCCGraph cg in
      initASTTable cg;
      if !pruneUnreachable then
        initSCCWork (pruneCG cg sccCG)
      else 
        initSCCWork sccCG
      ;
      
      L.logStatus "Starting server";
      L.logStatus "-----";
      L.flushStatus ();
      startServer ();
      quit 0;
    end
  with e -> 
    L.logStatus ("Exc. in Server: " ^
                    (Printexc.to_string e)) ;
    Stat.print Pervasives.stdout "STATS:\n";
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;

