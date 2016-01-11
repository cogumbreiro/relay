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
(** Module that handles requests between the server and the worker processes. 
    - client requests work
    - client notifies server of work completion
    - etc. 

    @see messages.ml for some of the messages exchanged
*)

open Messages
open Unix
open Fstructs
open Scc
open Scp
open Stdutil
open Warn_reports

module L = Logging
module Dis = Distributed
module Stat = Mystats
module BS = Backed_summary
module FS = File_serv
module Conf = Config

module RP = Race_reports

(**************** settings **********************)

let cwd = Sys.getcwd ()

let servAddr = ref (ADDR_INET (inet_addr_any, 0))

let myAddr = ref (ADDR_INET (inet_addr_any, 0))

let setServerIP ip_string =
  match !servAddr with
    ADDR_INET (_, oldPort) ->
      servAddr := ADDR_INET (inet_addr_of_string ip_string, oldPort)
  | _ ->
      failwith "Request: servAddr not set to an ADDR_INET?"

let setServerPort port =
  match !servAddr with
    ADDR_INET (oldIA, _) ->
      servAddr := ADDR_INET (oldIA, port)
  | _ ->
      failwith "Request: servAddr not set to an ADDR_INET?"

let setMyIP () = 
  let host = gethostbyname (gethostname ()) in
  let addr = host.h_addr_list.(0) in
  L.logStatus ("myIP: " ^ (string_of_inet_addr addr));
  myAddr := ADDR_INET (addr, 0)
    

let readFromFile filename =
  let doRead fd =
    try input_line fd;
    with e -> 
      L.logError ("Request: can't read ip/port from file: " ^
                    (Printexc.to_string e)); 
      raise e
  in
  open_in_for filename doRead


let localDir = ref ""


(** Initialize distributed computing management based on part 
    of the given config file *)
let init settings : unit =
  let _ = Unix.umask 0o000 in
  setMyIP ();
  let reqSettings = Conf.getGroup settings "REQUEST" in
  Conf.iter 
    (fun fieldName v ->
       match fieldName with
         "SERVER_IP" -> setServerIP (Strutil.strip v)
       | "SERVER_PORT" -> setServerPort (int_of_string (Strutil.strip v))
       | "SERVER_IP_FILE" -> setServerIP (Strutil.strip (readFromFile v))
       | "SERVER_PORT_FILE" -> 
           setServerPort (int_of_string (Strutil.strip (readFromFile v)))
       | "LOCAL_SRCS" -> 
           (localDir := Strutil.strip v;
            Filetools.ensurePath !localDir;
            L.logStatus ("using " ^ !localDir ^ 
                           " to store acquired files"))
       | _ -> 
           L.logError ("corrupt val in Request config " ^ v);
    ) reqSettings



let quit exit_code =
  Stat.print Pervasives.stdout "STATS:\n";
  Gc_stats.printStatistics ();
  exit exit_code

(**************** request operations *************)

(* 
class virtual reqHandler = object (self)

  method virtual initServer : unit -> int

  method virtual getSCCWork : 'a. unit -> 'a message

  method virtual sccDone : scc -> (fKey * string) list -> unit
    
  method virtual requestSumm : fKey list -> (fKey * BS.dbToken) list
    
  method virtual requestData : string -> string -> unit
    
  method virtual reqWarnBarrier : int -> unit
    
  method virtual lockWarn : 'a. fKey -> fKey -> 'a message
    
  method virtual unlockWarn : fKey -> fKey -> unit
    
  method virtual notifyWarn : 'a . (#warnReports as 'a) -> unit

  method virtual notifyRace : RP.raceTable -> unit

(*
  method virtual notifyRace : RP.raceReports -> unit
*)

(* if client converts report to string first, required buffer is too large *)

end 

*)


(***** Helper functions for Socket-based implementation *****)

let userName = ref ""

let setUser n =
  userName := n


(** get wildcard summary filename (making assumptions on naming scheme) *)
let makeSummPath path fkey =
  Filename.concat path ((string_of_fkey fkey) ^ ".*")
    (* watch out... may need to add an escape like ".\\*" *)


let num_retries = 3
let retry_wait = 3.0


(** Send one message, get a reply, then disconnect. *)
let sendMessage msg =
  let rec doSend (_, sock_in, sock_out) =
    try
      writeMessage sock_out msg;
      let reply = readMessage sock_in in
      reply
    with e ->
      L.logError ("Request doSend failed: " ^ (Printexc.to_string e));
      raise e
  and faultHandler eMsg =
    L.logError ("Request fault handler with " ^ eMsg);
    let reply = ref MFail in
    (Timeout.retry 
       (fun () -> 
          L.logError ("Request retrying");
          reply := open_conn_for !servAddr doSend)
       (fun () -> L.logError ("Request giving up")) (* allow scp to try? *)
       num_retries retry_wait);
    !reply
  in
  try open_conn_for !servAddr doSend
  with 
    Unix_error (e, _, _) -> 
      faultHandler (Unix.error_message e)
  | e ->
      faultHandler (Printexc.to_string e)
    

exception SummariesNotFound




(** Request handler that communicates w/ server through sockets *)

(*
class sockBasedReqHandler = object (self)
  inherit reqHandler 
*)

  (** Initialize communication between client and server. Server
      assigns the same Run Number / Generation Num to all workers *)
  let initServer () : int =
    try
      match sendMessage MInit with
        MInitReply i -> i
      | _ -> 
          L.logError ("initServer: failed, shutting down");
          quit 1
    with Unix_error (e, _, _) ->
      L.logError ("initServer: failed - " ^ (error_message e));
      quit 1
    
  (** Ask server for the next SCC available for analysis *)
  let getSCCWork () =
    try
      sendMessage MReqSCCWork
    with Unix_error (em, _, _) as e ->
      L.logError ("getSCCWork: failed - " ^ (error_message em));
      raise e

  (** Inform server that the [scc] is now complete, and summaries
      for its functions are located at [summPaths] *)
  let sccDone scc summPaths =
    try
      match sendMessage (MNotiSCCDone 
                           (scc, !userName, !myAddr, summPaths)) with
        MSuccess -> ()
      | _ ->
          L.logError ("sccDone: failed");
    with Unix_error (em, _, _) as e->
      L.logError ("sccDone: failed - " ^ (error_message em));
      raise e


  (** Ask the server where summaries for the requested functions 
      and analyses ([reqs]) can be found.
      TODO: modify server_socket.ml and distributed.ml to correlate the
      location of the summary w/ fKey + sumType also *)
  let askServerForPeers reqs =
    (* Hack for now... *)
    let reqs = List.map (fun (fKey, sumTyp) -> fKey) reqs in
    match sendMessage (MReqSum reqs) with
      MReplySum table -> 
        table
    | _ ->
        L.logError ("askServerForPeers: failed (unexpected reply)");
        raise SummariesNotFound


  (** Download the requested summaries. *)
  let requestSumm (reqs : (fKey * BS.sumType) list) : 
      (fKey * BS.sumType * BS.dbToken) list =
    (* Track the locations of successfully downloaded function summaries *)
    let (succeeded : (fKey * BS.sumType * BS.dbToken) list ref) = ref [] in
    let addToSucceeded tok fname =
      let k = BS.key_of_name fname in
      let sumT = BS.stype_of_name fname in
      (* TODO: get the type of summary from the fname as well, or think
         of how to pass the original fkey + sumtype *)
      succeeded := (* may be fed same key multiple times for diff summs *)
        if (List.exists (fun (k', t', _) -> k = k' && sumT = t') !succeeded) 
        then !succeeded
        else (k, sumT, tok) :: !succeeded
    in

    (* Copy from one peer identified as ([srcUser], [srcAddr]) all
       the function summaries known to be stored 
       (as tracked by [keyPaths]) *)
    let doPeer (srcUser, srcAddr) (keyPaths : (fKey * string) list) =
      (* just pick a random destination path to download all summs *)
      let dest, tok = BS.anyDBPath () in
      let dest = if (Filename.is_relative dest) then
        Filename.concat cwd dest
      else
        dest in
      
      (* get actual filenames for each fkey requested *)
      let srcs = List.fold_left 
        (fun l (fkey, path) ->
           let names = BS.possibleNames fkey in
           List.rev_append (List.map (fun n -> (n, path)) names) l
        ) [] keyPaths in
      
      (* Try to use file server to acquire summaries *)
      let doneFS = Stat.time "scp" 
        (FS.requestFiles srcAddr srcs) dest in
      
      (* If any are left, use scp *)
      let notDone = List.filter 
        (fun (fname, _) -> 
           not (List.exists (fun other -> fname = other) doneFS)
        ) srcs in
      let doneSCP = 
        if (notDone != []) then
          (L.logError "requestSumm: resorting to scp";
           Stat.time "scp" 
             (fun dest ->
                match batchScp (srcUser, srcAddr) notDone dest with
                  (0, _) -> 
                    (* on normal exit: assume all succeeded *)
                    List.map (fun (n, _) -> n) notDone
                | (_, comm) ->
                    L.logError ("requestSumm scp failed: " ^ comm);
                    []
             ) dest
          )
        else []
      in
      
      (* Track final set of completed keys *)
      List.iter (addToSucceeded tok) doneFS;
      List.iter (addToSucceeded tok) doneSCP;
    in  (* end doPeer *)
    
    (* Coordinate transfers from all the peers *)
    let getFromPeers table =
      (* reply is a table of peers + the files the peer has  *)
      Hashtbl.iter doPeer table;
      
      (* Warn if things aren't acquired! *)
      let lenR = List.length reqs in
      let lenS = List.length !succeeded in
      if (lenR <> lenS) then (
        L.logError ("requestSumm: Asked for " ^ (string_of_int lenR) ^ 
                      " sums, got " ^ (string_of_int lenS) ^ "\n");
        List.iter 
          (fun (reqFkey, reqStyp) ->
             (* TODO: check the reqStyp also *)
             if (not (List.exists (fun (fk, st, tok) -> reqFkey = fk) 
                        !succeeded)) 
             then L.logError ("Missing: " ^ (string_of_fkey reqFkey))) reqs;
        raise SummariesNotFound
      );
      
      (* Finally, return the list of summaries that were acquired,
         along with the local location in which they were stored   *)
      !succeeded
    in (* end getFromPeers *)

    let t = 
      try askServerForPeers reqs
      with Unix_error (e, _, _) ->
        L.logError ("requestSumm: server error - " ^ (error_message e));
        raise SummariesNotFound
    in
    getFromPeers t


  (** Request that the server coordinate transfer of file [fname] to
      the local [destPath]. Meant to be used for AST files, etc. 
      This means [fname] is relative to the callgraph / AST directory.
      UNTESTED!!! *)
  let requestData fname destPath =
    try
      match sendMessage (MReqData (fname, destPath)) with
        MSuccess -> ()
      | _ -> L.logError ("requestData: failed");
    with Unix_error (em, _, _) as e ->
      L.logError ("requestData: failed - " ^ (error_message em));
      raise e

  (** Request to begin the warning generation phase, indicating that
      [num] pairs of function roots are to be compared.
      TODO: Have server assign work, so [num] may be phased-out *)
  let reqWarnBarrier num = 
    try
	  match sendMessage (MWarnBarrier num) with
	    MSuccess -> ()
	  | _ -> L.logError ("reqWarnBarrier: failed");
    with Unix_error (em, _, _) as e ->
	  L.logError ("reqWarnBarrier: failed - " ^ (error_message em));
      raise e

  (** Request from this worker that warnings be checked for the
      function pair ([fk1], [fk2]) *)
  let lockWarn (fk1:fKey) (fk2:fKey) =
    try
      sendMessage (MLockWarn (fk1, fk2))
    with Unix_error (e, _, _) ->
      L.logError ("lockWarn: failed - " ^ (error_message e));
      MFail

  (** Inform server that this worker has checked function pair ([fk1], [fk2]) *)
  let unlockWarn fk1 fk2 =
    try
      match sendMessage (MUnlockWarn (fk1, fk2)) with
        MSuccess -> ()
      | _ -> L.logError ("unlockWarn: failed");
    with Unix_error (em, _, _) as e ->
      L.logError ("unlockWarn: failed - " ^ (error_message em));
      raise e


(*
  let notifyWarn warnData =
    try
      match sendMessage (MNotiWarn warnData) with
        MSuccess -> ()
      | _ -> L.logError ("notifyWarn: failed");
    with Unix_error (em, _, _) as e ->
      L.logError ("notifyWarn: failed - " ^ (error_message em));
      raise e
*)

  (** Send server all the data-race warnings found by this worker *)
  let notifyRace warnData =
    try
      match sendMessage (MNotiRace warnData) with
        MSuccess -> ()
      | _ -> L.logError ("notifyRace: failed");
    with Unix_error (em, _, _) as e ->
      L.logError ("notifyRace: failed - " ^ (error_message em));
      raise e

(*
end (* end socket-based req handler *)
*)



(******* External interface for request satisfaction *******)

(* (Skipped right to it (above)... previous class had trouble compiling
   when using the patched ocaml compiler that tracks memory usage


let handler = new sockBasedReqHandler

let initServer () =
  handler#initServer ()

let getSCCWork () =
  Stat.time "Req getScc" handler#getSCCWork ()

let sccDone scc summPaths =
  Stat.time "Req sccDone" (handler#sccDone scc) summPaths

let requestSumm req =
  handler#requestSumm req 

let requestData fname destPath =
  handler#requestData fname destPath

let reqWarnBarrier num = 
  handler#reqWarnBarrier num


let lockWarn fk1 fk2 =
  handler#lockWarn fk1 fk2


let unlockWarn fk1 fk2 =
  handler#unlockWarn fk1 fk2


let notifyWarn (warnData : #warnReports) =
  handler#notifyWarn warnData


let notifyRace warnData = 
  handler#notifyRace warnData

*)


(***** Clear out the local scratch directory *******)

(** Clear any stale downloaded disk state (files in directories
    that do not match the current [gen_num]) *)
let clearState gen_num =
  let gen_file = Filename.concat !localDir "gen_num.txt" in
  let clearFunc = 
    (fun () ->
       Filetools.walkDirSimple 
         (fun file ->
            Sys.remove file) !localDir
    ) in
  clearDirGen gen_num gen_file clearFunc
    
