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


(** Worker process for analyzing a program for possible 
    data races (launch these processes along w/ the server) *)

open Gc_stats
open Readcalls
open Callg
open Cil
open Pretty
open Fstructs
open Scc
open Stdutil
open Cilfiles
open Cilinfos

module A = Alias
module GA = Guarded_access
module RS = Racesummary
module SS = Symsummary
module SPTA = Symstate2
module Race = Racestate
module Du = Cildump
module Th = Threads
module BS = Backed_summary  
module Warn = Race_warnings
module DC = Default_cache
module L = Logging
module Dis = Distributed
module Req = Request
module Stat = Mystats
module FS = File_serv
module I = Inspect

module Checkp = Checkpoint

(* TODO:

   * Need to be able to get the correlations between statements
   for post-thread-creation interaction (and possibly before join)?
   Currently assume entire thread-creator runs in parallel.

*)

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let configFile = ref "client.cfg"

let logDir = ref ""

let restart = ref false

let no_warn = ref false

let userName = ref ""

let statusFile = ref ""

(* Command-line argument parsing *)

let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-i", Arg.String 
      I.inspector#addInspect, "inspect state of function (given name)");
   ("-nw", Arg.Set no_warn, "do not generate warnings");
   ("-r", Arg.Set restart, "causes analyzer to clear state and restart");
   ("-u", Arg.Set_string userName, "username to use");
   ("-l", Arg.Set_string logDir, "log status and errors to given dir");
   ("-st", Arg.Set_string statusFile,
    "file storing work status (current scc/pass/analysis)")]

let anonArgFun (arg:string) : unit =
  ()

let usageMsg = getUsageString "-cg fname -u username [options]\n"


(***************************************************)
(* Determine what summaries need to be kept        *)


(** Get the list functions whose summaries are needed by the end *)
let getNeededFuncs (cg) = begin
  (* Find which functions actually fork new threads *)
  let threadCreatorCallers = Th.findTCCallers cg in
  
  (* Find the fork targets (i.e., thread roots) *)
  let threadRoots = Th.getThreadRoots cg threadCreatorCallers in

  (* Also need the functions called by the threadCreator itself 
     (so that Symstate pass will work) *)
  let results = List.fold_left
    (fun cur (fid, tc) ->
       try
         let fnode = FMap.find fid cg in
         List.fold_left 
           (fun cur fkey ->
              FSet.add fkey cur
           ) cur fnode.callees
       with Not_found ->
         L.logError "getNeededFuncs: Can't find thread creator in CG!";
         cur
    ) threadRoots threadCreatorCallers in

  (* Finally, need entry-point functions *)
(*
  let results = List.fold_left
    (fun cur (fk, _) -> FSet.add fk cur) results (getRoots cg) in

*)
  FSet.elements results
    


end


(***************************************************)
(* Run                                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () : simpleCallG =
  try
    let settings = Config.initSettings !configFile in
    Req.init settings;
    Checkp.init !statusFile;
    DC.makeLCaches (!cgDir);
    Th.initSettings settings;
    Cilinfos.reloadRanges !cgDir;
    A.initSettings settings !cgDir;
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = readCalls cgFile in
    RS.initSummaries settings cg;
    SS.initSummaries cg;
    BS.init settings !cgDir ;
    RS.sum#cleanup ();  (* TODO: make this just happen in BS init? *)
    SS.sum#cleanup ();
    SPTA.init settings cg (RS.sum :> Modsummary.modSumm);
    Dis.init settings !cgDir;
    Req.setUser !userName;
    let _ = FS.init settings in (* ignore thread created *)
    let gen_num = Req.initServer () in
    if ( !restart ) then begin
      L.logStatus "trying to clear old summaries / local srcs, etc.";
      L.flushStatus ();
      BS.clearState gen_num;
      Req.clearState gen_num;
    end;
    GA.clearCache ();
    cg

  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e



(** Initiate analysis *)
let doRaceAnal () : unit =
  let cg = initSettings () in
  let sccCG = getSCCGraph cg in begin

    (* Figure out which summaries are important -- 
       (i.e., what summaries cannot be garbage collected) *)
    let neededFuncs = getNeededFuncs cg in

    (* Then do a bottom-up analysis *)
    Race.RaceBUTransfer.initStats cg sccCG neededFuncs;

    L.logStatus "Starting bottomup analysis";
    L.logStatus "-----";
    L.flushStatus ();

    Race.BUDataflow.compute cg sccCG;

    L.logStatus "Bottomup analysis complete";
    L.logStatus "-----";
    L.flushStatus ();
    
    if (not !no_warn) then begin
      L.logStatus "\n\n\nBeginning Thread Analysis:";
      L.logStatus "-----";
      L.flushStatus ();  
      Warn.flagRacesFromSumms cg !cgDir;
    end
  end


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" || !configFile = "" || !userName = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        if (!logDir <> "") then
          (L.setStatLog !logDir;
           L.setErrLog !logDir;
          )
        ;
        Cil.initCIL ();
        setGCConfig ();

        L.logStatus "Checking for data races";
        L.logStatus "-----";
        doRaceAnal ();
        Stat.print stdout "STATS:\n";
        printStatistics ();
        exit 0;
      end
  with e ->
    L.logError ~prior:0 ("Exc. in Race Analysis: " ^
                           (Printexc.to_string e)) ;
    Stat.print stdout "STATS:\n";
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
