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
(** Test code that checks if an lval is adequetly protected by locks
    or not -- for thread-aware dataflow analysis.
    Assumes race checking is already run and summaries built *)

open Gc_stats
open Readcalls
open Callg
open Cil
open Pretty
open Fstructs
open Scc
open Stdutil
open Manage_sums

module A = Alias
module GA = Guarded_access
module RS = Racesummary
module SS = Symsummary
module SPTA = Symstate2
module Race = Racestate
module Th = Threads
module BS = Backed_summary
module DC = Default_cache
module L = Logging
module Dis = Distributed
module Req = Request
module Stat = Mystats
module FS = File_serv
module I = Inspect
module LVG = Lval_graph

(***************************************************)
(* Commandline handling                            *)


let cgDir = ref ""

let configFile = ref "client.cfg"

let logDir = ref ""

let (inspect : string list ref) = ref []

let restart = ref false

let userName = ref ""

let outGraph = ref ""

(* Command-line argument parsing *)

let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-i", Arg.String 
      I.inspector#addInspect, "inspect state of given function");
   ("-r", Arg.Set restart, "causes analyzer to clear state and restart");
   ("-u", Arg.Set_string userName, "username to use");
   ("-o", Arg.Set_string outGraph, "filename for output dot graph");
   ("-l", Arg.Set_string logDir, "log status and errors to given dir")]

(* TODO: figure out how to distribute workload ... *)

let anonArgFun (arg:string) : unit =
  ()

let usageMsg = getUsageString "-cg fname [options]\n"



(***************************************************)
(* Run                                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () : simpleCallG =
  try
    let settings = Config.initSettings !configFile in
    Req.init settings;
    DC.makeLCaches !cgDir;
    Cilinfos.reloadRanges !cgDir;
    Th.initSettings settings;
    BS.init settings !cgDir;
    A.initSettings settings !cgDir;
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = readCalls cgFile in
    Dis.init settings !cgDir;
    Req.setUser !userName;
    let _ = FS.init settings in (* ignore thread created *)
    SPTA.init settings cg (RS.sum :> Modsummary.modSumm);
    GA.clearCache ();
    cg
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


(* TODO: make this not hardCoded to allTypes, just use what's needed *)
let hardCodedSumTypes () =
  !BS.allTypes


let prepSumms cg =
  (* Just try to get summs for all funcs *)
  let funs = FMap.fold (fun fkey _ curList -> fkey :: curList) cg [] in
  prepareSumms funs (hardCodedSumTypes ())



(** Initiate analysis *)
let testLvGraph () =
  let cg = initSettings () in
  let sccCG = getSCCGraph cg in
  
  L.logStatus "Preparing summaries";
  L.logStatus "-----";
  L.flushStatus ();
  
  prepSumms cg;
  
  L.logStatus "Building graph for lock Lvals";
  L.logStatus "-----";
  L.flushStatus ();
  
  let lvG = LVG.buildLockLvalGraph sccCG cg in
  
  L.logStatus "Graph building complete";
  L.logStatus "-----";
  L.flushStatus ();

  if (!outGraph <> "") then
    (L.logStatus ("Printing graph to: " ^ !outGraph);
     lvG#printGraph !outGraph)


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" || !configFile = "" || !userName = "") then
      (Arg.usage argSpecs usageMsg;
       exit 1)
    else
      (if (!logDir <> "") then
         (L.setStatLog !logDir;
          L.setErrLog !logDir;
         )
      ;
       Cil.initCIL ();
       setGCConfig ();
       
       L.logStatus "Testing LV graph";
       L.logStatus "-----";
       testLvGraph ();
       Stat.print stdout "STATS:\n";
       printStatistics ();
       exit 0;
      )
  with e ->
    L.logError ("Exc. in Test LV graph: " ^
                    (Printexc.to_string e)) ;
    Stat.print stdout "STATS:\n";
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
