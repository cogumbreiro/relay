(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Ravi Chugh, Jan Voung
  
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

(** Analyze a program for null pointer violations (launch these
    processes w/ the server to run in parallel mode)... 

    TEMPORARY clone of race_anal

 *)

open Gc_stats
open Callg
open Cil
open Pretty
open Fstructs
open Stdutil
open Cilfiles
open Cilinfos
open Logging

module RS = Racestate.RS
module SPTA = Racestate.SPTA
module BS = Backed_summary  
module DC = Default_cache
module Stat = Mystats

module Ana = Nullstate
module Warn = Null_warnings

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let configFile = ref "client.cfg"

let logDir = ref ""

let restart = ref false

let no_warn = ref true

let userName = ref ""

let statusFile = ref ""

(* Command-line argument parsing *)

let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-i", Arg.String Inspect.inspector#addInspect,
    "inspect state of function (given name)");
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
(* Run                                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () =
  let settings = Config.initSettings !configFile in
  Request.init settings;
  Checkpoint.init !statusFile;
  DC.makeLCaches (!cgDir);
  Cilinfos.reloadRanges !cgDir;
  Alias.initSettings settings !cgDir;
  Threads.initSettings settings;

  (* Set the null warnings summary *)
  let nwSums = new Warn.NSummary.data (BS.makeSumType "nwarn1") in
  Warn.setSums nwSums;

  (* Get Callgraph structures *)
  let cgFile = Dumpcalls.getCallsFile !cgDir in
  let cg = readCalls cgFile in
  let sccCG = Scc_cg.getSCCGraph cg in
  let () = BS.init settings !cgDir cg sccCG in
  Distributed.init settings !cgDir;
  Request.setUser !userName;
  SPTA.init settings cg (RS.sum :> Modsummaryi.absModSumm);
  Entry_points.initSettings settings;
  let _ = File_serv.init settings in (* ignore thread created *)
  let gen_num = Request.initServer () in
  if ( !restart ) then begin
    logStatus "trying to clear old summaries / local srcs, etc.";
    flushStatus ();
    BS.clearState gen_num;
    Request.clearState gen_num;
  end;
  cg, sccCG



(** Initiate analysis *)
let doAnal () : unit = begin
  let cg, sccCG = initSettings () in
  Guarded_access.clearCache ();
  
  (* Initialize misc modules *)
  Ana.NullBUTransfer.initStats cg sccCG;

  (* RAVI *)
  Ana.NullBUTransfer.initializeFuncsWithoutBody cg;

  logStatus "Starting bottomup analysis";
  logStatus "-----";
  flushStatus ();

  Ana.BUDataflow.compute cg sccCG;

  logStatus ("doPlusCount: " ^ string_of_int !Rns.doPlusCount
             ^ "( " ^ string_of_int !Rns.countAddrGlob
             ^ ","  ^ string_of_int !Rns.countCondNotNull
             ^ ","  ^ string_of_int !Rns.countCondLval 
             ^ ","  ^ string_of_int !Rns.countAssignNotNull ^ ")");

  logStatus ("pseudoAccessCount: " ^ 
               string_of_int !Nullstate.pseudoAccessCount);

  let safe = !Ana.NullBUTransfer.numDerefsSafe in
  let unsafe = !Ana.NullBUTransfer.numDerefsWarn in
  Warn.printDerefReport "sequential" safe unsafe;

  (* Write out the null warnings here for now 
     (may need to coord w/ server if we want to distribute...) *)
  let null_seq_data = Filename.concat !cgDir "unsafe_derefs_seq.dat" in
  let safe_derefs_data = Filename.concat !cgDir "safe_derefs_seq.dat" in

  Warn.prepareNullSums cg;

  let unsafeDerefs = Warn.getUnsafe () in
  unsafeDerefs#serialize null_seq_data;
  let safeDerefs = Warn.getSafe () in
  safeDerefs#serialize safe_derefs_data;
  let () = Warn.printDanglingDerefChecks cg !cgDir in
  
  logStatus "Bottomup analysis complete";
  logStatus "-----";
  flushStatus ();
  
end


let printStatistics () = begin
  Stat.print stdout "STATS:\n";
  Gc_stats.printStatistics ();
  flushStatus ()
end

(** Entry point *)
let main () = 
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
        (setStatLog !logDir;
         setErrLog !logDir;
        );
      Stdutil.printCmdline ();
      Pervasives.at_exit printStatistics;
      Cil.initCIL ();
      setGCConfig ();

      logStatus "Checking for null pointers";
      logStatus "-----";
      doAnal ();
      printStatistics ();
      exit 0;
    end

;;
main () ;;
