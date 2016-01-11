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
open Callg
open Cil
open Pretty
open Fstructs
open Stdutil
open Cilfiles
open Cilinfos
open Thread_needed_funcs

module A = Alias
module RS = Racesummary
module SPTA = Symstate2
module Race = Racestate_temp2
module Du = Cildump
module Th = Threads
module BS = Backed_summary  
module Warn = Race_warnings2
module DC = Default_cache
module L = Logging
module Dis = Distributed
module Req = Request
module Stat = Mystats
module FS = File_serv
module I = Inspect


module Checkp = Checkpoint

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
(* Run                                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () =
  try
    let settings = Config.initSettings !configFile in
    Req.init settings;
    Checkp.init !statusFile;
    DC.makeLCaches (!cgDir);
    Th.initSettings settings;
    Cilinfos.reloadRanges !cgDir;
    A.initSettings settings !cgDir;

    (* Hack to get this summary registered *)
    let () = BS.registerType Pseudo_access.sums in

    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = Ci_cg.SCCCG.readCalls cgFile in
    let () = BS.init settings !cgDir cg in
    SPTA.init settings cg (RS.sum :> Modsummaryi.absModSumm);
    Dis.init settings !cgDir;
    Req.setUser !userName;
    Entry_points.initSettings settings;
    let _ = FS.init settings in (* ignore thread created *)
    let gen_num = Req.initServer () in
    if ( !restart ) then begin
      L.logStatus "trying to clear old summaries / local srcs, etc.";
      L.flushStatus ();
      BS.clearState gen_num;
      Req.clearState gen_num;
    end;
    cg

  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e



(** Initiate analysis *)
let doRaceAnal () : unit =
  let cg = initSettings () in
  let sccCG = Ci_cg.SCCCG.getSCCGraph cg in begin

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
    end;

    Warn.printGammaReport ();
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
