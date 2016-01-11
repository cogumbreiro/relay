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

open Cil
open Pretty
open Logging
open Stdutil

module Race = Racestate
module RS = Race.RS
module SPTA = Race.SPTA
module BS = Backed_summary  
module Warn = Race_warnings

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
      Inspect.inspector#addInspect, "inspect state of function (given name)");
   ("-nw", Arg.Set no_warn, "do not generate warnings");
   ("-r", Arg.Set restart, "causes analyzer to clear state and restart");
   ("-u", Arg.Set_string userName, "username to use");
   ("-l", Arg.Set_string logDir, "log status and errors to given dir");
   ("-st", Arg.Set_string statusFile,
    "file storing work status (current scc/pass/analysis)");
   ("-vs", Arg.Set RS.verboseSum, 
    "print verbose function summaries");
   ("-time", Arg.Set Mystats.doTime, "Time operations");
   ("-mem", Arg.Set Osize.checkSizes, "Check memory usage");
  ]

let anonArgFun (arg:string) : unit =
  ()

let usageMsg = getUsageString "-cg fname -u username [options]\n"



(***************************************************)
(* Run                                             *)

let printKnownFuns cg =
  logStatusD 
    (map_to_doc line Callg.FMap.iter
       (fun k n -> dprintf "%s : %s : %b" n.Callg.name 
          (Callg.fid_to_string k) n.Callg.hasBody)
       cg nil)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () : Callg.callG * Scc_cg.sccGraph =
  Cilinfos.reloadRanges !cgDir;
  let settings = Config.initSettings !configFile in
  Request.init settings;
  Request.setUser !userName;
  Checkpoint.init !statusFile;
  Default_cache.makeLCaches (!cgDir);
  Threads.initSettings settings;
  Alias.initSettings settings !cgDir;
  let cgFile = Dumpcalls.getCallsFile !cgDir in
  let cg = Callg.readCalls cgFile in
  let sccCG = Scc_cg.getSCCGraph cg in

  (* DEBUG *)
(*
  printKnownFuns cg;
*)

  let () = BS.init settings !cgDir cg sccCG in

  SPTA.init settings cg (RS.sum :> Modsummaryi.modSum);
  Distributed.init settings !cgDir;
  Entry_points.initSettings settings;
  let _ = File_serv.init settings in (* ignore thread created *)
  let gen_num = Request.initServer () in
  if ( !restart ) then begin
    logStatus "trying to clear old summaries / local srcs, etc.";
    flushStatus ();
    BS.clearState gen_num;
    Request.clearState gen_num;
  end;
  (cg, sccCG)


(** Initiate analysis *)
let doRaceAnal () : unit = begin
  let cg, sccCG = initSettings () in
  
  (* Then do a bottom-up analysis *)
  Race.RaceBUTransfer.initStats cg sccCG;

  logStatus "Starting bottomup analysis";
  logStatus "-----";
  flushStatus ();

  Race.BUDataflow.compute cg sccCG;

  logStatus "Bottomup analysis complete";
  logStatus "-----";
  flushStatus ();
  
  if (not !no_warn) then begin
    logStatus "\n\n\nBeginning Thread Analysis:";
    logStatus "-----";
    flushStatus ();  
    Warn.flagRacesFromSumms cg !cgDir;

    (* Try printing alias example requests *)
    logStatus "\n\nPrinting Alias assumptions used by warnings";
    logStatus "-----";
    flushStatus ();
    Warn.printAliasUses ();
  end;
  
  Alias.finalizeAll ();
end
 

let printStatistics () = begin
(*
  Lvals.printHashStats ();
  Cil_lvals.printHashStats ();
*)
  Mystats.print stdout "STATS:\n";
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
        )
      ;
      combineLogs (); (* Make sure errors show up in the right place *)
      Stdutil.printCmdline ();
      Pervasives.at_exit printStatistics;
      Cil.initCIL ();
      Gc_stats.setGCConfig ();

      logStatus "Checking for data races";
      logStatus "-----";
      doRaceAnal ();
      exit 0;
    end

;;
main () ;;
