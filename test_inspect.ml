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
(** 
    Crappy post-analysis run that inspects and replays an analysis 
    performed during the original run. 
    
    GOTCHAS: 
    - Which analysis to inspect is hard-coded for now. 
    - Also, this should only be run on the node w/ the summaries!
*)


open Gc_stats
open Callg
open Cil
open Pretty
open Fstructs
open Scc_cg
open Stdutil
open Cilfiles
open Cilinfos

module A = Alias
module Intra = IntraDataflow
module Race = Racestate
module RS = Race.RS
module SPTA = Race.SPTA
module Du = Cildump
module Th = Threads
module BS = Backed_summary  
module DC = Default_cache
module L = Logging
module Dis = Distributed
module Req = Request
module Stat = Mystats
module FS = File_serv
module I = Inspect


(***************************************************)
(* Commandline handling                            *)

let cgFile = ref ""

let cgDir = ref ""

let configFile = ref "client.cfg"

let logDir = ref ""

let restart = ref false

let no_warn = ref false

let userName = ref "johndoe"

(* Command-line argument parsing *)

let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "name of call graph file");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-i", Arg.String 
      I.inspector#addInspect, "inspect state of function (given name)");
   ("-nw", Arg.Set no_warn, "do not generate warnings");
   ("-r", Arg.Set restart, "causes analyzer to clear state and restart");
   ("-u", Arg.Set_string userName, "username to use");
   ("-l", Arg.Set_string logDir, "log status and errors to given dir")]

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
    DC.makeLCaches (!cgDir);
    Th.initSettings settings;
    Cilinfos.reloadRanges !cgDir;
    A.initSettings settings !cgDir;
    let cg = readCalls !cgFile in
    let sccCG = Scc_cg.getSCCGraph cg in 
    let () = BS.init settings !cgDir cg sccCG in
    Dis.init settings !cgDir;
    Req.setUser !userName;
    SPTA.init settings cg (RS.sum :> Modsummaryi.absModSumm);
    (cg, sccCG);
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


(* TODO: make this not hardCoded to all types, just what's needed *)
let hardCodedSumTypes () =
  BS.getDescriptors [RS.sum#sumTyp;
                     SPTA.SS.sum#sumTyp;]
    

let inspectFun cg sccCG fname = begin
  L.logStatus ("Inspecting: " ^ fname);
  (* Look for functions that match the same name *)
  FMap.iter 
    (fun fk fnode ->
       if (fnode.name = fname) then (
         (* First get all summaries for all callees... 
            hmm sucks that it tries to communicate (only wanted it
            to seek the disk, really)... *)
         let callees = calleeKeys fnode in
         let () = Manage_sums.prepareSumms callees (hardCodedSumTypes ()) in
         let _ = Race.RaceBUTransfer.doFunc fk fnode in
         ()
       )
    ) cg;
end


(** Initiate analysis *)
let doRaceAnal () : unit = begin
  (* Get Callgraph structures *)
  let cg, sccCG = initSettings () in
  
  (* Then do a bottom-up analysis *)
  Race.RaceBUTransfer.initStats cg sccCG ;

  L.logStatus "Inspecting:";
  L.logStatus "-----";
  L.flushStatus ();

  Inspect.inspector#iter (inspectFun cg sccCG);

  L.logStatus "Inspection complete";
  L.logStatus "-----";
end


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgFile = "" || !configFile = "") then
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
        cgDir := Filename.dirname !cgFile;
        Cil.initCIL ();
        setGCConfig ();

        L.logStatus "Post analysis re-inspection";
        L.logStatus "-----";
        doRaceAnal ();
        Stat.print stdout "STATS:\n";
        printStatistics ();
        exit 0;
      end
  with e ->
    L.logError ("Exc. in Test Inspect: " ^
                    (Printexc.to_string e)) ;
    Stat.print stdout "STATS:\n";
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
