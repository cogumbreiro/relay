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

(** Load filtered races and mark the corresponding pseudo accesses NotRacy *)

open Gc_stats
open Callg
open Cil
open Fstructs
open Stdutil
open Cilfiles
open Cilinfos

module A = Alias
module Intra = IntraDataflow
module RS = Racesummary

module SPTA = Symstate2
module Th = Threads
module BS = Backed_summary  
module DC = Default_cache
module L = Logging
module Dis = Distributed
module Req = Request
module Stat = Mystats

module Null = Nullstate2
module Warn = Null_warnings

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let configFile = ref "client.cfg"

let logDir = ref ""

(* Command-line argument parsing *)

let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-l", Arg.Set_string logDir, "log status and errors to given dir")]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg dirname [options]\n"


(***************************************************)
(* Run                                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () : unit =
  try
    let settings = Config.initSettings !configFile in
    DC.makeLCaches (!cgDir);
    Cilinfos.reloadRanges !cgDir;
    A.initSettings settings !cgDir;
    Th.initSettings settings;
    (* Get Callgraph structures *)
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = Ci_cg.SCCCG.readCalls cgFile in
    let () = BS.init settings !cgDir cg in
    Dis.init settings !cgDir;
    SPTA.init settings cg (RS.sum :> Modsummaryi.absModSumm);

  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


(** Prepare the PAR summaries before touching them *)
let prepareSummaries cluster_id_2_pakey =
  let toPrepare = 
    Hashtbl.fold 
      (fun cid fk2pakey cur ->
         Ci_cg.SCCCG.FMap.fold 
           (fun fkey _ cur -> List_utils.addOnce cur (BS.inputFreeSumKey fkey)) 
           fk2pakey cur
      ) cluster_id_2_pakey [] in
  Manage_sums.prepareSumms 
    toPrepare (BS.getDescriptors [Pseudo_access.sums#sumTyp])


(** Initiate analysis *)
let doAnal () : unit =
  let () = initSettings () in
  begin

    (* Read in the race cluster id -> pakey mapping *)
    let cluster_id_2_pakey = Race_warnings2.loadR2PA 
      (Filename.concat !cgDir Race_warnings2.r2pakey_file) in

    (* Prepare the summaries that may be touched *)
    prepareSummaries cluster_id_2_pakey;

    (* Update the summaries *)
    Pseudo_access.overruleFiltered 
      (Filename.concat !cgDir "removed_warnings.txt")
      cluster_id_2_pakey;

  end


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" || !configFile = "") then
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

        L.logStatus "Marking filtered races Not Racy";
        L.logStatus "-----";
        doAnal ();
        Stat.print stdout "STATS:\n";
        printStatistics ();
        exit 0;
      end
  with e ->
    L.logError ("Exc. in Pseudo Filter: " ^
                    (Printexc.to_string e)) ;
    Stat.print stdout "STATS:\n";
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
