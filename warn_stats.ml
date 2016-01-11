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

(** Process that reads data race warning data and collects statistics. 
    Also, writes the XML representation *)


open Gc_stats
open Cil
open Pretty
open Fstructs
open Scc_cg
open Stdutil
open Cilfiles
open Cilinfos

module A = Alias
module RP = Race_reports
module L = Logging
module FC = Filecache
module DC = Default_cache
module Lv = Lvals

(***************************************************)
(* Commandline handling                            *)

let warnFile = ref ""

let configFile = ref "client.cfg"

let cgDir = ref ""

let cgFile = ref ""

let saveXML = ref false

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-i", Arg.Set_string warnFile, "input warning file");
   ("-x", Arg.Set saveXML, "save warnings to XML");
   ("-cg", Arg.Set_string cgFile, "call graph file")]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-i warn_file [options]\n"


(***************************************************)
(* State / Utilities :                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () =
  try
    Cilinfos.reloadRanges !cgDir;
    DC.makeLCaches !cgDir;
    let settings = Config.initSettings !configFile in
    A.initSettings settings !cgDir;
    
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


(***************************************************)
(* Run                                             *)


(** Go through warning tables and collect stats *)
let collectStats () =
  let warnings = RP.loadRaces !warnFile in
  warnings#printStats;
  if (!saveXML) then
    warnings#saveToXML (Filename.concat !cgDir "warnings.xml")



(** Start printing warning statistics *)
let printWarnStats () : unit =  
  initSettings ();
  collectStats ()


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgFile = "" || !configFile = "" || !warnFile = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        cgDir := Filename.dirname !cgFile;
        Cil.initCIL ();
        setGCConfig ();
        
        L.logStatus "Collecting statistics from warnings";
        L.logStatus "-----";
        printWarnStats ();
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in Warning Stats: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
