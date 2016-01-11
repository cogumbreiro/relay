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


(** Just check data race warnings. Assumes analysis has been run,
    and the server is up *)

open Gc_stats
open Cil
open Pretty
open Fstructs
open Stdutil
open Cilfiles
open Cilinfos

module A = Alias
module SS = Symsummary
module Race = Racestate
module RS = Race.RS
module SPTA = Race.SPTA
module Th = Threads
module BS = Backed_summary  
module Warn = Race_warnings
module Req = Request
module DC = Default_cache
module L = Logging
module FS = File_serv

(***************************************************)
(* Commandline handling                            *)

let cgFile = ref ""

let cgDir = ref ""

let configFile = ref "client.cfg"

let userName = ref ""

let printAlias = ref false

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-u", Arg.Set_string userName, "username to use");
   ("-pa", Arg.Set printAlias, "print lvals pairs with iffy aliasing");
  ]


let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname -u username [options]\n"


(***************************************************)
(* Run                                             *)

(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () =
  try
    Cilinfos.reloadRanges !cgDir;
    let settings = Config.initSettings !configFile in
    Th.initSettings settings;
    Req.init settings;
    DC.makeLCaches !cgDir;
    A.initSettings settings !cgDir;

    (* Get Callgraph structures after initializing alias analysis *)
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = Callg.readCalls cgFile in
    let sccCG = Scc_cg.getSCCGraph cg in

    let () = BS.init settings !cgDir cg sccCG in
    let _ = FS.init settings in (* ignore thread created *)
    SPTA.init settings cg (RS.sum :> Modsummaryi.absModSumm);
    Req.setUser !userName;
    Entry_points.initSettings settings;
(*    Dis.init settings !cgDir; *)
    cg
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e

      
(** Start printing warnings *)
let printWarnings () : unit =
  let cg = initSettings () in
  
  L.logStatus "\n\n\nBeginning Thread Analysis:";
  L.logStatus "-----";
  L.flushStatus ();    
  Warn.flagRacesFromSumms cg !cgDir;

  if !printAlias then begin
    L.logStatus "\n\nPrinting Alias assumptions used by warnings";
    L.logStatus "-----";
    L.flushStatus ();
    Warn.printAliasUses ()
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
        Cil.initCIL ();
        setGCConfig ();
        
        L.logStatus "Checking for data races (given summaries)\n";
        L.logStatus "-----\n";
        printWarnings ();
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in Print Warnings: " ^
                    (Printexc.to_string e) ^ "\n") ;
    printStatistics ();
    flush stdout;
    raise e
;;
main () ;;
