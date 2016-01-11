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

(** Pre-pass to make IDs unique in files, annotate scopes,
    run the PTA, and make the callgraph file in terms of the unique 
    function IDs.
    TODO: May want to separate the different phases into different
    executables, but right now we just use command-line options
*)

open Gc_stats
open Cil
open Pretty
open Fstructs
open Stdutil

module L = Logging
module DC = Default_cache
module A = Alias

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let outDir = ref ""

let idFix = ref true

let pta = ref true

let doCG = ref true

let filterFunSig = ref true

let configFile = ref "client.cfg"


(* Command-line argument parsing *)
  
let argSpecs =
  [("-cg", Arg.Set_string cgDir, "source of call graph data (asts, etc)");
   ("-o", Arg.Set_string outDir, "optional output directory");
   ("-ng", Arg.Clear doCG, "do not generate the call graph");
   ("-ni", Arg.Clear idFix, "do not fix ids");
   ("-np", Arg.Clear pta, "do not generate PTA info");
   ("-ns", Arg.Clear Pta_compile.simplify, 
    "if doing PTA, don't simplify constraints");
   ("-nt", Arg.Clear filterFunSig, "don't filter fp calls by fun-sig/type");
   ("-su", Arg.Set_string configFile, "set config / summary bootstrap file");
   ("-nfpf", Arg.Set Filter_dumpcalls.dropFPFields, 
    "drop call edges from function pointer fields");
   ("-nfp", Arg.Set Filter_dumpcalls.dropFP, "drop fp calls period");
   ("-fpc", Arg.Set_string Filter_dumpcalls.filterClasses,
    "specify a file w/ function pointer classes -- use to filter");
   ("-fpcs", Arg.Set Filter_dumpcalls.strictClasses,
    "when filtering by FP class, give classless funcs own class");
   ("-fb", Arg.Set Filter_dumpcalls.fieldBased,
    "filter FPs based on an unsound field-based analysis");
   ("-cb", Arg.Set Filter_dumpcalls.dropCallbacks,
    "filter callbacks");
  ]


let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg desiredCgfile [options]\n"


(***************************************************)
(* Run                                             *)


(** Initialize watchlist of special functions (e.g., pthread_create) *)
let initSettings () = begin
  if !outDir = "" then
    outDir := !cgDir;
  let settings = Config.initSettings !configFile in
  A.initSettings settings !cgDir;
  A.setFilterFunSig !filterFunSig;
end



(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" ) then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Cil.initCIL ();
        setGCConfig ();

        if (!idFix) then begin
          (* Do the scoping first so that varinfos are updated before we
             dump the varinfo index (happens as part of updating IDs)... *)
          Scope.doAnnotateScope !cgDir;
          Id_fixer.ensureUniqueIDs !cgDir;
        end;
        
        (* TODO: split these up, but ensure that they happen in this order *)
        DC.makeLCaches (!cgDir);
        Cilinfos.reloadRanges !cgDir;
        if (!pta) then begin
          L.logStatus "Pre-pass analysis of pta constraints";
          L.flushStatus ();
          (* Pta_compile.analyzeAll !cgDir; *)
          Pta_compile.analyzeAllInOne !cgDir;
          L.logStatus "Pre-pass done";
          L.flushStatus ();
        end;
        
        if (!doCG) then begin
          (* Do this after the PTA pre-pass to choose which PTA to initialize *)
          initSettings ();
          (* Finally, dump the call graph based on the PTA results *)
          L.logStatus "Preparing call graph file\n-------";
          L.flushStatus ();
          Filter_dumpcalls.pickFilter !cgDir;
          Dumpcalls.setDumpTo !outDir;
          Dumpcalls.writeCallgraph !cgDir;
        end;
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in fix_id_cg: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
