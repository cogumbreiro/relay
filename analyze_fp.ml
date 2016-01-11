(*
  Copyright (c) 2008-2009, Regents of the University of California

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

(** Driver for the function pointer analysis. *)

open Gc_stats
open Callg
open Scc_cg
open Fstructs
open Faddr_taken
open Cilinfos
open Stdutil 
open Fp_types
open Fp_agg_merge
open Globals_ref
open Logging

module BS = Backed_summary
module I = Inspect
module DC = Default_cache
module Ana = Fp_analysis

(***************************************************)
(* Commandline handling                            *)

(** directory containing pre-processed call graph info *)
let cgDir = ref ""

let cgFile = ref ""

let configFile = ref "client.cfg"

let userName = ref "xyz"

let restart = ref false

let printsum = ref ""

let outFile = ref ""

let contextLimit = ref (-1)

let restartLimit = ref (-1)

let mergeEquivCon = ref true

let noAggressiveMerge = ref false

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap");
   ("-i", Arg.String 
      I.inspector#addInspect, "inspect state of function (given name)");
   ("-u", Arg.Set_string userName, "username to use");
   ("-r", Arg.Set restart, "restart analysis");
   ("-ps", Arg.Set_string printsum, "print summary stored in given file");
   ("-hcm", Arg.Set_int Fp_types.heapMax, "max level of heap cloning");
   ("-offw", Arg.Set Fp_types.offsetOfWeaken, "weaken based on offsetof data");
   ("-time", Arg.Set Mystats.doTime, "print timing profile data");
   ("-cd", Arg.Set_int Fp_summary.context_depth, 
    "deref-depth used for context");
   ("-o", Arg.Set_string outFile, "where to dump the more-precise call graph");
   ("-con", Arg.Set_int contextLimit, "limit number of contexts to k");
   ("-res", Arg.Set_int restartLimit, "limit number of function restarts");
   ("-necon", Arg.Clear mergeEquivCon, 
    "do not merge contexts that are equiv. modulo data");
   ("-nam", Arg.Set noAggressiveMerge, "do not do aggressive merges");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg astdir -o outfile [options] \n"

(***************************************************)
(* Run                                             *)        

(*
let matchCallFile () =
  let cgRegex = Str.regexp "calls.*" in
  let files = Sys.readdir !cgDir in
  match Array.fold_left 
    (fun cur fname ->
       match cur with 
         Some _ -> cur
       | None -> 
           if Str.string_match cgRegex fname 0
           then Some (Filename.concat !cgDir fname)
           else None
    ) None files with 
      None -> failwith "can't find (approx) callgraph file"
    | Some f -> 
        cgFile := f;
        logStatusF "Using (approx) callgraph file: %s\n" f
*)

let pickContextMatcher cg sccCg settings =
  if !mergeEquivCon then begin
    Fp_intraproc.conLimiter :=
      new Fp_context_lim.equivModFPContext !contextLimit settings cg sccCg
  end 
  else Fp_intraproc.conLimiter := new Fp_context_lim.noLimitContext;
  if !restartLimit == -1 then Fp_context_lim.set_restartLim max_int
  else Fp_context_lim.set_restartLim !restartLimit
    
          
let initSettings () =
  Cilinfos.reloadRanges !cgDir;
  let settings = Config.initSettings !configFile in
  DC.makeLCaches (!cgDir);

  (* Just using this old cg file to get the list of functions w/ bodies *)
  Alias.initSettings settings !cgDir;
  let cgFile = Dumpcalls.getCallsFile !cgDir in
  let cg = Callg.readCalls cgFile in
  let sccCg = Scc_cg.getSCCGraph cg in
  let () = BS.init settings !cgDir cg sccCg in
  Ana.initSettings settings cg; 
  (* cg for the list of functions, not for callees *)
  
  (* Locally check generation num instead of asking server *)
  let gen_num = Gen_num.getNum "gen_num.txt" in
  if ( !restart ) then begin
    logStatus "trying to clear old summaries / local srcs, etc.";
    flushStatus ();
    BS.clearState gen_num;
  end;

  (* Command line args specific to this analysis *)
  pickContextMatcher cg sccCg settings;
  
  if not (!noAggressiveMerge) then begin
    myMerger := (new trackedMerger cg sccCg settings :> unifier)
  end;
  cg


let printWithBars printer =
  let bars = "===================================" in
  logStatus bars;
  printer ();
  logStatus bars

let finalPrint () =
  logStatusF "\n!currentLoc before exit: %s\n\n" 
    (Cildump.string_of_loc !Cil.currentLoc);
  printStatistics ()


(************************************************************)


let printSummary fname =
  (* Try matching extension w/ any of the known summary types *)
  let ext = get_extension fname in
  let extFree = Filename.chop_extension fname in
  if ext = (BS.string_of_sumType globalSum#sumTyp) then
    (logStatus "Printing globals summary:";
     let gsum = globalSum#getFromFile extFree in
     VarMap.iter 
       (fun var gv ->
          printVar var; 
          printVal var gv.gVal;
       ) gsum)
  else if ext = (BS.string_of_sumType Ana.InterDF.fpinfoSums#sumTyp) then
    (logStatus "Printing dataflow info summary:";
     let gsum = Ana.InterDF.fpinfoSums#getFromFile extFree in
     Ana.InterDF.printFpinfoSums gsum;)
  else if ext = (BS.string_of_sumType Fp_summary.sum#sumTyp) then
    (logStatus "Printing fp in/out summary:";
     let sum = Fp_summary.sum#getFromFile extFree in
     printState sum.Fp_summary.fpIn;
     printState sum.Fp_summary.fpOut;)
  else failwith "TODO: print other summary types"

(************************************************************)    

let doCleanup () =
  Fp_intraproc.printFPCalls (); (* at least get some of the results *)
  Stat.print stdout "FPA:";
  finalPrint ()

exception SigInt

(** Make ctrl-c by doing a more normal exit *)
let () = Sys.catch_break true
  
module PrMalloc = Prune_mallocs.PruneMallocs
  
let main () = 
  Arg.parse argSpecs anonArgFun usageMsg;
  (* Didn't know how to require the -cg file, etc., so check manually *)
  if (!cgDir = "" || !outFile = "") then begin
    Arg.usage argSpecs usageMsg;
    exit 1
  end else begin
    combineLogs ();
    Stdutil.printCmdline ();
    Cil.initCIL ();
    Pervasives.at_exit doCleanup;

(*    matchCallFile (); *)

    let cg = initSettings () in 

    if !printsum <> "" then begin
      printSummary !printsum
    end else begin
      logStatus "Running FP Analysis";
      logStatus "-----";
      let addrTk = getFAddrTaken !cgDir in
      printAddrTaken addrTk;
      let addrTk = toSet addrTk in
      let cg = PrMalloc.pruneMallocFuns addrTk cg in
      let () = Ana.InterDF.compute !cgDir addrTk cg in
      Fp_cg_construct.buildCSCG !cgDir (Filename.concat !cgDir !outFile)
    end;
    exit 0
  end

;;
main () ;;
