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
open Type_utils
open Fp_rci_types
open Fp_rci_unify
open Fp_rci_globals
open Logging

module BS = Backed_summary
module I = Inspect
module DC = Default_cache
module Ana = Fp_rci_anal

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

let testCIFI = ref false

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap");
   ("-i", Arg.String 
      I.inspector#addInspect, "inspect state of function (given name)");
   ("-u", Arg.Set_string userName, "username to use");
   ("-r", Arg.Set restart, "restart analysis");
   ("-ps", Arg.Set_string printsum, "print summary stored in given file");
   ("-hcm", Arg.Set_int Fp_rci_types.heapMax, "max level of heap cloning");
   ("-recm", Arg.Set_int Fp_rci_types.recsMax, "max # separate records");
   ("-offw", Arg.Set Fp_rci_types.offsetOfWeaken, 
    "weaken based on offsetof data");
   ("-noNFP", Arg.Clear Fp_rci_types.filterNFP, "Do not mark values as non-FP");
   ("-maxt", Arg.Set_int Fp_rci_lattice_ops.maxSameNonPoly, 
    "max # same-typed elems to assume exist along access paths");
   ("-maxp", Arg.Set_int Fp_rci_lattice_ops.maxPoly, 
    "max # (poly aka void *) elems to assume exist along access paths");
   ("-time", Arg.Set Mystats.doTime, "print timing profile data");
   ("-o", Arg.Set_string outFile, "where to dump the more-precise call graph");
   ("-con", Arg.Set_int contextLimit, "limit number of contexts to k");
   ("-res", Arg.Set_int restartLimit, "limit number of function restarts");
   ("-necon", Arg.Clear mergeEquivCon, 
    "do not merge contexts that are equiv. modulo data");
   ("-nam", Arg.Set noAggressiveMerge, "do not do aggressive merges");
   ("-noArtAlias", Arg.Clear Fp_rci_summary.doArtificialAlias, 
    "do not induce artificial aliases between similar types");
   ("-fsig", Arg.Clear Fp_rci_intraproc.noFilterFuncs, 
    "filter fp targets by func signature");
   ("-noSubsumeA", Arg.Clear Fp_rci_summary.subsumeAliases, 
    "Do not choose contexts that subsume earlier contexts in terms of aliases");
   ("-noFunkPtrA", Arg.Clear Fp_rci_types.nonUniformPtrArith,
    "Do not allow ptr arith on non-uniform offsets (warn)");
   ("-testCIFI", Arg.Set testCIFI, "test context-insens, field insens");
   ("-fullFI", Arg.Set Fp_rci_types.fullFI, "test field insens");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg astdir -o outfile [options] \n"

(***************************************************)
(* Run                                             *)        

          
let initSettings () =
  Cilinfos.reloadRanges !cgDir;
  let settings = Config.initSettings !configFile in
  DC.makeLCaches (!cgDir);

  (* Just using this old cg file to get the list of functions w/ bodies *)
  Alias.initSettings settings !cgDir;
  let cgFile = Dumpcalls.getCallsFile !cgDir in
  let cg = readCalls cgFile in
  let sccCG = Scc_cg.getSCCGraph cg in

  let () = BS.init settings !cgDir cg sccCG in
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
  if not (!noAggressiveMerge) then begin
    myMerger := (new trackedMerger cg sccCG settings :> unifier)
  end;
  cg


let printWithBars printer =
  let bars = "===================================" in
  logStatus bars;
  printer ();
  logStatus bars


(************************************************************)


let printSummary fname =
  (* Try matching extension w/ any of the known summary types *)
  let ext = get_extension fname in
  let extFree = Filename.chop_extension fname in
  if ext = (BS.string_of_sumType globalSum#sumTyp) then
    (logStatus "Printing globals summary:";
     let gsum = globalSum#getFromFile extFree in
     match gsum with
       Some info -> printVal info.gVal (Some (findTyp info.gType))
     | None -> logStatus "No global info"
    )
  else if ext = (BS.string_of_sumType Ana.InterDF.fpinfoSums#sumTyp) then
    (logStatus "Printing dataflow info summary:";
     let dfsum = Ana.InterDF.fpinfoSums#getFromFile extFree in
     Ana.InterDF.printFpinfoSums dfsum;)
  else if ext = (BS.string_of_sumType Fp_rci_summary.sum#sumTyp) then
    (logStatus "Dot-graph of in/side/out summary:";
     (* Parse the extFree part to get the funID *)
     failwith "TODO")
  else failwith "TODO: print other summary types"
    
(************************************************************)    

let finalPrint () =
  logStatusF "\n!currentLoc before exit: %s\n\n" 
    (Cildump.string_of_loc !Cil.currentLoc);
  printStatistics ()

let doCleanup () =
  Fp_rci_intraproc.printFPCalls (); (* at least get some of the results *)
  Stat.print stdout "FPA:";
  finalPrint ()

exception SigInt

(** Make ctrl-c by doing a more normal exit *)
let () = Sys.catch_break true
    
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
(*
      let cg = Prune_mallocs.pruneMallocFuns addrTk cg in
*)
      let () = Ana.InterDF.compute !cgDir addrTk cg in
      let outCgFile = Filename.concat !cgDir !outFile in
      Fp_rci_cg.buildCSCG cg !cgDir outCgFile;
      if !testCIFI then begin
        logStatus "ALSO dumping CI / FI results";
        Fp_rci_cg.buildCICG cg !cgDir outCgFile;
        Fp_rci_cg.buildFICICG cg !cgDir outCgFile;
      end;
    end;
    exit 0
  end

;;
main () ;;
