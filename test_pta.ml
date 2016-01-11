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

(** Test the pta (post-compile phase) *)

open Gc_stats
open Readcalls
open Cil
open Pretty
open Fstructs
open Stdutil
open Cilfiles

module PTA_Steens = Pta_fi_eq
module PTA_Ander = Pta_fs_dir
module PLT = Pta_link_test
module PTFP = Pta_fp_test
module FC = Filecache
module DC = Default_cache
module L = Logging

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let printResults = ref false

let printVars = ref false

let printFP = ref false

let printFTypes = ref false

let noRestart = ref false

let printConstraints = ref false

let doSteens = ref false

let doAnders = ref false

let filterTypes = ref true

let whyBulk = ref ""

let inspectAlias = ref []
let inspectPtsTo = ref []

(* Command-line argument parsing *)

let anonArgFun (arg:string) : unit =
  ()

let setRecurseLimit n =
  PLT.CheckAnders.setLimit n

let usageMsg = getUsageString "-cg dirname [options]\n"
  
let rec argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-ps", Arg.Set printResults, "print pts to sets");
   ("-pv", Arg.Set printVars, "print var id -> varname / decl mappings");
   ("-pf", Arg.Set printFP, "print function pointer mappings");
   ("-pft", Arg.Set printFTypes, "print function types");
   ("-pc", Arg.Set printConstraints, "print constraints");
   ("-nr", Arg.Set noRestart, "don't restart -- use old results");
   ("-s", Arg.Set doSteens, "test Steensgaard"); 
   ("-a", Arg.Set doAnders, "test Andersen");
   ("-nt", Arg.Clear filterTypes, "don't filter funcs w/ type");
   ("-wa", Arg.String whyAliased, "check why ptrs alias: 'sc1:lv1/sc2:lv2'");
   ("-wp", Arg.String whyPoints, "check why ptr pts to targ: 'sc1:ptr/sc2:t");
   ("-o", Arg.Set_string PLT.outDir, "write output of why XYZ to given dir");
   ("-wb", Arg.Set_string whyBulk, "answer a batch of queries from file");
   ("-lim", Arg.Int setRecurseLimit, "limit recursive explaination");
  ]

and parseLvPair lvpair =
  match Str.split (Str.regexp "/") lvpair with
    [lv1; lv2] -> (lv1, lv2)
  | _ -> 
      begin 
        Arg.usage argSpecs usageMsg;
        exit 1
      end 
        
and whyAliased lvpair =
  let pair = parseLvPair lvpair in
  inspectAlias := addOnce !inspectAlias pair

and whyPoints lvpair =
  let pair = parseLvPair lvpair in
  inspectPtsTo := addOnce !inspectPtsTo pair


(***************************************************)
(* Run                                             *)

let printHeaderDoAnalysis name analyzeAll =
  L.logStatusF "Testing %s\n===============================\n" name;
  L.logStatus "Loading / linking PTA info";
  analyzeAll !cgDir (not !noRestart);
  L.logStatus "analysis pass done"

let testSteens () =
  if !doSteens then begin
    (* Either solve normally and print normal stats, or do
       the inspect solve, and print those results *)
    if (!inspectAlias <> [] 
        || !inspectPtsTo <> []
        || !whyBulk <> "") then begin
      printHeaderDoAnalysis "Steens-Test" PLT.CheckSteens.analyzeAll;

      List.iter PLT.CheckSteens.whyAliased !inspectAlias;
      List.iter PLT.CheckSteens.whyPointsTo !inspectPtsTo;
      PLT.BulkSteens.bulkQuery !whyBulk;

    end
    else begin
      printHeaderDoAnalysis "Steensgaard" PTA_Steens.analyzeAll;
      
      
      if (!printResults) then
      PTA_Steens.printPtsToSets ()
      ;
      
      if (!printFP) then begin
        let tester = new PTFP.fpTestDriver 
          (new PTA_Steens.fpTest) (new PTFP.cilFPTest) in
        tester#testFunPtrs !cgDir
      end;
      
    end;

    printStatistics ();
  end

      
let testAnders () =
  if !doAnders then begin
    (* Either solve normally and print normal stats, or do
       the inspect solve, and print those results *)
    if (!inspectAlias <> [] 
        || !inspectPtsTo <> []
        || !whyBulk <> "") then begin
      printHeaderDoAnalysis "Anders-Test" PLT.CheckAnders.analyzeAll;

      if !printResults then
        PTA_Ander.DebugSolver.printPtsToSets () 
          (* TODO: this is still not the same solver in above analyzeAll... *)
      ;

      PLT.CheckAnders.resetStats ();
      List.iter PLT.CheckAnders.whyAliased !inspectAlias;
      List.iter PLT.CheckAnders.whyPointsTo !inspectPtsTo;
      PLT.CheckAnders.printStats ();

      PLT.BulkAnders.bulkQuery !whyBulk;

    end
    else begin

      printHeaderDoAnalysis "Andersen" PTA_Ander.analyzeAll;

      if (!printResults) then
        PTA_Ander.DebugFinal.printPtsToSets ()
      ;

      if (!printFP) then begin
        let tester = new PTFP.fpTestDriver 
          (new PTA_Ander.fpTest) (new PTFP.cilFPTest) in
        tester#testFunPtrs !cgDir
      end;
      
      (*** DEBUG ***)
      PTA_Ander.checkAllTargets ();
      
    end;
    
    printStatistics ();
  end

let init () = begin
  Cil.initCIL ();
  setGCConfig ();
        
  DC.makeLCaches !cgDir;
  Cilinfos.reloadRanges !cgDir;

  PTA_Steens.setFilter !filterTypes;
  PTA_Ander.setFilter !filterTypes;

  Random.self_init ();
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
        init ();
        if (!printVars) then
          Pta_compile.printVarIDs !cgDir
        ;

        if (!printFTypes) then
          Pta_compile.printFunTypes ()
        ;

        if (!printConstraints) then
          Pta_compile.printConstraints !cgDir
        ;
        
        testSteens ();
        testAnders ();
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in test_pta: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    flush stdout;
    raise e
;;
main () ;;
