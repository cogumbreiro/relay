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

(** Compare callgraphs from function pointer analyses. 
    Especially differences in fan-out within big SCCs *)

open Pretty
open Gc_stats
open Callg
open Scc_cg
open Fstructs
open Faddr_taken
open Stdutil 
open Type_utils
open Logging

module DC = Default_cache

type compareScope = CompareAll | CompareBiggest

type compareKind = Normal | NameOnly


(***************************************************)
(* Commandline handling                            *)

(** directory containing pre-processed call graph info *)
let cgDir = ref ""

let configFile = ref "client.cfg"

let conSensFile = ref "calls.fp_rci"

let conInsensFile = ref "calls.anders"

let nameOnly = ref false
let biggestOnly = ref false

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap");
   ("-con", Arg.Set_string conSensFile, "name of context-sensitive cg");
   ("-ci", Arg.Set_string conInsensFile, "name of context-insensitive cg");
   ("-name", Arg.Set nameOnly, "If only fun names match in callgraph");
   ("-big", Arg.Set biggestOnly, "If we should only check the biggest SCC");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg astdir [options] \n"

(***************************************************)
(* Run                                             *)        

let initSettings () =
  Cilinfos.reloadRanges !cgDir;
  DC.makeLCaches (!cgDir)


let printWithBars printer =
  let bars = "===================================" in
  logStatus bars;
  printer ();
  logStatus bars

(************************************************************)
(* "Normal" diffs where the callgraphs actually have detail
   about what is a FP call and what is a direct call as well
   as matching function IDs                                 *)

module NormalDiff = struct

  type diffs = {
    onlyFirst : callTarget list;
    onlySecond : callTarget list;
  }

  type which = First | Second
      (* "First" is CS, "Second" is CI *)

  let inverseW w = 
    match w with 
      First -> Second 
    | Second -> First

  type diffMap = diffs FMap.t

  let collectDiffs ciConCG ciCG funsToCheck : diffMap =
    let addADiff which fk diffCall diffs =
      let oldDiffs =
        try FMap.find fk diffs
        with Not_found -> { onlyFirst = []; onlySecond = []; } in
      let newDiffs = 
        match which with
          First -> { oldDiffs with onlyFirst
                       = List_utils.addOnce oldDiffs.onlyFirst diffCall; }
        | Second -> { oldDiffs with onlySecond
                        = List_utils.addOnce oldDiffs.onlySecond diffCall; } in
      FMap.add fk newDiffs diffs    
    in

    let collectDiff which fk calls diffs otherCall =
      match otherCall with
        CDirect _ -> 
          if not (List.mem otherCall calls) then begin
            logErrorF "Missing direct call in %s\n" (fid_to_string fk);
            addADiff (inverseW which) fk otherCall diffs
          end else diffs
      | CIndirect (pp, otherIndirect) ->
          try
            let matchedCall = List.find 
              (fun call ->
                 match call with CDirect _ -> false
                 | CIndirect (pp2, _) -> pp = pp2
              ) calls in 
            match matchedCall with
              CIndirect (_, thisIndirect) ->
                let diffList = List_utils.diff thisIndirect otherIndirect in
                if diffList = [] then diffs
                else addADiff which fk (CIndirect (pp, diffList)) diffs
                  
            | CDirect _ -> failwith "find returned direct call"
          with Not_found ->
            logErrorF "Missing indirect call in %s\n" (fid_to_string fk);
            addADiff (inverseW which) fk otherCall diffs
    in

    (** Diffs are maps from functions to a list of indirect callsites 
        w/ fewer targets in the CI vs the CS cg *)
    let diffs = FSet.fold
      (fun fk diffs ->
         try
           let ciNode = FMap.find fk ciCG in
           let csNode = FMap.find fk ciConCG in
           let ciCalls = calleeDetail ciNode in
           let csCalls = calleeDetail csNode in
           let diffs = 
             List.fold_left (collectDiff First fk csCalls) diffs ciCalls in
           List.fold_left (collectDiff Second fk ciCalls) diffs csCalls
         with Not_found ->
           failwith (fid_to_string fk ^ " NotFound?!")
      ) funsToCheck FMap.empty in
    diffs

  let getFunName cg fk = 
    try 
      let node = FMap.find fk cg in
      node.name
    with Not_found -> "???"

  let fidstring cg fid =
    getFunName cg fid ^ ":" ^ (fid_to_string fid)

  let fids_to_string cg fids =
    let doc = seq_to_doc (text ", ") List.iter
      (fun fid -> text (fidstring cg fid))
      fids nil in
      sprint 80 doc

  let diffs_to_string cg diffs =
    let doc = seq_to_doc (text ";\n") List.iter
      (fun callT ->
         match callT with
           CDirect (pp, fid) -> 
             dprintf "(Direct) %s at %s" (fidstring cg fid) 
               (Cil.string_of_pp pp)
         | CIndirect (pp, fids) ->
             dprintf "(Ind) {%s} at %s" (fids_to_string cg fids) 
               (Cil.string_of_pp pp)
      ) diffs nil in
    sprint 80 doc

  let reportDiffs (diffs : diffMap) cg =
    let enumerated = mapToList FMap.fold diffs in
    let sorted = enumerated in
    let total = List.fold_left
      (fun total (fk, diffs) ->
         logStatusF "Diffs for %s:\n" (fidstring cg fk);
         if diffs.onlyFirst <> [] then
           logStatusF " CS-ONLY [%s]\n" (diffs_to_string cg diffs.onlyFirst);
         if diffs.onlySecond <> [] then 
           logStatusF " CI-ONLY [%s]\n" (diffs_to_string cg diffs.onlySecond);
         logStatus "";
         total + 1
      ) 0 sorted in
    logStatusF "\nTOTAL diffs: %d\n" total
    
      
end

(************************************************************)
(* Name-only diffs. Where the callgraphs only have function
   names and no other detail                                *)

(* Only a 1-way diff (what is missing in the context-sens callgraph) *)
module NameOnlyDiff = struct

  type nameOnlyGraph = (string, string list) Hashtbl.t
  type nameOnlySet = (string, unit) Hashtbl.t
  type diffs = string list
  type diffMap = (string, diffs) Hashtbl.t

  let lookupName fid cg =
    let node = FMap.find fid cg in
    node.name

  (** Convert callgraph to finite map of String -> String list *)
  let toNameOnlyCG cg : nameOnlyGraph =
    let nameOnlyGraph = Hashtbl.create 17 in
    let addCallees key callees =
      try
        let old = Hashtbl.find nameOnlyGraph key in
        Hashtbl.replace nameOnlyGraph key (List_utils.union old callees)
      with Not_found -> Hashtbl.add nameOnlyGraph key callees
    in
    FMap.iter
      (fun fid node ->
         let myname = node.name in
         let calleeNames = 
           List.fold_left
             (fun newNames calleeK ->
                try List_utils.addOnce newNames (lookupName calleeK cg)
                with Not_found -> newNames
             ) [] (calleeKeys node) in
         addCallees myname calleeNames
      ) cg;
    nameOnlyGraph
      
  (** Convert func id set to a set of names only *)
  let toNameOnlyFSet fset origCG : nameOnlySet = 
    let nameOnlySet = Hashtbl.create 17 in
    FSet.iter
      (fun fid ->
         try 
           let name = lookupName fid origCG in
           Hashtbl.replace nameOnlySet name ()
         with Not_found -> ()
      ) fset;
    nameOnlySet


  let collectDiffs ciConCG ciCG funsToCheck : diffMap =
    let diffs = Hashtbl.create 17 in
    let addADiff fname diffCall =
      let oldDiffs =
        try Hashtbl.find diffs fname
        with Not_found -> [] in
      Hashtbl.replace diffs fname (List_utils.addOnce oldDiffs diffCall)
    in
    
    let allocRegexp = Str.regexp ".*alloc" in
    let mayBeAlloc caller callee =
      if Str.string_match allocRegexp callee 0 then begin
        logStatusF "Diff: %s -> %s // REASON malloc transform\n"
          caller callee;
        true
      end else false
    in

    let mayBeRenamed caller callee csCalls =
      let calleeRegexp = Str.regexp (callee ^ "___") in
      try
        let renamedCallee =
          List.find
            (fun otherCallee ->
               Str.string_match calleeRegexp otherCallee 0) csCalls in
        logStatusF "Diff: %s -> %s // REASON renamed to %s\n"
          caller callee renamedCallee;
        true
      with Not_found ->
        false
    in

    let collectDiff fname csCalls ciCall =
      if not (List.mem ciCall csCalls) then 
        if not (mayBeAlloc fname ciCall) && 
          not (mayBeRenamed fname ciCall csCalls) then 
          addADiff fname ciCall
    in

    let funs = toNameOnlyFSet funsToCheck ciCG in
    let ci = toNameOnlyCG ciCG in
    let con = toNameOnlyCG ciConCG in

    Hashtbl.iter
      (fun fname () ->
         try
           let ciCallees = Hashtbl.find ci fname in
           try
             let conCallees = Hashtbl.find con fname in
             List.iter (collectDiff fname conCallees) ciCallees
           with Not_found ->
             logErrorF "NotFound CON: %s\n" fname
         with Not_found ->
           logErrorF "NotFound CI: %s\n" fname           
      ) funs;
    diffs

  let diffs_to_string diffs = 
    let doc = seq_to_doc (text "; ") List.iter text diffs nil in
    sprint 80 doc

  let reportDiffs (diffs : diffMap) cg =
    let enumerated = mapToList Hashtbl.fold diffs in
    let sorted = enumerated in
    let total = List.fold_left
      (fun total (fname, diffs) ->
         logStatusF "Diffs for %s: [%s]\n" fname (diffs_to_string diffs);
         total + 1
      ) 0 sorted in
    logStatusF "\nTOTAL diffs: %d\n" total

end


(************************************************************)

let compareCGs conSensFile conInsensFile compareKind compareScope =
  let conCG = readCalls conSensFile in
  let ciConCG = consSensToInsens conCG in
  
  let ciCG = readCalls conInsensFile in
  let ciSCC = Scc_cg.getSCCGraph ciCG in

  (*** DEBUG ***)
(*
  let dumpRawCG cg =
    FMap.iter
      (fun fk fn ->
         logStatusF "  %s: [%s]\n" fn.name 
           (NormalDiff.diffs_to_string cg (calleeDetail fn))
      ) cg
  in
  logStatus "Plain CS Callgraph\n==============================\n";
  dumpRawCG conCG;
  logStatus "CS Callgraph PROJECTED to CI\n==============================\n";
  dumpRawCG ciConCG;
  logStatus "CI Callgraph\n==============================\n";
  dumpRawCG ciCG;
*)

  (*** /DEBUG ***)
  
  let toInclude = match compareScope with 
      CompareBiggest -> Scc_cg.findBiggestScc ciSCC
    | CompareAll -> Scc_cg.allFunctions ciSCC
  in
  match compareKind with
    Normal -> 
      let diffs = NormalDiff.collectDiffs ciConCG ciCG toInclude in
      NormalDiff.reportDiffs diffs ciCG
  | NameOnly ->
      let diffs = NameOnlyDiff.collectDiffs ciConCG ciCG toInclude in
      NameOnlyDiff.reportDiffs diffs ciCG
        
(************************************************************)

let finalPrint () =
  logStatusF "\n!currentLoc before exit: %s\n\n" 
    (Cildump.string_of_loc !Cil.currentLoc);
  printStatistics ()

let doCleanup () =
  finalPrint ()

let main () = 
  Arg.parse argSpecs anonArgFun usageMsg;
  (* Didn't know how to require the -cg file, etc., so check manually *)
  if (!cgDir = "") then begin
    Arg.usage argSpecs usageMsg;
    exit 1
  end else begin
    combineLogs ();
    Stdutil.printCmdline ();
    Cil.initCIL ();
    Pervasives.at_exit doCleanup;

    let () = initSettings () in

    logStatusF "Comparing callgraphs from %s vs %s" !conSensFile !conInsensFile;
    logStatus "-----";
    let compScope = if !biggestOnly then CompareBiggest else CompareAll in
    let compKind = if !nameOnly then NameOnly else Normal in
    let conFile = Filename.concat !cgDir !conSensFile in
    let ciFile = Filename.concat !cgDir !conInsensFile in
    let () = compareCGs conFile ciFile compKind compScope in
    exit 0
  end

;;
main () ;;
