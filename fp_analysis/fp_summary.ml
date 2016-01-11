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

(** Function summaries that track (input -> output) effects in FP analysis *)

open Cil
open Type_utils
open Fp_types
open Fp_lattice_ops
open Fp_store
open Fp_hashcons
open Logging


(************************************************************)

type sumval = {
  fpIn : fpState;
  fpOut : fpState;
}

(* Generic one just for doing lookups and removes *)
let retVar = FRet (addTyp (TVoid []))

let getRetValue state = 
  getBinding retVar state

let assignRetValue state retVar v = 
  addBinding state retVar v

let removeRetVal state =
  { state with bindings = VarMap.remove retVar state.bindings; }

let bottomSummary = { fpIn = bottomState; fpOut = bottomState; }

let isBottomSummary v = 
  isBottomState v.fpOut

let topSummary = { fpIn = topState; fpOut =topState; }
 
let isTopSummary v =
  isTopState v.fpOut

let makeUnknownSummary retTyp =
  (* TODO make TOP instead of just NFP?
     Or use the gen_default_val thing instead? But that could reference
     existing vars or generate new ones? ...  *)
  let retVar = FRet (addTyp retTyp) in
  let stWithRetNfp = assignRetValue emptyState retVar (FNFP lookupNfp) in
  { fpIn = stWithRetNfp; fpOut = stWithRetNfp; }

(* Hack: The only summary with a return value in input state... *)
let isWildcardSummary v =
  try
    let _ = getRetValue v.fpIn in
    true
  with Not_found ->
    false


(************ Wrap it up ***************)

module FPSum = struct

  type t = sumval

  type simpleSum = t

  let simplify v = { fpIn = hc_state v.fpIn; fpOut = hc_state v.fpOut; }

  let desimplify v =  { fpIn = hc_state v.fpIn; fpOut = hc_state v.fpOut; }
    
  let initVal = bottomSummary

  (* Hmm... may want the real type... *)
  let unknownSummary = makeUnknownSummary (TVoid [])

end

module FPS = Cache_sum.Make (FPSum)
  
let sum = new FPS.data 96 (Backed_summary.makeSumType "fpsum")
let () = Backed_summary.registerType sum

type sumdb = FPS.data
class data = FPS.data


(*********************************************************
 * Test / Debug code
 *********************************************************)

let printSummary sumdb sumkey =
  let v = sumdb#find sumkey in
  logStatus "FP Summary (in):";
  printState v.fpIn;
  logStatus "FP Summary (diff):";
  printState v.fpOut

(************************************************************)

(** Take k-levels of deref from the formals as the parts of the input state
    that are relevant to the context *)

let context_depth = ref 1

module VarQ = Queueset.Make(OrderedFVar)

(*
let getSumContext formals inState =
  let visDepth = VarH.create 11 in
  let work = VarQ.create () in
  let addDepth prevVar curVar =
    if VarH.mem visDepth curVar then ()
    else 
      let pvDepth = VarH.find visDepth prevVar in
      let nextDepth = pvDepth + 1 in
      if nextDepth > !context_depth then ()
      else
        (VarH.add visDepth curVar nextDepth;
         VarQ.addOnce curVar work)
  in
  let seedWork () =
    (* Add formals as roots *)
    List.iter 
      (fun var ->
         VarH.replace visDepth var 0;
         VarQ.addOnce var work
      ) formals;
    (* Also add any globals in the inState as roots (so that we don't go and
       say two inStates w/ different global vals are the same context! *)
    VarMap.iter 
      (fun var _ ->
         (* TODO: check this on HTTPD (can't find attrib on some dude *)
         if isGlobalDebug "getSumCont" false inState var then
           (VarH.replace visDepth var 0;
            VarQ.addOnce var work)) inState.bindings
  in
  let calcDepths () =
    while not (VarQ.is_empty work) do
      let curVar = VarQ.pop work in
      try
        let curVal = getBinding curVar inState in
        foldTargetsVal 
          (fun (var, _) () -> addDepth curVar var) curVal ()
      with Not_found ->
        if isGlobalDebug "calcDepths" false inState curVar then ()
        else failwith "calcDepths can't find binding for non-global"
    done
  in
  let extractRelevant () =
    let newStore, newAtts = 
      VarH.fold
        (fun var _ (curSt, curAtts) -> 
           try
             let v = getBinding var inState in
             let newSt = VarMap.add var v curSt in
             let newAtts = match var with
                 FHeap _ -> 
                   let att = VarMap.find var inState.hAttrs in
                   VarMap.add var att curAtts
               | _ -> curAtts in
             newSt, newAtts
           with Not_found ->
             if isGlobalDebug "extractRel" true inState var 
             then (curSt, curAtts)
             else failwith "extractRel can't find binding for non-global"
        ) visDepth (VarMap.empty, VarMap.empty) in
    { bindings = newStore; hAttrs = newAtts; }
  in
  seedWork ();
  calcDepths ();
  extractRelevant ()
*)
let getSumContext formals inState = inState


let makeSumKey (fkey, contextState) =
  Inout_summary.sumKeyOfKeyString fkey (rawstring_of_state contextState)


let makeTopSumKey fkey =
  (fkey, "top")


(************************************************************)


(** LUB of diffs (different from LUB of normal state) *)
let combineSummaries s1 s2 =
  let comboIn = combineStates s1.fpIn s2.fpIn in
  if isBottomState s1.fpOut then { s2 with fpIn = comboIn; }
  else if isBottomState s2.fpOut then { s1 with fpIn = comboIn; }
  else if isTopState s1.fpOut then { s1 with fpIn = comboIn; }
  else if isTopState s2.fpOut then { s2 with fpIn = comboIn; }
  else
    let o1 = makeOverride s1.fpIn s1.fpOut in
    let o2 = makeOverride s2.fpIn s2.fpOut in
    let comboOut = combineStates o1 o2 in
    { fpIn = comboIn; fpOut = diffState comboIn comboOut; }


let eqSummaries s1 s2 =
  eqStModRep s1.fpIn s2.fpIn && eqStModRep s1.fpOut s2.fpOut 

    
