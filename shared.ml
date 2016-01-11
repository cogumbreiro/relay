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


(** Query a very simple sharing/escape analysis. Basically just checks
    the alias analysis for reachability from thread-root-formals / 
    globals. Doesn't require actual usage within more than one thread.
*)

open Cil
open Logging
open Fstructs
open Callg
open Scope

module CLv = Cil_lvals
module Lv = Lvals
module A = Alias
module DC = Default_cache
module Stat = Mystats

(*************************************************
             Super coarse, scope-based 
**************************************************)

let varShareable vi =
  vi.vglob && not (Cil.isFunctionType vi.vtype)

let isGlobalDataVar vi =
  vi.vglob && 
    not ((Cil.isFunctionType vi.vtype) || (Trans_alloc.isAllocVar vi.vname))
    

(** Given an lval, determines if it is shareable and worth tracking.
    If so, then it will return the reason (in this case, the scope
    of the lval). Returns None if not worthy. *)
let isShareable (curFunc:Cil.fundec) (lval:Cil.lval) : scope option =
  match lval with
    (Var(vi), off) -> 
      if((vi.vglob) && 
           not (Cil.isFunctionType vi.vtype) &&
           not (Trans_alloc.isAllocVar vi.vname)) then   
        (* TODO: don't filter yet; we need it for the mod analysis *)        
        Some (SGlobal)
      else
        None
          
  | (Mem(ptrExp), off) ->
      try
        let vi = CLv.findBaseVarinfoExp ptrExp in
        if((vi.vglob) && 
             not (Cil.isFunctionType vi.vtype) &&
             not (Trans_alloc.isAllocVar vi.vname)) then
          (* TODO: don't filter yet; we need it for the mod analysis *)
          Some(SGlobal)
        else 
          (match (Ciltools.getIndex curFunc.sformals vi) with
             Some (n) -> Some (SFormal n)
           | None -> None
          )
      with CLv.BaseVINotFound ->
        None


(** Return scope indicating why a caller of the curFunc may be able to modify
    the given lval, or return None *)
let isShareableAbs (curFunc:Cil.fundec) lval : scope option =
  match Lv.getScopeParanoid curFunc lval with
    STBD
  | SFunc -> None
  | (SFormal _) as s -> 
      (match lval with 
         (Lv.CVar _, _) -> None (* the formal itself is local *)
       | _ -> Some (s))
  | SGlobal -> Some (SGlobal)
      

(************************************************************
   Less coarse but still coarse Flow-insensitive-AA-based 
************************************************************)

module Th = Threads

let threadActuals = ref []
let threadFormals = ref []

(* DEBUG counts *)
let reachFromActs = ref 0
let reachFromForms = ref 0
let reachFromGlob = ref 0

let printEscStats () =
  logStatusF "Reach from global %d" !reachFromGlob;
  logStatusF "Reach from actual %d" !reachFromActs;
  logStatusF "Reach from formal %d" !reachFromForms

let sameNode n1 n2 = 
  A.Abs.compare n1 n2 == 0

let addOnceNode ls n =
  List_utils.addOnceP sameNode ls n

let collectThreadActuals curList argExp =
  try
    (* Convert argument to the absNode representing its target
       (reachability is reflexive) *)
    let targs = A.Abs.deref_exp argExp in
    List.fold_left addOnceNode curList targs
  with A.UnknownLoc ->
    logError ("SH: no target for thread argument " ^ 
                (Cildump.string_of_exp argExp));
    curList 
      
let collectThreadFormals curList formalVar =
  let lval = (Var formalVar, NoOffset) in
  try
    let targ = A.Abs.getNodeLval lval in
    addOnceNode curList targ
  with A.UnknownLoc ->
    logError ("SH: not target for thread formal " ^
                (Cildump.string_of_lval lval));
    curList

let initEscapeable cg =
  (* Go through all the thread roots and gather their thread arguments *)
  let thCreators = Th.findTCCallers cg in
  let thActuals, thFormals = Th.getThreadActuals cg thCreators in
  threadActuals := List.fold_left collectThreadActuals [] thActuals;
  threadFormals := List.fold_left collectThreadFormals [] thFormals;
  logStatusF "SH: initEscapeable found (%d, %d) thread (acts, formals)\n\n" 
    (List.length !threadActuals) (List.length !threadFormals)

let debugEscapeableNode n =
  let rg = A.Abs.reachableFromG n in
  let ra = A.Abs.reachableFrom n !threadActuals in
  let rf = A.Abs.reachableFrom n !threadFormals in
  if rg then incr reachFromGlob;
  if ra then incr reachFromActs;
  if rf then incr reachFromForms;
  rg || ra || rf

let escapeableNode n : bool =
  A.Abs.reachableFromG n || 
    A.Abs.reachableFrom n !threadActuals ||
    A.Abs.reachableFrom n !threadFormals
    
let escapeableAbs aLv : bool =
  match aLv with
    Lv.CVar vi, _ -> 
      let vi = Lv.var_of_abs vi in
      isGlobalDataVar vi || 
        (Trans_alloc.isAllocVar vi.vname &&
           (try 
              let node = Lv.node_of_absLval aLv in
              escapeableNode node
            with A.UnknownLoc -> false)
        ) (* don't try the reach check w/ locals *)
  | Lv.CMem exp, _ -> 
      (try
         let targs = Lv.deref_absExp exp in
         List.exists (fun t -> escapeableNode t) targs
       with A.UnknownLoc-> false)
  | Lv.AbsHost n, _ -> 
      (* Hmm... PTA doesn't have the constraints from parameter
         passing for thread-creation sites when the code
         of the thread-creator isn't available. This is the case
         for Pthreads. Just say representatives escape for now. 
         TODO: check reachability from both formals and actuals 
      *)
      (*       escapeableNode n *)
      true
