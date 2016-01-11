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
open Scope
open Fstructs
open Callg

module L = Logging
module CLv = Cil_lvals
module Lv = Lvals
module A = Alias
module Th = Threads
module DC = Default_cache

(*************************************************
             Super coarse, scope-based 
**************************************************)

let varShareable vi =
  vi.vglob && not (Cil.isFunctionType vi.vtype)


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
          (match (CLv.getIndex curFunc.sformals vi) with
             Some (n) -> Some (SFormal n)
           | None -> None
          )
      with CLv.BaseVINotFound ->
        None


(** For abs lvals *)
let isShareableAbs (curFunc:Cil.fundec) lval : scope option =
  match Scope.annotScope curFunc lval with
    STBD
  | SFunc -> None
  | s -> Some (s)


(*************************************************
     Less coarse but still coarse FI-AA-based 
**************************************************)

let threadFormals = ref []

let gatherFormals curList threadFunc =
  match threadFunc.sformals with
    [formal] -> 
      (* Convert formal to the absNode representing its target *)
      (try 
         let targs = Lv.deref_absLval 
           (Lv.CVar formal, NoOffset) in
         List.rev_append targs curList
       with 
         Not_found 
       | A.UnknownLoc ->
           L.logError ("SH: No ptNode for: " ^ formal.vname ^ " in: " ^ 
                         threadFunc.svar.vname ^ "\n");
           curList
      )
  | [] -> curList
  | _ -> 
      L.logError ("SH: Thread root " ^ threadFunc.svar.vname ^ 
                    " has more than one formal\n");
      curList
        


let initEscapebale cg =
  (* Go through all the thread roots and gather their thread arguments *)
  let thCreators = Th.findTCCallers cg in
  let thRoots = Th.getThreadRoots cg thCreators in
  threadFormals := 
    FSet.fold
      (fun fk curList ->
         try 
           let finfo = FMap.find fk cg in
           let ast = !DC.astFCache#getFile finfo.defFile in
           match Cilinfos.getCFG fk ast with
             Some func -> 
               gatherFormals curList func
           | _ ->
               L.logError ("SH: No CFG for thread root:" ^ 
                             finfo.name ^ "\n");
               curList
         with Not_found ->
           L.logError ("SH: No call graph node for thread root:" ^ 
                         (string_of_fkey fk) ^ "\n");
           curList
      ) thRoots []


let escapeableAbs aLv : bool =
  let escapeableNode n : bool =
    A.Abs.reachableFrom n !threadFormals
    || A.Abs.reachableFromG n
  in
  match aLv with
    Lv.CVar vi, _ -> varShareable vi
  | Lv.CMem exp, _ -> 
      (try
         let targs = Lv.deref_absExp exp in
         List.exists (fun t -> escapeableNode t) targs
       with 
         Not_found 
       | A.UnknownLoc->
         false
      )
  | Lv.AbsHost n, _ -> 
      escapeableNode n

