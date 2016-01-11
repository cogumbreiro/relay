(*
  Copyright (c) 2007-2008, Regents of the University of California

  Authors: Jan Voung, Ravi Chugh
  
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



(** Extensions of the [IntraDataflow] module to handle basic
    dataflow analysis for state using [Relative_set] *)

open Relative_set
open Logging
open Cildump

module Intra = IntraDataflow
module A = Alias
module Lv = Lvals
module Stat = Mystats
module SPTA = Symstate2
module LS = Lockset
module BS = Backed_summary

(********** Simple, partially-defined, relative state managers **********)
  

(** Input module for extending a stateLattic / manager with relative
    state operations based on [Relative_set] *)
module type ComposedRel = sig

  type st

  type relSt (* How to enforce that relSt is a relSet? *)

  val projRel : st -> relSt

  val injRel : st -> relSt -> st

  (** The class [c] has basic operations on the state. [c] be extended 
      with relative state operations by the next module 
      (@see MakeRelStateManager). *)
  class c : [st, st] Intra.stateLattice

end

(** Functor for creating an extended version of stateLattice with
    the additional operations that make it a relativeState *)
module MakeRelStateManager (R:Relative_set.S) (C:ComposedRel) = struct

  type st = C.st

  type relSt = R.relSet

  type info = R.value

  class rc = object
    inherit C.c

    (***** Operations for relative state *****)
    method projRel = C.projRel
     
    method injRel = C.injRel

    method getPlus = R.getPlus

    method getMinus = R.getMinus

    method doPlus = R.doPlus
      
    method doMinus = R.doMinus

    method fold_rel
           : 'a . (R.key -> info -> 'a -> 'a) ->  info R.S.t -> 'a -> 'a = 
      R.S.fold

    method uniq_rel = R.unique

    (* rkc: Exposing some more operations from MapSet *)
    (* TODO Learn how these universals are actually working *)
    method mem : (R.key -> info R.S.t -> bool) = 
      R.S.mem

    method cardinal : (info R.S.t -> int) =
      R.S.cardinal

    method find : (R.key -> info R.S.t -> info) =
      R.S.find

    method iter : ((R.key -> info -> unit) -> info R.S.t -> unit) =
      R.S.iter

  end

end


(*************** Simple transfer functions ***************)
  
(** Basic class providing a transfer function for handling function calls
    with relative state. Assumes the elements (keys) of the 
    relative sets are {!Lv.aLval} (it is restricted by the interaction
    with Symstate2 for substitution) *)
class ['state, 'relSt, 'part, 'info] relTransFunc 
  (stMan : ('state, 'relSt, 'part, Lv.aLval, 'info, 'state) Intra.relativeState) 
  = object (self) 
    inherit ['state] Intra.idTransFunc (stMan :> ('state, 'sum) Intra.stateLattice)
      
    (* TODO: see if we can hide relState info types? *)
      
    (* TODO rkc: I made these methods private, and renamed multiple uses
       of curState within the same functions.  make sure this still
       does the right thing. *)
      
    (** Apply one update on [curState] based on a "plus" element of the 
        function summary for the call at the given [callSite]. The summary
        says that [lv] and [info] are in the final "plus" set *)
    method private doPlusSum actuals callSite lv (info:'info) (curSt:'relSt)
        : 'relSt =
      let pp = Cil.getCurrentPP () in
      let mustAlias, subbedLvs = SPTA.substActForm2 pp actuals lv in
      (* Only update if mustAlias... how to generalize soundness requirement? *)
      if (mustAlias) then (
        (if (List.length subbedLvs > 1) 
         then logError ("RS: summary lock maps to multiple @ " ^ 
                            (string_of_loc callSite))
        );
        List.fold_left 
          (fun curState subbedLv ->
             stMan#doPlus curState subbedLv info
          ) curSt subbedLvs )
      else
        curSt
          
    (** Apply one update on [curState] based on a "minus" element from
        the function summary for the call at the given [callSite]. *)
    method private doMinusSum actuals callSite lv (info:'info) (curSt:'relSt)
        : 'relSt =
      let pp = Cil.getCurrentPP () in
      let mustAlias, subbedLvs = SPTA.substActForm2 pp actuals lv in
      (* Always update, even if not a must alias relationship *)
      List.fold_left 
        (fun curState subbedLv ->
           stMan#doMinus curState subbedLv info
        ) curSt subbedLvs
        
    (** Apply all updates to the current relative state [curState] based
        one a function summary for the call at the given [callSite] *)
    method private applyRelSum actuals callSite diffSum (curState:'relSt)
        : 'relSt =
      let applyPlus = self#doPlusSum actuals callSite in
      let applyMinus = self#doMinusSum actuals callSite in
      let p = 
        stMan#fold_rel applyPlus (stMan#getPlus diffSum) curState in
      let pm = 
        stMan#fold_rel applyMinus (stMan#getMinus diffSum) p in
      (* Finally, hash-cons the result *)
      let finalSt = stMan#uniq_rel pm in
      finalSt
        
        
    method private applySum actuals loc (sumState:'st) (inState:'st) : 'st =
      (* Apply the summary only to the relative part of the state *)
      let relSum = stMan#projRel sumState in
      let relState = stMan#projRel inState in
      let newRel = self#applyRelSum actuals loc relSum relState in
      stMan#injRel inState newRel
        
    (** Partially handle function calls by only updating the relative state *)
    method handleCallExp funs callexp actuals loc (inState : 'st) =
      (* Handle each of the called functions (may be >1 if based
         on a function pointer) by merging the outputs of each call *)
      (List.fold_left 
         (fun curState fkey ->
            (* Find the output state for this call by assuming 
               each funptr call started with inState *)
            let newState =
              let sumState = stMan#sums#find fkey in
              if (stMan#isBottom sumState) then stMan#bottom
              else self#applySum actuals loc sumState inState
            in (* Summary should not be Not_found *)

            (* Finally, merge outcome with the rest of the results *)
            Stat.time "LS combineStates"
              (stMan#combineStates curState) newState
         ) stMan#bottom funs)

  end
  



(** Transfer function helper. This is useful if some component
    of the state involves relative locksets. 
    Assumes SPTA info is available for curFunc *)
class lockTransfer = object (self)

  (** Apply a summary that says the lock represented by
      formalLv has been acquired *)
  method applyLocked pp actuals lv lockInfo curLS =
    let mustAlias, subbedLvs = SPTA.substActForm2 pp actuals lv in
    (* Only update if mustAlias *)
    if (mustAlias) then begin
      List.fold_left 
        (fun curLS subbedLv ->
           LS.LS.doPlus curLS subbedLv lockInfo
        ) curLS subbedLvs 
    end
    else
      curLS


  (** Apply a summary that says the lock represented by formalLv
      has been released *)
  method applyUnlocked pp actuals lv lockInfo curLS =
    let mustAlias, subbedLvs = SPTA.substActForm2 pp actuals lv in
    (* Always update, even if not a must alias relationship *)
    List.fold_left 
      (fun curLS subbedLv ->
         LS.LS.doMinus curLS subbedLv lockInfo
      ) curLS subbedLvs


  (** Apply the lockDiff from another function *)
  method applyLDiff pp actuals lockDiff curLS =
    let appLock = self#applyLocked pp actuals in
    let appUn   = self#applyUnlocked pp actuals in
    let didLocked = 
      LS.LS.S.fold appLock (LS.LS.getPlus lockDiff) curLS in
    let didUnlocked = LS.LS.unique
      (LS.LS.S.fold appUn (LS.LS.getMinus lockDiff) didLocked) in
    didUnlocked


end
