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


(** Basic mod summaries (track what externally visible 
    writes occur in a function). *)

open Cil
open Pretty
open Fstructs
open Logging

module A = Alias
module Stat = Mystats
module Intra = IntraDataflow
module SPTA = Symex 
  (* hmm... centralize the choice in some Symex-chooser module? *)

(************************************************************
       An implementation -- Just a set of lvals
  TODO: refactor and separate interface from implementation
 ************************************************************)

module Lvs = Set.Make (Lvals.OrderedLval)

type state = Lvs.t

type summary = state

(* Provide a hash-cons'er for sets? *)


(****** Make a Singleton representing \bottom ********)

let bottomLval = Lvals.dummyLval

let bottomSet = Lvs.add bottomLval Lvs.empty

let bottomSummary = bottomSet

let emptySummary = Lvs.empty

let isBottom x =
  x == bottomSet || Lvs.equal bottomSet x

(****** Define + Instantiate the summary class *******)

module ModSumType = struct

  type t = summary

  type simpleSum = summary

  let simplify x = x (* TODO: have it do some hash-consing *)

  let desimplify x = x

  let initVal = bottomSummary

  let unknownSummary = emptySummary

end

module ModSum = Safer_sum.Make (ModSumType)

(*** Tack on the required getMods method ***)

let iterMods = Lvs.iter


class modSummary sumID = object (self)
  inherit ModSum.data sumID

  method findTestSum fkey =
    let sum = self#find fkey in
    if isBottom sum then
      raise Modsummaryi.BottomSummary
    else sum

  method getMods fkey = 
    let sum = self#findTestSum fkey in
    Lvs.fold 
      (fun lval cur ->
         let scope = Lvals.getScope lval in
         (lval, scope) :: cur
      ) sums []
        
  method getGlobalMods fkey =
    let sum = self#findTestSum fkey in
    Lvs.filter
      (fun lval ->
         match Lvals.getScope lval with
           SGlobal _ -> true | _ -> false) sum

  method getLocalMods fkey = 
    let sum = self#findTestSum fkey in
    Lvs.fold
      (fun lval cur ->
         let scope = Lvals.getScope lval in
         match scope with
           SFormal _ -> (lval, scope) :: cur
         | SGlobal _ 
         | STBD | SFunc -> cur) sums []

  method evictSummaries =
    self#evictSummaries
      
end

let sums = new modSummary (Backed_summary.makeSumType "mods")

let _ = Backed_summary.registerType (sums :> ModSum.data)


(************************************************************
       Computing the mod summaries
 ************************************************************)

(** state lattice operations for mods *)
class modStateLattice : [state, state] Intra.stateLattice  = object (self)

  val mutable thesums = (sums :> ModSum.data)

  method sums = thesums

  method setTheSums newSumDB =
    thesums <- newSumDB

  (***** Special values of the state *****)

  method bottom = bottomSet
    
  method initialState = Lvs.empty
  method setInitialState (st:state) = 
    failwith "Modsummary's initial state should be constant"

  method isBottom st =
    isBottom st

  method stateSubset st1 st2 =
    if st1 == st2 then true
    else if isBottom st1 then true
    else Lvs.subset st1 st2
    
  method combineStates st1 st2 =
    Lvs.union st1 st2
            
  (***** Debugging *****)
 
  method printState st =
    let doc = 
      seq_to_doc 
        (text ", ")
        Lvs.iter
        (fun lv -> text (Lvals.string_of_lval lv))
        st
        Pretty.nil in
    logStatusD doc;
    logStatus "\n"

  method printSummary fkey =
    let theSum = sums#find fkey in
    logStatus "Mod summary:";
    self#printState theSum

end


(** transfer functions for mods *)
class modTransfer stMan = object (self) 
  inherit ([state] Intra.idTransFunc stMan) as super
  inherit [state] Intra.inspectorGadget stMan "mods"
    
  method addWrite lv st =
    Lvs.add lv st

  (** Add any mods to the state. Eagerly filters lvals that should 
      be considered *)
  method handleAssignLeft (lv:Cil.lval) inSt =
    match lv with
      (* [COPY]  x = newVal *)
      (Var(vi) as host, off) ->
        if Shared.varShareable vi then
          let newOff, _ = Cil_lvals.canonicizeOff off in
          self#addWrite (Lvals.abs_of_lval (host, newOff)) inSt
        else
          inSt
            
    (* [STORE] *e = newVal *)
    | (Mem(ptrExp),_) -> begin
        let pp = getCurrentPP () in
        let mustPt, targets = SPTA.derefLvalAt pp lv in

        (* Add correlation regardless of must/may point to status *)
        List.fold_left 
          (fun curSt curLv ->
             match Shared.isShareableAbs self#curFunc curLv with
               None -> curSt
             | Some(scope) ->
                 self#addWrite curLv curSt
          ) inSt targets
      end

  (** Analyze assignment instructions of the form [lv := exp] *)
  method handleAssign lv exp loc inSt =
    Dataflow.Done (self#handleAssignLeft lv inSt)

  (** Analyze the return value of a function call [lv := callexp(acts)] *)
  method handleCallRet lval targs callexp args loc inSt =
    self#handleAssignLeft lval inSt

  method foldSummaryWrites actuals sumLv curWrites =
    (* curWrites *)
    failwith "TODO"

  (** Accumulate the writes from callee *)
  method findApplySum actuals key curState =
    let sumWrites = stMan#sums#find key in
    let apply = self#foldSummaryWrites actuals in
    let applied = Lvs.fold apply sumWrites curState in
    applied
      
  (** Analyze the actual call *)
  method handleCallExp targs callexp args loc inSt =
    List.fold_left 
      (fun curState sumKey ->
         self#findApplySum args sumKey curState 
      ) inSt targs


end

let stLattice = new modStateLattice
let transFunc = new modTransfer stLattice

let scopeMods curFunc st =
  let scopePruned = Lvs.filter 
    (fun lv ->
       match Shared.isShareableAbs curFunc lv with
         None -> false | Some _ -> true
    ) st in
   scopePruned 

(** Expected dataflow analysis interface *)
module ModTransfer = struct

  (** Debug settings for the CIL dataflow framework *)
  let debug = false

  (** Name of the analysis given to CIL *)
  let name = "mod analysis"

  (** The type of state tracked at each program point (possibly bottom) *)
  type st = state

  type sum = st

  (** an instance of the class/interface for operations on state *)
  let stMan = stLattice

  (** an instance of the class/interface for transfer function operations *)
  let transF = ref (transFunc :> st Intra.transFunc)


end

module ModIntraProc = IntraDataflow.FlowInsensitive (ModTransfer)
  (* TODO: need to know if program points are reachable before adding writes? *)


(** Package up the mods analysis *)
class modAnalysis = object (self) 
  
  method setInspect yesno =
    (* TODO: make this work if the flow function is changed *)
    transFunc#setInspect yesno

  method isFinal fk = 
    false
      
  method compute funID cfg : unit =
    (* TODO: get rid of input *)
    let input = Lvs.empty in
    ModIntraProc.initialize funID cfg input;
    logStatus "doing mods analysis";
    flushStatus ();
    Stat.time "mods analysis: " ModIntraProc.compute cfg

  method summarize fkey cfg =
    if self#isFinal fkey then
      false
    else begin
      let newSummary = ModIntraProc.getOutState () in
      Intra.checkupSummary fkey cfg newSummary 
        stLattice#sums#find
        stLattice#isBottom
        stLattice#stateSubset
        stLattice#combineStates
        scopeMods
        stLattice#sums#addReplace
        transFunc#inspect
        stLattice#printState;
    end
end
