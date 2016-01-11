
(*
  Copyright (c) 2007-2008, Regents of the University of California

  Authors: Ravi Chugh
  
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


(** Definition and operations for relative null sets. *)


open Cil
open Relative_df
open Sym_types
open Logging

module LP = Lockset_partitioner
module Par = Pseudo_access
module AU = All_unlocks
module DF = Dataflow
module PPHash = Ciltools.PPHash
module Intra = IntraDataflow
module LSHash = Par.LSHash
module LvalHash = Par.LvalHash


let inspect = ref false


type paSafetyCheck =
  | PaFound of Par.paKeyHead * bool (* true for safe or false for unsafe *)
  | PaNotFound of Lvals.aLval
  | PaNotFoundPlaceHolder

module BreadCrumbs = Set.Make(
  struct
    type t = paSafetyCheck
    let compare = Pervasives.compare
  end
)

let emptyCrumbs = BreadCrumbs.empty


module A = struct

  type key = Lvals.aLval

  type value = BreadCrumbs.t

  let compareK a b = Lvals.compare_lval a b

  let compareV a b = Some (BreadCrumbs.compare a b)

  let combineV a b = BreadCrumbs.union a b 
    (* TODO test memory *) (*BreadCrumbs.singleton (BreadCrumbs.min_elt b)*)
                           (*BreadCrumbs.empty*)

  let equalV a b = BreadCrumbs.equal a b

  let hashK = Lvals.hash_lval

  let uniqueK a = Lvals.mergeLv a

end

module RNS = Relative_set.Make(A)

type state = RNS.relSet option
type sumval = state


module NullSummary_ = struct

  type t = state

  type simpleSum = t

  let simplify s = s

  let desimplify s =
    match s with
      None -> None
    | Some rns -> Some (RNS.unique rns)

  let initVal = None

  let unknownSummary = Some (RNS.empty)

end

module NullSummary = Safer_sum.Make (NullSummary_)

let sums = new NullSummary.data (Backed_summary.makeSumType "rns")

let _ = Backed_summary.registerType sums

exception IllegalProject

(************************************************)
      
let scopeLval (f:Cil.fundec) lv (_) (curRns) =
  match Shared.isShareableAbs f lv with
    None -> RNS.S.remove lv curRns
  | _    -> curRns


let scopeRns2 (f:Cil.fundec) (rns: RNS.relSet) : RNS.relSet = 
  let oldPlus = RNS.getPlus rns in
  let newPlus = RNS.S.fold (scopeLval f) 
    oldPlus oldPlus in
  let oldMinus = RNS.getMinus rns in
  let newMinus = RNS.S.fold (scopeLval f) 
    oldMinus oldMinus in
  RNS.unique (RNS.composeNew newPlus newMinus)
 

(**************************************************)


(* TODO: RS should be renamed *)
module RS = struct

  type relSt = RNS.relSet 

  type st = relSt option

  let projRel (a:st) : relSt=
    match a with
      | None -> raise IllegalProject
      | Some aa -> aa

  let injRel a b = Some (RNS.unique b)
  
  let string_of_rns rns =
    let f = RNS.S.fold 
      (fun lv _ acc ->
         acc ^ (Lv.string_of_lvscope lv) ^ " ") in
    Printf.sprintf "N+ = {%s} N- ={%s} (%d,%d)"
      (f (RNS.getPlus rns) " ")
      (f (RNS.getMinus rns) " ")
      (RNS.S.cardinal (RNS.getPlus rns))
      (RNS.S.cardinal (RNS.getMinus rns))
      
  let string_of_rns_noscope rns =
    let f =
      RNS.S.fold
        (fun lv _ acc ->
           acc ^ Lvals.string_of_lval lv ^ " ") in
    Printf.sprintf "N+ = {%s} N- ={%s}"
      (f (RNS.getPlus rns) " ") (f (RNS.getMinus rns) " ")

  class c = object (self)

    val bot: st = None

    method bottom = bot 

    method stateSubset a b = 
      match a, b with
        None, _ -> (* if a is bottom, then a must be a subset *)
          true
      | _, None ->
          false
      | Some aa, Some bb ->
          RNS.subset aa bb

    method combineStates a b =
      match a, b with
        None, None -> None
      | None, _ -> b
      | _, None -> a
      | Some aa, Some bb ->
          let result = RNS.inter aa bb in
          Some (RNS.unique result)

    method initialState = Some (RNS.empty)
    method setInitialState (st:st) : unit = 
      failwith "RNS lattice initialState shouldn't change"

    method isBottom (a: st) =
      match a with
        None -> true
      | _    -> false

    method printState a = 
      match a with
        None -> logStatus "RNS: NONE\n"
      | Some data -> logStatus (string_of_rns data) 
          

    method printSummary key =
      let summ = sums#find key in
      logStatus "RNS SUMMARY: ";
      self#printState summ

    val mutable thesums = sums
    method setTheSums s = thesums <- s

    method sums = thesums

  end

end


module NullState = MakeRelStateManager (RNS) (RS)

type relChangeFromAssign =
    APlus
  | AMinus
  | ADefault

(* note that APlus acts as the identity element *)
let combine a b : relChangeFromAssign =
  match a, b with
    ADefault, ADefault -> ADefault
  | ADefault, AMinus   -> AMinus
  | ADefault, APlus    -> ADefault
  | APlus,    ADefault -> ADefault
  | APlus,    AMinus   -> AMinus
  | APlus,    APlus    -> APlus
  | AMinus,   ADefault -> AMinus
  | AMinus,   AMinus   -> AMinus
  | AMinus,   APlus    -> AMinus

type relChangeFromAssignWithCrumbs =
  relChangeFromAssign * BreadCrumbs.t

let combine2 a b : relChangeFromAssignWithCrumbs =
  match a, b with
    (ADefault, c1), (ADefault, c2) -> ADefault, emptyCrumbs
  | (ADefault, c1), (AMinus,   c2) -> AMinus,   c2
  | (ADefault, c1), (APlus,    c2) -> ADefault, emptyCrumbs
  | (APlus,    c1), (ADefault, c2) -> ADefault, emptyCrumbs
  | (APlus,    c1), (AMinus,   c2) -> AMinus,   c2
  | (APlus,    c1), (APlus,    c2) -> APlus,    emptyCrumbs
  | (AMinus,   c1), (ADefault, c2) -> AMinus,   c1
  | (AMinus,   c1), (AMinus,   c2) -> AMinus,   BreadCrumbs.union c1 c2
  | (AMinus,   c1), (APlus,    c2) -> AMinus,   c1



(* counters for how many lvals get added to N+ over time *)
let doPlusCount = ref 0
let countAddrGlob = ref 0
let countCondNotNull = ref 0
let countCondLval = ref 0
let countAssignNotNull = ref 0

module NullTransfer = struct

  let debug = false

  let name = "NullPtrDF"

  type st = NullState.st

  let relStMan = new NullState.rc
    
  let stMan = (relStMan :> (st, st) Intra.stateLattice)

  (** @return true if the st1 is no different from st2 *)
  let noChange st1 st2 =
    match st1, st2 with
      Some _, Some _ ->
        RNS.equal (relStMan#projRel st1) (relStMan#projRel st2)
    | None, Some _ 
    | Some _, None -> false
    | _ -> true


  (* supplies basic operations to add to N+ and N-;
     adding an lval to N- also adds all of its targets to N- *)
  class ['state, 'relSt, 'part, 'info] basicTransferOps stMan =
    object(self)
    inherit ['state, 'relSt, 'part, 'info] relTransFunc stMan

    method wrapDoPlusA ?(crumbs=emptyCrumbs) inState alv =
      let newState =
        stMan#injRel
          inState
          (stMan#doPlus (stMan#projRel inState) alv crumbs)
      in
        newState

    method wrapDoPlus (inState: 'state) lval =
      let alv = Lvals.abs_of_lval lval in
      self#wrapDoPlusA inState alv

    method wrapDoMinusA ?(crumbs=emptyCrumbs) (inState: 'state) alv =
      let newState =
        stMan#injRel
          inState
          (stMan#doMinus (stMan#projRel inState) alv crumbs)
      in
        newState

    method wrapDoMinusNormalized ?(crumbs=emptyCrumbs) inState alv =
      let targetLvs =
        match alv with
        | (Lv.CVar vi), _ ->
            [alv]
        | (Lv.CMem ptrExp), _ ->
            let pp = getCurrentPP () in
            let _, mayPt = SPTA.derefALvalAt pp alv in
            alv :: mayPt
        | _ ->
            logError ~prior:1
              "wrapDoMinus: unknown lv structure";
            [] in

      List.fold_left
        (fun st tlv -> self#wrapDoMinusA ~crumbs:crumbs st tlv)
         inState targetLvs

    method wrapDoMinus ?(crumbs=emptyCrumbs) (inState: 'state) lval =
      let alv = Lvals.abs_of_lval lval in
      self#wrapDoMinusNormalized ~crumbs:crumbs inState alv

    end
  

  (* just the sequential null analysis *)
  class ['state, 'relSt, 'part, 'info] seqTransFunc
                                         stMan =
    object(self)
    inherit ['state, 'relSt, 'part, 'info] basicTransferOps stMan
        
(*
    (* TODO old crumbless version, still used by handleCall *)
    method getEffectOfAssign exp inState : relChangeFromAssign =
      match inState with
        None -> (* instead, should this function require that inState
            is not None before calling?... *)
          ADefault
      | _ ->
        begin
          match exp with
          | AddrOf (Var v, _) ->
              if v.vglob then
               (incr countAddrGlob;
                APlus)
              else
                AMinus
          | Lval ((_, _) as rhs) ->
              let alv = Lvals.abs_of_lval rhs in
              if RNS.S.mem
                 alv
                 (stMan#getPlus (stMan#projRel inState)) then
               (incr countAssignNotNull;
                APlus)
              else 
                AMinus
          | _ ->
             AMinus
        end
*)

    (* crumb-y version of getEffectOfAssign *)
    method getEffectOfAssign2 exp inState
           : relChangeFromAssign * BreadCrumbs.t =
      match inState with
        None ->
          ADefault, emptyCrumbs

      | _ -> begin
          match exp with

          | AddrOf (Var v, _) ->
              APlus, emptyCrumbs
            (* try always adding to N+ 
              if v.vglob then
               (incr countAddrGlob;
                APlus, emptyCrumbs)
              else
                AMinus, emptyCrumbs
            *)

          | Lval ((_, _) as rhs) ->
              let alv = Lvals.abs_of_lval rhs in
              if RNS.S.mem alv (stMan#getPlus (stMan#projRel inState))
                then
                 (incr countAssignNotNull;
                  APlus, emptyCrumbs)
              else if RNS.S.mem alv (stMan#getMinus (stMan#projRel inState))
                then
                  (* propagating crumbs across assignments *)
                  AMinus, RNS.S.find alv
                            (stMan#getMinus (stMan#projRel inState))
              else
                (* mark as modded *)
                AMinus, emptyCrumbs

          | StartOf (Var v, _) ->
              APlus, emptyCrumbs

          | BinOp (PlusPI, ptr, _, _)
          | BinOp (MinusPI, ptr, _, _)
          | BinOp (IndexPI, ptr, _, _) ->
              self#getEffectOfAssign2 ptr inState

          (*
          | AddrOf (Mem ...

          | StartOf (Mem ...
          *)

          | _ ->
             AMinus, emptyCrumbs
        end



    method handleGuard (gexp:Cil.exp) (inState:'state) :'state DF.guardaction =
      match inState with
        | None ->
            Dataflow.GUnreachable
        | _ ->
          begin
            match gexp with
            | BinOp (Ne,
                     CastE (_, (Lval ((_, _) as lval))),
                     CastE (_, CastE (_, Const (CInt64 (0L, _, _)))),
                     _)
            | BinOp (Ne,
                     CastE (_, CastE (_, Const (CInt64 (0L, _, _)))),
                     CastE (_, (Lval ((_, _) as lval))),
                     _) ->
               (incr countCondNotNull;
                Dataflow.GUse (self#wrapDoPlus inState lval))

            (* NOTE removed clause equal to null check *)

            | Lval ((_, _) as lval) ->
               (incr countCondLval;
                Dataflow.GUse (self#wrapDoPlus inState lval))

            (* handle !'s in succession *)
            | UnOp (LNot, UnOp (LNot, exp, _), _) ->
                self#handleGuard exp inState

            (* handle negations of Ne and Eq *)
            | UnOp (LNot, BinOp (Ne, e1, e2, typ), _) ->
                self#handleGuard (BinOp (Eq, e1, e2, typ)) inState

            | UnOp (LNot, BinOp (Eq, e1, e2, typ), _) ->
                self#handleGuard (BinOp (Ne, e1, e2, typ)) inState

            | _ -> 
                Dataflow.GDefault

          end

    method handleAssign lv exp loc inState : 'state DF.action =
      match inState with (* right now, checking inState even though
          getEffectOfAssign does too *)
        | None ->
            Dataflow.Default
        | _ ->
            match self#getEffectOfAssign2 exp inState with
              APlus, _ ->
                Dataflow.Done (self#wrapDoPlus inState lv)
            | AMinus, crumbs ->
                Dataflow.Done (self#wrapDoMinus ~crumbs:crumbs inState lv)
            | ADefault, _ ->
                Dataflow.Default

    (* override default behavior *)
    (* TODO test if this actually has any effect *)
    method handleCallRet lv targs callexp actuals loc inState =
      let computeEffectOfFun funID = begin
        let ss = Symsummary.sum#find funID in
        match ss with
          Vbot -> AMinus, emptyCrumbs

        | Sym_types.Vmustptr (symaddr, offset) ->
          begin
            let alval = (symaddr.saHost, offset) in
            match Lvals.lvals_of_abs alval with
              (lh, off) :: [] ->
                self#getEffectOfAssign2 (Cil.mkAddrOf (lh, off)) inState
            | _ ->
               (logError
                  "intraDataflow: handleCallRet: aLval yields multiple Lvals";
                ADefault, emptyCrumbs)
          end

        | Sym_types.Vmayptr (_, syms)
        | Sym_types.Vextptr (_, syms) ->
          begin
            let alvals = ref [] in
            let lvals = ref [] in
            (* collect aLvals *)
            Sym_types.AddrOffSet.iter
              (fun (symaddr, offset) ->
                 alvals := (symaddr.saHost, offset) :: !alvals
              ) syms;
            (* convert aLvals to Lvals *)
            List.iter
              (fun alv ->
                 match Lvals.lvals_of_abs alv with
                   rhs_lv :: [] ->
                     lvals := rhs_lv :: !lvals
                 | _ ->
                   logError "intraDataflow: handleCallRet:
                     aLval yields multiple Lvals"
               ) !alvals;
            (* compute effect of the union of these may Lvals *)
            let effect = List.fold_left
              (fun a (lh, off) -> 
                 combine2
                   a
                  (self#getEffectOfAssign2 (Cil.mkAddrOf (lh, off)) inState)
              ) (APlus, emptyCrumbs) !lvals in
            effect
          end

        (* think about how to handle Vval;
           should probably cause a pointer to become unsafe *)
        | Sym_types.Vval _ ->
            AMinus, emptyCrumbs

        | _ ->
            ADefault, emptyCrumbs

      end (* end computeEffectOfFun *) in

      if targs = [] then
        inState
      else
        let overallEffect =
          List.fold_left
            (fun a b -> combine2 a (computeEffectOfFun b))
            (APlus, emptyCrumbs) targs in

        match overallEffect with
          ADefault, _ ->
            inState

        | APlus, _ ->
            self#wrapDoPlus inState lv

        | AMinus, crumbs ->
            self#wrapDoMinus ~crumbs:crumbs inState lv

  end (* end seqTransFunc class *)

  (* just the adjust for the null analysis *)
  class ['state, 'relSt, 'part, 'info] adjTransFunc
                                         stMan =
    object(self)
    inherit ['state, 'relSt, 'part, 'info] basicTransferOps stMan

    val mutable fkey = Callg.dummyFID
    val mutable par = LSHash.create 0
    val mutable pp2int = PPHash.create 0
    val mutable int2ls = Inthash.create 0

    method loadPseudoAccessInfo (fk:Callg.funID) =
      fkey <- fk;
      (* get summaries *)
      let p2i, i2l = LP.sums#find fkey in
      let parfromdisk, _, _ = Par.sums#find fkey in
      (* copy summaries into tables *)
      par <- LSHash.copy parfromdisk;
      pp2int <- PPHash.copy p2i;
      int2ls <- Inthash.copy i2l;
      LP.sums#evictOne fkey;
      Par.sums#evictOne fkey


    method getCurrentLockset =
      let pp = getCurrentPP () in
      try
        LP.getLocksetAtPP pp (pp2int, int2ls)
      with
        LP.LsAtPP ->
          logError ~prior:1 ("getCurrentLockset failed");
          Racesummary.emptyLS


    method checkPseudoAccessIsSafe lv ls =
      (try let _, paBindings = LSHash.find par ls in
       (try let pakh, targetStatus = LvalHash.find paBindings lv in
        
        PaFound (pakh, (Par.areTargetsSafe targetStatus))
          
        with Not_found ->
          logError ~prior:1
            ("checkPseudoAccessIsSafe( " ^ Lvals.string_of_lval lv
             ^ " ) fkey " ^ Callg.fid_to_string fkey
             ^ " - pakeyhead, targetStatus not found");
          PaNotFound (Lvals.mergeLv lv))
         
       with Not_found ->
         (*logError ~prior:1*)
         logStatus
           ("checkPseudoAccessIsSafe( " ^ Lvals.string_of_lval lv
            ^ " ) fkey " ^ Callg.fid_to_string fkey
            ^ " - reprloc, paBindings not found for lockset:");
         Racesummary.printLockset ls;
         PaNotFound (Lvals.mergeLv lv))
        
        
        
    (* check all dataflow facts for races.
       TODO a more efficient way would be to only do this when the
       current lockset has changed. *)
    method adjustAllPropagatedFacts inState ls =

      match inState with
        None -> None
      | _ ->

      let newstate = ref inState in
      RNS.S.iter
        (fun lv _ ->
           
           match self#checkPseudoAccessIsSafe lv ls with
             PaFound (_, true) ->
               logStatus ("racyPseudo false " ^ Lv.string_of_lval lv);
               ()
           | PaFound (pakh, false) as crumb ->
               logStatus ("racyPseudo true " ^ Lv.string_of_lval lv);
               newstate := self#wrapDoMinusNormalized
                             ~crumbs:(BreadCrumbs.singleton crumb)
                             !newstate lv
           | PaNotFound _ ->
               logError ("racyPseudo ?? " ^ Lv.string_of_lval lv);
               (* TODO carry along actual PaNotFound crumb *)
               let crumb = PaNotFoundPlaceHolder in
               newstate := self#wrapDoMinusA
                             ~crumbs:(BreadCrumbs.singleton crumb)
                             !newstate lv

        ) (stMan#getPlus (stMan#projRel inState));
      !newstate

  method handleInstr i inState =
    let newState =
      match i with
        Set (_, _, _)
      | Asm (_, _, _, _, _, _) ->
          self#adjustAllPropagatedFacts inState self#getCurrentLockset

      | Call (_, _, _, _) ->
          let weakerls = AU.getWeakerLS self#curCG 
            self#curFunID self#getCurrentLockset i in
          self#adjustAllPropagatedFacts inState weakerls
    in

    if noChange inState newState then
      DF.Default
    else
      DF.Done newState


  method handleGuard g inState =
    let ls = self#getCurrentLockset in
    let newState = self#adjustAllPropagatedFacts inState ls in
    if noChange inState newState then
      DF.GDefault
    else
      DF.GUse newState

  end (* end adjTransFunc class *)

  (* composition of sequential null analysis and adjust for null analysis *)
  class ['state, 'relSt, 'part, 'info] composedTransFunc
                                         stMan
                                         seqTrans
                                         adjTrans
                                         adjustOnThisPhase =
    object(self)
    inherit ['state, 'relSt, 'part, 'info] relTransFunc stMan

    val mutable setRnsAtPP = (fun pp x -> ())

    method setSetRnsAtPP foo =
      setRnsAtPP <- foo

    method loadPseudoAccessInfo (funID:Callg.funID) : unit =
      adjTrans#loadPseudoAccessInfo funID

    method handleInstr i inState =
      let pp = getCurrentPP () in

      if adjustOnThisPhase = false then
        seqTrans#handleInstr i inState
      else
        let midState =
          match adjTrans#handleInstr i inState with
            DF.Post _
          | DF.Default -> inState
          | DF.Done st -> st in

        setRnsAtPP pp midState;

        let outState = 
          match seqTrans#handleInstr i midState with
            DF.Post _
          | DF.Default -> midState
          | DF.Done st -> st in

        if noChange inState outState then
          DF.Default
        else
          DF.Done outState


    method handleGuard g inState =
      let pp = getCurrentPP () in

      if adjustOnThisPhase = false then
        seqTrans#handleGuard g inState
      else
        let midState =
          match adjTrans#handleGuard g inState with
            DF.GUnreachable
          | DF.GDefault -> inState
          | DF.GUse st  -> st in

        setRnsAtPP pp midState;

        let outState =
          match seqTrans#handleGuard g midState with
            DF.GUnreachable
          | DF.GDefault -> midState
          | DF.GUse st  -> st in

        if noChange inState outState then
          DF.GDefault
        else
          DF.GUse outState

  end (* end composedTransFunc class *)


  (* pessimistic adjust *)
  class ['state, 'relSt, 'part, 'info] pessimisticAdjTransFunc stMan =
  object(self)
    inherit ['state, 'relSt, 'part, 'info] basicTransferOps stMan

    method adjustAllPropagatedFacts inState =

      match inState with
        None -> None
      | _ ->
      
      let newstate = ref inState in
      RNS.S.iter
        (fun lv _ ->
           match Shared.escapeableAbs lv with
             true ->
               (* only using this crumb to stop the null-warning-prop *)
               let crumb = PaNotFoundPlaceHolder in
               newstate := self#wrapDoMinusNormalized
                 ~crumbs:(BreadCrumbs.singleton crumb)
                 !newstate lv;
               logStatus ("escapeableAbs true " ^ Lv.string_of_lval lv);
           | false ->
               logStatus ("escapeableAbs false " ^ Lv.string_of_lval lv);
        ) (stMan#getPlus (stMan#projRel inState));
      !newstate

  method handleInstr i inState =
    let newState = self#adjustAllPropagatedFacts inState in
    if noChange inState newState then
      DF.Default
    else
      DF.Done newState

  method handleGuard g inState =
    let newState = self#adjustAllPropagatedFacts inState in
    if noChange inState newState then
      DF.GDefault
    else
      DF.GUse newState

  (* placeholder to typecheck... *)
  method loadPseudoAccessInfo (fid:Callg.funID) = ()

  end (* end pessimisticAdjTransFunc class *)



  (****************************** FOR PASS 2 ******************************)

  (* instantiate the sequential transfer function *)
  let sequentialTransF = new seqTransFunc relStMan


  (****************************** FOR PASS 4 ******************************)

  (* instantiate the composed transfer function that doesn't adjust *)
  let transferWithNoAdjust =
    let seqF = (new seqTransFunc relStMan) in
    let adjF = (new adjTransFunc relStMan) in
      (new composedTransFunc relStMan seqF adjF false)

  (* instantiate the composed transfer function that does adjust *)
  let transferWithAdjust =
    let seqF = (new seqTransFunc relStMan) in
    let adjF = (new adjTransFunc relStMan) in
      (new composedTransFunc relStMan seqF adjF true)

  (* instantiate the pessimistic transfer function that doesn't adjust *)
  (* could just use transferWithNoAdjust, but making this version just
     for symmetry *)
  let pessimisticWithNoAdjust =
    let seqF = (new seqTransFunc relStMan) in
    let adjF = (new pessimisticAdjTransFunc relStMan) in
      (new composedTransFunc relStMan seqF adjF false)

  (* instantiate the pessimistic transfer function that does adjust *)
  let pessimisticWithAdjust =
    let seqF = (new seqTransFunc relStMan) in
    let adjF = (new pessimisticAdjTransFunc relStMan) in
      (new composedTransFunc relStMan seqF adjF true)


end (* end NullTransfer module *)

class rnsKnowledgeVisitor caption getRns stMan = object (self)
  inherit Knowledge_pass.knowledgeVisitor caption

  method incrementKnowledge () =
    let pp = getCurrentPP () in
    let data = getRns pp in
    match data with
      None -> ()
    | Some rns ->
        let nplus = stMan#getPlus rns in
        let count = RNS.S.cardinal nplus in
        knowledge <- knowledge + count

end
