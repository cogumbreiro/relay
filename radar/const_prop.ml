(*
  Copyright (c) 2009, Regents of the University of California

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


(** Radar client: Constant propagation *)

open Cil
open Callg
open Pretty
open Logging
open Cildump

module LocSet = Lockset_partitioner.LocSet
module Lvs = Radar.Lvs


(************************************************************
            Access to mod information 
************************************************************)

let modSumms = ref (new Modsummaryi.absModSumm)

let setModSumm (newSummaries) =
  modSumms := newSummaries


(***************************************************************************)
(* DATAFLOW Domain, etc.                                                   *)
(***************************************************************************)

module LvMap = Mapset.Make
  (struct 
     type t = Cil.lval 
     let compare x y = Ciltools.compare_lval x y
   end)

type nonBottomState = constant LvMap.t

(* Per-program point *)
type cpState = nonBottomState option
let bottom = None
let empty = Some (LvMap.empty)

type cpSummary = unit (* intra-procedural *)

(************************************************************)

let subsetVals v1 v2 = 
  v1 = v2

let combineVal v1 v2 =
  if v1 = v2 then Some v1
  else None

(************************************************************)
(* Hash cons stuff *)

(* TODO: if needed, keep a hashtbl of the unique lvmaps *)

module LvMapHashable = struct
  type t = nonBottomState
  let equal x y = 
    LvMap.equal subsetVals x y
  let hash x =
    LvMap.sampleHash 
      (fun k v -> (Ciltools.hash_lval k) lxor (Hashtbl.hash v lsl 2)) x
end

module LVMH = Weak.Make(LvMapHashable)

let goldenLVMH = LVMH.create 128

let hcLvMap lvmap =
  LVMH.merge goldenLVMH (LvMap.mapChK Cil_lvals.mergeLv lvmap)

(************************************************************)

class constLattice : [cpState, cpSummary] IntraDataflow.stateLattice = 
object (self)
  
  method stateSubset st1 st2 = 
    if st1 == st2 then true
    else match st1, st2 with
      None, _ -> true
    | Some _, None -> false
    | Some st1, Some st2 -> (* Could probably make first part standard... *)
        (* Superset because st1 is "less close to top" if 
           st1 has MORE constants than st2 (top is empty) *)
        LvMap.subsetB subsetVals st2 st1

  method combineStates st1 st2 =
    if st1 == st2 then st1
    else match st1, st2 with
      None, None -> st1
    | (Some _ as nonBot), None 
    | None, (Some _ as nonBot) -> nonBot
    | Some st1, Some st2 -> (* Could probably make first part standard... *)
        Some (hcLvMap (LvMap.interO combineVal st1 st2))

  method isBottom st =
    match st with 
      None -> true | Some _ -> false
    
  (***** Special values of the state + ops *****)

  method bottom = bottom
    
  val mutable initSt = empty
  method initialState = initSt

  method setInitialState st =
    initSt <- st
    
  (***** Debugging *****)
 
  method printState st =
    match st with 
      None -> logStatus "CONSTANTS = $BOTTOM"
    | Some st ->
        if LvMap.is_empty st then logStatus "CONSTANTS = $EMPTY"
        else 
          let doc = 
            text "CONSTANTS = {\n" ++
              indent 2 (map_to_doc line LvMap.iter
                          (fun lv v -> dprintf "%a -> %a" d_lval lv d_const v)
                          st nil) ++ text "\n}\n" in
              logStatusD doc
              

  (***** Operations on summaries *****)
 
  method printSummary sk =
    ()

  (* rkc: hack alert, to allow Radar to plug in seq or adj summary database *)
  (* val mutable sumtyp : BS.sumType *)
  (* method setSumTyp : BS.sumType -> unit *)
  val mutable thesums = new Backed_summary.noneSummary 
    (Backed_summary.makeSumType "const_p")
  method setTheSums newsums = 
    thesums <- newsums
  method sums = thesums

end

let cpLat = new constLattice

(************************************************************)


class constPropTF latt modSumRef = object(self)
  inherit [cpState] IntraDataflow.inspectingTransF latt "const_prop" as super
  inherit [nonBottomState] Modsum_call.modAtCallTF modSumRef
    
  method handleFunc fid fundec =
    let () = !modSumRef#evictSummaries in
    super#handleFunc fid fundec
    

  method private isConstant st exp =
    match exp with
      CastE(t, e') -> self#isConstant st e'
    | Const c -> Some c
    | SizeOf _ | SizeOfE _ | SizeOfStr _ 
    | AlignOf _ | AlignOfE _ -> failwith "didn't const fold first?"
    | Lval (lv) ->
        (try Some (LvMap.find lv st)
         with Not_found -> None)
    | AddrOf _ | StartOf _ -> 
        None (* addresses of globals / static strings? *)
    | UnOp (unop, e1,  t) ->
        (match self#isConstant st e1 with
           None -> None
         | Some c -> 
             let foldedUnop = constFold true (UnOp (unop, Const c, t)) in
             (match foldedUnop with
                Const c -> Some c
              | _ -> None))
    | BinOp (binop, e1, e2, t) ->
        (match self#isConstant st e1, self#isConstant st e2 with
           None, _ | _, None -> None
         | Some c1, Some c2 ->
             let foldedBinop = constFold true 
               (BinOp (binop, Const c1, Const c2, t)) in
             (match foldedBinop with
                Const c -> Some c
              | _ -> None))

  method private setConstant st lv c =
    LvMap.add lv c st

  method private mayAlias lv1 lv2 = 
    let lv1, _ = Cil_lvals.simplifyLval lv1 in
    let lv2, _ = Cil_lvals.simplifyLval lv2 in
    match lv1, lv2 with
      (Var v1, off1), (Var v2, off2) -> Ciltools.compare_lval lv1 lv2 == 0
    | (Mem e1, off1), (Mem e2, off2) ->
        Ciltools.compare_offset off1 off2 == 0 &&
          Alias.may_alias e1 e2
    | (Var v, off1), (Mem e, off2) 
    | (Mem e, off1), (Var v, off2) ->
        Ciltools.compare_offset off1 off2 == 0 &&
          Alias.points_to e v


  (** Kill constant prop facts if aliases don't agree on the constant *)
  method private smashAliasesConst st lv const = 
    (* See if any lval in st may alias lv. Those may be smashed *)
    LvMap.fold 
      (fun lv2 v acc ->
         if subsetVals v const then acc
         else if self#mayAlias lv lv2 then
           LvMap.remove lv2 acc
         else acc
      ) st st

  (** Kill const prop facts for aliases of lv *)
  method private smashAliases st (lv:Cil.lval) = 
    (* See if any lval in st may alias lv. Those may be smashed *)
    LvMap.fold 
      (fun lv2 v acc ->
         if self#mayAlias lv lv2 then
           LvMap.remove lv2 acc
         else acc
      ) st st

  (** Abstract lval versions *)
  method private mayAliasALv alv lv = 
    let (h1, o1), _ = Lvals.simplifyLval alv in
    let lv2, _ = Cil_lvals.simplifyLval lv in
    let (h2, o2) = Lvals.abs_of_lval lv2 in
    (Ciltools.compare_offset o1 o2) == 0 && 
      match Lvals.sameHost h1 h2 with
        Some _ -> true | None -> false

  method private smashAliasesALv st (alv:Lvals.aLval) = 
    (* See if any lval in st may alias lv. Those may be smashed *)
    LvMap.fold 
      (fun lv2 v acc ->
         if self#mayAliasALv alv lv2 then
           LvMap.remove lv2 acc
         else acc
      ) st st

          
  method handleAssign lv exp loc st =
    match st with
      None -> Dataflow.Default
    | Some st ->
        (match self#isConstant st (constFold true exp) with 
           Some const ->
             let newSt = self#smashAliasesConst st lv const in
             let newSt = self#setConstant newSt lv const in
             Dataflow.Done (Some newSt)
         | None -> 
             let newSt = self#smashAliases st lv in
             Dataflow.Done (Some newSt)
        )

  method handleGuard gexp inState =
    match inState with 
      None -> Dataflow.GUnreachable
    | Some st ->
        let foldedExp = Cil.constFold true gexp in
        let castFreeGuard = Cil_lvals.omitCast foldedExp in
        let rec checkExp gexp st =
          match gexp with
            BinOp (Eq, (Lval (lv1) as lve1), (Lval (lv2) as lve2), _) ->
              (match self#isConstant st lve1, self#isConstant st lve2 with
                 Some c1, Some c2 -> 
                   if subsetVals c1 c2 then Dataflow.GDefault
                   else Dataflow.GUnreachable
               | Some c1, None ->
                   Dataflow.GUse (Some (self#setConstant st lv2 c1))
               | None, Some c2 ->
                   Dataflow.GUse (Some (self#setConstant st lv1 c2))
               | None, None -> Dataflow.GDefault)
              
          | BinOp (Eq, Lval (lv), exp, _)
          | BinOp (Eq, exp, Lval lv, _) ->
              (match self#isConstant st exp with
                 Some c -> Dataflow.GUse (Some (self#setConstant st lv c))
               | None -> Dataflow.GDefault)

          (* handle !'s in succession *)
          | UnOp (LNot, UnOp (LNot, exp, _), _) ->
              checkExp exp st

          (* handle negations of Ne and Eq *)
          | UnOp (LNot, BinOp (Ne, e1, e2, typ), _) ->
              checkExp (BinOp (Eq, e1, e2, typ)) st

          | UnOp (LNot, BinOp (Eq, e1, e2, typ), _) ->
              checkExp (BinOp (Ne, e1, e2, typ)) st

          | Lval (lv) ->
              checkExp (BinOp (Eq, gexp, zero, boolType)) st

          | UnOp (LNot, (Lval lv as lvexp), t) ->
              checkExp (BinOp (Ne, lvexp, zero, t)) st

          | _ -> Dataflow.GDefault
        in
        checkExp castFreeGuard st

  method handleCallRet lv funs callexp actuals loc inState =
    (* Don't gain knowledge from function calls *)
    match inState with
      None -> inState 
    | Some st -> Some (self#smashAliases st lv)

  method handleCallExp funs callexp actuals loc inState =
    (* Don't gain knowledge from function calls *)
    match inState with
      None -> inState
    | Some st -> 
        (* Use mods summary *)
        Some (self#handleCallModsFuns actuals st funs)

  method dependsOn (st : cpState) : Lvs.t =
    match st with
      None -> Lvs.empty
    | Some st -> 
        LvMap.fold 
          (fun lv _ acc -> 
             Lvs.add (Lvals.abs_of_lval lv) acc
          ) st Lvs.empty

  method havoc (st: cpState) (crumbLvals : Radar.crumbsandlvals) : cpState =
    match st with
      None -> st
    | Some st -> 
        let newSt = 
          Radar.CrumbyLvs.fold 
            (fun (aLv, crumbs) st ->
               let lvs = Lvals.lvals_of_abs aLv in
               List.fold_left 
                 (fun st lv -> 
(*                    logStatusF "Adj removed: %s\n" (string_of_lval lv); *)
                    LvMap.remove lv st
                 ) st lvs
            ) crumbLvals st in
        Some newSt


end


(***************************************************************************)
(* KNOWLEDGE VISITOR PASS                                                  *)
(***************************************************************************)

class cpKnowVisitor (getDF : prog_point -> cpState) (caption : string) =
object(self)
  inherit Knowledge_pass.knowledgeVisitor caption
    
  method incrementKnowledge () =
    let pp = getCurrentPP () in
    let data = getDF pp in
    (match data with 
       None -> ()
     | Some st -> knowledge <- knowledge + (LvMap.cardinal st))

end


let noSkip (_:Callg.funID) = false

let makeKnowAna kvis = new Knowledge_pass.knowledgeAnalysis noSkip kvis


(***************************************************************************)
(* GET EVERYTHING PACKAGED UP                                              *)
(***************************************************************************)

(** Info needed by the Radar inter-proc driver *)

let seqsums = new Backed_summary.noneSummary 
  (Backed_summary.makeSumType "cp_seq")
let adjSumsL = new Backed_summary.noneSummary
  (Backed_summary.makeSumType "cp_adjL")
let adjSumsNL = new Backed_summary.noneSummary
  (Backed_summary.makeSumType "cp_adjNL")
let pessSums = new Backed_summary.noneSummary
  (Backed_summary.makeSumType "cp_pess")
let parL = new Pseudo_access.PARsummary.data 
  (Backed_summary.makeSumType "cp_par_l")
let parNL = new Pseudo_access.PARsummary.data 
  (Backed_summary.makeSumType "cp_par_nl")


module RadParams = struct
  type st = cpState
  type sum = cpSummary

  let name = "const_prop"
  let summaryExt = "const_p"

  let stMan = (cpLat :> (st, sum) IntraDataflow.stateLattice)

  let printState = cpLat#printState
  class seq = constPropTF stMan modSumms

  let seqSums = seqsums
  (*let adjsums = adjsums*)
  let adjSumsL = adjSumsL
  let adjSumsNL = adjSumsNL
  let pessSums = pessSums
  (*let pardb = pardb*)
  let parL = parL
  let parNL = parNL

  class summarizer = [st, sum] IntraDataflow.noneSummarizer

end

module RadarDFs = Radar.MakeRadarForwardDFs (RadParams)

let getSeq = RadarDFs.getSeqDataBefore
let getPess = RadarDFs.getPessDataBefore
let getAdjL = RadarDFs.getAdjLDataBefore
let getAdjNL = RadarDFs.getAdjNLDataBefore

module NonFixPasses = struct

  let seqNonfixPasses =
    [ makeKnowAna (new cpKnowVisitor getSeq "SEQ")]
    
  let adjLNonfixPasses =
    [ makeKnowAna (new cpKnowVisitor getAdjL "RADAR")]

  let adjNLNonfixPasses =
    [ makeKnowAna (new cpKnowVisitor getAdjNL "RADAR-NL")]

  let pessNonfixPasses =
    [ makeKnowAna (new cpKnowVisitor getPess "STEENS")]

  (* Ugly as hell??? *)
  let setCG cg sccCG =
    ()

end


module Radarayed = Radar.Radar (RadarDFs) (NonFixPasses)

let dummyBefore (settings:Config.settings) 
    (cg:Callg.callG) (sccCg:Scc_cg.sccGraph) : unit = 
  (* Use Relay to generate mod summaries ... *)
  setModSumm (Racestate.RS.sum :> Modsummaryi.modSum) 


let dummyAfter (cg:Callg.callG) (_:string) : unit = ()


module PseudoRacePass = Radarayed.PseudoRacePass

