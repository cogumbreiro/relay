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


(** Radar client: Very busy expressions *)

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

module ExpSet = Set.Make
  (struct 
     type t = Cil.exp 
     let compare x y = Ciltools.compare_exp x y
   end)

module LvalSet = Set.Make
  (struct
     type t = Cil.lval
     let compare x y = Ciltools.compare_lval x y
   end)

(* Per-program point *)
type nonBottomState = ExpSet.t
type vbeState = nonBottomState option
let bottom = None
let empty = Some (ExpSet.empty)

type vbeSummary = unit (* intra-procedural *)

(************************************************************)

let subsetVals v1 v2 = 
  v1 = v2

let combineVal v1 v2 =
  if v1 = v2 then Some v1
  else None

class vbeLattice : [vbeState, vbeSummary] IntraDataflow.stateLattice = 
object (self)
  
  method stateSubset st1 st2 = 
    if st1 == st2 then true
    else match st1, st2 with
      None, _ -> true
    | Some _, None -> false
    | Some st1, Some st2 -> (* Could probably make first part standard... *)
        ExpSet.subset st2 st1
    
  method combineStates st1 st2 =
    if st1 == st2 then st1
    else match st1, st2 with
      None, None -> st1
    | Some _, None -> st1
    | None, Some _ -> st2
    | Some st1, Some st2 -> (* Could probably make first part standard... *)
        Some (ExpSet.inter st1 st2)
    
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
      None -> logStatus "VBE = $BOTTOM"
    | Some st ->
        if ExpSet.is_empty st then logStatus "VBE = $EMPTY"
        else 
          let doc = 
            text "VBE = {\n" ++
              indent 2 (seq_to_doc line ExpSet.iter
                          (fun e -> dprintf "%a" d_exp e)
                          st nil) ++ text "\n}\n" in
          logStatusD doc

  (***** Operations on summaries *****)
 
  method printSummary sk =
    ()

  val mutable thesums = new Backed_summary.noneSummary 
    (Backed_summary.makeSumType "vbe")
  method setTheSums newsums = 
    thesums <- newsums
  method sums = thesums

end

let vbeLat = new vbeLattice

(************************************************************)
(* Utils *)


(** Generic "crawl" over expressions given two functions:
   (1) a "combiner" : 'a -> 'a -> 'a
   (actually, make the second arg lazy to handle short circuitry)
   (2) a "doLv" : lv -> 'a   *)
let rec foldOverLvExp 
    (doLv : Cil.lval -> 'a) (combine: 'a -> 'a Lazy.t -> 'a) (acc: 'a)
    (exp: Cil.exp) : 'a =
  match exp with
    Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _
  | AlignOf _ | AlignOfE _ -> acc
  | CastE (t, e') -> foldOverLvExp doLv combine acc e'
  | AddrOf (Var _, off) | StartOf (Var _, off) ->
      foldOverLvOff doLv combine acc off
  | AddrOf (Mem e', off) | StartOf (Mem e', off) -> 
      combine 
        (foldOverLvExp doLv combine acc e')
        (lazy (foldOverLvOff doLv combine acc off))
  | BinOp (_, e1, e2, _) ->
      combine 
        (foldOverLvExp doLv combine acc e1)
        (lazy (foldOverLvExp doLv combine acc e2))
  | UnOp (_, e1, _) ->
      foldOverLvExp doLv combine acc e1
  | Lval ((Var _, _) as lv2) ->
      (* Requires exact equality, not checking aliasing *)
      combine acc (lazy (doLv lv2))
  | Lval ((Mem e, off) as lv2) ->
      (combine 
         (doLv lv2)
         (lazy (combine 
                  (foldOverLvExp doLv combine acc e)
                  (lazy (foldOverLvOff doLv combine acc off)))))


and foldOverLvOff doLv combine acc off =
  match off with
    NoOffset -> acc
  | Field (_, moreOff) -> foldOverLvOff doLv combine acc moreOff
  | Index (e, moreOff) -> 
      combine 
        (foldOverLvExp doLv combine acc e)
        (lazy (foldOverLvOff doLv combine acc moreOff))


(************************************************************)

(** True if the expression does not depend on the lval (directly) *)
let rec independentExp (lv: Cil.lval) (exp: Cil.exp) =
  match exp with
    Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _
  | AlignOf _ | AlignOfE _ -> true
  | CastE (t, e') -> independentExp lv e'
  | AddrOf (Var _, off) | StartOf (Var _, off) -> 
      independentOffset lv off
  | AddrOf (Mem e', off) | StartOf (Mem e', off) -> 
      independentExp lv e' && independentOffset lv off
  | BinOp (_, e1, e2, _) ->
      independentExp lv e1 && independentExp lv e2
  | UnOp (_, e1, _) ->
      independentExp lv e1
  | Lval ((Var _, _) as lv2) ->
      (* Requires exact equality, not checking aliasing *)
      Ciltools.compare_lval lv lv2 <> 0
  | Lval ((Mem e, off) as lv2) ->
      (Ciltools.compare_lval lv lv2 <> 0) &&
        (independentExp lv e && independentOffset lv off)

and independentOffset lv off =
  match off with
    NoOffset -> true
  | Field (_, moreOff) -> independentOffset lv moreOff
  | Index (e, moreOff) -> 
      independentExp lv e && independentOffset lv moreOff


(************************************************************)


class vbeTF latt modSumRef = object(self)
  inherit [vbeState] IntraDataflow.inspectingTransF latt "very_busy" as super
  inherit [nonBottomState] Modsum_call.modAtCallTF modSumRef

  method handleFunc fid fundec =
    let () = !modSumRef#evictSummaries in
    super#handleFunc fid fundec

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
    
  (** Kill vbe facts for aliases of lv *)
  method private smashAliases st lv = 
    ExpSet.filter 
      (fun exp ->
         let lvs = foldOverLvExp 
           (fun lv -> LvalSet.singleton lv)
           (fun lvs1 lvs2Lazy -> LvalSet.union lvs1 (Lazy.force lvs2Lazy))
           LvalSet.empty exp in
         not (LvalSet.exists (self#mayAlias lv) lvs)
      ) st


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
    ExpSet.filter
      (fun exp ->
         let lvs = foldOverLvExp 
           (fun lv -> LvalSet.singleton lv)
           (fun lvs1 lvs2Lazy -> LvalSet.union lvs1 (Lazy.force lvs2Lazy))
           LvalSet.empty exp in
         not (LvalSet.exists (self#mayAliasALv alv) lvs)
      ) st

  method handleAssign lv exp loc st =
    match st with
      None -> 
        logStatusF "DF default @ %s\n" (string_of_loc loc);
        Dataflow.Default
    | Some st ->
        let newSt = self#smashAliases st lv in
        let newSt = self#generateExps newSt exp in
        Dataflow.Done (Some newSt)

  method handleGuard gexp inState =
    match inState with
      None -> Dataflow.GDefault
    | Some st ->
        Dataflow.GUse (Some (self#generateExps st gexp))

  method handleCallRet lv funs callexp actuals loc inState =
    match inState with
      None -> inState
    | Some st ->
        let newSt = self#smashAliases st lv in
        (Some newSt)

  method handleCallExp funs callexp actuals loc inState =
    (* TODO : smash from mod summary *)
    match inState with
      None -> inState
    | Some st ->
        (* Use mods summary *)
        let st = self#handleCallModsFuns actuals st funs in
        (* Generate expressions used in arguments *)
        let newSt = List.fold_left self#generateExps st actuals in
        Some newSt

  method private generateExps st exp =
    (* Don't add subexpressions and simple constants / expressions
       that don't require any work to compute... *)
    match exp with
      Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _
    | AlignOf _ | AlignOfE _ -> st
    | AddrOf (Var _, NoOffset) -> st
    | CastE (_, e') -> self#generateExps st e'
    | Lval (Var v, NoOffset) ->
        if v.vglob then ExpSet.add exp st
        else st (* assume locals don't require work (e.g., load to regs) *)
    | BinOp _ | UnOp _ | Lval _ | AddrOf _ | StartOf _ ->
        ExpSet.add exp st
          
  method handleStmt stmt st =
    match st with
      None -> Dataflow.SDefault
    | Some st ->
        (match stmt.skind with
           Return (Some (e), _) 
         | Switch (e, _, _, _) ->
             Dataflow.SUse (Some (self#generateExps st e))
         | _ -> Dataflow.SDefault ) 

  (************************************************************)

  method dependsOn (st : vbeState) : Lvs.t =
    match st with
      None -> Lvs.empty
    | Some st -> ExpSet.fold self#dependsOnExp st Lvs.empty

  method private dependsOnExp exp acc =
    let lvs = foldOverLvExp 
      (fun lv ->
         Lvs.singleton (Lvals.abs_of_lval lv))
      (fun lvs1 lvs2Lazy -> Lvs.union lvs1 (Lazy.force lvs2Lazy))
      acc exp in
    lvs

  method havoc (st: vbeState) (crumbLvals : Radar.crumbsandlvals) : vbeState =
    match st with
      None -> st
    | Some st ->
        let newSt = 
          Radar.CrumbyLvs.fold 
            (fun (aLv, crumbs) st ->
               let lvs = Lvals.lvals_of_abs aLv in
               List.fold_left self#filterHavoced st lvs
            ) crumbLvals st in
        Some newSt

  method private filterHavoced st lv =
    ExpSet.filter 
      (fun exp ->
         if independentExp lv exp then begin
           (* logStatusF "Didn't filter %s for %s\n" (string_of_lval lv) *)
           (*   (string_of_exp exp); *)
           true
         end
         else begin
           (* logStatusF "Adj Filtered: %s\n" (string_of_exp exp); *)
           false
         end
      ) st
      
end


(***************************************************************************)
(* KNOWLEDGE VISITOR PASS                                                  *)
(***************************************************************************)

class vbeKnowVisitor (getDF : prog_point -> vbeState) (caption : string) =
object(self)
  inherit Knowledge_pass.knowledgeVisitor caption
    
  method incrementKnowledge () =
    let pp = getCurrentPP () in
    let data = getDF pp in
    (match data with 
       None -> ()
     | Some st -> knowledge <- knowledge + (ExpSet.cardinal st))

end


let noSkip (_:Callg.funID) = false

let makeKnowAna kvis = new Knowledge_pass.knowledgeAnalysis noSkip kvis


(***************************************************************************)
(* GET EVERYTHING PACKAGED UP                                              *)
(***************************************************************************)

(** Info needed by the Radar inter-proc driver *)

let seqsums = new Backed_summary.noneSummary 
  (Backed_summary.makeSumType "vbe_seq")
let adjSumsL = new Backed_summary.noneSummary
  (Backed_summary.makeSumType "vbe_adjL")
let adjSumsNL = new Backed_summary.noneSummary
  (Backed_summary.makeSumType "vbe_adjNL")
let pessSums = new Backed_summary.noneSummary
  (Backed_summary.makeSumType "vbe_pess")
let parL = new Pseudo_access.PARsummary.data 
  (Backed_summary.makeSumType "cp_par_l")
let parNL = new Pseudo_access.PARsummary.data 
  (Backed_summary.makeSumType "cp_par_nl")


module RadParams = struct
  type st = vbeState
  type sum = vbeSummary

  let name = "very_busy"
  let summaryExt = "vbe"

  let stMan = (vbeLat :> (st, sum) IntraDataflow.stateLattice)

  let printState = vbeLat#printState
  class seq = vbeTF stMan modSumms

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

module RadarDFs = Radar.MakeRadarBackwardDFs (RadParams)

let getSeq = RadarDFs.getSeqDataBefore
let getPess = RadarDFs.getPessDataBefore
let getAdjL = RadarDFs.getAdjLDataBefore
let getAdjNL = RadarDFs.getAdjNLDataBefore

module NonFixPasses = struct

  let seqNonfixPasses =
    [ makeKnowAna (new vbeKnowVisitor getSeq "SEQ")]
    
  let adjLNonfixPasses =
    [ makeKnowAna (new vbeKnowVisitor getAdjL "RADAR")]

  let adjNLNonfixPasses =
    [ makeKnowAna (new vbeKnowVisitor getAdjNL "RADAR-NL")]

  let pessNonfixPasses =
    [ makeKnowAna (new vbeKnowVisitor getPess "STEENS")]

  (* Ugly as hell??? *)
  let setCG cg sccCG =
    ()

end


module Radarayed = Radar.Radar (RadarDFs) (NonFixPasses)

let dummyBefore (settings:Config.settings) (cg:Callg.callG) 
    (sccCg:Scc_cg.sccGraph) : unit = 
  (* Use Relay to generate mod summaries ... *)
  setModSumm (Racestate.RS.sum :> Modsummaryi.modSum) 

let dummyAfter (cg:Callg.callG) (_:string) : unit = ()

module PseudoRacePass = Radarayed.PseudoRacePass
