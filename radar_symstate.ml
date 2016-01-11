
open Cil
open Pretty
open Symex_types
open Symex
open Logging

module BS = Backed_summary


(************************************************************)

let init = init (* from Symex *)


(*******************)

class symKnowledgeVisitor caption getState (stMan:symLatt) = object (self)
  inherit Knowledge_pass.knowledgeVisitor caption
    
  method private sumKnowledgeVal v =
    match v with
      Vval _ -> knowledge <- knowledge + 1
    | Vmustptr _ -> knowledge <- knowledge + 1
    | Vstruct offmap -> OffsetMap.iter (fun _ v -> self#sumKnowledgeVal v) offmap
    | Vmayptr _
    | Vextptr _
    | Vbot
    | Vtop -> ()
        
  method incrementKnowledge () =
    let pp = getCurrentPP () in
    let state = getState pp in
    AddrMap.iter (fun addr v -> self#sumKnowledgeVal v) state.store

end



(* Extra Radar summary instantiations *)

let seqsums = new SS.data (BS.makeSumType "ss_seq")
let adjSumsL = new SS.data (BS.makeSumType "ss_adj_l")
let adjSumsNL = new SS.data (BS.makeSumType "ss_adj_nl")
let pessSums = new SS.data (BS.makeSumType "ss_pess")
let parL = new Pseudo_access.PARsummary.data (BS.makeSumType "ss_par_l")
let parNL = new Pseudo_access.PARsummary.data (BS.makeSumType "ss_par_nl")
(* let radar register *)


(* Extended transfer func *)

class radarSymTransFunc stLat = object (self)
  inherit symTransFunc stLat

  (************* Radar stuff ***************)

  (** Add the dependencies from uses of initial values *)
  method private dependsOnInitial curSet =
    let pp = getCurrentPP () in
    if self#isFirstStmt pp then begin
      (* debug *) 
      if self#inspect then begin 
        let header = text "dependsOnInitial: " in
        let body = (seq_to_doc (text ", ") Lvs.iter 
                      (Lv.d_alval ()) !usedInitialVals nil) in
        let tail = text "\n" in
        logStatusD (header ++ body ++ tail);
        (* /debug *)
      end;
      Lvs.union curSet !usedInitialVals 
        (* don't interpret the values generated *)
    end else curSet
      
  method dependsOn (state: symState) : Lvs.t =
    let rec addLvalsOfExp exp cur =
      match exp with
        Lv.CLval lv 
      | Lv.CAddrOf lv
      | Lv.CStartOf lv -> addLvalsOfLval lv cur
      | Lv.CBinOp (_, e1, e2, _) -> 
          let temp = addLvalsOfExp e1 cur in
          addLvalsOfExp e2 temp
      | Lv.CUnOp (_, e1, _) ->
          addLvalsOfExp e1 cur
      | Lv.CCastE (_, e1) ->
          addLvalsOfExp e1 cur
      | Lv.CConst _
      | Lv.CSizeOfStr _
      | Lv.CSizeOf _
      | Lv.CSizeOfE _
      | Lv.CAlignOf _
      | Lv.CAlignOfE _ -> cur
    and addLvalsOfLval lv cur =
      let cur = Lvs.add lv cur in
      match lv with
        Lv.CVar _, _ -> cur (* TODO: check suboffsets? *)
      | Lv.CMem exp, _ -> addLvalsOfExp exp cur
      | Lv.AbsHost _, _ -> cur
    in

    let rec dependsOnValue addr v cur =
      let lval = hostOfAddr addr, NoOffset in
      dependsOnLvalValue lval v cur

    and dependsOnLvalValue lval v cur =
      let fromVal = match v with
          Vval exp ->       
            let cur = Lvs.add lval cur in
            addLvalsOfExp exp cur
        | Vmustptr _ 
        | Vmayptr _ 
        | Vextptr _
        | Vbot ->
            Lvs.add lval cur
        | Vstruct offMap ->
            OffsetMap.fold 
              (fun off nextV cur ->
                 let nextLval = Lv.addOffsetLval off lval in
                 dependsOnLvalValue nextLval nextV cur) offMap cur
        | Vtop -> cur
      in
      fromVal
    in
    let fromState = AddrMap.fold dependsOnValue state.store Lvs.empty in
    self#dependsOnInitial fromState


  method havoc (state: symState) (crumbLvals : Radar.crumbsandlvals) : symState =
    if self#inspect then begin
      let header = text "havoc'ed: " in
      let body = seq_to_doc (text ", ") Radar.CrumbyLvs.iter 
        (fun (lv, crumb) -> Lv.d_alval () lv) crumbLvals nil in
      let tail = text "\n" in
      logStatusD (header ++ body ++ tail);
    end;
    Radar.CrumbyLvs.fold
      (fun ((host, off), crumb) curState ->
         let addr = getAddr host false in
         self#havocTarget curState (addr, off)
      ) crumbLvals state

end

(** Info needed by the Radar inter-proc driver *)

module RadParams = struct
  type st = symState
  type sum = SS.sumval

  let name = "symex"
  let summaryExt = "symex"

  let stMan = (fullLattice :> (st, sum) IntraDataflow.stateLattice)

  let printState = printSymState
  class seq = radarSymTransFunc stMan

  let seqSums = seqsums
  (*let adjsums = adjsums*)
  let adjSumsL = adjSumsL
  let adjSumsNL = adjSumsNL
  let pessSums = pessSums
  (*let pardb = pardb*)
  let parL = parL
  let parNL = parNL

  class summarizer = symSummarizer stMan
end


module RadarDFs = Radar.MakeRadarForwardDFs (RadParams)

let knowPassSeq = new Knowledge_pass.knowledgeAnalysis sumIsFinal
  (new symKnowledgeVisitor "SEQ" 
     RadarDFs.getSeqDataBefore RadParams.stMan)

let knowPassAdjL = new Knowledge_pass.knowledgeAnalysis sumIsFinal
  (new symKnowledgeVisitor "RADAR" 
     RadarDFs.getAdjLDataBefore RadParams.stMan)

let knowPassAdjNL = new Knowledge_pass.knowledgeAnalysis sumIsFinal
  (new symKnowledgeVisitor "RADAR-NL" 
     RadarDFs.getAdjNLDataBefore RadParams.stMan)

let knowPassPess = new Knowledge_pass.knowledgeAnalysis sumIsFinal
  (new symKnowledgeVisitor "STEENS" 
     RadarDFs.getPessDataBefore RadParams.stMan)


module NonFix = struct
  let seqNonfixPasses = [ knowPassSeq ]
  let adjLNonfixPasses = [ knowPassAdjL ]
  let adjNLNonfixPasses = [ knowPassAdjNL ]
  let pessNonfixPasses = [ knowPassPess ]

  let setCG cg sccCG = ()
end

module Radarayed = Radar.Radar (RadarDFs) (NonFix)

let doBefore settings cg (sccCG : Scc_cg.sccGraph) =
  init settings cg (Racestate.RS.sum :> Modsummaryi.absModSumm)

let doAfter (cg:Callg.callG) (cgDir:string) =
  ()
