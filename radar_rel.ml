
open Cil
open Fstructs
open IntraDataflow
open Radar (* note: Lvs, lvals, etc. brought into scope from Radar *)
open Logging

module SPTA = Symstate2

  
(******************************************************************************)
(*** MINIMAL ABSTRACTION FOR GENERIC RELATIVE DATAFLOW                      ***)
(******************************************************************************)


(******************************************************************************)
(*** RELATIVE MAPSET INSTANTIATION                                          ***)
(******************************************************************************)

module type ValueType = sig
  type value
  val compareV : value -> value -> int option
  val combineV : value -> value -> value
  val equalV : value -> value -> bool
  (* should there be a uniqueV also? *)
end

module MakeCrumbyRelType (V : ValueType) = struct

  type key = Lvals.aLval
  let compareK = Lvals.compare_lval
  let hashK = Lvals.hash_lval
  let uniqueK = Lvals.mergeLv

  type value = V.value * Crumbs.t
  
  let compareV (v1, c1) (v2, c2) =
    match V.compareV v1 v2 with
      | None   -> None
      | Some 0 -> Some (Crumbs.compare c1 c2)
      | x      -> x

  let combineV (v1, c1) (v2, c2) =
    (V.combineV v1 v2, Crumbs.union c1 c2)
      
  let equalV (v1, c1) (v2, c2) =
    V.equalV v1 v2 && Crumbs.equal c1 c2

end


module type RELDATAFLOWSET = Relative_set.S with type key = Lvals.aLval

module type RADAR_RMS_PARAMS_1 = sig
  module VT : ValueType
  val name : string
  val summaryExt : string
 end

module type RADAR_RMS_ANALYSIS = sig
  include RADAR_RMS_PARAMS_1
  type st
  val sums : st Backed_summary.base
end

exception IllegalProject

(* This functor will create the ComposedRel and then create the
 * RelStateManager.  The ComposedRel structure assumes that the state
 * is nothing more than a relative MapSet (option).  If the state
 * needs to maintain something more than just a pair of PLUS/MINUS
 * sets, then this is not for you.
 *)
module MakeDataflowManager
         (RelDFS : RELDATAFLOWSET)
         (RA : RADAR_RMS_ANALYSIS with type st = RelDFS.relSet option) =
   struct

  module R = struct
  
    type st = RelDFS.relSet option
  
    let summaryExt = RA.summaryExt

(* JAN PRUNE
    module RelDFS = RelDFS
*)

    let scopeLval (f:fundec) lv (_) curData =
      match Shared.isShareableAbs f lv with
        | None -> RelDFS.S.remove lv curData
        | _    -> curData

    let scopeDfs (f:fundec) (dfs:RelDFS.relSet) : RelDFS.relSet =
      let oldPlus = RelDFS.getPlus dfs in
      let newPlus = RelDFS.S.fold (scopeLval f) oldPlus oldPlus in
      let oldMinus = RelDFS.getMinus dfs in
      let newMinus = RelDFS.S.fold (scopeLval f) oldMinus oldMinus in
        RelDFS.unique (RelDFS.composeNew newPlus newMinus)

    let sums = RA.sums

  end (* module R *)

  module C = struct

    type relSt = RelDFS.relSet

    type st = R.st

    let projRel (a:R.st) : relSt =
      match a with
        | None -> raise IllegalProject
        | Some aa -> aa

    let injRel a b = Some (RelDFS.unique b)
    
    (* right now, only prints keys and not values *)
    let string_of_dfs dfs =
      let f = RelDFS.S.fold
                (fun lv _ acc -> acc ^ Lvals.string_of_lval lv ^ " ") in
      "D+ = {" ^ f (RelDFS.getPlus dfs) " " ^ "} " ^
      "D- = {" ^ f (RelDFS.getMinus dfs) " " ^ "}"


    class c =
      object(self)

        (* TODO abstract this common setup out *)
        val mutable thesums = R.sums (* initialized to whichever is passed in
                                      * at first, usually seq *)

        method setTheSums s = thesums <- s

        method sums = thesums

        val bot : R.st = None
        
        method bottom = bot

        method stateSubset a b =
          match a, b with
            | None, _ -> true
            | _, None -> false
            | Some aa, Some bb -> RelDFS.subset aa bb

        method combineStates a b =
          match a, b with
            | None, None -> None
            | None, _ -> b
            | _, None -> a
            | Some aa, Some bb ->
                let result = RelDFS.inter aa bb in
                Some (RelDFS.unique result)

        method initialState = Some (RelDFS.empty)
        method setInitialState (st:R.st) : unit =
          failwith "Relative DF lattice should have fixed initialState"

        method isBottom (a:R.st) =
          match a with
            | None -> true
            | _ -> false

        method printState a =
          match a with
          | None -> Printf.printf "DFS: NONE\n"
          | Some data -> Printf.printf "%s\n" (string_of_dfs data)

        method printSummary key =
          let summ = thesums#find key in
          Printf.printf "SUMMARY: ";
          self#printState summ

      end
      
    end (* module C *)

    module RSM = Relative_df.MakeRelStateManager (RelDFS) (C)

end

  


class ['st, 'relSt, 'part, 'info] rmsSeqTransFunc relStMan 
(*  : ['st] radarSEQTRANSFUNC = *) (* not restricting type because of wrap *)
  =

  object(self)
  inherit ['st, 'relSt, 'part, 'info] Relative_df.relTransFunc relStMan

  (* only adjust the plus set *)
  method dependsOn state  =
    try
      let relstate = relStMan#projRel state in
      let plus = relStMan#getPlus relstate in
      self#relDependsOn plus
    with
      IllegalProject -> Lvs.empty

  method havoc state cl =
    try
      let oldrel = relStMan#projRel state in
      let newrel = self#relHavoc oldrel cl in
      relStMan#injRel state newrel
    with
      IllegalProject -> relStMan#bottom

  
  method private relDependsOn (p:'part) : lvals =
    relStMan#fold_rel (fun lv _ set -> Lvs.add lv set) p Lvs.empty

  method private relHavoc (rs:'relSt) (cls:crumbsandlvals) : 'relSt =

    (* finds the crumb if lv is racy.
     * assumes that lv will have at most crumbylval;
     * but if there are multiple, this arbitrarily returns the last one *)
    let findCrumb lv : crumb option =
      CrumbyLvs.fold
        (fun cl cr -> match Lvals.compare_lval (fst cl) lv with
                        | 0 -> Some (snd cl)
                        | _ -> cr
        ) cls None
    in
    relStMan#fold_rel
      (fun k v rel ->
         match findCrumb k with
           | None   -> rel
           | Some c ->
               let newinfo = addCrumb v c in
               self#wrapDoMinusRelNorm rel k (fst newinfo) ~cr:(snd newinfo)
      ) (relStMan#getPlus rs) rs


  method private wrapDoPlus ?(cr=Crumbs.empty) st lv v =
    let alv = Lvals.abs_of_lval lv in
    self#wrapDoPlusA st alv v ~cr:cr

  method private wrapDoPlusA ?(cr=Crumbs.empty) st alv v =
    let rs =
      try relStMan#projRel st
      with IllegalProject -> relStMan#projRel relStMan#initialState in
    let newrs = relStMan#doPlus rs alv (v, cr) in
    relStMan#injRel st newrs

  method private wrapDoMinus ?(cr=Crumbs.empty) st lv v =
    let alv = Lvals.abs_of_lval lv in
    self#wrapDoMinusA st alv v ~cr:cr
    
  method private wrapDoMinusA ?(cr=Crumbs.empty) st alv v =
    let rs =
      try relStMan#projRel st
      with IllegalProject -> relStMan#projRel relStMan#initialState in
    let newrs = relStMan#doMinus rs alv (v, cr) in
    relStMan#injRel st newrs
    
  method private wrapDoMinusRelNorm ?(cr=Crumbs.empty) rs alv v =
    let targetLvs =
      match alv with
        | (Lvals.CVar vi), _ ->
            [alv]
        | (Lvals.CMem ptrExp), _ ->
            let pp = getCurrentPP () in
            let _, mayPt = SPTA.derefALvalAt pp alv in
            alv :: mayPt
        | _ ->
            logError "wrapDoMinus: unknown lv structure";
            []
    in
(*
    let newst =
      List.fold_left
        (fun st tlv ->
           self#wrapDoMinusA st tlv v ~cr:cr
        ) (relStMan#injRel relStMan#initialState rs) targetLvs
    in
    relStMan#projRel newst
*)
    List.fold_left (fun rs tlv -> relStMan#doMinus rs tlv (v, cr)) 
      rs targetLvs (* avoid some calls to inject *)
    
end 


module type RADAR_RMS_SUPPLIES = sig

  type st
  type relSt
  type part
  type key = Lvals.aLval
  type value
  type info = value * Crumbs.t

  val relStMan : (st, relSt, part, key, info, st) relativeState

  module DataflowSummary : Backed_summary.S with type sum = st
  val seqSums : DataflowSummary.data
  (*val adjsums : DataflowSummary.data*)
  val adjSumsL : DataflowSummary.data
  val adjSumsNL : DataflowSummary.data
  (*val pardb : PA.PARsummary.data (* Backed_summary.base *) (* TODO are db types ok? *)*)
  val pessSums : DataflowSummary.data
  val parL : Pseudo_access.PARsummary.data
  val parNL : Pseudo_access.PARsummary.data

  val name : string
  val ext : string
  val scopeIt : fundec -> st -> st
end


module Radar_RMS_1 (R : RADAR_RMS_PARAMS_1)
       : RADAR_RMS_SUPPLIES with type value = R.VT.value = struct
       (* rkc: added this constraint to make end transfer function happy *)

  let ext = R.summaryExt
  let name = R.name

  module RT = MakeCrumbyRelType (R.VT)
  module RS = Relative_set.Make (RT)

  module DataflowSummary = Safer_sum.Make
    (struct
       type t = RS.relSet option
       type simpleSum = t
       let simplify = function None -> None | Some dfs -> Some (RS.unique dfs)
       let desimplify = function None -> None | Some dfs -> Some (RS.unique dfs)
       let initVal = None
       let unknownSummary = Some (RS.empty)
     end)

  let seqSums = new DataflowSummary.data 
    (Backed_summary.makeSumType (catSeq ext))
  let adjSumsL = new DataflowSummary.data 
    (Backed_summary.makeSumType (catAdjL ext))
  let adjSumsNL = new DataflowSummary.data 
    (Backed_summary.makeSumType (catAdjNL ext))
  let parL = new Pseudo_access.PARsummary.data 
    (Backed_summary.makeSumType (catParL ext))
  let parNL = new Pseudo_access.PARsummary.data 
    (Backed_summary.makeSumType (catParNL ext))
  let pessSums = new DataflowSummary.data 
    (Backed_summary.makeSumType (catPess ext))

  (* TODO should I set sums fields explicitly, instead of include R? *)
  module Rblah = struct
    include R
    type st = RS.relSet option
    let sums = seqSums (* arbitrary, to set correct summary type *)
  end

  module DFM = MakeDataflowManager (RS) (Rblah)

  type st = RS.relSet option
  type relSt = RS.relSet
  type key = Lvals.aLval
  type value = R.VT.value (* client-supplied value type *)
  type info = RT.value    (* client-supplied value type + crumbs *)
  type part = info RS.S.t

  (* we'll use the same relative state manager for both passes, but we will
   * need to hook in the correct summary database for each pass *)
  let relStMan = new DFM.RSM.rc

  let scopeIt fdec dfs =
    match dfs with
      | None   -> None
      | Some x -> Some (DFM.R.scopeDfs fdec x)

end



module type RADAR_RMS_PARAMS_2 = sig
  type st
(*
  val seq : st radarSEQTRANSFUNC
*)
  class seq : [st] radarSEQTRANSFUNC
                                     
end


module Radar_RMS_2 
  (A : RADAR_RMS_SUPPLIES)
  (R : RADAR_RMS_PARAMS_2 with type st = A.st)
  : RADAR_FIXPOINT_PASS with type RA_SEQ.st = A.st = struct

  module RadarParams = struct
  
    type st = A.st
    type sum = A.st
    let name = A.name
    let summaryExt = A.ext
    let stMan = (A.relStMan :> (st, sum) stateLattice)
    let printState = stMan#printState
    (*let seq = R.seq *)
    class seq = R.seq
    let seqSums = A.seqSums
    (*let adjsums = A.adjsums*)
    let adjSumsL = A.adjSumsL
    let adjSumsNL = A.adjSumsNL
    (*let pardb = A.pardb*)
    let parL = A.parL
    let parNL = A.parNL
    let pessSums = A.pessSums

    class summarizer (sums: st Backed_summary.base) = object (self)
      inherit [st, st] IntraDataflow.summaryIsOutput stMan sums
      method scopeIt = A.scopeIt
    end

  end

  module Z = MakeRadarForwardDFs (RadarParams)

  (* expose RADAR_FIXPOINT_PASS *)
  include Z

end



(******************************************************************************)
(*** TESTING                                                                ***)
(******************************************************************************)

(*
module YY_VT = struct
  type value = unit
  let compareV a b = Some 0
  let combineV a b = ()
  let equalV a b = true
end

module YY_P1 = struct
  module VT = YY_VT
  let name = "dummy_flow"
  let summaryExt = "dum"
end

module YY_S = Radar_RMS_1 (YY_P1)

class testTransF stMan =
  object
  inherit [YY_S.st, YY_S.relSt, YY_S.part, YY_S.info] rmsSeqTransFunc stMan

  method handleInstr i st = DF.Default

end

module YY_P2 = struct
  type st = YY_S.st
  let seq = new testTransF YY_S.relStMan
end

module YY_RadaredAnalysis = Radar_RMS_2 (YY_S) (YY_P2)
*)




