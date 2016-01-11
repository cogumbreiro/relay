
open Cil
module L = Logging
module Lv = Lvals

(** Interface expected of symex implementations *)


(** Null checking stuff *)
module type SYMEX_NULL = sig

  type nullStat =
      NotNull
    | RepNode
    | Null

  val compareNullStat : nullStat -> nullStat -> int

  val expMayBeNullBool : Lvals.aExp -> bool

  val mayBeNullBool : Lvals.aExp list -> bool

  val lvMayBeNull : Lvals.aLval -> nullStat
            
  val expMayBeNull : Lvals.aExp -> nullStat

  val mayBeNull : Lvals.aExp list -> nullStat

  val filterNulls : Lvals.aExp list -> Lvals.aExp list

  val filterNullsLvals : Lvals.aLval list -> Lvals.aLval list

end

module type SYMEX_NULL_GEN_INPUT = sig
  
  val isNullHost : Lvals.aHost -> bool
    
end
  
module SYMEX_NULL_GEN (S : SYMEX_NULL_GEN_INPUT) = struct

  type nullStat =
      NotNull
    | RepNode
    | Null

  let compareNullStat a b =
    match a, b with
      NotNull, NotNull
    | Null, Null
    | RepNode, RepNode -> 0
    | NotNull, _ -> -1
    | _, NotNull -> 1
    | RepNode, _ -> -1
    | _, _ -> 1

  let rec expMayBeNull exp = 
    let warn () =
      L.logError ("assuming exp may be null: " ^ (Lv.string_of_exp exp))  
    in
    match exp with
      Lv.CLval lv
    | Lv.CAddrOf lv
    | Lv.CStartOf lv -> lvMayBeNull lv
    | Lv.CBinOp (op, e1, _, _) ->
        (match op with
           PlusPI
         | MinusPI
         | IndexPI -> expMayBeNull e1
         | _ -> 
             warn (); 
             Null)
    | _ ->
        warn ();
        Null
          
  and lvMayBeNull lv =
    match lv with
      (Lv.CVar v as h), _ -> if S.isNullHost h then Null else NotNull
    | Lv.AbsHost _, _ -> RepNode
    | (Lv.CMem e as h), _ -> 
        if S.isNullHost h then Null
        else expMayBeNull e (* shouldn't have to recurse though? *)
        
  let expMayBeNullBool exp =
    match expMayBeNull exp with
      NotNull -> false
    | _ -> true

  let mayBeNullBool exps =
    List.exists expMayBeNullBool exps

  let mayBeNull exps =
    List.fold_left 
      (fun cur exp ->
         if cur = Null then cur
         else let other = expMayBeNull exp in
         if compareNullStat cur other < 0 then other
         else cur
      ) NotNull exps

  let notNullExp exp =
    match expMayBeNull exp with
      NotNull | RepNode -> true
    | Null -> false

  let filterNulls exps =
    List.filter notNullExp exps

  let notNullLval exp =
    match lvMayBeNull exp with
      NotNull | RepNode -> true
    | Null -> false

  let filterNullsLvals lvals =
    List.filter notNullLval lvals

end


(** Actual Symex *)
module type SYMEX = sig


  (** Expose symex DF type for optimizations... *)
  type symSt
  val getSymstate : Cil.prog_point -> symSt
  val statesEqual : symSt -> symSt -> bool

  module SS : sig

    type sumval
    type sumdb = sumval Backed_summary.base

    class data : Backed_summary.sumType -> [sumval] Backed_summary.base

    val sum : sumdb

    val printSummary : sumdb -> Summary_keys.sumKey -> unit

  end

  val init : Config.settings -> Callg.callG -> Modsummaryi.modSum -> unit

  val printExitState : unit -> unit

  val derefLvalAt : Cil.prog_point -> Cil.lval -> 
    (bool * (Lvals.aLval list))

  val derefALvalAt : Cil.prog_point -> Lvals.aLval -> 
    (bool * (Lvals.aLval list))

  val getAliasesAt : Cil.prog_point -> Lvals.aLval -> 
    (bool * (Lvals.aExp list))

  val substActForm2 : Cil.prog_point -> Cil.exp list -> Lvals.aLval ->
    (bool * (Lvals.aLval list))

  val substActForm2FI : Cil.exp list -> Lvals.aLval ->
    (bool * (Lvals.aLval list))

  val substActForm3 : Cil.prog_point -> Cil.exp list -> Lvals.aLval -> (Lvals.aLval list)

  val lvalsOfExps : Lvals.aExp list -> Lvals.aLval list

  class symexAnalysis : IntraDataflow.analysis

  module NULL : SYMEX_NULL

end
