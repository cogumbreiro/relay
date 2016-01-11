
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
    | Lv.CMem e, _ -> expMayBeNull e (* shouldn't have to recurse though *)
        
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

end


(** Actual Symex *)
module type SYMEX = sig

  open Fstructs
  open Callg

  module SS : sig

    type sumval
    type sumdb = sumval Backed_summary.base

    class data : Backed_summary.sumType -> [sumval] Backed_summary.base

    val sum : sumdb

    val printSummary : sumdb -> fKey -> unit

  end

  val init : Config.settings -> simpleCallG -> Modsummaryi.modSum -> unit

  val printExitState : unit -> unit

  val derefLvalAt : Cil.prog_point -> Cil.lval -> 
    (bool * (Lvals.aLval list))

  val derefALvalAt : Cil.prog_point -> Lvals.aLval -> 
    (bool * (Lvals.aLval list))

  val getAliasesAt : Cil.prog_point -> Lvals.aLval -> 
    (bool * (Lvals.aExp list))

  val substActForm2 : Cil.prog_point -> Cil.exp list -> Lvals.aLval ->
    (bool * (Lvals.aLval list))

  val substActForm3 : Cil.prog_point -> Cil.exp list -> Lvals.aLval -> (Lvals.aLval list)

  val lvalsOfExps : Lvals.aExp list -> Lvals.aLval list

  class symexAnalysis : Analysis_dep.analysis

  module NULL : SYMEX_NULL

end
