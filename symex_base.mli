
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
  
module SYMEX_NULL_GEN : functor (S : SYMEX_NULL_GEN_INPUT) -> (SYMEX_NULL)
  

(** Actual Symex *)
module type SYMEX = sig
  
  (** Expose symex DF type for optimizations... *)
  type symSt
  val getSymstate : Cil.prog_point -> symSt
  val statesEqual : symSt -> symSt -> bool

  (** Basic requirements of summary used by the Symex *)
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

  module NULL: SYMEX_NULL

end
