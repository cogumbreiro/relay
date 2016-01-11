open Symsummary
open Callg


(* TODO: change input from Racesummary.RaceSum.data to any class which
   has the ability to list MOD info *)
val init : Config.settings -> simpleCallG -> Modsummary.modSumm -> unit

val doSymState : Cil.fundec -> bool

val printExitState : unit -> unit

val derefLvalAtInstr : Cil.stmt -> Cil.instr -> Cil.lval -> 
  (bool * (Lvals.aLval list))

val getAliasesAtInstr : Cil.stmt -> Cil.instr -> Cil.exp -> 
  (bool * (Lvals.aExp list))

val printCurFuncState : unit -> unit
