

(* theorem provers are incomplete, so often "Unknown" is the answer *) 
type tp_result = 
  | Must_Be_True
  | Must_Be_False
  | Unknown


(* Decide if a given boolean exp can be proven true
 * Assumptions list -> boolean exp -> result 
 *) 
val decide : Cil.exp list -> Cil.exp -> tp_result

val setQuiet : bool -> unit 

val setDebug : bool -> unit 
