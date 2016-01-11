(** Query a very simple sharing/escape analysis. Basically just checks
    the alias analysis for reachability from thread-root-formals / 
    globals. Doesn't require actual usage within more than one thread.
*)

open Cil
open Scope
open Callg
open Lvals


(*************************************************
             Super coarse, scope-based 
**************************************************)


(** Determine if the (lval, location) should be tracked as shared and
    can lead to finding a race *)
val isShareable : fundec -> lval -> scope option

val isShareableAbs : fundec -> aLval -> scope option

(** True if a read/write on the memory location of this var matters *)
val varShareable : varinfo -> bool


(*************************************************
     Less coarse but still coarse FI-AA-based 
**************************************************)

val initEscapeable : callG -> unit
  
val escapeableAbs : aLval -> bool
