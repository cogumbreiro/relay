(** Simplified canonical represenation of an lval *)

open Cil
open Pretty

exception ExpNotLval (* of string *)

(* String describing the lval (or exp that may be an lval) and the 
   location lval was declared, if known (may not know if exp is not an lval) *)
type canon_lvalexp = {
  canon_desc : string;
  canon_loc  : location option;
}

val findTopLval : exp -> lval

val findTopVinfo : lval -> varinfo
      
val findTopLoc : lval -> location

val canonicize_lvexp : exp -> canon_lvalexp

val canonicize_lval : lval -> canon_lvalexp

