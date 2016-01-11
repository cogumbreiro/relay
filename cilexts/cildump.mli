(** Collection of "to string" functions for various Cil types *)

open Cil
open Pretty

(** Make a string out of an expression *)
val string_of_exp : exp -> string

(** Make a string out of an lval *)
val string_of_lval : lval -> string

(** Make a string out of a location *)
val string_of_loc : location -> string

(** Make a string out of a prog_point *)
val string_of_pp : prog_point -> string

(** Make a string out of a call expression *)
val string_of_cexp : exp -> string 

(** Make a string out of a type *)
val string_of_ftype : typ -> string

val string_of_stmt : stmt -> string

val string_of_instr : instr -> string

val string_of_type : typ -> string

val string_of_offset : offset -> string

