(** Additional operations related to Cil offsets *)

open Cil

exception UnknownOffset

val bitsToOffset : typ -> int -> offset
