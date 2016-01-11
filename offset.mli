(** Additional operations related to Cil offsets *)

open Cil

exception UnknownOffset

val bitsToOffset : typ -> int -> offset

val bitsToOffsetNoWrap : typ -> int -> offset

val bitsToOffsetAll : typ -> int -> offset list

val canonicizeOff : bool -> offset -> offset * bool
