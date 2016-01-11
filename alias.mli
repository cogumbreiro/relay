(** Query different alias analyses at different phases *)

open Fstructs
open Alias_types

(***********************************************************)
(* Set up                                                  *)

val initSettings : Config.settings -> string -> unit

val setCurrentFile : Cil.file -> unit

val setFilterFunSig : bool -> unit

(***********************************************************)
(* Finish up                                               *)

val finalizeAll : unit -> unit

(***********************************************************)
(* Wrapper functions around PTA                            *)

exception UnknownLoc

val may_alias : Cil.exp -> Cil.exp -> bool

val deref_exp : Cil.exp -> Cil.varinfo list

val points_to : Cil.exp -> Cil.varinfo -> bool

(** Low-level function pointer dereference. Given the actual pointer
    that is dereferenced, return the list of possible functions *)
val deref_funptr : Cil.exp -> (fKey list)

(** High-level function resolution (given the call expression) *)
val funsForCall : Cil.exp -> (fKey list)

(** High-level function resolution (given the address of a function) *)
val funsFromAddr : Cil.exp -> (fKey list)


(**********************************************************)
(* Interface using representative nodes                   *)


(* Ways to get and use abstract nodes *)

module Abs : sig
  
  val deref_lval : Cil.lval -> ptaNode list

  val deref_exp : Cil.exp -> ptaNode list

  val getNodeLval : Cil.lval -> ptaNode

  val getNodeExp : Cil.exp -> ptaNode

  val getNodeVar : Cil.varinfo -> ptaNode

  val represents : ptaNode -> Cil.lval list

  val may_alias : ptaNode -> ptaNode -> bool

  val location_alias : ptaNode -> ptaNode -> bool

  val points_to : ptaNode -> ptaNode -> bool

  val deref : ptaNode -> ptaNode

  val compare : ptaNode -> ptaNode -> int

  val hash : ptaNode -> int

  val string_of : ptaNode -> string

  val pts_size : ptaNode -> int

  val label_size : ptaNode -> int

  val reachableFrom : ptaNode -> ptaNode list -> bool

  val reachableFromG : ptaNode -> bool

end

(** Simple bit of "reflection" to identify what analysis is used *)
  
val identifyFPA : unit -> string
