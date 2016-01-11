open Scope
open Lvals

(*******************************************************************)
(* Lock representation and operations                              *)


type lockProt = 
    LPWrite
  | LPRead
(*
type lock = {
  mutable lockScope : scope;
  lockProt : lockProt;
}
*)

type lockInfo = unit

module LS : Relative_set.S with type key = aLval and type value = lockInfo

type fullLS = LS.relSet

(** Make a simple lock that is a simply based on a variable,
    given the name of the variable *)
val makeSimpleLock : string -> scope -> lockProt -> (aLval * lockInfo)


(******* Higher Level Ops ********)

val scopeLockset : Cil.fundec ->  fullLS -> fullLS

val string_of_lock : aLval -> lockInfo -> string

(******* XML printer *******)

class locksXMLPrinter : object 
  inherit lvalXMLPrinter

  method pLocks : unit -> fullLS -> Pretty.doc

end

