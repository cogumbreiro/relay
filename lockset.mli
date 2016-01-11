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

val set_to_buf : Buffer.t -> lockInfo LS.S.t -> unit

val d_lockset : unit -> lockInfo LS.S.t -> Pretty.doc

val d_fullLS : unit -> fullLS -> Pretty.doc

val inter_isEmpty : fullLS -> fullLS -> bool

val hasSummaryLock : fullLS -> bool

(******* XML printer *******)

class locksXMLPrinter : object 
  inherit lvalXMLPrinter

  method pLocks : unit -> fullLS -> Pretty.doc

end


(*******************************************************************)
(* Cache of common operation results                               *)

val clearCache : unit -> unit

module LSPH : Hashtbl.S with type key = (fullLS * fullLS)

module LSPHC : Hashtbl.S with type key = (fullLS * fullLS)

val combineLS : fullLS -> fullLS -> fullLS


