(*
 * Reads a call graph from a file (@see dumpcall.ml for the format)
 *)

open Fstructs
open Callg

(* --- Functions --- *)

(*********************************************************************)
(* Graph ops                                                         *)

(* Set up callers / isRoot, etc. *)
val completeCG : simpleCallG -> unit

(*********************************************************************)
(* Read functions                                                    *)

(* Reads and returns a callgraph *)
val readCalls : string -> simpleCallG

