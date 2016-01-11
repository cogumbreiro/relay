open Cil
open Lockset
open Fstructs
open Scope

(*******************************************************************)
(* Cache of common operation results                               *)

val clearCache : unit -> unit

(*******************************************************************)
(* Correlation and constraint operations                           *)

module Locs :  Set.S with type elt = fKey * Cil.location

type accessInfo = Locs.t

val firstLocation : accessInfo -> Cil.location

val string_of_accesses : accessInfo -> string

val hcAccesses : accessInfo -> accessInfo

val eqAccesses : accessInfo -> accessInfo -> bool

val combineAccs : accessInfo -> accessInfo -> accessInfo

(** Correlation is between lvalue access & a set of locks known to 
    be acquired or released *)
type correlation = {
  mutable corrScope : scope;       (* scope of memory accessed *)
  mutable corrLocks : fullLS;      (* set of possibly protecting locks *)
  mutable corrAccess : accessInfo; (* locations of shared access *)
  mutable corrLEmpty : location;   (* location locks were last made empty *)
                                   (* uses Cil.locUnknown if not known *)
}

type lv = Lvals.aLval

module CMap : Mapset.S with type key = lv

type straintMap = correlation CMap.t

(************************************************************
         Ops like subset and combine
************************************************************)

module LSPH : Hashtbl.S with type key = (fullLS * fullLS)

val hashCM : straintMap -> int

module CMPH : Hashtbl.S with type key = (straintMap * straintMap) 

val cacheCM : straintMap -> straintMap

(* Combining things *)
val combineCorr : correlation -> correlation -> correlation

val combineLS : fullLS -> fullLS -> fullLS

val combineCM : straintMap -> straintMap -> straintMap

val cmAppEQ : straintMap -> straintMap -> bool

val cmSubs : straintMap -> straintMap -> bool


(* Add things *)

val addCorr : lv -> fullLS -> location -> fKey -> scope -> straintMap -> straintMap

val updateCorr : correlation -> lv -> fullLS -> straintMap -> straintMap

(* Scope things *)

(** Usage: scopeStraintMap getIndex scopeLocks map.
    curFunc: current function
    scopeLocks: scopes out a lockset
    map: the map to scope out
*)
val scopeStraintMap : fundec -> (fullLS -> fullLS) ->  straintMap -> straintMap
