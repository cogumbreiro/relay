open Cil

(*** Function key data structures ***)

module IntMap : Map.S with type key = int
  
module IntSet : Set.S with type elt = int

type fKey = int

val funToKey : fundec -> fKey

module FMap : Map.S with type key = fKey

module FSet : Set.S with type elt = fKey

module OrderedFKeys : Map.OrderedType with type t = fKey

val string_of_fkey : fKey -> string

val fkey_of_string : string -> fKey

val getFkey : Cil.fundec -> fKey

val compareFKey : fKey -> fKey -> int

(*** Name + type string data structures *)

type fTS = string

type fNT = (string * fTS)

val string_of_fNT : fNT -> string

val compareNT : fNT -> fNT -> int

module FNTMap : Map.S with type key = fNT

module FNTSet : Set.S with type elt = fNT
      

module FNTHash : Hashtbl.S with type key = fNT

module OrderedFNTs : Map.OrderedType with type t = fNT


(*** cil expression / lval structures ***)

module OrderedExp : Map.OrderedType with type t = Cil.exp

module OrderedLval : Map.OrderedType with type t = Cil.lval

module ExpSet : Set.S with type elt = Cil.exp

module ExpMap : Map.S with type key = Cil.exp

module LvalSet : Set.S with type elt = Cil.lval

module LvalMap : Map.S with type key = Cil.lval

module HashedLval : Hashtbl.HashedType with type t = Cil.lval

module HashedExp : Hashtbl.HashedType with type t = Cil.exp

module ExpHash : Hashtbl.S with type key = Cil.exp
