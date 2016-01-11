open Cil

(*** Function key data structures ***)

type fKey = int

val funToKey : fundec -> fKey

val string_of_fkey : fKey -> string

val fkey_of_string : string -> fKey

val getFkey : Cil.fundec -> fKey

val compareFKey : fKey -> fKey -> int

val dummyFKey : fKey

module FKMap : Map.S with type key = fKey

module FKSet : Sparsebitv.S

module OrderedFKeys : Map.OrderedType with type t = fKey

module IntMap : Map.S with type key = int
  
module IntSet : Sparsebitv.S


(*** Name + type string data structures *)

type fTS = string

type fNT = (string * fTS)

val string_of_fNT : fNT -> string

val compareNT : fNT -> fNT -> int

module FNTMap : Map.S with type key = fNT

module FNTSet : Set.S with type elt = fNT
      

module FNTHash : Hashtbl.S with type key = fNT

module OrderedFNTs : Map.OrderedType with type t = fNT

