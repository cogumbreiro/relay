open Fstructs

(******* sumKey operations and typical datastructures *******)

type sumKey = fKey * string

val compareSumKey : sumKey -> sumKey -> int
val string_of_sumKey : sumKey -> string 
val sumKey_of_string : string -> sumKey
val fkey_of_sumKey : sumKey -> fKey

val inputFreeSumKey : fKey -> sumKey

val wildCardkey : fKey -> sumKey
val isWildCard : sumKey -> bool

module SM : Map.S with type key = sumKey

val separator : string
val splitter : string -> string list
