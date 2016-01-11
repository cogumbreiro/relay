
open Intrange

(** apply ID fixer to each file rooted at the given path *)
val ensureUniqueIDs : string ->  unit


val loadRanges : string -> string RangeMap.t * string RangeMap.t
