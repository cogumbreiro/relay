

(** Extra operations for polymorphic lists *)

val addOnce : 'a list -> 'a -> 'a list

val addOnceP : ('a -> 'a -> bool) -> 'a list -> 'a -> 'a list 

val insertSort : ('a -> 'a -> int) -> 'a -> 'a list -> 'a list
val insertSortCombine : ('a -> 'a -> int) -> ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a list

val union : 'a list -> 'a list -> 'a list 

val unionEq : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list

val mergeUnique : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
val mergeUniqueCombine : ('a -> 'a -> int) -> ('a -> 'a -> 'a) ->
  'a list -> 'a list -> 'a list


val diff : 'a list -> 'a list -> 'a list

val indexOf : ('a -> bool) -> 'a list -> int 

val listSliceHead : 'a list -> int -> 'a list

val listSliceTailRev : 'a list -> 'a list -> int -> 'a list 
        
val listSlice : 'a list -> int -> int -> 'a list 

val listSplit : 'a list -> int -> ('a list * 'a list)

val listFindReplace : ('a -> 'b -> bool) -> ('a -> 'b -> 'a) -> 'b -> 'a list -> 
  ('a * 'a list)

val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

val eq : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

val hash : ('a -> int) -> 'a list -> int

val hash_slice : ('a -> int) -> 'a list -> int -> int

val pickFromList : 'a list -> bool -> 'a option

val removeNth : 'a list -> int -> ('a * 'a list)

val removeMinShuffle : ('a -> 'a -> int) -> 'a list -> 'a * 'a list

val stealFromList : 'a list -> bool -> ('a option * 'a list) 

val pickK : int -> int -> int list 

val shuffleList : 'a list -> 'a list

val listPrefix : 'a list -> int -> 'a list 

val findStreaks : ('a -> bool) -> 'a list -> 'a list list 

val listIterOrderedPairs : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit 

val listIterPairs : ('a -> 'a -> unit) -> 'a list -> unit 

val addToPartition : ('a -> bool) -> 'a -> 'a list list -> 'a list list 
       
val rev_mapCross : ('a -> 'b list) -> 'a list -> 'b list

val mapCross : ('a -> 'b list) -> 'a list -> 'b list

