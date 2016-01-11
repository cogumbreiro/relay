
(***************************************************
 * Augment the standard library
 ***************************************************)

val quit : int -> 'a

val getUsageString : string -> string

val clearDir : string -> (string -> bool) -> unit

val clearDirGen : int -> string -> (unit -> unit) -> unit 

val addOnce : 'a list -> 'a -> 'a list

val addOnceP : ('a -> 'a -> bool) -> 'a list -> 'a -> 'a list

val union : 'a list -> 'a list -> 'a list

val mapSize : 'a -> (('b -> 'c -> int -> int) -> 'a -> int -> int) -> int

val indexOf : ('a -> bool) -> 'a list -> int



(***************************************************
 * File / resource functions
 ***************************************************)

val get_extension : string -> string

val open_for : ('a -> 'b) -> ('b -> unit) ->  'a  -> ('b ->  'c) -> 'c


(** Some shortcuts for files *)
val open_in_bin_for : string -> (in_channel -> 'a) -> 'a

val open_in_for : string -> (in_channel -> 'a) -> 'a

val open_out_bin_for : string -> (out_channel -> 'a) -> 'a

val open_out_for : string -> (out_channel -> 'a) -> 'a

(** Some shortcuts for sockets *)

open Unix

val open_conn_for : sockaddr -> 
  ((file_descr * in_channel * out_channel)-> 'a) -> 'a


(************************************************************
    Read/write simple data structures into simple text files
 ***********************************************************)


val tableToFile : ?sep:string -> (string, string) Hashtbl.t -> string -> unit

val fileToTable : ?sep:string -> string -> (string, string) Hashtbl.t 
