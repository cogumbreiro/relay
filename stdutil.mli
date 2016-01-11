
(***************************************************
 * Augment the standard library
 ***************************************************)

val getUsageString : string -> string

val printCmdline : unit -> unit

val clearDir : string -> (string -> bool) -> unit

val clearDirGen : int -> string -> (unit -> unit) -> unit 

(********* Collection utils ************)

val mapSize : 'a -> (('b -> 'c -> int -> int) -> 'a -> int -> int) -> int

val seqToString : (('a -> unit) -> 'b -> unit) -> 'b -> 
  ('a -> string) -> string -> string

val mapToList : (('a -> 'b -> ('a * 'b) list -> ('a * 'b) list) -> 
                   'c -> ('a * 'b) list -> ('a * 'b) list) ->
  'c -> ('a * 'b) list

    
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


(********************** Hashtbl utils ***********************)

val string_of_hashstats : ('t -> int * int * int * int * int * int) -> 
  't -> string -> string

(************************************************************)

val negF : ('a -> bool) -> ('a -> bool)
