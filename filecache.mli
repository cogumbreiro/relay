
open Fstructs
open Cilfiles


(** Polymorphic file caches *)
module C : Cache.S with type key = string


exception File_not_found of string

class ['a] fcache :
  (string -> 'a) -> string -> int ->
object
  val loader : (string -> 'a)
  method addFile : C.key -> 'a -> unit
  method clear : unit -> unit
  method getFile : C.key -> 'a
  method resize : int -> unit
  method setRoot : string -> unit
end


(************** Basic Loaders ****************)

val cilLoader : string -> Cil.file

val viLoader : string -> viMap

val ciLoader : string -> ciMap

