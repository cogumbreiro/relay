open Fstructs

type dbToken

type sumType


val string_of_token : dbToken -> string
val token_of_string : string -> dbToken


val dummyToken : dbToken

val init : Config.settings -> string -> Callg.simpleCallG -> unit 

val getBasename : fKey -> sumType -> string

val getFName : fKey -> sumType -> string -> string

val pathFromToken : dbToken -> string

val pathToToken : string -> dbToken

val chooseDBPath : fKey -> string * dbToken

val anyDBPath : unit -> string * dbToken

val find : fKey -> sumType -> string * dbToken

val discover : fKey -> sumType -> (fKey -> sumType -> dbToken option) -> dbToken option

val deserializeFromPath : fKey -> string -> string -> 'a * dbToken

val deserializeFromFile : string -> string -> 'a

val removeSummary : fKey -> sumType -> dbToken -> unit

val setFinal : fKey -> sumType -> unit

val isFinal : fKey -> sumType -> bool

val clearState : int -> unit

(**********************************************************)

val key_of_name : string -> fKey

val stype_of_name : string -> sumType

val possibleNames : fKey -> string list



(********************* Functor Interface ********************)

val makeSumType : string -> sumType

val string_of_sumType : sumType -> string

module type Summarizeable = sig

  type t (* type of the summary *)
  
  type simpleSum 

  val simplify : t -> simpleSum

  val desimplify : simpleSum -> t

  (** value to use when summary isn't found the first time 
      (e.g., 1st iteration of SCC) *)
  val initVal : t 

  (** default value for external functions *)
  val unknownSummary : t

end


(** Interface to summary database for maintainence, inspection, etc. 
    Things does not depend on the actual summaries *)
class type dbManagement = object

  val mutable initialized : bool

  (** Bit of reflection to identify the kind of summary tracked *)
  method sumTyp : sumType

  (** Handles any cleanup of partially read/written summaries on reboot *)
  method cleanup : unit -> unit

  (** Save all (in-memory) summaries to disk, and allow garbage collection *)
  method serializeAndFlush : unit
    
  (** evict all in-memory summaries which have already been written to disk *)
  method evictSummaries : unit

  (** Given a list of functions and storage locations, assume 
      the summaries for those functions can be found at 
      the corresponding locations *)
  method assumeComplete : ((fKey * dbToken) list) -> unit 

  (** Write the summary for the given function to disk *)
  method flushOne : fKey -> unit
    
  method evictOne : fKey -> unit

  method locate : fKey list -> (fKey * dbToken) list
    
  method sizeInMem : unit -> int

  method sizesOf : fKey list -> (fKey * int) list

  (** Initialize the summaries for special functions / external funcs *)
  method initSummaries : Config.settings -> Callg.simpleCallG -> unit

end
  
(** "Full" Interface to summary database *)
class type ['sum] base = object
  inherit dbManagement
    
  (** Find and return the summary for the given function. If "Not_found",
      return a specified initial value instead of raising the exception *)
  method find : fKey -> 'sum
    
  (** Replace an old summary (if any) for the given function w/ a new one *)
  method addReplace : fKey -> 'sum -> unit
        
  (** Load the summary from the given file *)
  method getFromFile : string -> 'sum
        
  (** Low-level serialization. Avoid using, but feel free to extend *)
  method private serialize : fKey -> 'sum -> dbToken
    
  (** Low-level deserialization. Avoid using, but feel free to extend *)
  method private deserialize : fKey -> dbToken -> 'sum * dbToken
    
  (** Log an error, given the body of the message *)
  method err : string-> unit
    
  method fold : 'a. ('sum -> 'a -> 'a) -> 'a -> 'a

end


module type S  = sig

  (** The type of summaries *)
  type sum

  class data : sumType -> [sum] base

end


module Make (I:Summarizeable) : S with type sum = I.t


(************************************************************
 Operations on all known summary DBs
 ************************************************************)

val registerType : 'a base -> unit

val getDescriptors : sumType list -> dbManagement list

val flushAll : unit -> unit

val sizeOfAll : unit -> int

val printSizeOfAll : string -> unit

