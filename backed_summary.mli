open Fstructs
open Summary_keys

type dbToken
type sumType

(********)

val dummyToken : dbToken
val string_of_token : dbToken -> string
val token_of_string : string -> dbToken

val init : Config.settings -> string -> Callg.callG -> Scc_cg.sccGraph -> unit 

val getBasename : sumKey -> sumType -> string

val getFName : sumKey -> sumType -> string -> string

val pathFromToken : dbToken -> string

val pathToToken : string -> dbToken

val chooseDBPath : sumKey -> string * dbToken

val anyDBPath : unit -> string * dbToken

val discover : sumKey -> sumType -> 
  (sumKey -> sumType -> dbToken option) -> dbToken option

val deserializeFromPath : sumKey -> string -> string -> 'a * dbToken

val deserializeFromFile : string -> string -> 'a

val removeSummary : sumKey -> sumType -> dbToken -> unit

val setFinal : sumKey -> sumType -> unit

val isFinal : sumKey -> sumType -> bool

val clearState : int -> unit

(**********************************************************)

val key_of_name : string -> sumKey

val stype_of_name : string -> sumType

val possibleNames : sumKey -> string list



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

(* Only expose this sumStub so that inheriters can inspect *)
type 'a sumStub = (* parameter is summary type *)
    InMemSumm of (bool * dbToken option * 'a)
      (** dirty bit, 
          old storage location on disk if any, 
          Summary (of type 'a) *)

  | OnDiskSumm of dbToken
      (** Summary is on disk, stored at the path represented by dbToken *)


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
  method assumeComplete : ((sumKey * dbToken) list) -> unit 

  (** Write the summary for the given function to disk *)
  method flushOne : sumKey -> unit
    
  method evictOne : sumKey -> unit

  method locate : sumKey list -> (sumKey * dbToken) list
    
  method getDependentKeys : sumKey list -> (sumKey * dbManagement) list

  method sizeInMem : unit -> int

  method sizesOf : sumKey list -> (sumKey * int) list

  (** Initialize the summaries for special functions / external funcs *)
  method initSummaries : Config.settings -> Callg.callG -> Scc_cg.sccGraph -> unit

end
  
(** "Full" Interface to summary database *)
class type ['sum] base = object
  inherit dbManagement
    
  val summs : ('sum sumStub) SM.t

  (** Find and return the summary for the given function. If "Not_found",
      return a specified initial value instead of raising the exception *)
  method find : sumKey -> 'sum
    
  (** Replace an old summary (if any) for the given function w/ a new one *)
  method addReplace : sumKey -> 'sum -> unit
        
  (** Low-level management *)
  method private addReplaceBase : sumKey -> ('sum sumStub) -> unit
  method private willAdd : sumKey -> unit

  (** Load the summary from the given file *)
  method getFromFile : string -> 'sum
        
  (** Low-level serialization. Avoid using, but feel free to extend *)
  method private serialize : sumKey -> 'sum -> dbToken
    
  (** Low-level deserialization. Avoid using, but feel free to extend *)
  method private deserialize : sumKey -> dbToken -> 'sum * dbToken

  (** Low-level debug / assertion *)
  method private checkInit : unit

    
  (** Log an error, given the body of the message *)
  method err : string-> unit
    
  method fold : 'a. (sumKey -> 'sum -> 'a -> 'a) -> 'a -> 'a

  method foldOnKey : 'a. (sumKey -> 'sum -> 'a -> 'a) -> fKey -> 'a -> 'a

end


module type S  = sig

  (** The type of summaries *)
  type sum

  class data : sumType -> [sum] base

end


module Make (I:Summarizeable) : S with type sum = I.t


class ['sum] noneSummary : sumType -> ['sum] base

(************************************************************
 Operations on all known summary DBs
 ************************************************************)

val registerType : 'a base -> unit

val getDescriptors : sumType list -> dbManagement list

val flushAll : unit -> unit

val sizeOfAll : unit -> int

val printSizeOfAll : string -> unit

