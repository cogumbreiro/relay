open Fstructs

type dbToken

type sumType


val string_of_token : dbToken -> string
val token_of_string : string -> dbToken

type 'a sumStub = (* parameter is summary type *)
    InMemSumm of (dbToken option * 'a) (* old storage location, if known *)
  | OnDiskSumm of dbToken

val dummyToken : dbToken

val init : Config.settings -> string -> unit 

val deserializeFromToken : fKey -> sumType -> dbToken -> 'a

val pathFromToken : dbToken -> string

val pathToToken : string -> dbToken

val chooseDBPath : fKey -> string * dbToken

val anyDBPath : unit -> string * dbToken

val find : fKey -> sumType -> string * dbToken

val discover : fKey -> sumType -> (fKey -> sumType -> dbToken option) -> dbToken option

val deserializeFromPath : fKey -> string -> string -> 'a * dbToken

val deserializeFromFile : string -> string -> 'a

val serializeSummary : fKey -> string -> 'a -> dbToken

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

  val id : sumType

  val simplify : t -> simpleSum

  val desimplify : simpleSum -> t

  (** value to use when summary isn't found *)
  val initVal : t 

end


(** Class representing interface to summary database *)
class type ['sum] base = object
  
  method cleanup : unit -> unit
    
  method find : fKey -> 'sum
    
  method addReplace : fKey -> 'sum -> unit
    
  method serializeAndFlush : unit
    
  method evictSummaries : unit
    
  method getFromFile : string -> 'sum
    
  method assumeComplete : ((fKey * dbToken) list) -> unit 
    
  method private serialize : fKey -> 'sum -> dbToken
    
  method private deserialize : fKey -> dbToken -> 'sum * dbToken
    
  method err : string-> unit
    
  method flushOne : fKey -> unit
    
  method locate : (fKey list) -> (fKey * dbToken) list
    
  method typ : sumType
    
end

module type S  = sig

  (** The type of summaries *)
  type sum

  class data : [sum] base

end


module Make (I:Summarizeable) : S with type sum = I.t


(************************************************************
  Interface / callbacks that are not parametric 
  (and therefore can be placed in collections (like lists)
 ************************************************************)

type sumDescriptor = {
  sumTyp : sumType;
  sumCompletor : (fKey * dbToken) list -> unit;
  (* locator, etc ? *)
}

val registerType : ('a) base -> unit

(* TODO: don't expose this... only expose what's needed (how to
   link summary type w/ the analysis? *)
val allTypes : sumDescriptor list ref
