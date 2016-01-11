
open Cil
open Fstructs
open Lockset
open Access_info

(**** Associate locking, etc with an lval ****)

type lv = Lvals.aLval

module CMap : Mapset.S with type key = lv

(*********** Track pseudo-access attributes here for now ***********)

type origLvId = int

type targetLvId = int

type pseudoAttrib = Callg.funID * origLvId * targetLvId

module PAS : Set.S with type elt = pseudoAttrib

type pseudoSet = PAS.t

val combinePseudos : pseudoSet -> pseudoSet -> pseudoSet

(************************************************************)

val splitGlobalsFormalsBase : ('a -> bool) -> 'a CMap.t -> 'a CMap.t * 'a CMap.t

(************)

module type GUARDED_ACCESS = sig

  type correlation 
  type straintMap = correlation CMap.t

  val dummyCorr : correlation

  val uniqueCM : straintMap -> straintMap

  (* Combining and compare sets *)
  val combineCM : straintMap -> straintMap -> straintMap
    
  val cmSubs : straintMap -> straintMap -> bool
    
  val cmEQ : straintMap -> straintMap -> bool
    
  val clearCache : unit -> unit

  val printCacheStats : unit -> unit

  (* Add / Update things *)
    
  val addCorr : ?pseudo:PAS.t option -> lv -> fullLS -> location -> 
    Callg.funID -> straintMap  -> straintMap
    
  val updateAcc : correlation -> (fullLS -> fullLS) -> correlation
    
  val updateCorr : lv -> correlation -> straintMap -> straintMap
    
  val addPseudo : lv -> fullLS ->  location ->  Callg.funID -> pseudoAttrib -> straintMap-> straintMap

  (* Scope pruning *)

  val scopeStraintMap : fundec -> (fullLS -> fullLS) ->  straintMap -> straintMap

  (* Access things *)
    
  val enumAccesses : straintMap -> lv  list
    
  val iterCorrs :  (lv -> correlation -> unit) -> straintMap -> unit
  
  val iterGuardedAccs : 
    (accessInfo -> fullLS -> location -> PAS.t -> unit) -> correlation -> unit
    
  val hasPseudo : correlation -> bool

  val printCorrMap : straintMap -> (fullLS -> unit) -> unit

  val splitGlobalsFormals : straintMap -> straintMap * straintMap

end

