
open Cil
open Callg
open Fstructs
open Lockset
open Access_info

module Lv = Lvals

(*********** Track pseudo-access attributes here for now ***********)

type origLvId = int

type targetLvId = int

type pseudoAttrib = funID * origLvId * targetLvId

module PAS = Set.Make
  (struct
     type t = pseudoAttrib
     let compare = Pervasives.compare
   end)


type lv = Lv.aLval

module CMap = Mapset.Make (Lv.OrderedLval)

(** What guarded access implementations should provide *)
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
    funID -> straintMap  -> straintMap
    
  val updateAcc : correlation -> (fullLS -> fullLS) -> correlation
    
  val updateCorr : lv -> correlation -> straintMap -> straintMap
    
  val addPseudo : lv -> fullLS ->  location ->  funID -> pseudoAttrib -> straintMap-> straintMap

  (* Scope pruning *)

  val scopeStraintMap : fundec -> (fullLS -> fullLS) ->  straintMap -> straintMap

  (* Access things *)
    
  val enumAccesses : straintMap -> lv  list
    
  val iterCorrs :  (lv -> correlation -> unit) -> straintMap -> unit
  
  val iterGuardedAccs : 
    (accessInfo -> fullLS -> location -> PAS.t -> unit) -> correlation -> unit
    
  val hasPseudo : correlation -> bool

  val printCorrMap : straintMap -> (fullLS -> unit) -> unit

end


