
open Cil
open Logging
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

type pseudoSet = PAS.t

let combinePseudos p1 p2 = 
  if p1 == p2 then p1
  else PAS.union p1 p2

type lv = Lv.aLval

(************************************************************)

module CMap = Mapset.Make (Lv.OrderedLval)

(** Take a scoped-out guarded access set and split globals from the rest *)
let splitGlobalsFormalsBase corrFormal (cmap : 'a CMap.t) = 
  let swap lv corr (globs, nonglobs) =
    (CMap.remove lv globs, CMap.add lv corr nonglobs)
  in
  CMap.fold 
    (fun lv corr (globs, nonglobs) ->
       match Lv.getScope lv with
         Scope.SGlobal -> 
           if corrFormal corr 
           then swap lv corr (globs, nonglobs)
           else (globs, nonglobs)
       | Scope.SFormal _ -> 
           swap lv corr (globs, nonglobs)
       | _ ->
           (* Local for function variable??? *)
           logErrorF "splitGlobalsFormals: local variable %s\n"
             (Lv.string_of_lval lv);
           failwith "splitGlobalsFormals has local"
    ) cmap (cmap, CMap.empty)


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

  val splitGlobalsFormals : straintMap -> straintMap * straintMap

end
