

(** Module defining a guarded access (a read/write of an lval + the
    set of locks held at the time of access + bookkeeping).
    This version clusters each access by lockset 
    (those with different locksets remain separate). *)

open Fstructs
open Cil
open Pretty
open Callg
open Guarded_access_base
open Access_info
open Logging

module LS = Lockset
module Lv = Lvals

(************************************************************)

type accesses = {
  mutable corrPseudo : PAS.t;
  mutable corrAccesses : accessInfo;
}

module LMap = Mapset.Make
  (struct
     type t = LS.fullLS
     let compare = LS.LS.compare
   end)

type correlation  = accesses LMap.t

type straintMap = correlation CMap.t


(************************************************************)

let dummyCorr = LMap.empty


(******************* Comparison funcs *********************)

let accsEQ a1 a2 =
  Locs.equal a1.corrAccesses a2.corrAccesses &&
    PAS.equal a1.corrPseudo a2.corrPseudo


(** True if the two correlations are exactly equal *)
let corrExactEQ (c1:correlation) (c2:correlation) : bool =
  LMap.equal accsEQ c1 c2

let accsSubs a1 a2 =
  if Locs.subset a1.corrAccesses a2.corrAccesses &&
    PAS.subset a1.corrPseudo a2.corrPseudo 
  then Some (-1) else None
    
(** True if the two correlations are approximately subsets of 
    each other. Assumes lvals match *)
let subsCorr (c1:correlation) (c2:correlation) =
  if LMap.subset accsSubs c1 c2 
  then Some (-1) else None


let cmSubs = CMap.subset subsCorr

let cmEQ = CMap.equal corrExactEQ

(******************** Combining functions *********************)

let combineAccesses a1 a2 =
  { corrAccesses = combineAccs a1.corrAccesses a2.corrAccesses;
    corrPseudo = combinePseudos a1.corrPseudo a2.corrPseudo;
  }

(** Combine two correlations by intersection. Assumes lvals match *) 
let combineCorr (a:correlation) (b:correlation) : correlation =
  LMap.union combineAccesses a b

let combineCM (a:straintMap) (b:straintMap) : straintMap =
  CMap.union combineCorr a b
  

(******************** Hashing / Hash-consing *******************)

let hashAccesses ls accesses =
  LS.LS.hash ls

let hashCorrs corrs =
  LMap.sampleHash hashAccesses corrs

let hashGA lv corr =
  (Lv.hash_lval lv) lxor (hashCorrs corr)

let hashCM (x:straintMap) = 
  CMap.sampleHash hashGA x

(** Hashtable for a straintMaps, requiring structural equality *)
module HashedCMap = 
  struct
    type t = straintMap
    let equal = cmEQ (* hashtbl needs exactness *)
    let hash = hashCM
  end


module CMHash = Weak.Make (HashedCMap)

let cmCache = CMHash.create 237

(** Make sure the maps share lvals, etc. w/ the rest of the system *)
let hcStraints (old:straintMap) : straintMap =
  (* Hashcons the keys -- note: Mapk must not change the order of the keys *)
  let mappedLvals = CMap.mapk (fun lv -> Lv.mergeLv lv) old in
  (* Hashcons the rest *)
  let mapped = CMap.map 
    (fun corrs ->
       let mappedLSes = LMap.mapk (fun ls -> LS.LS.unique ls) corrs in
       let _ = LMap.map 
         (fun acc -> 
            acc.corrAccesses <- hcAccesses acc.corrAccesses;
         ) mappedLSes in
       mappedLSes
    ) mappedLvals in
  mapped
    

(** Make sure there's only one golden copy of each correlation map *)
let uniqueCM (possiblyNewCM:straintMap) : straintMap =
  try 
    CMHash.find cmCache possiblyNewCM
  with Not_found ->
    let uniqCM = hcStraints possiblyNewCM in
    CMHash.add cmCache uniqCM;
    uniqCM


(** Reset the bin op caches *)
let clearCache () = begin
  LS.clearCache ();
  CMHash.clear cmCache;
end

let printCacheStats () = begin
  logStatus (Stdutil.string_of_hashstats CMHash.stats
    cmCache "Golden GAs");
end



(*********************************************************)

(** Update a guarded access set by adding a new guarded access
    (assumes there was no old corresponding guarded access)  *)
let addCorr ?(pseudo=None) lv curLocks location fk curMap =
  let pseudo = match pseudo with None -> PAS.empty | Some s -> s in
  let oldLSM = try CMap.find lv curMap with Not_found -> LMap.empty in
  let newLSM = 
    let newAccs = 
      try 
        let oldAccs = LMap.find curLocks oldLSM in
        { corrAccesses = Locs.add (fk, location) oldAccs.corrAccesses;
          corrPseudo = combinePseudos pseudo oldAccs.corrPseudo; }
      with Not_found ->
        { corrAccesses = Locs.singleton (fk, location);
          corrPseudo = pseudo;
        } 
    in
    LMap.add curLocks newAccs oldLSM 
  in
  CMap.add lv newLSM curMap (* not addComb? *)


(** Update access info across functions (at a function call). *)
let updateAcc (oldAccs : correlation) (lockUpdater) =
  LMap.fold 
    (fun ls accs cur ->
       LMap.addComb combineAccesses (lockUpdater ls) accs cur
    ) oldAccs LMap.empty 


(** Add a pseudo access w/ the given pseudoAttrib *)
let addPseudo lv curLocks location fk pseudoattrib curMap =
  let newAttribs = PAS.singleton pseudoattrib in
  addCorr ~pseudo:(Some newAttribs) lv curLocks location fk curMap
(* TODO: get rid of the "option"... just use empty set, etc. *)


let updateCorr lval newCorr curConstMap =
  CMap.addComb combineCorr lval newCorr curConstMap
    

let scopeCorrelation curFunc scopeLocks 
    corrLv corr (cur:straintMap) : straintMap =
  (match Shared.isShareableAbs curFunc corrLv with
     None -> CMap.remove corrLv cur
   | _ ->
       let newCorr = updateAcc corr (fun ls -> LS.LS.unique (scopeLocks ls)) in
       CMap.add corrLv newCorr cur
  )


let scopeStraintMap curFunc scopeLocks (cmap:straintMap) =
  CMap.fold (scopeCorrelation curFunc scopeLocks) cmap cmap

let corrMentionsFormal corr =
  let setHasFormal set = 
    LS.LS.S.exists
      (fun lv _ -> 
         match Lvals.getScope lv with
           Scope.SFormal _ -> true
         | Scope.SGlobal | Scope.SFunc | Scope.STBD -> false) set
  in
  LMap.fold
    (fun locks _ has ->
       has || 
         setHasFormal (LS.LS.getPlus locks) ||   
         setHasFormal (LS.LS.getMinus locks)
    ) corr false

let splitGlobalsFormals cmap =
  splitGlobalsFormalsBase corrMentionsFormal cmap


(************** Race warnings stuff ****************)

(* TODO: standardize interface *)
let enumAccesses (accs : straintMap) =
  CMap.fold (fun lval _ curList -> lval :: curList) accs []

let iterCorrs = CMap.iter

let iterGuardedAccs foo corr =
  LMap.iter 
    (fun ls acc -> foo acc.corrAccesses ls locUnknown acc.corrPseudo) corr

let hasPseudo corr =
  LMap.exists (fun _ acc -> not (PAS.is_empty acc.corrPseudo)) corr

(******************** Printing *********************)

let printCorrMap cm printLocks =
  CMap.iter 
    (fun clval corr ->
       LMap.iter 
         (fun ls c ->
            logStatus ((Lv.string_of_lvscope clval) ^ ": " ^ 
                           (string_of_accesses c.corrAccesses) ^ "~");
            printLocks ls;
            logStatus "") corr
    ) cm
