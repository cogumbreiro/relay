(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following 
  conditions are met:
  
  1. Redistributions of source code must retain the above copyright 
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above 
  copyright notice, this list of conditions and the following disclaimer 
  in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the University of California, San Diego, nor 
  the names of its contributors may be used to endorse or promote 
  products derived from this software without specific prior 
  written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
*)


(** Module defining a guarded access (a read/write of an lval + the
    set of locks held at the time of access + bookkeeping).
    This version keeps info from each access separate. *)

open Fstructs
open Cil
open Pretty
open Callg
open Guarded_access_base
open Logging

module LS = Lockset
module Lv = Lvals
module Accs = Access_info


(*******************************************************************)
(* Correlation between lval access (read/write) and locks held     *)


(** Info associated with an access to an Lval *)  
type gaccess = {
  mutable corrLocks : LS.fullLS;      (* conflated version of LS *)
  mutable corrAccess : Accs.funLoc;   (* info about when lvals were accessed *)
  (* mutable corrLEmpty : Cil.location; 
     not merging locksets, so don't need this *)
  mutable corrPseudo : pseudoAttrib option;
}


(* Make sets of access infos. It is possible to generate a different 
   lockset for the location of access (given different calling contexts). 
   Because we aren't keeping the calling context, it may look weird
   to have two accesses w/ different locksets, but that is why...
   The other option is to merge different contexts.  *)

(* Compare for Set functionality (equality checking), not for subsetness *)
let compareGAcc a1 a2 =
  let c = Accs.compareFunLoc a1.corrAccess a2.corrAccess in
  if c == 0 then
    let c = LS.LS.compare a1.corrLocks a2.corrLocks in
    if c == 0 then
      Pervasives.compare a1.corrPseudo a2.corrPseudo
    else c
  else c        

module AccSet = Set.Make (
  struct
    type t = gaccess
    let compare = compareGAcc
  end)

type correlation = AccSet.t

(** map from lvalue -> set of access info (locks and bookkeeping) *)
type straintMap = correlation CMap.t


(********************** Singleton correlations *****************)


let dummyCorr = AccSet.empty


(*********************** Comparison funcs ************************)


(** True if the two correlations are exactly equal *)
let accessExactEQ (a1) (a2) : bool =
  AccSet.equal a1 a2

(** True if the two correlations are approximately subsets of 
    each other. Assumes lvals match *)
let subsAccesses (a1) (a2) =
  if (a1 == a2 || AccSet.subset a1 a2) 
  then Some (-1)
  else None

let cmEQ = CMap.equal accessExactEQ

let cmSubs = CMap.subset subsAccesses


(******************* Base Combine operations ***********************)


let combineAccs a b =
  if a == b then a
  else AccSet.union a b


(********* Hash-consing ********)

let hashCorr corr = 
  (LS.LS.hash corr.corrLocks) lxor
    (Hashtbl.hash corr.corrAccess)
    (* not hashing the rest *)

let hashAccs x =
  (* Sigh... if it was OO then it hashCorr would have been an override
     of the default hash, and we wouldn't need to write this to get it
     to call the overridden hash... *)
  let size = (AccSet.cardinal x) in
  let sizeH = Hashtbl.hash size in
  if size = 0 then sizeH
  else if size = 1 then
    let corr = AccSet.choose x in
    sizeH lxor (hashCorr corr)
  else
    let min_corr = AccSet.min_elt x in
    let minH = sizeH lxor (hashCorr min_corr) in
    let med_corr = AccSet.choose x in
    let medH = minH lxor (hashCorr med_corr) in
    let max_corr = AccSet.max_elt x in
    medH lxor (hashCorr max_corr)
      
let hashGA lv accs =
  Lv.hash_lval lv lxor hashAccs accs


(** Hash function for straintMap that only samples it *)
let hashCM (x:straintMap) = 
  if (CMap.is_empty x) then 0
  else
    let size = (CMap.cardinal x) in
    let sizeH = Hashtbl.hash size in
    if size = 1 then
      let lv, corr = CMap.choose x in
      sizeH lxor (hashGA lv corr)
    else
      let min_lv, min_corr = CMap.min_binding x in
      let minH = sizeH lxor (hashGA min_lv min_corr) in
      let med_lv, med_corr = CMap.choose x in
      let medH = minH lxor (hashGA med_lv med_corr) in
      let max_lv, max_corr = CMap.max_binding x in
      medH lxor (hashGA max_lv max_corr)


(** Hashtable for a straintMaps, requiring structural equality *)
module HashedCMap = 
  struct
    type t = straintMap
    let equal = cmEQ (* hashtbl needs exactness *)
    let hash = hashCM
  end

module HashedAccs =
  struct
    type t = AccSet.t
    let equal = AccSet.equal
    let hash = hashAccs
  end

module CMHash = Weak.Make (HashedCMap)

module AHash = Weak.Make (HashedAccs)

let cmCache = CMHash.create 237

let cmCacheMerge = CMHash.merge cmCache

let accCache = AHash.create 17


let hcAccesses accs =
  try
    AHash.find accCache accs
  with Not_found ->
    let () = AccSet.iter 
      (fun corr -> 
         corr.corrLocks <- LS.LS.unique corr.corrLocks;
         corr.corrAccess <- Accs.hcAccLocation corr.corrAccess;
      ) accs in
    AHash.add accCache accs;
    accs
  

(** Make sure the maps share lvals, etc. w/ the rest of the system *)
let hcStraints (old:straintMap) : straintMap =
  (* Hashcons the keys -- note: Mapk must not change the order of the keys *)
  let temp = CMap.mapk (fun lv -> Lv.mergeLv lv) old in
  (* Hashcons the rest *)
  CMap.map (fun accs -> hcAccesses accs) temp
    
    
(** Make sure there's only one golden copy of each correlation map *)
let uniqueCM (possiblyNewCM:straintMap) : straintMap =
  try 
    CMHash.find cmCache possiblyNewCM
  with Not_found ->
    let uniqCM = hcStraints possiblyNewCM in
    CMHash.add cmCache uniqCM;
    uniqCM


let cmUnion = CMap.union combineAccs

let combineCM (a:straintMap) (b:straintMap) : straintMap =
  if a == b then a
  else uniqueCM (cmUnion a b)


(*** All caches ***)


(** Reset the bin op caches *)
let clearCache () = begin
  LS.clearCache ();
  CMHash.clear cmCache;
  AHash.clear accCache
end

let printCacheStats () = begin
  logStatus (Stdutil.string_of_hashstats CMHash.stats
    cmCache "Golden GAs");
end



(*************** Add and update *********************)

(** Update a guarded access set by adding a new guarded access
    (assumes there was no old corresponding guarded access)  *)
let addCorr ?(pseudo:PAS.t option =None) lv curLocks location fk curMap =
  let pseudoAcc = match pseudo with None -> None
    | Some s -> 
        assert (PAS.cardinal s == 1);
        Some (PAS.min_elt s) in
  let newCorr = AccSet.singleton {corrLocks = curLocks;
                                  corrAccess = (fk, location);
                                  corrPseudo = pseudoAcc;
                                 } in
  CMap.addComb combineAccs lv newCorr curMap
(* TODO: change the pseudo to just take one thing instead of a set *)

(** Add a pseudo access w/ the given pseudoAttrib *)
let addPseudo lv curLocks location fk pseudoattrib curMap =
  addCorr ~pseudo:(Some (PAS.singleton pseudoattrib)) 
    lv curLocks location fk curMap


(** Update a guarded access set by merging a new guarded access *)
let updateAcc oldAccs updateLocks =
  AccSet.fold 
    (fun oldCorr cur ->
       let newCorr = { oldCorr with 
                         corrLocks = updateLocks oldCorr.corrLocks; } in
       AccSet.add newCorr cur
    ) oldAccs AccSet.empty


(** Update a guarded access set by merging a new guarded access *)
let updateCorr lval newAccs curConstMap =
  CMap.addComb combineAccs lval newAccs curConstMap



(************** Annotate scopes ***************)

let scopeAccs curFunc scopeLocks corrLv accs (curSet:straintMap) : straintMap =
  (* Do some pruning of what can be shared while annotating scope *)
  match Shared.isShareableAbs curFunc corrLv with
    None -> 
      CMap.remove corrLv curSet
  | _ ->
      AccSet.iter 
        (fun corr ->
           corr.corrLocks <- LS.LS.unique (scopeLocks corr.corrLocks);
        ) accs;
      curSet

let scopeStraintMap curFunc scopeLocks (cmap:straintMap) =
  CMap.fold (scopeAccs curFunc scopeLocks) cmap cmap

let corrMentionsFormal corr =
  let setHasFormal set = 
    LS.LS.S.exists
      (fun lv _ -> 
         match Lvals.getScope lv with
           Scope.SFormal _ -> true
         | Scope.SGlobal | Scope.SFunc | Scope.STBD -> false) set
  in
  AccSet.exists
    (fun ga ->
       setHasFormal (LS.LS.getPlus ga.corrLocks) ||   
         setHasFormal (LS.LS.getMinus ga.corrLocks)) corr 
         
let splitGlobalsFormals cmap =
  splitGlobalsFormalsBase corrMentionsFormal cmap


(************* Race checking ***************)

let enumAccesses cmap =
  CMap.fold (fun lval accs curList -> lval :: curList) cmap []

let iterCorrs f cmap =
  CMap.iter f cmap

let iterGuardedAccs foo accs =
  AccSet.iter 
    (fun corr ->
       let pseudoAtts = match corr.corrPseudo with 
           None -> PAS.empty | Some pa -> (PAS.singleton pa) in
       foo (Accs.Locs.singleton corr.corrAccess) corr.corrLocks 
         locUnknown pseudoAtts
    ) accs
         
let hasPseudo accs =
  AccSet.exists 
    (fun corr ->
       match corr.corrPseudo with None -> false | Some pa -> true
    ) accs

let set_of_accesses corr =
  Accs.Locs.singleton corr.corrAccess


let foreachPseudo2 foo corr1 corr2 =
  match corr1.corrPseudo, corr2.corrPseudo with
    None, None -> ()
  | None, Some ((fk, _, _) as pakey)
  | Some ((fk, _, _) as pakey), None ->
      foo fk pakey
  | Some ((fk, _, _) as pakey1), Some pakey2 when pakey1 = pakey2 ->
      foo fk pakey1
  | Some ((fk1, _, _) as pakey1), Some ((fk2, _, _) as pakey2) -> 
      foo fk1 pakey1;
      foo fk2 pakey2

(******************** Printing *********************)

let d_accLoc (fk, loc) =
  Cil.d_loc () loc

let d_accs accs printLocks =
  seq_to_doc
    Pretty.line
    AccSet.iter
    (fun corr ->
       text "loc: " ++ d_accLoc corr.corrAccess ++ text " LS: " ++
         (* Ignore printLocks *)
         LS.d_fullLS () corr.corrLocks ++ line
         (* TODO: print pseudo-acc info *)
    )
    accs
    Pretty.nil

let d_corrMap cm printLocks =
  map_to_doc 
    Pretty.line
    CMap.iter
    (fun clval accs ->
       text ((Lv.string_of_lvscope clval) ^ ": [") ++ line ++
         indent 2 (d_accs accs printLocks) ++ text "]" ++ line)
    cm
    Pretty.nil

let printCorrMap cm printLocks =
  let doc = d_corrMap cm printLocks in
  logStatusD doc;
