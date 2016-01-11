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
    set of locks held at the time of access + bookkeeping). Also
    describes sets of guarded accesses *)

open Fstructs
open Cil
open Pretty
open Callg
open Access_info
open Guarded_access_base

module D = Cildump
module LS = Lockset
module L = Logging
module Stat = Mystats
module Lv = Lvals
module CLv = Cil_lvals



(*******************************************************************)
(* Correlation between lval access (read/write) and locks held     *)


(** True if the pseudo-access attribute sets are equal *)
let pseudoAttsEQ a1 a2 =
  match a1, a2 with 
    Some s1, Some s2 -> PAS.equal s1 s2
  | None, None -> true
  | _ -> false

let pseudoAttsSubs a1 a2 =
  match a1, a2 with
    Some s1, Some s2 -> PAS.subset s1 s2
  | None, _ -> true
  | _ -> false 
      (* Do some error checking? should be comparing a pseudo access w/
         a non-pseudo access? *)


(** Correlation is between lvalue access & a set of locks known to 
    be acquired or released *)  
type correlation = {
  mutable corrLocks : LS.fullLS;      (* conflated version of LS *)
  mutable corrAccess : accessInfo;    (* info about when lvals were accessed *)
  mutable corrLEmpty : Cil.location;   (* first place LS became empty *)
  mutable corrPseudo : PAS.t option
}


(** map (from lvalue -> access info... locks and bookkeeping) *)
type straintMap = correlation CMap.t


(********************** Singleton correlations *****************)


let dummyCorr = 
  { corrLocks = LS.LS.empty;
    corrAccess = Locs.empty;
    corrLEmpty = Cil.locUnknown;
    corrPseudo = None;
  }


(*********************** Comparison funcs ************************)


(** True if the two correlations are exactly equal *)
let corrExactEQ (c1:correlation) (c2:correlation) : bool =
  (LS.LS.equal c1.corrLocks c2.corrLocks) &&
    (Locs.equal c1.corrAccess c2.corrAccess) &&
    (Cil.compareLoc c1.corrLEmpty c2.corrLEmpty == 0) &&
    (pseudoAttsEQ c1.corrPseudo c2.corrPseudo)


(** True if the two correlations are approximately subsets of 
    each other. Assumes lvals match *)
let subsCorr (c1:correlation) (c2:correlation) =
  if (c1 == c2 || 
        (LS.LS.subset c1.corrLocks c2.corrLocks && 
           Locs.subset c1.corrAccess c2.corrAccess &&
           locSubs c1.corrLEmpty c2.corrLEmpty && 
           pseudoAttsSubs c1.corrPseudo c2.corrPseudo)
     )
  then Some (-1)
  else None


let cmSubs = CMap.subset subsCorr

let cmEQ = CMap.equal corrExactEQ


(*******************************************************************)
(* Cache of common correlations, operation results, etc.           *)

(*** Combine pseudo access attributes ***)

let combinePseudo a1 a2 =
  match a1, a2 with
    None, _ -> a2
  | _, None -> a1 (* Should this even happen? *)
  | Some s1, Some s2 -> Some (combinePseudos s1 s2)


(** Combine two correlations by intersection. Assumes lvals match *) 
let combineCorr (a:correlation) (b:correlation) : correlation =
  let newLocks = LS.combineLS a.corrLocks b.corrLocks in
  let locksEmpty =
    (* record the FIRST time locks go from non-empty to empty *)
    if ((not (LS.LS.emptyPlus a.corrLocks) 
         || not (LS.LS.emptyPlus b.corrLocks)) 
        && (LS.LS.emptyPlus newLocks)) then
      if (a.corrLEmpty == Cil.locUnknown) then
        if (b.corrLEmpty == Cil.locUnknown) then
          !Cil.currentLoc
        else
          b.corrLEmpty
      else
        a.corrLEmpty
    else
      if (a.corrLEmpty == Cil.locUnknown) then
        b.corrLEmpty
      else
        a.corrLEmpty in
  { 
    corrLocks = newLocks;
    
    corrAccess = 
      combineAccs a.corrAccess b.corrAccess;
    
    corrLEmpty =
      locksEmpty;

    corrPseudo = combinePseudo a.corrPseudo b.corrPseudo;

  }


(*** Correlation maps ***)


let hashGA lv corr =
  (Lv.hash_lval lv) lxor 
    (LS.LS.hash corr.corrLocks) lxor
    (Hashtbl.hash corr.corrLEmpty)

(** Hash function for straintMap that only samples it *)
let hashCM (x:straintMap) = 
  if (CMap.is_empty x) then 0
  else
    let size = (CMap.cardinal x) in
    let sizeH = Hashtbl.hash size in
    if size == 1 then
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

module CMHash = Weak.Make (HashedCMap)

let cmCache = CMHash.create 237

let cmCacheMerge = CMHash.merge cmCache

(** Make sure the maps share lvals, etc. w/ the rest of the system *)
let hcStraints (old:straintMap) : straintMap =
  (* Hashcons the keys -- note: Mapk must not change the order of the keys *)
  let temp = CMap.mapk (fun lv -> Lv.mergeLv lv) old in
  (* Hashcons the rest *)
  let () = CMap.iter 
    (fun _ corr ->
       corr.corrLocks <- LS.LS.unique corr.corrLocks;
       corr.corrLEmpty <- CLv.distillLoc corr.corrLEmpty;
       corr.corrAccess <- hcAccesses corr.corrAccess;
    ) temp in
  temp
    

(** Make sure there's only one golden copy of each correlation map *)
let uniqueCM (possiblyNewCM:straintMap) : straintMap =
  try 
    CMHash.find cmCache possiblyNewCM
  with Not_found ->
    let uniqCM = hcStraints possiblyNewCM in
    CMHash.add cmCache uniqCM;
    uniqCM


(*** Binary ops for Correlation Maps ***)

module HashedCMPointerPairCommutable =
struct
  type t = straintMap * straintMap
  let equal (f1,s1) (f2,s2) =
    (* assume all given CMaps are the main copy *)
    (f1 == f2 && s1 == s2) ||
      (f1 == s2 && s1 == f2)
  let hash (f, s) = (hashCM f) lxor (hashCM s)
end


module CMPH = Hashtbl.Make (HashedCMPointerPairCommutable)
  
let combineCMCache = CMPH.create 17

let cmUnion = CMap.union combineCorr

let combineCM (a:straintMap) (b:straintMap) : straintMap =
  (*
  try 
    CMPH.find combineCMCache (a, b)
  with Not_found ->
    let result = cacheCM (cmUnion a b) in
    CMPH.add combineCMCache (a, b) result;
    result
  *)
  if a == b then a
  else uniqueCM (cmUnion a b)


(*** All caches ***)

(** Reset the bin op caches *)
let clearCache () = begin
  LS.clearCache ();
  CMPH.clear combineCMCache;
  CMHash.clear cmCache
end

let printCacheStats () = begin
  L.logStatus (Stdutil.string_of_hashstats CMHash.stats
    cmCache "Golden GAs");
end

(*********************************************************)

(** Update a guarded access set by adding a new guarded access
    (assumes there was no old corresponding guarded access)  *)
let addCorr ?(pseudo=None) lv curLocks location fk curMap =
  let newCorr = {corrLocks = curLocks;
                 corrAccess = Locs.add (fk, location) Locs.empty;
                 corrLEmpty = Cil.locUnknown;
                 corrPseudo = pseudo;
                } in
  CMap.addComb combineCorr lv newCorr curMap


(** Update access info across functions (at a function call).
    TODO: add calling context info? *)
let updateAcc oldAccs updateLocks =
  { oldAccs with corrLocks = updateLocks oldAccs.corrLocks; }


let updateCorr lval newCorr curConstMap =
  CMap.addComb combineCorr lval newCorr curConstMap
    

(** Add a pseudo access w/ the given pseudoAttrib *)
let addPseudo lv curLocks location fk pseudoattrib curMap =
  let newAttribs = PAS.singleton pseudoattrib in
  addCorr ~pseudo:(Some newAttribs) lv curLocks location fk curMap
    

 

let scopeCorrelation curFunc scopeLocks 
      corrLv corr (curSet:straintMap) : straintMap =
  (corr.corrLocks <- LS.LS.unique (scopeLocks corr.corrLocks);
   (* Do some pruning of what can be shared while annotating scope *)
   match Shared.isShareableAbs curFunc corrLv with
     None -> CMap.remove corrLv curSet
   | _ -> curSet
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
  setHasFormal (LS.LS.getPlus corr.corrLocks) ||   
    setHasFormal (LS.LS.getMinus corr.corrLocks)

let splitGlobalsFormals cmap =
  splitGlobalsFormalsBase corrMentionsFormal cmap

(************** Race warnings stuff ****************)

(* TODO: standardize interface *)
let enumAccesses accs =
  CMap.fold (fun lval corr curList -> lval :: curList) accs []

let iterCorrs = CMap.iter

let iterGuardedAccs foo corr =
  let pseudoAtts = match corr.corrPseudo with
      None -> PAS.empty | Some s -> s in
  foo corr.corrAccess corr.corrLocks corr.corrLEmpty pseudoAtts


let set_of_accesses corr =
  corr.corrAccess

let hasPseudo corr =
  match corr.corrPseudo with None -> false | Some s-> not (PAS.is_empty s)

let foreachPseudo2 foo corr1 corr2 =
  let unionThem cur corr =
    match corr.corrPseudo with
      None -> cur
    | Some set -> combinePseudos cur set
  in
  let doThem ((fk, _, _) as pakey) =
    foo fk pakey
  in
  let pseudos = unionThem PAS.empty corr1 in
  let pseudos = unionThem pseudos corr2 in
  PAS.iter doThem pseudos

let locks_of_access corr =
  corr.corrLocks


(******************** Printing *********************)

let printCorrMap cm printLocks =
  CMap.iter 
    (fun clval c ->
       L.logStatus ((Lv.string_of_lvscope clval) ^ ": " ^ 
                      (string_of_accesses c.corrAccess) ^ "~");
       printLocks c.corrLocks;
       L.logStatus ""
    ) cm
