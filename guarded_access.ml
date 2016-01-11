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
open Scope

module D = Cildump
module LS = Lockset
module L = Logging
module Stat = Mystats
module Lv = Lvals
module CLv = Cil_lvals




(*******************************************************************)
(* Correlation between lval access (read/write) and locks held     *)

module Locs = Set.Make (
  struct 
    type t = (fKey * Cil.location)
    let compare (k1, l1) (k2, l2) = 
      let c = k1 - k2 in 
      if c == 0 then Cil.compareLoc l1 l2 else c
  end
)

type accessInfo = Locs.t

(** Correlation is between lvalue access & a set of locks known to 
    be acquired or released *)  
type correlation = {
  mutable corrScope : scope;          (* scope of memory accessed *)
  mutable corrLocks : LS.fullLS;      (* conflated version of LS *)
  mutable corrAccess : accessInfo;    (* info about when lvals were accessed *)
  mutable corrLEmpty : Cil.location;
  (* first place LS went from non-empty -> empty *)
}

type lv = Lvals.aLval

module CMap = Mapset.Make (Lv.OrderedLval)

(** map (from lvalue -> known correlations... the locks and bookkeeping) *)
type straintMap = correlation CMap.t



(********************** Access Info Funcs ***********************)

(** Pick out the first access location in the access info *)
let firstLocation a =
  let _, l = Locs.min_elt a in
  l

let string_of_accesses a =
  let outString = Buffer.create 10 in
  Buffer.add_string outString "[";
  Locs.iter (fun (_, l) -> Buffer.add_string outString 
               ((D.string_of_loc l) ^ ", ")) a;
  Buffer.add_string outString "]";
  Buffer.contents outString

(** Hash-cons the info related to an access *)
let hcAccesses a = 
  Locs.fold (fun (k, l) s -> Locs.add (k, CLv.distillLoc l) s) a Locs.empty

(** True if the given accesses infos are equal *)
let eqAccesses (a:accessInfo) (b:accessInfo) =
  Locs.equal a b

(** True if a location has as much info as other *)
let locSubs l1 l2 =
  if (l1 == Cil.locUnknown) then
    true
  else if (l2 == Cil.locUnknown) then
    false
  else (* neither are unknown, but don't compare the two *) 
    true

(** Merge two access infos *)
let combineAccs a1 a2 =
  Locs.union a1 a2


(*********************** Correlation funcs ************************)

(** True if the two correlations are approximately equal. 
    Assumes the keys (i.e., lvals) match *)
let corrApproxEQ (c1:correlation) (c2:correlation) : bool =
  (* assume scope also equal... *)
  (LS.LS.equal c1.corrLocks c2.corrLocks) &&
    (Locs.equal c1.corrAccess c2.corrAccess) && 
    (Cil.compareLoc c1.corrLEmpty c2.corrLEmpty == 0)
    

(** True if the two correlations are exactly equal *)
let corrExactEQ (c1:correlation) (c2:correlation) : bool =
  (LS.LS.equal c1.corrLocks c2.corrLocks) &&
    (compare c1.corrScope c2.corrScope == 0) &&
    (Locs.equal c1.corrAccess c2.corrAccess) &&
    (Cil.compareLoc c1.corrLEmpty c2.corrLEmpty == 0)


(** True if the two correlations are approximately subsets of 
    each other. Assumes lvals match *)
let subsCorr (c1:correlation) (c2:correlation) =
  if (c1 == c2 || 
        (LS.LS.subset c1.corrLocks c2.corrLocks && 
           Locs.subset c1.corrAccess c2.corrAccess &&
           locSubs c1.corrLEmpty c2.corrLEmpty))
    (* Ignore scoping *)
  then
    Some (-1)
  else
    None


let cmSubs = CMap.subset subsCorr

let cmAppEQ = CMap.equal corrApproxEQ      

let cmExEQ = CMap.equal corrExactEQ



(*******************************************************************)
(* Cache of common correlations, operation results, etc.           *)


(*** Bin Ops for Locksets ***)

module HashedLSPointerPairCommutable =
struct
  type t = LS.fullLS * LS.fullLS
  let equal (f1,s1) (f2,s2) =
    (* assume all given locksets are the main copy *)
    (f1 == f2 && s1 == s2) ||
      (f1 == s2 && s1 == f2)
  let hash (f, s) = (LS.LS.hash f) lxor (LS.LS.hash s)
end
  
module LSPH = Hashtbl.Make (HashedLSPointerPairCommutable)
  
let combineLCache = LSPH.create 17

let combineLS (a:LS.fullLS) (b:LS.fullLS) : LS.fullLS =
  try 
    LSPH.find combineLCache (a, b)
  with Not_found ->
    let result = LS.LS.unique (LS.LS.inter a b) in
    LSPH.add combineLCache (a, b) result;
    result

(*** Single correlations ***)

(** Combine two correlations by intersection. Assumes lvals match *) 
let combineCorr (a:correlation) (b:correlation) : correlation =
  let newLocks = combineLS a.corrLocks b.corrLocks in
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
    
    corrScope = 
      (match (a.corrScope, b.corrScope) with
         SGlobal, SGlobal  ->
           a.corrScope
             
       | SFormal n, SFormal m when n == m ->
           a.corrScope
             
       | SFormal n, SFormal m when n != m ->
           failwith "scoped a corr to two different formal indices"

       (* tbd comes from func application, but maybe a read/write access
          already resolved the scope *)
       | STBD, x
       | x, STBD ->
           x

       | _ ->
           failwith ("combining incompatible scopes: 1) " ^
                       (string_of_scope a.corrScope) ^ "   2) " ^
                       (string_of_scope b.corrScope) ^ "\n")
      );
  }


(*** Correlation maps ***)

(** Hash function for straintMap that only samples it *)
let hashCM (x:straintMap) = 
  if (CMap.is_empty x) then
    0
  else
    let sizeH = Hashtbl.hash (CMap.cardinal x) in
    let min_lv, min_corr = 
      CMap.min_binding x in
    let minH = sizeH lxor 
      (Lv.hash_lval min_lv) lxor 
      (LS.LS.hash min_corr.corrLocks) lxor 
      (Hashtbl.hash min_corr.corrScope) in
    let med_lv, med_corr = 
      CMap.choose x in
    let medH = minH lxor 
      (Lv.hash_lval med_lv) lxor 
      (LS.LS.hash med_corr.corrLocks) lxor 
      (Hashtbl.hash med_corr.corrScope) in
    let max_lv, max_corr = 
      CMap.max_binding x in
    medH lxor 
      (Lv.hash_lval max_lv) lxor 
      (LS.LS.hash max_corr.corrLocks) lxor 
      (Hashtbl.hash max_corr.corrScope)


(** Hashtable for a straintMaps, requiring structural equality *)
module HashedCMap = 
  struct
    type t = straintMap
    let equal = cmExEQ (* hashtbl needs exactness *)
    let hash = hashCM
  end

module CMHash = Weak.Make (HashedCMap)

let cmCache = CMHash.create 237

let cmCacheMerge = CMHash.merge cmCache

(** Make sure the maps share lvals w/ the rest of the system *)
let uniqLvalsCM (old:straintMap) : straintMap =
  CMap.mapk 
    (fun lv ->
       Lv.mergeLv lv) old
    
(** Make sure there's only one golden copy of each correlation map *)
let cacheCM (possiblyNewCM:straintMap) : straintMap =
  try 
    CMHash.find cmCache possiblyNewCM
  with Not_found ->
    let uniqCM = uniqLvalsCM possiblyNewCM in
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
  cacheCM (cmUnion a b)


(*** All caches ***)

(** Reset the bin op caches *)
let clearCache () = begin
  LSPH.clear combineLCache;
  CMPH.clear combineCMCache;
end


(*********************************************************)

(** Update a guarded access set by adding a new guarded access
    (assumes there was no old corresponding guarded access)  *)
let addCorr lv curLocks location fk scope curMap =
  let newCorr = {corrScope = scope;
                 corrLocks = curLocks;
                 corrAccess = Locs.add (fk, location) Locs.empty;
                 corrLEmpty = Cil.locUnknown;
                } in
  CMap.addComb combineCorr lv newCorr curMap


(** Update a guarded access set by merging a new guarded access *)
let updateCorr oldCorr lval newLS curConstMap =
    let newCorr =
      { oldCorr with
          corrLocks = newLS;
          corrScope = STBD;
      }
    in
    CMap.addComb combineCorr lval newCorr curConstMap
 

let scopeCorrelation curFunc scopeLocks 
    corrLv corr (curSet:straintMap) : straintMap =
  corr.corrLocks <- 
    (*    LS.LS.unique *)
    (scopeLocks corr.corrLocks);
  match corr.corrScope with
    STBD -> begin
      match Scope.annotScope curFunc corrLv with
        SGlobal
      | SFormal _ as scope -> 
          corr.corrScope <- scope; (* TODO: don't need this field *)
          curSet
      | STBD
      | SFunc ->
          (* 
             corr.corrScope <- SGlobal;
             curSet;
          *)
          CMap.remove corrLv curSet

    end
  | _ ->
      curSet

let scopeStraintMap curFunc scopeLocks (cmap:straintMap) =
  CMap.fold (scopeCorrelation curFunc scopeLocks) cmap cmap
