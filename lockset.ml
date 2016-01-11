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

(** Representation and operations on relative Locksets *)

open Cil
open Cilinfos
open Pretty

module D = Cildump
module L = Logging
module Lv = Lvals
module CLv = Cil_lvals


(************************************************************
    Lock representation and operations
************************************************************)

(** Type of access the lock can protect (reads + writes, or just reads) *)
type lockProt = 
    LPWrite
  | LPRead

(* Not using the "prot" and scope (scope is embedded in the var) 
   fields right now. *)
type lockInfo = unit

(************** Implementation of lockset based on Relative_set ************)

(** Input module for Relative_set *)
module RelLock = struct 

  type key = Lv.aLval

  type value = lockInfo

  let compareK = Lv.compare_lval

  let compareV a b = Some (0)

  let equalV a b = true

  let combineV a b = a

  let hashK = Lv.hash_lval

  let uniqueK = Lv.mergeLv

end

module LS = Relative_set.Make(RelLock)

type fullLS = LS.relSet
 

(************ Scope-based filtering ************)

(* TODO: provide a general mechanism for filtering "sets" 
   that have lvals as a component *)

open Scope

(** Annotate the lock [lv] w/ its scope, wrt to the given [curFun],
    and update the [curSet] of locks (prune locks that are out of scope) *)
let scopeLock curFun lv (_) (curSet: LS.value LS.S.t) =
  try
    match Lv.getScopeParanoid curFun lv with
      STBD 
    | SFunc ->
        L.logError ("Lock is local to " ^ curFun.svar.vname ^ 
                      ", pruning: " ^ (Lv.string_of_lval lv));
        LS.S.remove lv curSet
    | _ -> curSet
  with
    BadScope ->
      (* Not a valid lval? *)
      L.logError ("Lock has bad scope: " ^ (Lv.string_of_lval lv));
      raise BadScope
        

(** Annotate all locks in the set [ls]  w/ its scope, wrt to 
    the given function [f]. Also prune locks that are out of scope *)
let scopeLockset (f:Cil.fundec) (ls:LS.relSet) : LS.relSet = 
  let oldLocked = LS.getPlus ls in
  let scoper = scopeLock f in
  let newLocked = LS.S.fold scoper
    oldLocked oldLocked in
  let oldUnlocked = LS.getMinus ls in
  let newUnlocked = LS.S.fold scoper
    oldUnlocked oldUnlocked in
  LS.composeNew newLocked newUnlocked

  

(*********** Operations for race checking **********************)


(** @return true if the lockset uses a summary lock to ensure race-freedom *)
let hasSummaryLock ls =
  LS.S.exists
    (fun lv _ -> match lv with 
       Lv.AbsHost _, _ -> true
     | _ -> false
    ) (LS.getPlus ls)
    (* should check the size of the rep node too (if it's 1, no problem?)... *)


(** @return true if the intersection holds no locks *)
let inter_isEmpty ls1 ls2 =
  let ls1' = LS.ditchMinus ls1 in
  let ls2' = LS.ditchMinus ls2 in
  LS.emptyPlus (LS.inter ls1' ls2')

(*************** Print to XML *****************)

   
(** Print the lock lval along with its scope and lock type *)
let string_of_lock lv lockInfo =
  Lv.string_of_lvscope lv


class locksXMLPrinter = object (self) 
  inherit Lv.lvalXMLPrinter
    (* Inherit the ability to XML'ize lvals *)

  method pLocks () locks : doc =
    let sumLock = hasSummaryLock locks in
    dprintf "<locks rep=\"%B\">" sumLock ++ line ++
      LS.S.fold (fun lv _ doc -> 
                   doc ++ self#pALval (lv, None)) (LS.getPlus locks) nil ++
      text "</locks>" ++ line
      
end


(************************************************************
 Utilities for creating locks named by user-defined summaries
 If the user summary names a global lock, try to find it in
 the existing ASTs, etc.
************************************************************)

(* make sure to use the same lv for global locks *)
let globalLocks = Hashtbl.create 10


(** Make a simple lock that is a simply based on a variable,
    given the name of the variable *)
let makeSimpleLock (name:string) scope prot =
  let t = TVoid [] in
  (* Assume globals don't require deref, while formals do... *)
  let lv = match scope with
      SFormal index ->  
        let baseHost, baseOffset = Lv.makeFormalWithName name index in
        (Lv.mkMem
           (Lv.mkLval baseHost baseOffset) NoOffset)
    | SGlobal ->
        (try Hashtbl.find globalLocks name with Not_found ->
           (* Search for the global's varinfo *)
           let baseVar = match varinfo_of_name name with
               None -> L.logError
                 ("makeSimpleLock: Couldn't find global " ^ name);
                 CLv.mkVarinfo true name t
             | Some vi -> vi
           in
           (* Make sure variable already had a global scope annotation *)
           let oldScope = Scope.decipherScope baseVar in
           assert (oldScope = scope);
           let lv = (Lv.hostOfVar baseVar, NoOffset) in
           Hashtbl.add globalLocks name lv;
           lv)
    | _ ->
        failwith "Can't make lock w/ unknown scope"
  in
  (lv, () )

(************ Ugly Printing / Pretty printing *****************) 

(** Convert a (single) lockset into a string buffer *)
let set_to_buf buff s = 
  if (LS.S.is_empty s) then
    Buffer.add_string buff "empty\n"
  else begin
    Buffer.add_string buff "{";
    L.map_to_buf LS.S.iter string_of_lock s buff;
    Buffer.add_string buff ("} (" ^ (string_of_int 
                                       (LS.S.cardinal s))  ^  ")\n");
  end

(** Make a Pretty.doc out of a (single) lockset **)
let d_lockset () s =
  if (LS.S.is_empty s) then
    text "empty" ++ line
  else begin
    let header = text "{" in
    (L.map_to_doc 
       (text ", ") 
       LS.S.iter 
       (fun lv lockinfo -> text (string_of_lock lv lockinfo))
       (* TODO: have an actual doc convert, instead of "text"ing it *)
       s 
       header) ++ 
      text "} ("  ++ num (LS.S.cardinal s) ++ text ")" ++ line
  end
    
let d_fullLS () ls =
  text "L+ = " ++ d_lockset () (LS.getPlus ls) ++ 
    text "L- = " ++ d_lockset () (LS.getMinus ls) 
    



(*** Bin Ops for Locksets ***)


(* Hash for non-commuting binary ops on LSes 
   (assuming the inputs are have been hash-consed) *)
module HashedLSPointerPair =
struct
  type t = fullLS * fullLS
  let equal (f1,s1) (f2,s2) =
    (* assume all given locksets are the main copy *)
    (f1 == f2 && s1 == s2)
  let hash (f, s) = 
    (LS.hash f) lxor ((LS.hash s) lsl 3)
end

(* Commuting hash *)
module HashedLSPointerPairCommutable =
struct
  type t = fullLS * fullLS
  let equal (f1,s1) (f2,s2) =
    (* assume all given locksets are the main copy *)
    (f1 == f2 && s1 == s2) ||
      (f1 == s2 && s1 == f2)
  let hash (f, s) = (LS.hash f) lxor (LS.hash s)
end

module LSPH = Hashtbl.Make (HashedLSPointerPair)
 
module LSPHC = Hashtbl.Make (HashedLSPointerPairCommutable)

let combineLCache = LSPHC.create 17

let combineLS (a:fullLS) (b:fullLS) : fullLS =
  try 
    LSPHC.find combineLCache (a, b)
  with Not_found ->
    let result = LS.unique (LS.inter a b) in
    LSPHC.add combineLCache (a, b) result;
    result


let clearCache () = begin
  LSPHC.clear combineLCache; (* Hmm... not a big deal if this is not cleared? *)
  LS.clearCache (); (* so that the weak arrays don't get absurdly long? *)
end
