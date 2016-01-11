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
open Scope
open Cilinfos

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

(*
  type lock = {
    mutable lockScope : scope;
    lockProt : lockProt;
  }
*)

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


(************ Scoping ************)


(** Annotate the lock [lv] w/ its scope, wrt to the given [curFun],
    and update the [curSet] of locks (prune locks that are out of scope) *)
let scopeLock (curFun:Cil.fundec) lv (_) (curSet: LS.value LS.S.t) =
  try
    match Scope.annotScope curFun lv with
      STBD 
    | SFunc -> LS.S.remove lv curSet
    | _ -> curSet
  with
    Scope.BadScope ->
      (* Not a valid lval? *)
      LS.S.remove lv curSet


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

    
(** Print the lock lval along with its scope and lock type *)
let string_of_lock lv lockInfo =
  let scope = Scope.getScope lv in
  (Lv.string_of_lval lv) ^ (Scope.string_of_scope scope)


(*************** Print to XML *****************)

open Pretty


class locksXMLPrinter = object (self) 
  inherit Lv.lvalXMLPrinter
    (* Inherit the ability to XML'ize lvals *)

  method pLocks () locks : doc =
    text "<locks>" ++ line ++
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
      SFormal _ ->  
        (* TODO: search for the function's actual formal 
           (so as to match var ids) *)
        let baseVar = makeVarinfo false name t in
        Scope.setScope scope baseVar;
        (Lv.CMem
           (Lv.CLval
              (Lv.CVar baseVar, NoOffset)), NoOffset)
    | SGlobal ->
        (try Hashtbl.find globalLocks name with Not_found ->
           (* Search for the global's varinfo *)
           let baseVar = match varinfo_of_name name with
               None ->
                 L.logError "makeSimpleLock: Couldn't find global varinfo";
                 makeVarinfo true name t  
             | Some vi ->
                 vi
           in
           Scope.setScope scope baseVar;
           let lv = (Lv.CVar baseVar, NoOffset) in
           Hashtbl.add globalLocks name lv;
           lv)
    | _ ->
        failwith "Can't make lock w/ unknown scope"
  in
  (lv, () )
