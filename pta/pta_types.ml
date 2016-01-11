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
open Cil
open Cilinfos

module HC = Simplehc
module D = Cildump
module L = Logging

(**************** Basic types ************************)

type vid = int
  
type ctyp = string

type vinfo = vinfo_base HC.hash_consed    (* Information from a varinfo *)
and vinfo_base =  
    (* original program variables *)
    PGlobal of vid * ctyp
  | PLocal  of vid * ctyp

    (* made-up variables *)
  | PRet    of vid                        (* fun's vid *)

type ptaOff = Cil.offset HC.hash_consed

(* TODO: don't need to hashcons the pairing itself *)

type ptaLv = ptaLv_base HC.hash_consed
and ptaLv_base = (ptaHost * ptaOff)

and ptaHost = ptaHost_base HC.hash_consed
and ptaHost_base =
    PVar of vinfo
  | PDeref of ptaRv

and ptaRv = ptaRv_base HC.hash_consed
and ptaRv_base =
    PLv of ptaLv
  | PAddrOf of ptaLv
  | PCast of ctyp * ptaRv (* need to track what type to assign to/from *)



(** Information from a fundec *)
type funInfo =  {         
  funId :      int;
  funType :    ctyp;
  funFormals : vinfo list;
}


(** Information from a call site *)
type callInfo = {
  cexp  : ptaLv list;      (* list of possible functions called *)
  ctype : ctyp;            (* type of func called *)
  cloc  : Cil.location;
}

(** LHS, RHS *)
type ptaAssign = ptaLv * ptaRv

(** what the call expression is, the index of the actual, and 
    the actual (may have multiple representatives for the actual 
    in the case of funky ptr arith) *)
type ptaCall = callInfo * int * ptaRv list

(** special index for return values (never an index of an actual 
    argument) *)
let retIndex = -1


(**************** UTIL / Data-structure stuff *******)


let compareType a b = 
  Pervasives.compare a b (* Ciltools.compare_type a.HC.node b.HC.node *)

let compareVar a b =
  match a, b with
    PGlobal (i1, t1) , PGlobal (i2, t2) -> 
      let comp_id = i1 - i2 in
      if (comp_id == 0) then
        compareType t1 t2
      else
        comp_id

  | PLocal (i1, t1), PLocal (i2, t2) -> 
      let comp_id = i1 - i2 in
      if (comp_id == 0) then
        compareType t1 t2
      else
        comp_id

  | PRet fi1, PRet fi2 ->
      fi1 - fi2

  | _ -> Pervasives.compare a b


let comparePtOff = Ciltools.compare_offset

let rec comparePtLv (h1,o1) (h2,o2) =
  let ch = compareHost h1.HC.node h2.HC.node in
  if (ch == 0) then
    comparePtOff o1.HC.node o2.HC.node 
  else
    ch

and compareHost a b =
  match a, b with
    (PVar v1, PVar v2) ->
      compareVar v1.HC.node v2.HC.node
        
  | (PDeref r1, PDeref r2) ->
      comparePtRv r1.HC.node r2.HC.node

  | _ -> Pervasives.compare a b


and comparePtRv a b =
  match a, b with
    PAddrOf l1, PAddrOf l2
  | PLv l1, PLv l2 ->
      comparePtLv l1.HC.node l2.HC.node
  | PCast (t1, r1), PCast (t2, r2) ->
      let crv = comparePtRv r1.HC.node r2.HC.node in
      if (crv == 0) then
        compareType t1 t2
      else
        crv
  | _, _ ->
      Pervasives.compare a b

let compareLvList l1 l2 =
  let len_comp = (List.length l1) - (List.length l2) in
  if (len_comp == 0) then
    List.fold_left2 
      (fun comp_val lv1 lv2 ->
         if (comp_val == 0) then
           comparePtLv lv1.HC.node lv2.HC.node
         else
           comp_val) 0 l1 l2
  else
    len_comp

let compareRvList l1 l2 =
  let len_comp = (List.length l1) - (List.length l2) in
  if (len_comp == 0) then
    List.fold_left2 
      (fun comp_val lv1 lv2 ->
         if (comp_val == 0) then
           comparePtRv lv1.HC.node lv2.HC.node
         else
           comp_val) 0 l1 l2
  else
    len_comp



let compareCallInfo 
    ({cexp = ce1; ctype = ct1; cloc = cl1;})
    ({cexp = ce2; ctype = ct2; cloc = cl2;}) =
  let comp_l = Cil.compareLoc cl1 cl2 in
  if (comp_l == 0) then
    let comp_t = compareType ct1 ct2 in
    if (comp_t == 0) then
      compareLvList ce1 ce2
    else
      comp_t
  else
    comp_l
  

let compareCall 
    ((ci1, i1, lvs1) : callInfo * int * ptaRv list) 
    ((ci2, i2, lvs2) : callInfo * int * ptaRv list) =
  let comp_index = i1 - i2 in
  if (comp_index == 0) then
    let comp_ci = compareCallInfo ci1 ci2 in
    if (comp_ci == 0) then
      compareRvList lvs1 lvs2
    else
      comp_ci
  else
    comp_index



(****************** Stuff for hash consing ***************)

let hashType t = 
  Hashtbl.hash t

let hashVar a = 
  match a with
    PGlobal (i, t)
  | PLocal (i, t) -> (i lxor 3284983) lxor (hashType t)
  | PRet (i) -> i lxor 194749

let hashPtOff = Ciltools.hash_offset

let rec hashPtLv (host,off) = 
  (hashHost host.HC.node) lxor (hashPtOff off.HC.node)
    
and hashHost = function
    PVar v ->
      89721137 lxor (hashVar v.HC.node)
  | PDeref r ->
      12483241 lxor (hashPtRv r.HC.node)

and hashPtRv x =
  match x with
    PLv l ->
      71458499 lxor (hashPtLv l.HC.node)
  | PAddrOf l ->
      29143982 lxor (hashPtLv l.HC.node)
  | PCast (_, r) ->
      98721211 lxor (hashPtRv r.HC.node)

module HashedType =
struct 
  type t = ctyp
  let equal a b = compareType a b == 0
  let hash = hashType
end

module TyH = HC.Make (HashedType)

module HashedOffset =
struct 
  type t = Cil.offset
  let equal a b = Ciltools.compare_offset a b == 0
  let hash = Ciltools.hash_offset
end

module OH = HC.Make (HashedOffset)

module HashedPTAVar = 
struct
  type t = vinfo_base
  let equal a b = compareVar a b == 0
  let hash = hashVar
end

module VH = HC.Make (HashedPTAVar)

module HashedHost =
struct
  type t = ptaHost_base
  let equal a b = compareHost a b == 0
  let hash = hashHost
end

module HH = HC.Make (HashedHost)
      
module HashedPtaLv = 
struct
  type t = ptaLv_base
  let equal a b = comparePtLv a b == 0
  let hash = hashPtLv
end

module LVH = HC.Make (HashedPtaLv)

module HashedPtaRv = 
struct
  type t = ptaRv_base
  let equal a b = comparePtRv a b == 0
  let hash = hashPtRv
end

module RVH = HC.Make (HashedPtaRv)

module StringHash = 
struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end

module SH = Weak.Make(StringHash)


let strings = SH.create 237

let type_tab = TyH.create 237

let off_tab = OH.create 237

let var_tab = VH.create 237

let ho_tab = HH.create 237

let lv_tab = LVH.create 237

let rv_tab = RVH.create 237


(* Hashconsed constructors *)

let makeType t =
  SH.merge strings (D.string_of_ftype t)
  (*
  TyH.hashcons type_tab t
  *)

let makeOff o =
  OH.hashcons off_tab o

let makeVar v = 
  VH.hashcons var_tab v

let makeHost h =
  HH.hashcons ho_tab h

let makeLv lv =
  LVH.hashcons lv_tab lv

let makeRv rv =
  RVH.hashcons rv_tab rv

let rec makeDeref (rv : ptaRv) : ptaLv = 
  match rv.HC.node with
    PAddrOf lv' -> lv'
  | PCast (t, rv') -> begin
      let tempLv = makeDeref rv' in
      let ho, off = tempLv.HC.node in
      match ho.HC.node, off.HC.node with
        (PDeref x, _) ->
          let innerRv = makeRv (PCast (t, x)) in
          makeLv 
            (makeHost (PDeref innerRv), 
             off)
      | _ ->
          tempLv (* Deref was stripped, so it doesn't need the cast? *)
    end
  | PLv _ ->
      makeLv 
        (makeHost (PDeref rv), 
         makeOff NoOffset)
          
let makeAddr (lv : ptaLv) : ptaRv =
  let ho, off = lv.HC.node in
  match ho.HC.node, off.HC.node with
    (PDeref x, NoOffset) -> x
  | _ -> makeRv (PAddrOf lv)

(************ Datastructs for use after hashconsing *****)

module VarH = Hashtbl.Make (
  struct 
    type t = vinfo
    let equal a b = compareVar a.HC.node b.HC.node == 0
    let hash a = hashVar a.HC.node
  end
)

module LvalH = Hashtbl.Make (
  struct
    type t = ptaLv
    let equal a b = comparePtLv a.HC.node b.HC.node == 0
    let hash a = hashPtLv a.HC.node
  end
)

module LVS = Iset.Make (
  struct 
    type t = ptaLv
    let compare a b = comparePtLv a.HC.node b.HC.node
  end
)

let addOnce eq x curList =
  if (List.exists 
        (fun y ->
           eq x y) curList) then
    curList
  else
    x :: curList


let addOnceAssign =
  addOnce (fun ((l, r):ptaAssign) ((l', r'):ptaAssign) ->
             (comparePtLv l.HC.node l'.HC.node == 0) &&
               (comparePtRv r.HC.node r'.HC.node == 0))
    
let addOnceCall = addOnce (fun c1 c2 -> compareCall c1 c2 == 0)

(************* Re-hashing (in case objs were serialized *)

let rehashOff o = makeOff o.HC.node

let rehashType t = 
  SH.merge strings t (*  makeType t.HC.node *)

let rehashVar v =
  match v.HC.node with
    PGlobal (id, t) ->
      makeVar (PGlobal (id, rehashType t))
  | PLocal (id, t) ->
      makeVar (PLocal (id, rehashType t))
  | PRet id ->
      makeVar (PRet id)

let rec rehashRv rv =
  match rv.HC.node with
    PLv lv ->
      makeRv (PLv (rehashLv lv))
  | PAddrOf lv ->
      makeRv (PAddrOf (rehashLv lv))
  | PCast (t, rv') ->
      makeRv (PCast (rehashType t, rehashRv rv'))

and rehashLv lv =
  let ho, off = lv.HC.node in
  makeLv (rehashHost ho, rehashOff off)

and rehashHost h =
  match h.HC.node with
    PVar v ->
      makeHost (PVar (rehashVar v))
  | PDeref rv ->
      makeHost (PDeref (rehashRv rv))

let rehashAssign (lv, rv) =
  (rehashLv lv, rehashRv rv)

let rehashCallInfo ({cexp = cLvList;
                    ctype = ct;} as ci) =               
  { ci with
      cexp = List.map rehashLv cLvList;
      ctype = rehashType ct;
  }


let rehashCall (ci, index, rvList) =
  (rehashCallInfo ci, index, List.map rehashRv rvList)


(****************** Extract parts of lval ***************)

let rec baseVar (lv:ptaLv) : vinfo =
  let ho, off = lv.HC.node in
  match ho.HC.node with
    PVar x -> x
  | PDeref rv ->
      baseVarRv rv
        
and baseVarRv rv = 
  match rv.HC.node with
    PLv l
  | PAddrOf l -> baseVar l
  | PCast (_, r) -> baseVarRv r
      

let baseVars (lvs: ptaLv list) : vinfo list =
  List.map (fun lv -> baseVar lv) lvs

let baseVarsRv (rvs: ptaRv list) : vinfo list =
  List.map (fun rv -> baseVarRv rv) rvs


(*************** Computing types on simple lvals ****)

(*

exception TypeOfError

let rec typeOfPtLv (lv:ptaLv) : Cil.typ = 
  let host, off = lv.HC.node in
  let baseType = match host.HC.node with
      PVar v -> 
        (match v.HC.node with
           PGlobal (_, t)
         | PLocal (_, t) ->
             t.HC.node
         |  _  ->
              raise TypeOfError
        )
    | PDeref ptr ->
        (match typeOfPtRv ptr.HC.node with
           TPtr (t, _) ->
             t
         | _ ->
             raise TypeOfError
        )
  in
  Stdutil.typeOffsetUnsafe baseType off.HC.node

and typeOfPtRv = function
    PAddrOf baseLv ->
      let baseT = typeOfPtLv baseLv in
      TPtr (baseT, [])  (* Hmm hash-cons this constructed type? *)
  | PLv l ->
      typeOfPtLv l
  | PCast (t, rv) ->
      t.HC.node

*)

(***************** Ugly-printing ops  ***************)

let name_of_id id =
  try
    (string_of_int id) ^ ":" ^ (getVarinfo id).vname
  with e ->
    L.logError ("No varinfo for id? " ^ (Printexc.to_string e));
    string_of_int id

let printVinfo (vi:vinfo) =
  match vi.HC.node with
    PGlobal (id,_) -> print_string ("g:" ^ (name_of_id id))
  | PLocal (id,_) -> print_string ("l:" ^ (name_of_id id))
  | PRet fid -> print_string ("r:" ^ (name_of_id fid))
      
      
let rec printPtaLv (lv:ptaLv) =
  let ho, off = lv.HC.node in
  match ho.HC.node, off.HC.node with
    (PVar vi, off) ->
      printVinfo vi; 
      if (off <> NoOffset) then print_string ".someoff"
  | (PDeref ptrRv, off) ->
      print_string "*";
      printPtaRv ptrRv;
      if (off <> NoOffset) then print_string ".someoff"


and printPtaRv (rv:ptaRv) =
  match rv.HC.node with
  | PLv lv ->
      printPtaLv lv
  | PAddrOf baseLv ->
      print_string "&";
      printPtaLv baseLv
  | PCast (t, rv) ->
      printPtaRv rv
        
let printCallCons (cinfo, index, acts) = 
  print_string "Call to: ";
  List.iter (fun funlv -> printPtaLv funlv; print_string ", ") cinfo.cexp;
  print_string ("\n" ^ (string_of_int index) ^ " arg: ");
  List.iter (fun arg -> printPtaRv arg; print_string ", ") acts;
  print_string "\n"
