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
open Pretty
open Cilinfos
open Logging

module HC = Simplehc

(**************** Basic types ************************)

type vid = int
type ckey = int  

type ctyp = ctyp_base HC.hash_consed
and ctyp_base =
    PVoid
  | PInt
  | PFloat
  | PPtr of ctyp
  | PArray of ctyp
  | PFun of ctyp * ctyp list option * bool
  | PNamed of string * ctyp
  | PComp of ccomp
  | PEnum of string
  | PBuiltin_va_list

and ccomp = ckey

type vinfo = vinfo_base HC.hash_consed    (* Information from a varinfo *)
and vinfo_base = 
    (* original program variables *)
    PGlobal of vid * ctyp
  | PLocal  of vid * ctyp

    (* made-up variables *)
  | PRet    of vid                        (* fun's vid *)
  | PTemp   of vid  (* Temp vars used to simplify constraints *)


(* Convert to bits of offset (does not modulo stride if in an array),
   width of field, and type of field/element. *)
type ptaOff = int * int * ctyp


type ptaLv = ptaHost * ptaOff

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

(**************** UTIL / Data-structure stuff *******)

let compareType a b = 
  Pervasives.compare a b (* Ciltools.compare_type a.HC.node b.HC.node *)

let rec unrollPType t =
  match t.HC.node with
    PNamed (_, t) -> unrollPType t
  | _ -> t

let isVoid t =
  match (unrollPType t).HC.node with PVoid -> true | _ -> false


let rec compatibleTypes t1 t2 =
  match t1.HC.node, t2.HC.node with
    PVoid, PVoid -> true
  | PInt, PInt -> true
  | PFloat, PFloat -> true
  | PPtr t1, PPtr t2 ->
      if isVoid t1 || isVoid t2 then true else compatibleTypes t1 t2
  | PArray t1, PArray t2 ->
      if isVoid t1 || isVoid t2 then true else compatibleTypes t1 t2
  | PFun (r1, a1, va1), PFun (r2, a2, va2) ->
      if va1 == va2 then
        compatibleTypes r1 r2 && 
          (match a1, a2 with
             None, None -> true
           | Some l1, Some l2 ->
               (try List.for_all2 compatibleTypes l1 l2 
               with Invalid_argument _ -> false)
           | _, _ -> false
          )
      else false
  | PComp c1, PComp c2 -> c1 = c2
  | PEnum e1, PEnum e2 -> e1 = e2
  | PBuiltin_va_list, PBuiltin_va_list -> true
  | PNamed (n1, t1), PNamed (n2, t2) ->
      n1 = n2 || compatibleTypes t1 t2
  | PNamed (_, t), _ -> compatibleTypes t t2
  | _, PNamed (_, t) -> compatibleTypes t1 t
  | _ -> false


let rec compatibleTypesNoUnroll t1 t2 =
  match t1.HC.node, t2.HC.node with

    PVoid, PVoid -> true
  | PInt, PInt -> true
  | PFloat, PFloat -> true
  | PPtr t1, PPtr t2 ->
      compatibleTypesNoUnroll t1 t2
  | PArray t1, PArray t2 ->
      compatibleTypesNoUnroll t1 t2
  | PFun (r1, a1, va1), PFun (r2, a2, va2) ->
      if va1 == va2 then
        compatibleTypesNoUnroll r1 r2 && 
          (match a1, a2 with
             None, None -> true
           | Some l1, Some l2 ->
               (try List.for_all2 compatibleTypesNoUnroll l1 l2 
               with Invalid_argument _ -> false)
           | _, _ -> false
          )
      else false
  | PComp c1, PComp c2 -> c1 = c2
  | PEnum e1, PEnum e2 -> e1 = e2
  | PBuiltin_va_list, PBuiltin_va_list -> true
  | PNamed (n1, t1), PNamed (n2, t2) -> n1 = n2
  | PNamed (_, t), _ -> false
  | _, PNamed (_, t) -> false
  | _ -> false


let compareVarBase a b =
  match a, b with
    PGlobal (i1, t1) , PGlobal (i2, t2) -> 
      i1 - i2
(*      let comp_id = i1 - i2 in
      if (comp_id == 0) then
        compareType t1 t2
      else
        comp_id
*)

  | PLocal (i1, t1), PLocal (i2, t2) -> 
      i1 - i2
(*
      let comp_id = i1 - i2 in
      if (comp_id == 0) then
        compareType t1 t2
      else
        comp_id
*)

  | _ -> Pervasives.compare a b


let compareVar a b =
  compareVarBase a.HC.node b.HC.node

let compareOff = Pervasives.compare (* Ciltools.compare_offset *)

let rec compareLv (h1,o1) (h2,o2) =
  let ch = compareHost h1 h2 in
  if (ch == 0) then compareOff o1 o2 
  else ch

and compareHost a b =
  compareHostBase a.HC.node b.HC.node

and compareHostBase a b =
  match a, b with
    (PVar v1, PVar v2) ->
      compareVar v1 v2
        
  | (PDeref r1, PDeref r2) ->
      compareRv r1 r2


  | _ -> Pervasives.compare a b


and compareRv a b =
  compareRvBase a.HC.node b.HC.node

and compareRvBase a b =
  match a, b with
    PAddrOf l1, PAddrOf l2
  | PLv l1, PLv l2 ->
      compareLv l1 l2
  | PCast (t1, r1), PCast (t2, r2) ->
      let crv = compareRv r1 r2 in
      if (crv == 0) then
        compareType t1 t2
      else
        crv
  | _, _ ->
      Pervasives.compare a b

(***************** Constraint types *********************)

(** Information from a call site *)
type callInfo = {
  cexp  : ptaLv list;      (* list of expressions used to call function *)
  ctype : ctyp;          (* type of func called *)
  cloc  : Cil.location;    (* location of callsite *)
}

(** Assignment *)
type ptaAssign = { 
  lhs : ptaLv;
  rhs : ptaRv;
  aloc : Cil.location;
}

type formalIndex = int

(** Fun call: What the call expression is, and the list of 
    actual -> formal (index) assignment constraints. *)
type ptaCall = callInfo * (ptaRv list * formalIndex) list

(** special "formal index" for return values *)
let retIndex = -1


let makeAssign lhs rhs loc =
  { lhs = lhs;
    rhs = rhs;
    aloc = loc; }

let compareLvList l1 l2 =
  let len_comp = (List.length l1) - (List.length l2) in
  if (len_comp == 0) then
    List.fold_left2 
      (fun comp_val lv1 lv2 ->
         if (comp_val == 0) then
           compareLv lv1 lv2
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
           compareRv lv1 lv2
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

let maybeCompareActualFormal c (acts1, fi1) (acts2, fi2) =
  if c == 0 then
    let c = fi1 - fi2 in
    if c == 0 then
      compareRvList acts1 acts2
    else c
  else c

let compareArgsList al1 al2 =
  let l1 = List.length al1 in
  let l2 = List.length al2 in
  let c = l1 - l2 in
  if c == 0 then List.fold_left2 maybeCompareActualFormal 0 al1 al2
  else c

let compareCall ((ci1, al1) : ptaCall) ((ci2, al2) : ptaCall) =
  let comp_ci = compareCallInfo ci1 ci2 in
  if (comp_ci == 0) then compareArgsList al1 al2
  else comp_ci

let compareAssign 
    ({lhs = l1; rhs = r1; aloc = loc1;}:ptaAssign) 
    ({lhs = l2; rhs = r2; aloc = loc2;}:ptaAssign) =
  let c = compareLv l1 l2 in
  if c == 0 then 
    let c = compareRv r1 r2 in
    if c == 0 then
      Cil.compareLoc loc1 loc2
    else c
  else c
    



(****************** Stuff for hash consing ***************)

let hash_type (t:ctyp_base) = 
  Hashtbl.hash t

let hashVarBase (a:vinfo_base) = 
  match a with
    PGlobal (i, t)
  | PLocal (i, t) -> 
      (i lxor 3284983) (* lxor (hash_type t.HC.node) *)
  | PRet (i) -> i lxor 194749
  | PTemp (i) -> i lxor 7080421

let hashVar a = 
  hashVarBase a.HC.node

let hashOff ((o1, o2, t):ptaOff) = 
  Hashtbl.hash (o1, o2) lxor (hash_type t.HC.node)

let rec hashLv (host,off) =
  (hashHost host) lxor (hashOff off)

and hashHost h = 
  hashHostBase h.HC.node
    
and hashHostBase = function
    PVar v ->
      89721137 lxor (hashVar v)
  | PDeref r ->
      12483241 lxor (hashRv r)

and hashRv r =
  hashRvBase r.HC.node

and hashRvBase x =
  match x with
    PLv l ->
      71458499 lxor (hashLv l)
  | PAddrOf l ->
      29143982 lxor (hashLv l)
  | PCast (_, r) ->
      98721211 lxor (hashRv r)

module HType =
struct 
  type t = ctyp_base
  let equal a b = compareType a b == 0
  let hash = hash_type
end

module TyH = HC.Make (HType)

module HOffset =
struct 
  type t = ptaOff
  let equal a b = compareOff a b == 0
  let hash = hashOff
end

module OH = HC.Make (HOffset)

module HVar = 
struct
  type t = vinfo_base
  let equal a b = compareVarBase a b == 0
  let hash = hashVarBase
end

module VH = HC.Make (HVar)

module HHost =
struct
  type t = ptaHost_base
  let equal a b = compareHostBase a b == 0
  let hash = hashHostBase
end

module HH = HC.Make (HHost)
      
module HLv = 
struct
  type t = ptaLv
  let equal a b = compareLv a b == 0
  let hash = hashLv
end

module LVH = HC.Make (HLv)

module HRv = 
struct
  type t = ptaRv_base
  let equal a b = compareRvBase a b == 0
  let hash = hashRvBase
end

module RVH = HC.Make (HRv)

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

let hcType (t : ctyp_base) : ctyp =
  TyH.hashcons type_tab t

let rec rehashType t = 
  match t.HC.node with 
    PVoid 
  | PInt
  | PFloat
  | PComp _
  | PEnum _ 
  | PBuiltin_va_list -> hcType t.HC.node (* Really? *) 
  | PPtr t1 -> hcType (PPtr (rehashType t1))
  | PFun (ret, args, var) ->
      hcType (PFun (rehashType ret,
                      (match args with
                         None -> None
                       | Some l -> Some (List.map rehashType l)), var))
  | PArray t1 -> hcType (PArray (rehashType t1))
  | PNamed (n, t) -> hcType (PNamed (n, rehashType t))

let rec cil_type_to_ptype typ =
  match typ with 
    TVoid _ -> hcType (PVoid)
  | TInt _ -> hcType (PInt)
  | TFloat _ -> hcType (PFloat)
  | TPtr (t,_) -> hcType (PPtr (cil_type_to_ptype t))
  | TArray (t, len, _) -> hcType (PArray (cil_type_to_ptype t))
  | TFun (retType, args, varargP, _) -> 
      hcType (PFun (cil_type_to_ptype retType,
                    (match args with None -> 
                       None 
                     | Some l -> Some (List.map cil_args_to_pargs l)),
                    varargP))
  | TNamed (tinfo, _) -> 
      hcType (PNamed (tinfo.tname, cil_type_to_ptype tinfo.ttype))
  | TComp (cinfo, _) -> hcType (PComp (cinfo.ckey))
  | TEnum (einfo, _) -> hcType (PEnum (einfo.ename))
  | TBuiltin_va_list _ -> hcType (PBuiltin_va_list)

and cil_args_to_pargs (n, t, a) =
  cil_type_to_ptype t

let rec ptype_to_cil_type ptyp =
  match ptyp.HC.node with
    PVoid -> TVoid []
  | PInt -> TInt (IInt, []) (* Loss of info ... *)
  | PFloat -> TFloat (FFloat, []) (* Loss of info ... *)
  | PComp k -> 
      let ci = getCinfo k in
      TComp (ci, []) (* Lose attribs *)
  | PEnum n -> 
      TEnum ({ ename = n; eitems = []; 
               eattr = []; ereferenced = true; ekind = IInt; 
             }, [])
  | PBuiltin_va_list -> TBuiltin_va_list []
  | PPtr t1 -> TPtr (ptype_to_cil_type t1, [])
  | PFun (ret, args, var) ->
      TFun (ptype_to_cil_type ret, 
            (match args with 
               Some args ->
                 Some (List.map 
                         (fun atype -> ("", ptype_to_cil_type atype, [])) args)
             | None -> None
            ), var, [])
  | PArray t1 -> 
      TArray (ptype_to_cil_type t1, None, []) (* Lose array length *)
  | PNamed (n, t) -> 
      TNamed ({ tname = n; ttype = ptype_to_cil_type t; treferenced = true; },
              [])

let rec compatibleFunSig callType targType =
  let callFunSig = Type_utils.getFunSig (ptype_to_cil_type callType) in
  let targFunSig = Type_utils.getFunSig (ptype_to_cil_type targType) in
  match targFunSig, callFunSig with
    Some fpSig, Some callSig -> 
      Type_utils.funSigSubtype fpSig callSig
  | None, _
  | _, None -> false
(*  
  match callType.HC.node, targType.HC.node with
    PNamed (_, t1), _ -> compatibleFunSig t1 targType
  | _, PNamed (_, t2) -> compatibleFunSig callType t2
  | PFun (r1, a1, va1), PFun (r2, a2, va2) ->
      (va1 = va2) && 
        (match a1, a2 with 
           None, _ | _, None -> true
         | Some l1, Some l2 -> List.length l1 = List.length l2) &&
        if isVoid r2 then
          if isVoid r1 then true
          else (* "return ignored" *) true
        else not (isVoid r1)
  | _, _ -> 
      compatibleTypes callType targType
*)



let makeType t =
  (* SH.merge strings (Cildump.string_of_ftype t) *)
  rehashType t

let bits_to_bytes bits = begin
  assert (bits mod 8 == 0);
  bits / 8
end

let rec simplifyOff off = 
  match off with
    Cil.NoOffset -> off
  | Cil.Field (fi, moreOff) -> Cil.Field (fi, simplifyOff moreOff)
  | Cil.Index (ie, moreOff) ->
      let simpleIE = if Cil.isInteger ie <> None 
      then ie 
      else (Printf.fprintf stderr "Simplified offset: %s\n" 
              (Cildump.string_of_exp ie);
            Cil.zero) in
      Cil.Index (simpleIE, simplifyOff moreOff)

let dummyTyp = hcType PVoid
 
let noOffset = (0, 0, dummyTyp)

let isNoOffset ((off, _, _):ptaOff) =
  off = 0
        
let makeOff (hostType: Cil.typ) (o: Cil.offset) : ptaOff =
  match o with 
    NoOffset -> noOffset (* loses type but we aren't using right now... *)
  | _ ->
      noOffset
      (*
      let off = simplifyOff o in
      let offBits, widthBits = 
        try
          Cil.bitsOffset hostType off 
        with Cil.SizeOfError (s, t) when s = "abstract type" ->
          Printf.fprintf stderr "makeOff -- empty struct: %s\n"
            (Cildump.string_of_type t);
          (0, 0)
      in
      let typ = Cil.typeOffset hostType off in
      let ptaType = makeType (cil_type_to_ptype typ) in
      (offBits, widthBits, ptaType)
*)

        
let makeVar v : vinfo =
  VH.hashcons var_tab v

let makeHost h : ptaHost =
  HH.hashcons ho_tab h

let makeLv lv : ptaLv =
(*
  LVH.hashcons lv_tab lv
*)
  lv

let makeRv rv =
  RVH.hashcons rv_tab rv

let addOffset innerOff outerOff =
  let iB, iW, iT = innerOff in
  let oB, oW, oT = outerOff in
  (iB + oB, oW, oT)

let addLvalOffset (h, o) off =
  (h, addOffset o off)

let rec makeDeref (rv : ptaRv) off : ptaLv = 
  match rv.HC.node with
    PAddrOf lv' -> addLvalOffset lv' off
  | PCast (t, rv') -> begin
      let tempLv = makeDeref rv' off in
      let ho, off = tempLv in
      match ho.HC.node, off with
        (PDeref x, _) ->
          let innerRv = makeRv (PCast (t, x)) in
          makeLv (makeHost (PDeref innerRv), off)
      | _ ->
          tempLv (* Deref was stripped, so it doesn't need the cast? *)
    end
  | PLv _ ->
      makeLv 
        (makeHost (PDeref rv), off)
        


let makeAddr (lv : ptaLv) : ptaRv =
  let ho, off = lv in
  match ho.HC.node, off with
    (PDeref x, o) when isNoOffset (o)  -> x
  | _ -> makeRv (PAddrOf lv)


(************ Datastructs for use after hashconsing *****)

module VarH = Hashtbl.Make (
  struct 
    type t = vinfo
    let equal a b = compareVar a b == 0
    let hash a = hashVar a
  end
)

module HashableLv = struct
    type t = ptaLv
    let equal a b = compareLv a b == 0
    let hash a = hashLv a
end

module LvalH = Hashtbl.Make (HashableLv)

let addOnceAssign =
  List_utils.addOnceP (fun a1 a2 -> compareAssign a1 a2 == 0)
    
let addOnceCall = 
  List_utils.addOnceP (fun c1 c2 -> compareCall c1 c2 == 0)

(************* Re-hashing (in case objs were serialized *)

let rehashOff (off, width, typ) = 
  (off, width, rehashType typ)

let rehashVar v =
  match v.HC.node with
    PGlobal (id, t) ->
      makeVar (PGlobal (id, rehashType t))
  | PLocal (id, t) ->
      makeVar (PLocal (id, rehashType t))
  | PRet id ->
      makeVar (PRet id)
  | PTemp id ->
      makeVar (PTemp id)

let rec rehashRv rv =
  match rv.HC.node with
    PLv lv ->
      makeRv (PLv (rehashLv lv))
  | PAddrOf lv ->
      makeRv (PAddrOf (rehashLv lv))
  | PCast (t, rv') ->
      makeRv (PCast (rehashType t, rehashRv rv'))

and rehashLv lv =
  let ho, off = lv in
  makeLv (rehashHost ho, rehashOff off)

and rehashHost h =
  match h.HC.node with
    PVar v ->
      makeHost (PVar (rehashVar v))
  | PDeref rv ->
      makeHost (PDeref (rehashRv rv))

let rehashAssign ({lhs = lv; rhs = rv;} as assign) =
  { assign with 
      lhs = rehashLv lv;
      rhs = rehashRv rv; }
    
let rehashCallInfo ({cexp = cLvList;
                    ctype = ct;} as ci) =               
  { ci with
      cexp = List.map rehashLv cLvList;
      ctype = rehashType ct;
  }

let rehashArgs al =
  List.map (fun (rvList, fIndex) -> (List.map rehashRv rvList, fIndex)) al

let rehashCall (ci, al) =
  (rehashCallInfo ci, rehashArgs al)


(****************** Dummy values ***************)
  
let dummyVar = makeVar (PGlobal (-12345, dummyTyp))

let dummyLv = makeLv (makeHost (PVar dummyVar), noOffset)

let dummyRv = makeRv (PLv dummyLv)

(****************** Extract parts of lval ***************)

let rec baseVar (lv:ptaLv) : vinfo =
  let ho, off = lv in
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

(*
exception TypeMismatch

let rec ptype_of_lval_basetype lv baseType =
  let host, off = lv in
  match host.HC.node with
    PVar _ -> baseType
  | PDeref rv -> type_after_deref rv baseType

and type_after_deref rv baseType =
  match rv.HC.node, baseType with
    PLv l, PPtr t -> ptype_of_lval_basetype l t
  | PCast (t, r), _ -> ptype_of_rval_basetype r t
  | PAddrOf l, _ -> 
      
let ptype_of_lval lv =
  let vi = baseVar lv in
  let baseType = vi.HC.node.vtyp in
  ptype_of_lval_basetype baseType
*)

(** True if the rval is an addrOf *)
let rec isAddrOf rv =
  match rv.HC.node with
    PAddrOf lv -> true
  | PCast (_, rv') -> isAddrOf rv'
  | _ -> false

let isDeref lv =
  let host, off = lv in
  match host.HC.node with
    PDeref _ -> true
  | _ -> false

let isFPCall callLv =
  let host, _ = callLv in
  match host.HC.node with
    PDeref _ -> true
  | _ -> false

let isDirectCall callLv =
  let host, _ = callLv in
  match host.HC.node with
    PVar v -> 
      (match v.HC.node with PGlobal (id, _) -> Some id 
       | _ -> failwith "Non-global func, or non-func sent to isDirectCall")
  | _ -> None

let rec isTemp lv =
  let host, _ = lv in
  match host.HC.node with
    PVar (v) -> (match v.HC.node with PTemp _ -> true | _ -> false)
  | PDeref ptr -> isTempRv ptr

and isTempRv rv =
  match rv.HC.node with
    PAddrOf _ -> false
  | PCast (_, rv') -> isTempRv rv'
  | PLv lv -> isTemp lv

let rec isPointerType (t:ctyp) =
  match t.HC.node with
    PPtr _ -> true
  | PArray _ -> true
  | PNamed (_, t2) -> isPointerType t2
  | _ -> false

let isPointerVar vinfo =
  match vinfo.HC.node with
    PGlobal (_, t) ->
      isPointerType t
  | PLocal (_, t) ->
      isPointerType t
  | _ -> false


let isPoly curType =
  match curType.HC.node with
    PPtr t ->
      (match (unrollPType t).HC.node with
         PVoid -> true
       | PInt -> true
       | _ -> false
      )
  | _ -> false      

let isFunctionType t = 
  match (unrollPType t).HC.node with
    PFun _ -> true
  | _ -> false


let callUsesFP ({cexp = ce;}, _, _) =
  List.exists isFPCall ce

let rec getLval rv =
  match rv.HC.node with
    PLv l -> l
  | PCast (_, r) -> getLval r
  | PAddrOf _ -> raise Not_found



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
  List_utils.typeOffsetUnsafe baseType off.HC.node

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

(*
exception Found_you of string * ctyp
*)

let rec string_of_type ptyp =
  match ptyp.HC.node with
    PVoid -> "void"
  | PInt -> "int"
  | PFloat -> "float"
  | PComp k -> let ci = getCinfo k in ci.cname
  | PEnum n -> n
  | PBuiltin_va_list -> "va_list"
  | PPtr t1 -> "(" ^ string_of_type t1 ^ "*)"
  | PFun (ret, args, var) ->
      string_of_type ret ^ "(" ^
        (match args with
           None -> ""
         | Some l -> 
             Stdutil.seqToString List.iter l string_of_type ", "
        ) ^ ")"
  | PArray t1 -> string_of_type t1 ^ "[]"
  | PNamed (n, t) -> n

let name_of_id id =
  try
    (string_of_int id) ^ ":" ^ (getVarinfo id).vname
  with e ->
    logError ("No varinfo for id? " ^ (Printexc.to_string e));
    string_of_int id

let string_of_vinfo (vi:vinfo) =
  match vi.HC.node with
    PGlobal (id, t) ->
      let result = "g:" ^ (name_of_id id) in
(*      if id = 32225 then
        raise (Found_you (result, t));
*)    
      result
      
  | PLocal (id,_) -> "l:" ^ (name_of_id id)
  | PRet fid -> "r:" ^ (name_of_id fid)
  | PTemp id -> "t:" ^ (string_of_int id) ^ ":" ^ "__TEMP__"

let printVinfo (vi:vinfo) =
  let str = 
(*    try *)
      (string_of_vinfo vi) 
(*    with  Found_you (str, t) -> str *)
  in 
  print_string str

let string_of_ptaOff ((off, width, typ) as ptOff :ptaOff) =
  if isNoOffset ptOff then ""
  else 
    "." ^
      (try
         let cilTyp = ptype_to_cil_type typ in
         let cilOff = Offset.bitsToOffset cilTyp off in 
         Pretty.sprint 80 (Cil.d_offset Pretty.nil () cilOff)
       with Offset.UnknownOffset ->
         string_of_int off ^ ":" ^ string_of_int width)
           
let rec string_of_ptaLv (lv:ptaLv) =
  let ho, off = lv in
  match ho.HC.node, off with
    (PVar vi, off) ->
      let viStr = string_of_vinfo vi in 
      viStr ^ string_of_ptaOff off
  | (PDeref ptrRv, off) ->
      "(*" ^ string_of_ptaRv ptrRv ^ ")" ^
        string_of_ptaOff off

and string_of_ptaRv (rv:ptaRv) =
  match rv.HC.node with
  | PLv lv ->
      string_of_ptaLv lv
  | PAddrOf baseLv ->
      "&" ^ string_of_ptaLv baseLv
  | PCast (t, rv) ->
      string_of_ptaRv rv

let printPtaLv (lv:ptaLv) =
  print_string (string_of_ptaLv lv)

let printPtaRv (rv:ptaRv) =
  print_string (string_of_ptaRv rv)
        
let printCallCons (cinfo, args) = 
  print_string "Call to: [";
  List.iter (fun funlv -> printPtaLv funlv; print_string ", ") cinfo.cexp;
  print_string "]\n";
  List.iter (fun (acts, index) -> 
               print_string ("form #" ^ string_of_int index ^ " <- {" );
               List.iter (fun arg -> printPtaRv arg; print_string ", ") acts;
               print_string "}\n") args;
  print_string ("at " ^  Cildump.string_of_loc cinfo.cloc ^ "\n");
  print_string "\n"
    
let string_of_assign ({lhs = lhs; rhs = rhs; aloc = loc;} : ptaAssign) =
  string_of_ptaLv lhs ^ " = " ^ string_of_ptaRv rhs ^ " @ " ^
    (Cildump.string_of_loc loc)

let printAssignment (assign: ptaAssign) =
  print_string (string_of_assign assign ^ "\n")

let d_fun_formals funFormals = 
  text "[" ++ seq_to_doc (text ", ") List.iter
    (fun vinfo -> text (string_of_vinfo vinfo)) funFormals nil
  ++ text "]"


let printFunTypes funTable =
  print_string "PTA printing function types and formals\n";
  print_string "=======================================\n";
  Hashtbl.iter 
    (fun fid finfo ->
       let fname = name_of_id fid in
       print_string (fname ^ " : " ^ (string_of_type finfo.funType) ^ "\n");
       print_string ("  args: " ^ (sprint 80 (d_fun_formals finfo.funFormals)) 
                     ^ "\n");
    ) funTable;
  print_string "\n"


(******************* Convert back to CIL ******************)

let ptOff_to_cilOff off =
  (* TODO: how to convert something that was nested originally?
     Also, need the type... *)
  NoOffset

let rec lv_of_ptaLv ptaLv = 
  let ho, off = ptaLv in
  let cilOff = ptOff_to_cilOff off in
  match ho.HC.node with
    PVar vi ->
      (match vi.HC.node with
         PGlobal (id, _) 
       | PLocal (id, _) ->
           (Var (getVarinfo id), cilOff)
       | _ -> 
           raise Not_found
      )
  | PDeref rv ->
      let lv' = getLval rv in
      (Mem (Lval (lv_of_ptaLv lv')), cilOff)
