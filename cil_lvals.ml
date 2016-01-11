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


(** Utility functions for CIL lvals, consolidated *)


open Cil
open Fstructs

module D = Cildump
module E = Errormsg
module L = Logging


(***************************************************
 * Cil AST Search functions
 ***************************************************)


exception BaseVINotFound

(** Look through an expression that should reference a variable. Find
    that variable. The given expression is assumed to be used in the 
    context of an lval *)
let rec findBaseVarinfoExp (exp:Cil.exp) : Cil.varinfo =
  match exp with
    (* Simple variable reference *)
    Lval(Var(vi),_) ->
      vi
        
  (* Simple pointer dereference *)
  | Lval(Mem(ptrExp),_) ->
      findBaseVarinfoExp ptrExp
        
  (* Pointer + integer... assume Pointer is where it's at *)
  | BinOp(PlusPI,lhs,_,_) 
  | BinOp(MinusPI,lhs,_,_)
  | BinOp(IndexPI,lhs,_,_) ->
      findBaseVarinfoExp lhs

  | SizeOfE e
  | AlignOfE e
  | CastE (_, e) -> findBaseVarinfoExp e

  | AddrOf l
  | StartOf l -> findBaseVarinfoLval l
    
  (* Other cases shouldn't occur (non lvalues) *)
  | _ -> raise BaseVINotFound

and findBaseVarinfoLval (lval:Cil.lval) : Cil.varinfo =
  match lval with
    (Var(vi),_) ->
      vi
  | (Mem(ptrExp),_) ->
      findBaseVarinfoExp ptrExp

exception TopLvalNotFound

(** Look through an expression that should reference a variable. Find
    that variable. The given expression is assumed to be used in the
    context of an lval *)
let rec findTopLvalExp (exp:Cil.exp) : Cil.lval =
  match exp with
    Lval(l) ->
      l

  (* Pointer + integer... assume Pointer is where it's at *)
  | BinOp(PlusPI,lhs,_,_)
  | BinOp(MinusPI,lhs,_,_)
  | BinOp(IndexPI,lhs,_,_) ->
      findTopLvalExp lhs

  | SizeOfE e
  | AlignOfE e
  | CastE (_, e) -> findTopLvalExp e

  | AddrOf l
  | StartOf l -> l
      
  (* Other cases shouldn't occur (non lvalues) *)
  | _ -> raise TopLvalNotFound



let rec countOpsWithCount curExp curCount : int =  
  match curExp with
    Lval l  
  | AddrOf l
  | StartOf l->
      countOpsInLval l curCount 
  | BinOp(op, ce1, ce2, _) ->
      let ops = countOpsWithCount ce1 (curCount + 1) in
      countOpsWithCount ce2 ops
  | UnOp(op, ce, _) ->
      countOpsWithCount ce (curCount + 1)
  | SizeOfE e
  | AlignOfE e
  | CastE (_, e) -> 
      countOpsWithCount e curCount
  | Const _ 
  | SizeOf _
  | SizeOfStr _
  | AlignOf _ -> curCount

and countOpsInLval lv curCount =
  (* Ignore array index expressions *)
  match lv with
    (Var(_),_) -> curCount 
  | (Mem(e),_) -> countOpsWithCount e curCount

(** Count the number of ops in the expression *)
let countOpsInExp (exp:Cil.exp) : int =
  countOpsWithCount exp 0


let getIndex (formals:varinfo list) (vi:varinfo) : int option =
  (* Only need to match the first variable name in the lvalexp *)
  try
    Some (Stdutil.indexOf 
            (fun formalVI ->
               (Ciltools.compare_var vi formalVI == 0)
            ) formals)
  with Not_found ->
    None



(*****************************************************
 * Unsafe versions of Cil type calculation functions
 * Also ditches attributes
 * Still flags errors involving Arrays
 *****************************************************)

exception TypeNotFound

let rec unrollTypeNoAttrs (t: typ) : typ = 
  match t with 
    TNamed (r, a') -> unrollTypeNoAttrs r.ttype
  | x -> x

let rec typeOffsetUnsafe basetyp =
  function
      NoOffset -> basetyp
    | Index (_, o) -> begin
        match unrollTypeNoAttrs basetyp with
          TArray (t, _, _) ->
	        typeOffsetUnsafe t o
        | TPtr (t, _) ->
            L.logError "typeOffsetUnsafe: Index offset on a pointer\n";
            typeOffsetUnsafe t o
	    | t -> 
            L.logError "typeOffsetUnsafe: Index on a non-array/non-ptr\n";
            typeOffsetUnsafe t o
      end 
    | Field (fi, o) ->
        typeOffsetUnsafe fi.ftype o


let rec typeOfUnsafe (e: exp) : typ = 
  match e with
  | Const(CInt64 (_, ik, _)) -> TInt(ik, [])

    (* Character constants have type int.  ISO/IEC 9899:1999 (E),
     * section 6.4.4.4 [Character constants], paragraph 10, if you
     * don't believe me. *)
  | Const(CChr _) -> intType

    (* The type of a string is a pointer to characters ! The only case when 
     * you would want it to be an array is as an argument to sizeof, but we 
     * have SizeOfStr for that *)
  | Const(CStr s) -> !stringLiteralType

  | Const(CWStr s) -> TPtr(!wcharType,[])

  | Const(CReal (_, fk, _)) -> TFloat(fk, [])

  | Const(CEnum(_, _, ei)) -> TEnum(ei, [])

  | Lval(lv) -> typeOfLvalUnsafe lv
  | SizeOf _ | SizeOfE _ | SizeOfStr _ -> !typeOfSizeOf
  | AlignOf _ | AlignOfE _ -> !typeOfSizeOf
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLvalUnsafe lv, [])
  | StartOf (lv) -> begin
      match unrollTypeNoAttrs (typeOfLvalUnsafe lv) with
        TArray (t,_, _) -> TPtr(t, [])
      | _ -> 
          E.s (E.bug "typeOfUnsafe: StartOf on a non-array")
    end
      
and typeOfInitUnsafe (i: init) : typ = 
  match i with 
    SingleInit e -> typeOfUnsafe e
  | CompoundInit (t, _) -> t

and typeOfLvalUnsafe = function
    Var vi, off -> 
      typeOffsetUnsafe vi.vtype off
  | Mem addr, off -> begin
      match unrollTypeNoAttrs (typeOfUnsafe addr) with
        TPtr (t, _) -> unrollTypeNoAttrs (typeOffsetUnsafe t off)
      | t -> begin
          match off with
            NoOffset ->
              (* E.s (bug "typeOfLvalUnsafe: Mem on a non-pointer") *)
              TVoid []
          | _ -> typeOffsetUnsafe t off
        end
    end
      

let typeAfterDeref (exp:Cil.exp) =
  match unrollTypeNoAttrs (typeOfUnsafe exp) with
    TPtr (t,_) ->
      unrollTypeNoAttrs t
  | _ ->
      raise TypeNotFound

(** Non-unrolled type stuff *)

let typeAfterDerefNoUnroll (exp:Cil.exp) =
  match unrollTypeNoAttrs (typeOfUnsafe exp) with
    TPtr (t,_) -> t
  | _ -> raise TypeNotFound

let rec typeOfNoUnroll (e: exp) : typ = 
  match e with
  | Lval(lv) -> typeOfLvalNoUnroll lv
  | AddrOf (lv) -> TPtr(typeOfLvalNoUnroll lv, [])
  | _ -> 
      (* base cases stay the same *) 
      typeOfUnsafe e
      
and typeOfInitNoUnroll (i: init) : typ = 
  match i with 
    SingleInit e -> typeOfNoUnroll e
  | CompoundInit (t, _) -> t

and typeOfLvalNoUnroll = function
    Var vi, off -> 
      typeOffsetNoUnroll vi.vtype off
  | Mem addr, off -> begin
      match unrollTypeNoAttrs (typeOfUnsafe addr) with
        TPtr (t, _) -> (typeOffsetNoUnroll t off)
      | t -> begin
          match off with
            NoOffset ->
              (* E.s (bug "typeOfLvalUnsafe: Mem on a non-pointer") *)
              TVoid []
          | _ -> typeOffsetNoUnroll t off
        end
    end
      
and typeOffsetNoUnroll basetyp =
  function
      NoOffset -> basetyp
    | Index (_, o) -> begin
        match unrollTypeNoAttrs basetyp with
          TArray (t, _, _) ->
	        typeOffsetNoUnroll t o
        | TPtr (t, _) ->
            L.logError "typeOffsetUnsafe: Index offset on a pointer\n";
            typeOffsetNoUnroll basetyp o
	    | t -> 
            L.logError "typeOffsetUnsafe: Index on a non-array/non-ptr\n";
            typeOffsetNoUnroll basetyp o
      end 
    | Field (fi, o) ->
        typeOffsetNoUnroll fi.ftype o
          


(** "Compatiblity" checking *)

exception OffsetMismatch of (fieldinfo * typ) option

let string_of_field fi =
  fi.fcomp.cname ^ "." ^ fi.fname ^ "(" ^ string_of_int fi.fcomp.ckey ^ ")"

let string_of_type t =
  match t with
    TComp (ci, _) ->
      ci.cname ^ "(" ^ string_of_int ci.ckey ^ ")"
  | _ ->
      Cildump.string_of_type t

let fieldMatchesComp fi ci =
  List.exists 
    (fun otherFi -> otherFi.fname = fi.fname && 
        Ciltools.compare_type otherFi.ftype fi.ftype == 0
    ) ci.cfields
      
let string_of_offsetMiss oMiss =
  "offset mismatch " ^ 
    (match oMiss with
       Some (fi, t) ->
         string_of_field fi ^ " on " ^ string_of_type t  
     | None -> "")

(* TODO: too much ptr / cast info being thrown away to make these
   checks sound... (e.g., kernel.h  "container_of")   *)
let rec canAttachOffset hostTyp offset =
  match offset with
    Field (fi, _) -> begin
      match hostTyp with
        TComp (ci, _) -> 
          if Ciltools.compare_compi fi.fcomp ci == 0 ||
            fieldMatchesComp fi ci then (true, None)
          else (false, Some (fi, hostTyp))
      | TVoid _ 
      | TInt _ -> (* allow *)
          (true, None)
      | TNamed (tinfo, _) -> canAttachOffset tinfo.ttype offset
      | _ -> (false, Some (fi, hostTyp))
    end
  | _ -> (true, None)


(* TODO: too much ptr / cast info being thrown away to make these
   checks sound... (e.g., kernel.h  "container_of")   *)
let attachOffset (host:Cil.lhost) (offset:Cil.offset) : Cil.lval =
  let hostTyp = typeOfLvalUnsafe (host, NoOffset) in
  let canAttach, counter = canAttachOffset hostTyp offset in
  if canAttach then (host, offset)
  else raise (OffsetMismatch counter)


let mkMemChecked (ptrExp:Cil.exp) (offset:Cil.offset) : Cil.lval =
  try
    let hostTyp = typeAfterDeref ptrExp in
    let canAttach, counter = canAttachOffset hostTyp offset in
    if canAttach then mkMem ptrExp offset
    else raise (OffsetMismatch counter)
  with E.Error 
  | Failure _
  | TypeNotFound ->
      raise (OffsetMismatch None)


let cZero = Cil.integer 0

let cOne = Cil.integer 1

(** Canonicize the offset so that array indexing is set to [0]
    @returns   the (possibly) modified offset and a flag indicating whether
               a change was made  *) 
let rec canonicizeOff (off:Cil.offset) : (Cil.offset * bool) =  
  (* TODO, use the numeric offset thing done in IMPACT (Cheng, Hwu) that
     handles unions / funky casts as well? Probably not needed for now... *)
  match off with
    NoOffset -> (NoOffset, false)
  | Field (fi, moreOff) -> 
      let (rest, changed) = canonicizeOff moreOff in
      (Field (fi, rest), changed)
  | Index (indExp, moreOff) ->
      (* Convert the index to 0 *)
      let (rest, changed) = canonicizeOff moreOff in
      let finalChanged = changed || 
        ((Ciltools.compare_exp indExp cZero) <> 0) in
      (Index (cZero, rest), finalChanged)


(** Convert the expression to canonical form *)
let rec canonicizeExp (exp:Cil.exp) : (Cil.exp) =
  match exp with
    Lval(l) -> 
      Lval(canonicizeLval l)
  | AddrOf (l)
  | StartOf (l) ->
      Cil.mkAddrOrStartOf l
  | _ -> 
      exp

and canonicizeLval (host,off:Cil.lval) : (Cil.lval) =
  let newOff, _ = canonicizeOff off in
  match host with
    Var(vi) ->
      (host, newOff)
  | Mem(ptrExp) ->
      (Mem(canonicizeExp ptrExp), newOff)



(*************************************************************)


(** Remove nested structs of the same kind of struct 
    (impossible in reality, but possible w/ my weird ptr arith stuff)
    Quite a hack... *)
let rec simplifyOff (off:Cil.offset) : (Cil.offset) =
  doSimplifyOff off []

and doSimplifyOff (off:Cil.offset) seenFields = 
  match off with
    NoOffset -> 
      off
  | Field (fi, moreOff) ->
      let curKey = fi.fcomp.ckey in
      if (List.exists 
            (fun oldKey -> curKey == oldKey) seenFields) then
        NoOffset (* hit an impossibility, just cut it off *)
      else
        let newFields = curKey :: seenFields in
        Field (fi, doSimplifyOff moreOff newFields)
  | Index (e, moreOff) ->
      Index (e, doSimplifyOff moreOff seenFields)


(** Collapse chains of (field) derefs w/ the same type.  
    Return:
    1) the simplified lval
    2) a list of the first seen lval w/ a corresponding type
    3) a flag indicating whether or not the lval was simplified *)
let rec doSimplifyLv (lv:Cil.lval) : 
    Cil.lval * (Cil.typ * Cil.lval) list * bool = 
  match lv with
    (* Base case *)
    (Var(vi), off) ->
      let newLv = Var(vi), simplifyOff off in
      let curTyp = typeOfLvalUnsafe newLv in
      (newLv, [(curTyp, newLv)], false)
 
  | (Mem(ptrExp), off) ->
      let simplifiedPtr, seenTypes, changed = doSimplifyExp ptrExp in
      let curTyp = typeOfLvalUnsafe lv in
      try 
        let (_, simplerLv) = List.find 
          (fun (seenTyp, _) -> 
             ((Ciltools.compare_type curTyp seenTyp) == 0)
          ) seenTypes in
        if (Cil.isVoidType curTyp) then
          (simplerLv, seenTypes, true) (* TODO limit the number of void *)
        else
          (simplerLv, seenTypes, true)
      with
        Not_found ->
          let newOff = simplifyOff off in
          let newLv = (Cil.mkMem simplifiedPtr newOff) in
          (newLv, (curTyp, newLv) :: seenTypes, changed)

and doSimplifyExp (exp:Cil.exp) : 
    Cil.exp * (Cil.typ * Cil.lval) list * bool =
  match exp with
    Lval(lv) ->
      let simplerLv, seenTypes, changed = doSimplifyLv lv in
      (Lval (simplerLv), seenTypes, changed)
  | BinOp (bop, e1, e2, curTyp) ->
      (match bop with
         PlusPI 
       | IndexPI
       | MinusPI ->
           (* Ignore e2 for now and eliminate binop *)
           doSimplifyExp e1 
       | _ ->
           (* Ignore e2 for now and eliminate binop *)
           doSimplifyExp e1
      )
  | UnOp (unop, e1, curTyp) ->
      (* Ignore unop for now *)
      doSimplifyExp e1

  | CastE (_, e) ->
      doSimplifyExp e

  | _ ->
      L.logError ("doSimplifyExp: Shouldn't see this type of exp: "
                    ^ (D.string_of_exp exp));
      (exp, [], false)

(** May eliminate redundant parts of lval access path. 
    Returns the simplified lval and a flag indicating whether a change
    is made *)
let simplifyLval (lv:Cil.lval) : Cil.lval * bool =
  let simplerLv, _, changed = doSimplifyLv lv in
  (simplerLv, changed)
    
    





(***************************************************
 * Actual / Formal substitution on ASTs
 ***************************************************)

exception SubstInvalidArg

(** Find instances of the formal within given lval, and substitute w/ 
    the given actual lval *)
let rec substActForm actual lvalWithFormal : Cil.lval =
  match lvalWithFormal with 
    (Var(vi), formOff) ->
      (* NO, don't substitute! A straight-up var as a formal is always a
         different memory cell from a straight-up var in the other scope! *)
      raise SubstInvalidArg

  | (Mem (Lval(Var(vi), NoOffset)), outerOff) -> begin
      (* Assume any variable encountered is the formal... To fix this,
         should actually add a var attribute to compare against *)
      try
        mkMemChecked actual outerOff
      with 
        OffsetMismatch _
      | Errormsg.Error 
      | Failure _ ->
          raise SubstInvalidArg
    end
  | (Mem(ptrExp), outerOff) ->
      try
        let newExp = substActFormExp actual ptrExp in
        mkMemChecked newExp outerOff
      with 
        OffsetMismatch _
      | Errormsg.Error 
      | Failure _ ->
          raise SubstInvalidArg
          
        
and substActFormExp actual expWithFormal : Cil.exp =
  match expWithFormal with
    Lval(lv) ->           
      let newLval = substActForm actual lv in
      (Lval(newLval))
        
  | AddrOf (l) ->
      Cil.mkAddrOf (substActForm actual l)
  | StartOf(l) ->
      StartOf (substActForm actual l)

  | CastE(t, e) ->
      CastE (t, substActFormExp actual e)
        
  | AlignOfE(e) ->
      AlignOfE (substActFormExp actual e)
        
  | SizeOfE(e) ->
      SizeOfE (substActFormExp actual e)
        
  | UnOp (unop, e, t) ->
      let newExp = substActFormExp actual e in
      UnOp (unop, newExp, t)
        
  | BinOp (bop, e1, e2, t) ->
      (* Assume formal is (only) in the first exp for now *)
      let newExp = substActFormExp actual e1 in
      BinOp(bop, newExp, e2, t)
        
  | AlignOf _
  | SizeOf _
  | SizeOfStr _
  | Const _ ->
      let expStr = D.string_of_exp expWithFormal in 
      L.logError 
        ("substActFormExp encountered unknown exp: " ^ expStr ^ "\n");
      raise SubstInvalidArg


(** Find instances of (Mem(formal)) within given lval, and substitute w/
    the actual *)
let rec substActFormDeref actual lvalWithFormal : Cil.lval =
  match lvalWithFormal with
    (Mem(Lval(Var(vi), NoOffset)), outerOff) -> begin
      try
        let (aHost, aOff) = actual in
        (aHost, Cil.addOffset outerOff aOff)
      with  
        OffsetMismatch _
      | Errormsg.Error 
      | Failure _ ->
          raise SubstInvalidArg
    end
  | (Mem(ptrExp), outerOff) -> begin
      (* Maybe the deref is further in *)
      try
        let newExp = substActFormExpDeref actual ptrExp in
        Cil.mkMem newExp outerOff
      with SubstInvalidArg -> 
        (* Or maybe it is this deref *)
        let substExp = substActFormExp (Lval actual) ptrExp in
        try 
          findTopLvalExp substExp
        with TopLvalNotFound ->
          raise SubstInvalidArg
    end
  | _ -> 
      let lvStr = D.string_of_lval lvalWithFormal in
      L.logError 
        ("substActFormDeref given unknown formal-based lval: " ^ lvStr ^ "\n");
      raise SubstInvalidArg

and substActFormExpDeref actual expWithFormal : Cil.exp =
  match expWithFormal with
    Lval(lv) ->           
      let newLval = substActFormDeref actual lv in
      (Lval(newLval))

  | AddrOf (l) ->
      Cil.mkAddrOf (substActFormDeref actual l)

  | StartOf(l) ->
      StartOf (substActFormDeref actual l)


  | CastE(t, e) ->
      CastE(t, substActFormExpDeref actual e)

  | AlignOfE(e) ->
      AlignOfE (substActFormExpDeref actual e)

  | SizeOfE(e) ->
      SizeOfE (substActFormExpDeref actual e)

  | UnOp (unop, e, t) ->
      let newExp = substActFormExpDeref actual e in
      UnOp (unop, newExp, t)

  | BinOp (bop, e1, e2, t) ->
      (* Assume formal is (only) in the first exp for now *)
      let newExp = substActFormExpDeref actual e1 in
      BinOp(bop, newExp, e2, t)

  | AlignOf _
  | SizeOf _
  | SizeOfStr _
  | Const _ ->
      let expStr = D.string_of_exp expWithFormal in 
      L.logError
        ("substActFormExp encountered unknown exp: " ^ expStr ^ "\n");
      raise SubstInvalidArg



(****** Hash-consing / distillation *******)

module HashedLval = struct
  type t = Cil.lval
  let equal a b = Ciltools.compare_lval a b == 0
  let hash = Ciltools.hash_lval
end

module HashedExp = struct
  type t = Cil.exp
  let equal a b = (Ciltools.compare_exp a b) == 0
  let hash = Ciltools.hash_exp
end

module LvHash = Weak.Make(HashedLval)

module TyHash = Weak.Make(
  struct 
    type t = Cil.typ
    let equal a b = 
      (*
        Ciltools.compare_type a b == 0
      *)
      let s1 = D.string_of_type a in
      let s2 = D.string_of_type b in
      s1 = s2
      
    let hash x = 
      (* 
         Ciltools.hash_type
      *)
      Hashtbl.hash (D.string_of_type x)

  end
)

module CIHash = Weak.Make(
  struct
    type t = Cil.compinfo
    let equal a b = a.ckey = b.ckey
    let hash a = Hashtbl.hash a.ckey
  end
)

let goldenLvals = LvHash.create 173

let goldenTypes = TyHash.create 173

let goldenCompinfos = CIHash.create 173

let locIsUnknown loc =
  loc == Cil.locUnknown || Cil.compareLoc loc Cil.locUnknown == 0

let distillLoc loc =
  if locIsUnknown loc then Cil.locUnknown
  else loc


(** Try to reduce the memory footprint of lvals. *)
let rec distillLval = function
    Var(vi) as v, off ->
      distillVar vi;
      v, distillOff off

  | Mem(exp), off ->
      Mem (distillExp exp), distillOff off

and distillVar vi =
  vi.vtype <- mergeType vi.vtype;
  vi.vdecl <- distillLoc vi.vdecl  

and distillExp e =
  match e with 
    Const (CEnum (ce, name, ei)) ->
      distillEnuminfo ei;
      Const (CEnum (distillExp ce, name, ei))

  | SizeOfE (e) ->
      SizeOfE (distillExp e)

  | AlignOfE (e) ->
      AlignOfE (distillExp e)

  | Const _
  | SizeOfStr _ ->
      e
      
  | Lval (l) ->
      Lval (mergeLv  l)

  | AddrOf(l) ->
      AddrOf (mergeLv l)

  | StartOf(l) -> 
      StartOf (mergeLv l)

  | SizeOf (t) ->
      SizeOf (mergeType t)
  | AlignOf (t) -> 
      AlignOf (mergeType t)

  | CastE (t, e) ->
      CastE (mergeType t, distillExp e)

  | UnOp(op, e, t) ->
      UnOp(op, distillExp e, mergeType t)

  | BinOp(op, e1, e2, t) ->
      BinOp(op, distillExp e1, distillExp e2, mergeType t)

and distillOff o =
  match o with
    NoOffset ->
      o
  | Field (fi, moreO) ->
      fi.floc <- locUnknown; (* ignore compinfo and its other fields *)
      fi.fattr <- [];
(*      fi.ftype <- mergeType fi.ftype; *)
      Field (fi, distillOff moreO)

  | Index (e, moreO) ->
      Index (distillExp e, distillOff moreO)

and distillType t =
  match t with
    TVoid _ 
  | TInt _
  | TFloat _
  | TBuiltin_va_list _ ->
      t

  | TFun (rt, None, vargP, atts) ->
      TFun (mergeType rt, None, vargP, [])

  | TFun (rt, Some (args), vargP, atts) ->
      TFun (mergeType rt, 
            Some (List.map (fun (name, typ, atts) ->
                              (name, mergeType typ, [])) args), 
            vargP, [])

  | TEnum (ei,_) ->
      distillEnuminfo ei;
      t

  | TPtr (ptT, _) ->
      TPtr (mergeType ptT, [])
 
  | TArray (eltT, None, _) ->
      TArray (mergeType eltT, None, [])

  | TArray (eltT, Some(e), _) ->
      TArray (mergeType eltT, Some (distillExp e), [])
      
  | TNamed (tinfo, _) ->
      tinfo.ttype <- mergeType tinfo.ttype;
      t

  | TComp (ci, _) ->(*
      TComp(mergeCompinfo ci, [])
                    *)
      distillCompinfo ci;
      t

and distillCompinfo ci =
  ci.cattr <- [];
  List.iter (fun fi ->
               fi.floc <- locUnknown;
(*               fi.ftype <- mergeType fi.ftype; *)
(*               fi.fcomp <- ci; *)
            ) ci.cfields
      

and distillEnuminfo einfo =
  (* only hash-cons the strings that describe the location for now *)
  einfo.eitems <- List.map (fun (s, e, l) ->
                               (s, distillExp e, distillLoc l)) einfo.eitems


(***** Distill and Hash-cons ******)

(** Cache certain parts *)
and mergeType t =
  try
    TyHash.find goldenTypes t
  with Not_found ->
    let newT = distillType t in
    TyHash.add goldenTypes newT;
    newT
    

and mergeLv lv = 
  try 
    LvHash.find goldenLvals lv
  with Not_found ->
    let newLv = distillLval lv in
    LvHash.add goldenLvals newLv;
    newLv

and mergeCompinfo ci = 
  try 
    CIHash.find goldenCompinfos ci
  with Not_found ->
    (* make sure it finds the dude to terminate loops *)
    CIHash.add goldenCompinfos ci; 
    distillCompinfo ci;
    ci

let printHashStats () =
  let hashStats = Stdutil.string_of_hashstats LvHash.stats 
    goldenLvals "Golden lvals" in
  L.logStatus hashStats;
  let hashStats = Stdutil.string_of_hashstats TyHash.stats 
    goldenTypes "Golden types" in
  L.logStatus hashStats;
  let hashStats = Stdutil.string_of_hashstats CIHash.stats 
    goldenCompinfos "Golden compInfos" in
  L.logStatus hashStats
  
