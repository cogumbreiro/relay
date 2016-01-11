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
open Logging


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

let findTopOffsetLvExp exp =
  let lv = findTopLvalExp exp in
  match lv with
    Var _, off | Mem _, off -> off


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

let (* rec *) dropTypeAttribs t =
  match t with
    TVoid [] -> t
  | TVoid _ -> TVoid []
  | TInt (_, []) -> t
  | TInt (ik, _) -> TInt (ik, []) 
  | TFloat (_, []) -> t
  | TFloat (fk, _) -> TFloat (fk, [])
  | TBuiltin_va_list [] -> t
  | TBuiltin_va_list _ -> TBuiltin_va_list []
  | TFun (rt, args, vargP, []) -> 
      (* Just top level is enough *)
(*      let rt2 = dropTypeAttribs rt in
      if rt == rt2 then t
      else TFun (rt2, args, vargP, [])
*) 
      t
  | TFun (rt, args, vargP, _) -> 
      (* Just top level is enough *)
      TFun (rt, args, vargP, [])
  | TEnum (_,[]) -> t
  | TEnum (ei,_) -> TEnum (ei, [])
  | TPtr (ptT, []) -> 
      (* Just top level is enough *)
(*
      let ptT2 = dropTypeAttribs ptT in
      if ptT2 == ptT then t
      else TPtr (ptT2, [])
*)
      t
  | TPtr (ptT, _) -> 
      (* Just top level is enough *)
      TPtr (ptT, [])
  | TArray (eltT, sz, []) -> 
      (* Just top level is enough *)
(*
      let eltT2 = dropTypeAttribs eltT in
      if eltT2 == eltT then t
      else TArray (eltT2, sz, [])
*)
      t
  | TArray (eltT, sz, _) -> 
      (* Just top level is enough *)
      TArray (eltT, sz, [])
  | TNamed (_, []) -> t
  | TNamed (tinfo, _) -> TNamed (tinfo, [])
  | TComp (_, []) -> t
  | TComp (ci, _) -> TComp(ci, [])
      

let rec typeOffsetUnsafe basetyp =
  function
      NoOffset -> basetyp
    | Index (_, o) -> begin
        match unrollTypeNoAttrs basetyp with
          TArray (t, _, _) ->
	        typeOffsetUnsafe t o
        | TPtr (t, _) ->
            logError ~prior:3 "typeOffsetUnsafe: Index offset on a pointer";
            typeOffsetUnsafe t o
	    | t -> 
            logError ~prior:3 "typeOffsetUnsafe: Index on a non-array/non-ptr";
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
          Errormsg.s (Errormsg.bug "typeOfUnsafe: StartOf on a non-array")
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
              (* Errormsg.s (bug "typeOfLvalUnsafe: Mem on a non-pointer") *)
              TVoid []
          | _ -> typeOffsetUnsafe t off
        end
    end
      
let typeAfterDerefOfT typ =
  match unrollTypeNoAttrs typ with
    TPtr (t,_) -> unrollTypeNoAttrs t
  | _ -> raise TypeNotFound


let typeAfterDeref (exp:Cil.exp) =
  typeAfterDerefOfT (typeOfUnsafe exp) 


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
              (* Errormsg.s (bug "typeOfLvalUnsafe: Mem on a non-pointer") *)
              TVoid []
          | _ -> typeOffsetNoUnroll t off
        end
    end
      
and typeOffsetNoUnroll basetyp o =
  match o with 
    NoOffset -> basetyp
  | Index (_, o') -> begin
      match unrollTypeNoAttrs basetyp with
        TArray (t, _, _) ->
	      typeOffsetNoUnroll t o'
      | TPtr (t, _) ->
          logError "typeOffsetUnsafe: Index offset on a pointer\n";
          typeOffsetNoUnroll basetyp o'
	  | t -> 
          logError ~prior:3 
            "typeOffsetUnsafe: Index on a non-array/non-ptr\n";
          typeOffsetNoUnroll basetyp o'
    end 
  | Field (fi, o') ->
      typeOffsetNoUnroll fi.ftype o'



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
    (fun otherFi -> 
       otherFi.fname = fi.fname && Ciltools.equal_type otherFi.ftype fi.ftype
    ) (!Cil.getCfields ci)
    
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
  with Errormsg.Error 
  | Failure _
  | TypeNotFound ->
      raise (OffsetMismatch None)

let cZero = Cil.integer 0

let cOne = Cil.integer 1

(** Use our own VID range (all negative numbers instead) so as
    not to clash with VIDs that have already been assigned! *)
let nextGlobalVID = ref (-1)

(** Return the next fresh VID. Assume CIL doesn't use negative ints for ids *)
let newVID () = 
  let t = !nextGlobalVID in
  decr nextGlobalVID;
  t

(** Make variable infos w/ this, not Cil.makeVarinfo, so as not to
    clash with VIDs that have already been assigned.
    Caller must set the scope of the variable unless marked global. *)
let mkVarinfo global name typ =
  (* Besides the VID counter, the rest of the code should
     be a clone of the Cil code =/ *)
  (* Strip const from type for locals *)
  let vi = 
    { vname = name;
      vid   = newVID ();
      vglob = global;
      vtype = if global then typ else typeRemoveAttributes ["const"] typ;
      vdecl = locUnknown;
      vinline = false;
      vattr = [];
      vstorage = NoStorage;
      vaddrof = false;
      vreferenced = false;    (* sm *)
      vdescr = Pretty.nil;
      vdescrpure = true;
    } in
  if global then Scope.setScope Scope.SGlobal vi;
  (* Assume non-globals set the scope themselves *)
  vi

(** Canonicize the offset so that array indexing is set to [0]
    @returns   the (possibly) modified offset and a flag indicating whether
               a change was made  *) 
let rec canonicizeOff (off:Cil.offset) : (Cil.offset * bool) =  
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
             Ciltools.equal_type curTyp seenTyp
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

  | AddrOf lv -> 
      let simplerLv, seenTypes, changed = doSimplifyLv lv in
      AddrOf simplerLv, seenTypes, changed

  | StartOf lv -> 
      let simplerLv, seenTypes, changed = doSimplifyLv lv in
      StartOf simplerLv, seenTypes, changed

  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | SizeOfE _ | AlignOfE _ ->
      (exp, [], false)


(** May eliminate redundant parts of lval access path. 
    Returns the simplified lval and a flag indicating whether a change
    is made *)
let simplifyLval (lv:Cil.lval) : Cil.lval * bool =
  let simplerLv, _, changed = doSimplifyLv lv in
  (simplerLv, changed)
    


(***************************************************
 * Omit casts
 ***************************************************)

(** Omit casts up to an Lval or constant atom *)
let rec omitCast exp =
  match exp with
    CastE (t, e) -> omitCast e
  | Lval _  | AddrOf _ | StartOf _ | Const _ -> exp
  | BinOp (op, e1, e2, t) ->
      BinOp (op, omitCast e1, omitCast e2, t)
  | UnOp (op, e, t) ->
      UnOp (op, omitCast e, t)
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> exp


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
      let expStr = Cildump.string_of_exp expWithFormal in 
      logError 
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
      let lvStr = Cildump.string_of_lval lvalWithFormal in
      logError 
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
      let expStr = Cildump.string_of_exp expWithFormal in 
      logError
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

module HashedTyp = struct
  type t = Cil.typ
  let equal a b = 
    (* Ciltools.equal_type a b *)
    let s1 = Cildump.string_of_type a in
    let s2 = Cildump.string_of_type b in
    s1 = s2
        
  let hash x = 
    (* Ciltools.hash_type *)
    Hashtbl.hash (Cildump.string_of_type x)   
end

module TyHash = Weak.Make (HashedTyp)

let goldenLvals = LvHash.create 173

let goldenTypes = TyHash.create 173

let locIsUnknown loc =
  loc == Cil.locUnknown || Cil.compareLoc loc Cil.locUnknown == 0

let distillLoc loc =
  if locIsUnknown loc then Cil.locUnknown
  else loc


(** Try to reduce the memory footprint of lvals by removing "unneeded" 
    parts of the CIL AST / sharing equivalent parts *)
let rec distillLval lv = 
  match lv with
    Var(vi) as v, off ->
      distillVar vi;
      let newOff = distillOff off in 
      if off == newOff then lv
      else (v, newOff)

  | Mem(exp), off ->
      let newExp = distillExp exp in
      let newOff = distillOff off in
      if exp == newExp && off == newOff 
      then lv
      else (Mem newExp, newOff)
        
and distillVar vi =
  vi.vtype <- mergeType vi.vtype;
  vi.vdecl <- distillLoc vi.vdecl  

and distillExp e =
  match e with 
    Const (CEnum (ce, name, ei)) ->
      distillEnuminfo ei;
      let newCE = distillExp ce in
      if ce == newCE 
      then e
      else Const (CEnum (newCE, name, ei))

  | SizeOfE (exp) ->
      let newE = distillExp exp in
      if newE == exp 
      then e
      else SizeOfE newE

  | AlignOfE (exp) ->
      let newE = distillExp exp in
      if newE == exp 
      then e
      else AlignOfE newE

  | Const _
  | SizeOfStr _ -> e
      
  | Lval (l) ->
      let newL = mergeLv l in
      if newL == l 
      then e
      else Lval newL

  | AddrOf(l) ->
      let newL = mergeLv l in
      if newL == l 
      then e
      else AddrOf newL

  | StartOf(l) -> 
      let newL = mergeLv l in
      if newL == l 
      then e
      else StartOf newL

  | SizeOf (t) ->
      let newT = mergeType t in
      if t == newT 
      then e
      else SizeOf newT

  | AlignOf (t) -> 
      let newT = mergeType t in
      if t == newT 
      then e
      else AlignOf newT

  | CastE (t, exp) ->
      let newE = distillExp exp in
      let newT = mergeType t in
      if t == newT && exp == newE 
      then e
      else CastE (newT, newE)

  | UnOp(op, exp, t) ->
      let newE = distillExp exp in
      let newT = mergeType t in
      if t == newT && exp == newE 
      then e
      else UnOp(op, newE, newT)

  | BinOp(op, e1, e2, t) ->
      let newE1 = distillExp e1 in
      let newE2 = distillExp e2 in
      let newT = mergeType t in
      if t == newT && e1 == newE1 && e2 == newE2
      then e
      else BinOp(op, newE1, newE2, newT)

and distillOff o =
  match o with
    NoOffset -> o
  | Field (fi, moreO) ->
      fi.floc <- locUnknown; (* ignore compinfo and its other fields *)
      fi.fattr <- [];
      fi.fcomp <- distillCompinfo fi.fcomp;
      fi.ftype <- mergeType fi.ftype;
      let moreOff = distillOff moreO in
      if moreO == moreOff 
      then o
      else Field (fi, moreOff)

  | Index (e, moreO) ->
      let newE = distillExp e in
      let newO = distillOff moreO in
      if moreO == newO && newE == e 
      then o
      else Index (newE, newO)

and distillType t =
  match t with
    TVoid _ | TInt _ | TFloat _ | TBuiltin_va_list _ -> t

  | TFun (rt, None, vargP, atts) ->
      let newRT = mergeType rt in
      if newRT == rt 
      then t
      else TFun (newRT, None, vargP, atts)

  | TFun (rt, Some (args), vargP, atts) ->
      TFun (mergeType rt, 
            Some (List.map (fun (name, typ, atts) ->
                              (name, mergeType typ, [])) args), 
            vargP, atts)

  | TEnum (ei, _) ->
      distillEnuminfo ei;
      t

  | TPtr (ptT, atts) ->
      let newPT = mergeType ptT in
      if ptT == newPT 
      then t
      else TPtr (newPT, atts)
 
  | TArray (eltT, None, atts) ->
      let newET = mergeType eltT in
      if eltT == newET 
      then t
      else TArray (newET, None, atts)

  | TArray (eltT, Some(e), atts) ->
      let newE = distillExp e in
      let newET = mergeType eltT in
      if eltT == newET && newE == e 
      then t
      else TArray (newET, Some (newE), atts)
        
  | TNamed (tinfo, _) ->
      tinfo.ttype <- mergeType tinfo.ttype;
      t

  | TComp (ci, atts) ->
      (*  TComp(mergeCompinfo ci, []) *)
      let newCI = distillCompinfo ci in
      if ci == newCI 
      then t
      else TComp (newCI, atts)

and distillCompinfo ci =    
    (* Can drop cfields because we have that recorded elsewhere *)
    let ci = if ci.cfields = [] 
    then ci
    else { ci with cfields = []; } in
    ci.cattr <- [];
    ci

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

let printHashStats () =
  let hashStats = Stdutil.string_of_hashstats LvHash.stats 
    goldenLvals "Golden lvals" in
  logStatus hashStats;
  let hashStats = Stdutil.string_of_hashstats TyHash.stats 
    goldenTypes "Golden types" in
  logStatus hashStats
  
