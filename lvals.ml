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


(** Custom version of lvals. Manages translation of Cil lval 
    and expression ASTs to this format. 

    The main difference is any lval's host can be an alias analysis 
    representative node. The new definition is not completely 
    recursive (offsets use old Cil exps, so there's a 
    certain cut-off where it reverts to the old Cil ASTs)

    TODO: reduce memory usage (those CIL Typ structures take a lot of space?)
    Can try only track Varinfo keys and Compinfo keys instead of the 
    actual info struct. Load+cache the info struct when needed 
    (@see cilinfos.ml)

    Sidenote: Wish I had a "hash-consed" qualifier to ensure sharing *)


open Fstructs
open Cil
open Pretty

module L = Logging
module AT = Alias_types
module A = Alias
module E = Errormsg
module CLv = Cil_lvals
module Du = Cildump

module Stat = Mystats




(************************************************************
              The new ASTs for Lvals
 ************************************************************)

type aLval = aHost * offset

and aHost = 
    CVar of varinfo
  | CMem of aExp
  | AbsHost of AT.ptaNode

and aExp =
    CConst of constant
  | CLval of aLval
  | CSizeOf of typ
  | CSizeOfE of aExp
  | CSizeOfStr of string
  | CAlignOf of typ
  | CAlignOfE of aExp
  | CUnOp of unop * aExp * typ
  | CBinOp of binop * aExp * aExp * typ
  | CCastE of typ * aExp
  | CAddrOf of aLval 
  | CStartOf of aLval
    

(********* Conversions ************)

let rec abs_of_lval lv =
  match lv with
    Var vi, off -> CVar vi, off
  | Mem e, off -> CMem (abs_of_exp e), off

and abs_of_exp e =
  match e with
    Lval lv -> CLval (abs_of_lval lv)
  | AddrOf lv -> CAddrOf (abs_of_lval lv)
  | StartOf lv -> CStartOf (abs_of_lval lv)
  | Const c -> CConst c
  | SizeOf t -> CSizeOf t
  | SizeOfE e -> CSizeOfE (abs_of_exp e)
  | SizeOfStr s -> CSizeOfStr s
  | AlignOf t -> CAlignOf t
  | AlignOfE e -> CAlignOfE (abs_of_exp e)
  | UnOp (o, e, t) -> CUnOp (o, abs_of_exp e, t)
  | BinOp (bo, e1, e2, t) -> 
      CBinOp (bo, abs_of_exp e1, abs_of_exp e2, t)
  | CastE (t, e) -> CCastE (t, abs_of_exp e)
 

let rec lvals_of_abs alv =
  match alv with
    CVar vi, off -> [Var vi, off]
  | CMem e, off -> 
      let exps = exp_of_abs e in
      List.map (fun e' -> Mem e', off) exps
  | AbsHost h, off ->
      let lvals = A.Abs.represents h in
      List.map (fun (h, o) -> h, Cil.addOffset off o) lvals

and exp_of_abs ae =
  match ae with
  | CLval lv -> 
      let lvals = lvals_of_abs lv in
      List.map (fun lv' -> Lval lv') lvals
  | CAddrOf lv ->
      let lvals = lvals_of_abs lv in
      List.map (fun lv' -> AddrOf lv') lvals
  | CStartOf lv ->
      let lvals = lvals_of_abs lv in
      List.map (fun lv' -> StartOf lv') lvals

  | CConst c -> [Const c]
  | CSizeOf t -> [SizeOf t]
  | CSizeOfE e -> 
      let exps = exp_of_abs e in 
      List.map (fun e' -> SizeOfE e') exps
  | CSizeOfStr s -> [SizeOfStr s]
  | CAlignOf t -> [AlignOf t]
  | CAlignOfE e ->
      let exps = exp_of_abs e in 
      List.map (fun e' -> AlignOfE e') exps
  | CUnOp (o, e, t) -> 
      let exps = exp_of_abs e in
      List.map (fun e' -> UnOp (o, e', t)) exps
  | CBinOp (bo, e1, e2, t) ->
      let e1s = exp_of_abs e1 in
      let e2s = exp_of_abs e2 in
      List.fold_left 
        (fun l e1' -> 
           List.fold_left 
             (fun l e2' -> BinOp (bo, e1', e2', t) :: l) l e2s) [] e1s
  | CCastE (t, e) -> 
      let exps = exp_of_abs e in
      List.map (fun e' -> CastE (t, e')) exps


let rec node_of_absLval lv =
  match lv with 
    CVar vi, off -> 
      A.Abs.getNodeLval (Var vi, off)
        
  | AbsHost pNode, NoOffset ->
      pNode
        
  | AbsHost pNode, off ->
      (* Ignoring offset *)
      pNode
        
  | CMem exp, off ->
      let node = node_of_absExp exp in
      A.Abs.deref node

and node_of_absExp e =
    match e with
      CLval lv
    | CStartOf lv -> node_of_absLval lv
    | CAddrOf lv -> 
        raise A.UnknownLoc

    | CBinOp (_, e1, _, _) -> node_of_absExp e1 (* should get a list *)
    | CUnOp (_, e1, _) 
    | CCastE (_, e1) -> node_of_absExp e1

    | CConst _ 
    | CSizeOfE _
    | CSizeOf _
    | CSizeOfStr _ 
    | CAlignOfE _
    | CAlignOf _ ->
        raise A.UnknownLoc

let deref_absExp e =
  let n = node_of_absExp e in
  [A.Abs.deref n]
    
let deref_absLval lv =
  let n = node_of_absLval lv in
  [A.Abs.deref n]


let addOffsetLval toadd (b, off) =
  b, addOffset toadd off


let mkMem ~addr ~(off: offset) =  
  let res = 
    match addr, off with
      CAddrOf lv, _ -> addOffsetLval off lv
    | CStartOf lv, _ -> (* Must be an array *)
        addOffsetLval (Index(zero, off)) lv 
    | _, _ -> CMem addr, off
  in
  res

let mkLval h o =
  CLval (h, o)

let mkAddrOf ((b, off) as lval) = 
  (* Never take the address of a register variable. Make it a non-register
     variable in the case that happens *)
  (match b with
     CVar vi when vi.vstorage = Register -> vi.vstorage <- NoStorage
   | _ -> ()); 
  match b, off with
    CMem e, NoOffset -> e
  | b, Index(z, NoOffset) when isZero z -> CStartOf (b, NoOffset)(* array *)
  | _ -> CAddrOf lval

let mkVar global name typ =
  CVar (makeVarinfo global name typ)

let hostOfVar vi =
  CVar vi





(************ "New" search functions ************)

let rec findBaseVarinfoExp exp : Cil.varinfo =
  match exp with
    (* Simple variable reference *)
    CLval(lv) 
  | CAddrOf (lv) 
  | CStartOf (lv) ->
      findBaseVarinfoLval lv
  | CBinOp(PlusPI,lhs,_,_) 
  | CBinOp(MinusPI,lhs,_,_)
  | CBinOp(IndexPI,lhs,_,_) ->
      findBaseVarinfoExp lhs

  | CSizeOfE e
  | CAlignOfE e
  | CCastE (_, e) -> findBaseVarinfoExp e
 
  (* Other cases shouldn't occur (non lvalues) *)
  | _ -> raise CLv.BaseVINotFound

      
and findBaseVarinfoLval lval : Cil.varinfo =
  match lval with
    (CVar(vi),_) ->
      vi
  | (CMem(ptrExp),_) ->
      findBaseVarinfoExp ptrExp
  | (AbsHost _, _) ->
      raise CLv.BaseVINotFound


let rec countOpsWithCount (curExp) (curCount) = 
  match curExp with
    CLval l  
  | CAddrOf l
  | CStartOf l->
      countOpsInLval l curCount
  | CBinOp(_, ce1, ce2, _) ->
      let ops = countOpsWithCount ce1 (curCount + 1) in
      countOpsWithCount ce2 ops
  | CUnOp(_, ce, _) ->
      countOpsWithCount ce (curCount + 1)
  | CSizeOfE e
  | CAlignOfE e
  | CCastE (_, e) -> 
      countOpsWithCount e curCount
  | CConst _ 
  | CSizeOf _
  | CSizeOfStr _
  | CAlignOf _ -> curCount

and countOpsInLval lv curCount =
  (* Ignore array index expressions *)
  match lv with
    (CVar _, _)
  | (AbsHost _, _) -> curCount
  | (CMem e, _) -> countOpsWithCount e curCount
        
let countOpsInExp exp : int =
  countOpsWithCount exp 0


(*************** "New" Printers ***************)


let getParenthLevel = function
  | CBinOp((LAnd | LOr), _,_,_) -> 80
                                        (* Bit operations. *)
  | CBinOp((BOr|BXor|BAnd),_,_,_) -> bitwiseLevel (* 75 *)

                                        (* Comparisons *)
  | CBinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_) ->
      comparativeLevel (* 70 *)
                                        (* Additive. Shifts can have higher 
                                         * level than + or - but I want 
                                         * parentheses around them *)
  | CBinOp((MinusA|MinusPP|MinusPI|PlusA|
           PlusPI|IndexPI|Shiftlt|Shiftrt),_,_,_)  
    -> additiveLevel (* 60 *)

                                        (* Multiplicative *)
  | CBinOp((Div|Mod|Mult),_,_,_) -> 40

                                        (* Unary *)
  | CCastE(_,_) -> 30
  | CAddrOf(_) -> 30
  | CStartOf(_) -> 30
  | CUnOp((Neg|BNot|LNot),_,_) -> 30

                                        (* Lvals *)
  | CLval(CMem _ , _)
  | CLval(AbsHost _, _) -> 20
  | CLval(CVar _, (Field _|Index _)) -> 20
  | CSizeOf _ | CSizeOfE _ | CSizeOfStr _ -> 20
  | CAlignOf _ | CAlignOfE _ -> 20

  | CLval(CVar _, NoOffset) -> 0        (* Plain variables *)
  | CConst _ -> 0                        (* Constants *)

      

class absPrinterClass = object (self) 
  inherit defaultCilPrinterClass

  val mutable currentFormals : varinfo list = []
  method private getLastNamedArgument (s: string) : exp =
    match List.rev currentFormals with 
      f :: _ -> Lval (var f)
    | [] -> 
        E.s (warn "Can't find the last named arg when printing call to %s\n" s)

  (*** L-VALUES ***)
  method pALval () (lv:aLval) =  (* lval (base is 1st field)  *)
    match lv with
      CVar vi, o -> self#pOffset (self#pVar vi) o
    | CMem e, Field(fi, o) ->
        self#pOffset
          ((self#pExpPrec arrowLevel () e) ++ text ("->" ^ fi.fname)) o
    | CMem e, o ->
        self#pOffset
          (text "(*" ++ self#pExpPrec derefStarLevel () e ++ text ")") o
    | AbsHost h, o -> 
        self#pOffset (text (A.Abs.string_of h)) o


  method private pLvalPrec (contextprec: int) () lv = 
    if getParenthLevel (CLval(lv)) >= contextprec then
      text "(" ++ self#pALval () lv ++ text ")"
    else
      self#pALval () lv

  (*** EXPRESSIONS ***)
  method pAExp () (e: aExp) : doc = 
    let level = getParenthLevel e in
    match e with
      CConst(c) -> d_const () c
    | CLval(l) -> self#pALval () l
    | CUnOp(u,e1,_) -> 
        (d_unop () u) ++ chr ' ' ++ (self#pExpPrec level () e1)
          
    | CBinOp(b,e1,e2,_) -> 
        align 
          ++ (self#pExpPrec level () e1)
          ++ chr ' ' 
          ++ (d_binop () b)
          ++ chr ' '
          ++ (self#pExpPrec level () e2)
          ++ unalign

    | CCastE(t,e) -> 
        text "(" 
          ++ self#pType None () t
          ++ text ")"
          ++ self#pExpPrec level () e

    | CSizeOf (t) -> 
        text "sizeof(" ++ self#pType None () t ++ chr ')'
    | CSizeOfE (e) ->  
        text "sizeof(" ++ self#pAExp () e ++ chr ')'

    | CSizeOfStr s -> 
        text "sizeof(" ++ d_const () (CStr s) ++ chr ')'

    | CAlignOf (t) -> 
        text "__alignof__(" ++ self#pType None () t ++ chr ')'
    | CAlignOfE (e) -> 
        text "__alignof__(" ++ self#pAExp () e ++ chr ')'
    | CAddrOf(lv) -> 
        text "& " ++ (self#pLvalPrec addrOfLevel () lv)
          
    | CStartOf(lv) -> self#pALval () lv

  method private pExpPrec (contextprec: int) () (e: aExp) = 
    let thisLevel = getParenthLevel e in
    let needParens =
      if thisLevel >= contextprec then
	true
      else if contextprec == bitwiseLevel then
        (* quiet down some GCC warnings *)
	thisLevel == additiveLevel || thisLevel == comparativeLevel
      else
	false
    in
    if needParens then
      chr '(' ++ self#pAExp () e ++ chr ')'
    else
      self#pAExp () e

end (* class absPrinterClass *)

let absPrinter = new absPrinterClass
  
let printAExp pp () (e: aExp) : doc = 
  pp#pAExp () e

let printALval pp () (lv: aLval) : doc = 
  pp#pALval () lv


(* Now define some short cuts *)
let d_aexp () e = printAExp absPrinter () e
let d_alval () lv = printALval absPrinter () lv

let string_of_lval lv =
  sprint 80 (d_alval () lv)

let string_of_exp e =
  sprint 80 (d_aexp () e)

let string_of_lval_decl lv =
  try
    let baseVar = findBaseVarinfoLval lv in
    (string_of_lval lv) ^ " @ " ^ (Du.string_of_loc baseVar.Cil.vdecl)
  with CLv.BaseVINotFound ->
    string_of_lval lv

(*************** Print to XML *****************)

class lvalXMLPrinter = object (self) 

  method pALval (lv, blob) =  (* lval (base is 1st field)  *)
    text "<lval printed=\"" ++ absPrinter#pALval () lv ++ text "\" " ++
      (match lv with
         AbsHost h, o -> 
           text ("size=\"" ^ (string_of_int (A.Abs.size_of h)) ^ "\">")
       | _ ->
           let size = 
             (match blob with
                None -> nil
              | Some (s) -> text ("size=\"" ^ (string_of_int s) ^ "\" ")) in
           let vinfo = try
             let baseVar = findBaseVarinfoLval lv in
             text ("global=\"" ^ (string_of_bool baseVar.vglob) ^ 
                     "\" vid=\"" ^ (string_of_int baseVar.vid) ^ "\">") 
             ++ line ++
               self#pLoc () (baseVar.vdecl, None)
           with CLv.BaseVINotFound ->
             text ">" in
           size ++ vinfo
      ) ++ text "</lval>" ++ line

  method pLoc () (loc, parent) =
    if CLv.locIsUnknown loc then
      nil
    else
      indent 1 
        (text "<pp file=\"" ++ text loc.file ++ 
           (text "\" line=\"" ++
              text (string_of_int loc.line) ++ text "\" ") ++
           (match parent with
              None -> nil 
            | Some (k) -> text ("parent=\"" ^ (string_of_fkey k) ^ "\"")) ++ 
           text "></pp>" ++ line)

end

(*************** "New" comparison operations ***************)

let rec compare_lval (h1, o1) (h2, o2) =
  let ch = compare_host h1 h2 in
  if (ch == 0) then
    Ciltools.compare_offset o1 o2
  else
    ch

and compare_host h1 h2 =
  match h1, h2 with 
    CVar v1, CVar v2 ->
      Ciltools.compare_var v1 v2

  | CMem e1, CMem e2 ->
      compare_exp e1 e2

  | AbsHost a1, AbsHost a2 ->
      A.Abs.compare a1 a2

  | _ -> 
      compare h1 h2

and compare_exp x y =
  match x, y with
    CLval l1, CLval l2 
  | CAddrOf l1, CAddrOf l2
  | CStartOf l1, CStartOf l2 ->
      compare_lval l1 l2
  | CConst(CEnum(e1,s1,ei1)), CConst(CEnum(e2,s2,ei2)) -> 
      let s_comp = compare s1 s2 in 
      if (s_comp <> 0) then 
        s_comp
      else 
        let e_comp = Ciltools.compare_exp e1 e2 in
        if (e_comp <> 0) then
          e_comp
        else
          compare ei1.ename ei2.ename
 
  | CSizeOf(t1), CSizeOf(t2) | CAlignOf(t1), CAlignOf(t2) -> 
      Ciltools.compare_type t1 t2
  | CAlignOfE(e1), CAlignOfE(e2)  
  | CSizeOfE(e1), CSizeOfE(e2) ->
      compare_exp e1 e2 
  | CUnOp(o1,e1,t1), CUnOp(o2,e2,t2) ->
      let o_comp = compare o1 o2 in
      if (o_comp <> 0) then 
        o_comp
      else
        let e_comp = compare_exp e1 e2 in
        if( e_comp <> 0) then
          e_comp
        else
          Ciltools.compare_type t1 t2

  | CBinOp(o1,l1,r1,t1), CBinOp(o2,l2,r2,t2) ->
      let o_comp = compare o1 o2 in
      if (o_comp <> 0) then
        o_comp
      else
        let l_comp = compare_exp l1 l2 in
        if( l_comp <> 0) then
          l_comp
        else
          let r_comp = compare_exp r1 r2 in
          if ( r_comp<> 0) then 
            r_comp
          else
            Ciltools.compare_type t1 t2

  | CCastE(t1,e1), CCastE(t2,e2) ->
      let t_comp = Ciltools.compare_type t1 t2 in
      if (t_comp <> 0) then 
        t_comp
      else 
        compare_exp e1 e2 

  | _ -> compare x y



(*************** "New" hash functions ***************)
  
let rec hash_lval (h, o) =
  let hh = hash_host h in
  hh lxor (Ciltools.hash_offset o)

and hash_host h =
  match h with 
    CVar v ->
      Ciltools.hash_var v
  | CMem e ->
      hash_exp e
  | AbsHost a ->
      A.Abs.hash a
 
and hash_exp x =
  match x with
    CLval l ->
      70039403 lxor hash_lval l
  | CAddrOf l ->
      9083249 lxor hash_lval l
  | CStartOf l ->
      1243789 lxor hash_lval l
  | CConst(CEnum(e1,s1,ei1)) ->
      let s_hash = Hashtbl.hash s1 in 
      let e_hash = Hashtbl.hash e1 in
      let ei_hash = Hashtbl.hash ei1.ename in
      s_hash lxor e_hash lxor ei_hash
        
  | CSizeOf(t1) ->
      0x22198721 lxor (Ciltools.hash_type t1)
  | CAlignOf(t1) ->
      0x04358225 lxor (Ciltools.hash_type t1)
  | CSizeOfE e ->
       0x11237639 lxor (hash_exp e)
  | CAlignOfE(e1)  -> 
      0x19857434 lxor (hash_exp e1)
        
  | CUnOp(o1,e1,t1) ->
      let o_hash = Hashtbl.hash o1 in
      let e_hash = hash_exp e1 in
      let t_hash = Ciltools.hash_type t1 in
      o_hash lxor e_hash lxor t_hash
        
  | CBinOp(o1,l1,r1,t1) ->
      let o_hash = Hashtbl.hash o1 in
      let l_hash = hash_exp l1 in
      let r_hash = hash_exp r1 in
      let t_hash = Ciltools.hash_type t1 in
      o_hash lxor l_hash lxor r_hash lxor t_hash
        
  | CCastE(t1,e1) ->
      let t_hash = Ciltools.hash_type t1 in
      let e_hash = hash_exp e1 in
      0x33982173 lxor t_hash lxor e_hash
  | _ ->
      Hashtbl.hash x

(*************** "New" Set / Map / Hash modules *****)

(* Set of expressions, useful for arguments *)
module OrderedExp = struct 
  type t = aExp
  let compare = compare_exp
end

module ExpSet = Set.Make (OrderedExp)
module ExpMap = Map.Make (OrderedExp)

module OrderedLval = struct
  type t = aLval
  let compare = compare_lval
end

module LvalSet = Set.Make (OrderedLval)
module LvalMap = Map.Make (OrderedLval)

(* Hash tables *)

module HashedLval = struct
  type t = aLval
  let equal a b = compare_lval a b == 0
  let hash = hash_lval
end

module LvalHash = Hashtbl.Make (HashedLval)

module HashedExp = struct
  type t = aExp
  let equal a b = compare_exp a b == 0
  let hash = hash_exp
end

module ExpHash = Hashtbl.Make (HashedExp)


(*************** "New" distillation and hash-consing ***************)

module LvHash = Weak.Make(HashedLval)

let goldenLvals = LvHash.create 173

(** Try to reduce the memory footprint of lvals. For now, just remove
    decl information. Ignore attribute lists, etc. *)
let rec distillLval = function
    CVar(vi) as b, off ->
      CLv.distillVar vi;
      b, CLv.distillOff off

  | CMem(exp), off ->
      CMem (distillExp exp), CLv.distillOff off

  | AbsHost h as b, off ->
      b, CLv.distillOff off

and distillExp e =
  match e with 
    CLval (l) ->
      CLval (mergeLv l)

  | CAddrOf(l) ->
      CAddrOf (mergeLv l)

  | CStartOf(l) -> 
      CStartOf (mergeLv l)

  | CConst (CEnum (ce, name, ei)) ->
      CLv.distillEnuminfo ei;
      CConst (CEnum (CLv.distillExp ce, name, ei))

  | CSizeOfE (e) ->
      CSizeOfE (distillExp e)

  | CAlignOfE (e) ->
      CAlignOfE (distillExp e)

  | CConst _
  | CSizeOfStr _ ->
      e

  | CSizeOf (t) ->
      CSizeOf (CLv.mergeType t)
  | CAlignOf (t) -> 
      CAlignOf (CLv.mergeType t)

  | CCastE (t, e) ->
      CCastE (CLv.mergeType t, distillExp e)

  | CUnOp(op, e, t) ->
      CUnOp(op, distillExp e, CLv.mergeType t)

  | CBinOp(op, e1, e2, t) ->
      CBinOp(op, distillExp e1, distillExp e2, CLv.mergeType t)


and mergeLv lv = 
  try 
    LvHash.find goldenLvals lv
  with Not_found ->
    let newLv = distillLval lv in
    LvHash.add goldenLvals newLv;
    newLv



(************ "New" type functions **************)


let rec typeOfUnsafe e : typ = 
  match e with
    CLval lv -> typeOfLvalUnsafe lv
  | CAddrOf (lv) -> TPtr (typeOfLvalUnsafe lv, [])
  | CStartOf (lv) ->
      (match CLv.unrollTypeNoAttrs (typeOfLvalUnsafe lv) with
         TArray (t,_, _) -> TPtr(t, [])
       | _ -> 
           E.s (E.bug "typeOfUnsafe: StartOf on a non-array")
      )

  | CConst(CInt64 (_, ik, _)) -> TInt(ik, [])

  | CConst(CChr _) -> intType

  | CConst(CStr s) -> !stringLiteralType

  | CConst(CWStr s) -> TPtr(!wcharType,[])

  | CConst(CReal (_, fk, _)) -> TFloat(fk, [])

  | CConst(CEnum(_, _, ei)) -> TEnum(ei, [])

  | CSizeOf _ | CSizeOfE _ | CSizeOfStr _ -> !typeOfSizeOf
  | CAlignOf _ | CAlignOfE _ -> !typeOfSizeOf
  | CUnOp (_, _, t) 
  | CBinOp (_, _, _, t)
  | CCastE (t, _) -> t

and typeOfLvalUnsafe = function
    CVar vi, off -> 
      CLv.typeOffsetUnsafe vi.vtype off
  | CMem addr, off ->
      (match CLv.unrollTypeNoAttrs (typeOfUnsafe addr) with
         TPtr (t, _) -> CLv.typeOffsetUnsafe t off
       | t ->
           (match off with
              NoOffset ->
               TVoid []
            | _ -> 
                CLv.typeOffsetUnsafe t off 
          )
      )
  | AbsHost h, off ->
      TVoid [] (* Hack to make offset attachment allowed *)
        

let typeAfterDeref exp =
  match CLv.unrollTypeNoAttrs (typeOfUnsafe exp) with
    TPtr (t,_) ->
      CLv.unrollTypeNoAttrs t
(*  
    | TVoid x -> TVoid x (* Hack to make offset attachment allowed *)
*)
  | _ ->
      raise CLv.TypeNotFound


(* TODO: too much ptr / cast info being thrown away to make these
   checks sound... (e.g., kernel.h  "container_of")   *)
let attachOffset host offset =
  match offset with
    Field (fi, _) -> begin
      let hostTyp = typeOfLvalUnsafe (host, NoOffset) in
      match hostTyp with
        TComp (ci, _) -> 
          (* Assumes ckeys are fixed
             TODO check if types/offs are compatible instead *)
          if (fi.fcomp.cstruct <> ci.cstruct || 
                fi.fcomp.cname <> ci.cname) then 
            raise CLv.OffsetMismatch
          else
            (host, offset)
      | TVoid _ 
      | TInt _ -> (* allow *)
          (host, offset)
      | _ ->
          raise CLv.OffsetMismatch
    end
  | _ -> 
      (host, offset)


let mkMemChecked ptrExp offset =
  match offset with
    Field (fi, _) -> begin
      try 
        let hostTyp = typeAfterDeref ptrExp in
        match hostTyp with
          TComp (ci, _) ->
             if (fi.fcomp.cstruct <> ci.cstruct || 
                   fi.fcomp.cname <> ci.cname) then
               (* mkMem ptrExp offset *)
               raise CLv.OffsetMismatch 
            else
              mkMem ptrExp offset
        | TVoid _ 
        | TInt _ -> (* allow *)
            mkMem ptrExp offset
        | _ ->
            raise CLv.OffsetMismatch
      with 
        E.Error 
      | Failure _
      | CLv.TypeNotFound ->
          raise CLv.OffsetMismatch
    end
  | _ -> 
      mkMem ptrExp offset



(*************** "New" Formal -> Actual Substitution ***************)

(** Find instances of the formal within given lval, and substitute w/ 
    the given actual lval. 
    Assumes we are looking for inter-procedural effects *)
let rec substActForm (actual:aExp) (lvalWithFormal:aLval) : aLval =
  match lvalWithFormal with 
    (CVar(vi), formOff) ->
      (* NO, don't substitute! A straight-up var as a formal is always a
         different memory cell from a straight-up var in the other scope! *)
      raise CLv.SubstInvalidArg

  | (CMem (CLval (CVar(vi), NoOffset)), outerOff) -> begin
      (* Assume any variable encountered is the formal... To fix this,
         should actually add a var attribute to compare against *)
      try
        mkMemChecked actual outerOff
      with 
        CLv.OffsetMismatch
      | Errormsg.Error 
      | Failure _ ->
          raise CLv.SubstInvalidArg
    end
  | (CMem(ptrExp), outerOff) ->
      (try
         let newExp = substActFormExp actual ptrExp in
         mkMemChecked newExp outerOff
       with 
         CLv.OffsetMismatch
       | Errormsg.Error 
       | Failure _ ->
           raise CLv.SubstInvalidArg)

  | AbsHost h, outerOff ->
      L.logError "substActForm: tried to subs into abs host\n";
      raise CLv.SubstInvalidArg
             
        
and substActFormExp (actual:aExp) (expWithFormal:aExp) : aExp =
  match expWithFormal with
    CLval(lv) ->           
      let newLval = substActForm actual lv in
      (CLval(newLval))
        
  | CAddrOf (l) ->
      mkAddrOf (substActForm actual l)

  | CStartOf(l) ->
      CStartOf (substActForm actual l)

  | CCastE(t, e) ->
      CCastE (t, substActFormExp actual e)
        
  | CAlignOfE(e) ->
      CAlignOfE (substActFormExp actual e)
        
  | CSizeOfE(e) ->
      CSizeOfE (substActFormExp actual e)
        
  | CUnOp (unop, e, t) ->
      let newExp = substActFormExp actual e in
      CUnOp (unop, newExp, t)
        
  | CBinOp (bop, e1, e2, t) ->
      (* Assume formal is (only) in the first exp for now *)
      let newExp = substActFormExp actual e1 in
      CBinOp(bop, newExp, e2, t)
        
  | CAlignOf _
  | CSizeOf _
  | CSizeOfStr _
  | CConst _ ->
      raise CLv.SubstInvalidArg


(*************** "New" Simplifiers ***************)

(** Convert the expression to canonical form *)
let rec canonicizeExp exp =
  match exp with
    CLval(l) -> 
      CLval (canonicizeLval l)
  | CAddrOf (l) ->
      CAddrOf (canonicizeLval l)
  | CStartOf (l) ->
      CStartOf (canonicizeLval l)
  | _ -> 
      exp

and canonicizeLval (host, off) =
  let newOff, _ = CLv.canonicizeOff off in
  match host with
    CVar _ 
  | AbsHost _ ->
      (host, newOff)
  | CMem(ptrExp) ->
      (CMem(canonicizeExp ptrExp), newOff)


(** Collapse chains of (field) derefs w/ the same type.  
    Return:
    1) the simplified lval
    2) a list of the first seen lval w/ a corresponding type
    3) a flag indicating whether or not the lval was simplified *)
let rec doSimplifyLv (lv:aLval) : 
    aLval * (Cil.typ * aLval) list * bool = 
  match lv with
    (* Base case *)
    (CVar _ as host, off) ->
      let newLv = host, CLv.simplifyOff off in
      let curTyp = typeOfLvalUnsafe newLv in
      (newLv, [(curTyp, newLv)], false)

  | (AbsHost _ as host, off) ->
      (* Base case *)
      let newLv = host, CLv.simplifyOff off in
      (newLv, [], false)

  | (CMem (ptrExp), off) ->
      let simplifiedPtr, seenTypes, changed = doSimplifyExp ptrExp in
      let curTyp = typeOfLvalUnsafe lv in
      (try 
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
           let newOff = CLv.simplifyOff off in
           let newLv = (mkMem simplifiedPtr newOff) in
           (newLv, (curTyp, newLv) :: seenTypes, changed))


and doSimplifyExp (exp:aExp) : aExp * (Cil.typ * aLval) list * bool =
  match exp with
    CLval(lv) ->
      let simplerLv, seenTypes, changed = doSimplifyLv lv in
      (CLval (simplerLv), seenTypes, changed)
  | _ ->
      (exp, [], false)

(** May eliminate redundant parts of lval access path. Returns the 
    simplified lval and a flag indicating whether a change is made *)
let simplifyLval (lv:aLval) : aLval * bool =
  let simplerLv, _, changed = doSimplifyLv lv in
  (simplerLv, changed)



(******************* High-level compare ********************)


type imprecision =
    Syntactic              (* Syntactically equal *)
  | PtsToSame of int * int (* AA info + size of representative nodes
                              (should be the same w/ Steensgaard) *)
  | SameType               (* no AA info, but same type *)


let compare_imprec i1 i2 =
  match i1, i2 with
  | Syntactic, Syntactic -> 0
  | Syntactic, _ -> -1
  | _, Syntactic -> 1
  | PtsToSame (x, y), PtsToSame (x', y') ->
      (* smaller is less imprecise *)
      let c = x - x' in
      if c <> 0 then c
      else y - y'
  | PtsToSame _, SameType -> -1 (* type-based check is most imprecise *)
  | SameType, PtsToSame _ -> 1
  | SameType, SameType -> 0


(** Returns Some(imprec) if lv1 may be the same lval as lv2... 
    where imprec is a measure of imprecision... else returns None *)
let rec sameLval lv1 lv2 =
  if (compare_lval lv1 lv2 == 0) then (* first compare for syntactic eq *)
    match lv1, lv2 with 
      (AbsHost x, _), (AbsHost _, _) -> 
        let s = A.Abs.size_of x in
        Some (PtsToSame (s, s))       (* should be the same size if eq *)
    | _ -> Some (Syntactic)
  else if (Ciltools.compare_type 
             (typeOfLvalUnsafe lv1) 
             (typeOfLvalUnsafe lv2) == 0) then
    let (h1, o1), (h2, o2) = lv1, lv2 in
    if (Ciltools.compare_offset o1 o2 == 0) then         
      (* ignores case when ptr points to some offset *)
      sameHost h1 h2 
    else
      None
  else
    None

and sameHost h1 h2 =
  match h1, h2 with
    CVar v1, CVar v2 ->
      None (* compared for syntactic equality in sameLval already *)  
  | _ ->
      (try
         let n1s = nodesOf h1 in
         let n2s = nodesOf h2 in
         let nodeSizes = 
           (List.fold_left
              (fun found n1 -> 
                 List.fold_left
                   (fun found n2 -> 
                      match found with 
                        None -> 
                          if (A.Abs.compare n1 n2 == 0) then
                            let s1 = A.Abs.size_of n1 in
                            let s2 = A.Abs.size_of n2 in
                            Some (s1, s2)
                          else
                            None
                      | Some _ -> found
                   )  found n2s)
              None n1s) in
         match nodeSizes with
           None -> None
         | Some (a, b) -> Some (PtsToSame (a, b))
       with 
         A.UnknownLoc 
       | Not_found ->
           (* couldn't resolve aliases; try an approximation! *)
           try
             let t1 = typeOf h1 in
             let t2 = typeOf h2 in
             typeBasedCheck t1 t2
           with
             Errormsg.Error
           | Failure _ 
           | CLv.TypeNotFound ->
               None
      )

and typeBasedCheck typ1 typ2 =
  if (Ciltools.compare_type typ1 typ2 == 0) then
    Some (SameType)
  else
    None

and typeOf = function
    CVar v -> v.vtype
  | CMem e -> typeAfterDeref e
  | AbsHost _ -> failwith "no type"
      
and nodesOf = function
    CVar v -> [A.Abs.getNodeVar v]
  | CMem e -> deref_absExp e
  | AbsHost h -> [h]
