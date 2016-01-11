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
open Scope
open Logging

module Stat = Mystats


(************************************************************
              The new ASTs for Lvals
 ************************************************************)

type ptaNode = Alias_types.ptaNode

type lval_varid = int

type vinfo = 
    OrigVar of lval_varid
  | CreatedVar of varinfo

type aLval = aHost * offset

and aHost = 
    CVar of vinfo
  | CMem of aExp
  | AbsHost of ptaNode

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



(** raised lval uses an abstract host in an unexpected place *)
exception IsAbstract



(********* Conversions ************)

(****************************** From CIL ******************************)

let abs_of_var vinfo = 
  if vinfo.vid >= 0 then OrigVar vinfo.vid else CreatedVar vinfo

let rec abs_of_lval lv =
  match lv with
    Var vi, off -> CVar (abs_of_var vi), off
  | Mem e, off -> CMem (abs_of_exp e), off

and abs_of_exp e =
  match e with
    Lval lv -> CLval (abs_of_lval lv)
  | AddrOf lv -> CAddrOf (abs_of_lval lv)
  | StartOf lv -> CStartOf (abs_of_lval lv)
  | Const c -> CConst c
  | SizeOf t -> CSizeOf t
  | SizeOfE e -> 
      CSizeOfE (abs_of_exp e) (* shouldn't need to rec. but for uniformity *)
  | SizeOfStr s -> CSizeOfStr s
  | AlignOf t -> CAlignOf t
  | AlignOfE e -> 
      CAlignOfE  (abs_of_exp e)
  | UnOp (o, e, t) -> CUnOp (o, abs_of_exp e, t)
  | BinOp (bo, e1, e2, t) ->
      CBinOp (bo, abs_of_exp e1, abs_of_exp e2, t)
  | CastE (t, e) -> CCastE (t, abs_of_exp e)



(****************************** Back to CIL ******************************) 


let var_of_abs vi = 
  match vi with 
    OrigVar vid -> Cilinfos.getVarinfo vid
  | CreatedVar vinfo -> vinfo

let rec lvals_of_abs alv =
  match alv with
    CVar vid, off -> let vi = var_of_abs vid in [Var vi, off]
  | CMem e, off -> 
      let exps = exp_of_abs e in
      List.map (fun e' -> Mem e', off) exps
  | AbsHost h, off ->
      let lvals = Alias.Abs.represents h in
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

(** Conversion back to CIL, when the expression / lvals don't use rep. nodes *)
let rec exp_of_abs_simple ae =
  match ae with
  | CLval lv -> 
      let lv' = lval_of_abs_simple lv in
      Lval lv'

  | CAddrOf lv ->
      let lv' = lval_of_abs_simple lv in
      AddrOf lv'

  | CStartOf lv ->
      let lv' = lval_of_abs_simple lv in
      StartOf lv'

  | CConst c -> Const c
  | CSizeOf t -> SizeOf t
  | CSizeOfE e -> 
      SizeOfE (exp_of_abs_simple e)
  | CSizeOfStr s -> SizeOfStr s
  | CAlignOf t -> AlignOf t
  | CAlignOfE e -> 
      AlignOfE (exp_of_abs_simple e)
  | CUnOp (o, e, t) -> 
      let exp = exp_of_abs_simple e in
      UnOp (o, exp, t)
  | CBinOp (bo, e1, e2, t) ->
      let e1s = exp_of_abs_simple e1 in
      let e2s = exp_of_abs_simple e2 in
      BinOp (bo, e1s, e2s, t)
  | CCastE (t, e) -> 
      let exp = exp_of_abs_simple e in
      CastE (t, exp)

and lval_of_abs_simple alv =
  match alv with
    CVar vid, off -> let vi = var_of_abs vid in Var vi, off
  | CMem e, off -> 
      let exp = exp_of_abs_simple e in
      Mem exp, off
  | AbsHost h, off ->
      raise IsAbstract


let rec node_of_absLval lv =
  match lv with 
    CVar vid, off -> 
      let vi = var_of_abs vid in
      Alias.Abs.getNodeLval (Var vi, off)

  | AbsHost pNode, NoOffset ->
      pNode

  | AbsHost pNode, off ->
      (* Ignoring offset *)
      pNode

  | CMem exp, off ->
      let node = node_of_absExp exp in
      (* delay the deref? no.. need to return a pts to node *)
      Alias.Abs.deref node

and node_of_absExp e =
    match e with
      CLval lv
    | CStartOf lv -> node_of_absLval lv (* StartOf?! *)
    | CAddrOf (CMem e1, o) -> node_of_absExp e1
    | CAddrOf (CVar _, _) 
    | CAddrOf (AbsHost _, _) -> 
        raise Alias.UnknownLoc (* hmm ... *)

    | CBinOp (_, e1, _, _) -> node_of_absExp e1 (* should get a list *)
    | CUnOp (_, e1, _) 
    | CCastE (_, e1) -> node_of_absExp e1

    | CConst _ | CSizeOfE _ | CSizeOf _ | CSizeOfStr _ 
    | CAlignOfE _ | CAlignOf _ ->
        raise Alias.UnknownLoc

let deref_absExp e =
  try 
    let n = node_of_absExp e in
    [Alias.Abs.deref n]
  with Alias.UnknownLoc ->
    (* Maybe the exp was an addrOf to begin w/, so a deref cancels it out *)
    (let rec derefAddrOf e =
       match e with
         CAddrOf lv 
       | CStartOf lv -> [node_of_absLval lv]
       | CCastE (t, e') -> derefAddrOf e'
       | _ -> raise Alias.UnknownLoc
     in
     derefAddrOf e)


let deref_absLval lv =
  let n = node_of_absLval lv in
  [Alias.Abs.deref n]


(********* Constructors ***********)


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
     CVar vid ->
       let vi = var_of_abs vid in
       if vi.vstorage = Register then vi.vstorage <- NoStorage
   | _ -> ());

  match b, off with
    CMem e, NoOffset -> e
  | b, Index(z, NoOffset) when isZero z -> CStartOf (b, NoOffset)(* array *)
  | _ -> CAddrOf lval

let hostOfVar (vi: varinfo) =
  CVar (abs_of_var vi)


(************ Dummy values ***************)

let dummyVar = Cil_lvals.mkVarinfo true "DIM$UM" (TVoid [])

let dummyLval = (hostOfVar dummyVar, NoOffset)
    

(************ "New" search functions ************)

let rec findBaseVarinfoExp exp : Cil.varinfo =
  match exp with
    (* Simple variable reference *)
    CLval(lv) | CAddrOf (lv) | CStartOf (lv) ->
      findBaseVarinfoLval lv
  | CBinOp(PlusPI,lhs,_,_) 
  | CBinOp(MinusPI,lhs,_,_)
  | CBinOp(IndexPI,lhs,_,_) ->
      findBaseVarinfoExp lhs

  | CSizeOfE e | CAlignOfE e | CCastE (_, e) -> findBaseVarinfoExp e
 
  (* Other cases shouldn't occur (non lvalues) *)
  | _ -> raise Cil_lvals.BaseVINotFound

      
and findBaseVarinfoLval lval : Cil.varinfo =
  match lval with
    (CVar(vid),_) -> var_of_abs vid
  | (CMem(ptrExp),_) ->
      findBaseVarinfoExp ptrExp
  | (AbsHost _, _) ->
      raise Cil_lvals.BaseVINotFound

let rec isAbsLval lval =
  match lval with
    (CVar(vid),_) -> None
  | (CMem(ptrExp),_) ->
      isAbsLocExp ptrExp
  | (AbsHost h, _) ->
      Some h

and isAbsLocExp exp =
  match exp with
    CLval lv | CAddrOf lv | CStartOf lv -> 
      isAbsLval lv
  | CCastE (_, e) -> 
      isAbsLocExp e
  | CBinOp(_,lhs, _,_) -> 
      isAbsLocExp lhs
  | CUnOp(_,e,_) -> 
      isAbsLocExp e
  | CConst _ | CSizeOfE _ | CSizeOfStr _ | CSizeOf _ | CAlignOfE _ 
  | CAlignOf _ -> None

let isAbsVar (h, o) =
  match h with
    CVar _ | CMem _ -> None
  | AbsHost h -> Some h

let rec countOpsWithCount (curExp) (curCount) = 
  match curExp with
    CLval l  | CAddrOf l | CStartOf l->
      countOpsInLval l curCount
  | CBinOp(_, ce1, ce2, _) ->
      let ops = countOpsWithCount ce1 (curCount + 1) in
      countOpsWithCount ce2 ops
  | CUnOp(_, ce, _) ->
      countOpsWithCount ce (curCount + 1)
  | CSizeOfE e | CAlignOfE e | CCastE (_, e) -> 
      countOpsWithCount e curCount
  | CConst _  | CSizeOf _ | CSizeOfStr _ | CAlignOf _ -> curCount

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
    | [] -> Errormsg.s 
        (warn "Can't find the last named arg when printing call to %s\n" s)

  (*** L-VALUES ***)
  method pALval () (lv:aLval) =  (* lval (base is 1st field)  *)
    match lv with
      CVar vid, o -> self#pOffset (self#pVar (var_of_abs vid)) o
    | CMem e, Field(fi, o) ->
        self#pOffset
          ((self#pExpPrec arrowLevel () e) ++ text ("->" ^ fi.fname)) o
    | CMem e, o ->
        self#pOffset
          (text "(*" ++ self#pExpPrec derefStarLevel () e ++ text ")") o
    | AbsHost h, o -> 
        self#pOffset (text (Alias.Abs.string_of h)) o


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


(**** Now define some short cuts ****)

let d_aexp () e = printAExp absPrinter () e
let d_alval () lv = printALval absPrinter () lv

let string_of_lval lv =
  sprint 80 (d_alval () lv)

let string_of_exp e =
  sprint 80 (d_aexp () e)

let string_of_lval_decl lv =
  try
    let baseVar = findBaseVarinfoLval lv in
    (string_of_lval lv) ^ " @ " ^ (Cildump.string_of_loc baseVar.Cil.vdecl)
  with Cil_lvals.BaseVINotFound ->
    string_of_lval lv

(*************** Print to XML *****************)

class lvalXMLPrinter = object (self) 

  method pALval (lv, blob) =  (* lval (base is 1st field)  *)
    text "<lval printed=\"" ++ absPrinter#pALval () lv ++ text "\" " ++
      (match lv with
         AbsHost h, o -> 
           text ("size=\"" ^ (string_of_int (Alias.Abs.pts_size h)) ^ "\">")
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
           with Cil_lvals.BaseVINotFound ->
             text ">" in
           size ++ vinfo
      ) ++ text "</lval>" ++ line

  method pLoc () (loc, parent) =
    if Cil_lvals.locIsUnknown loc then
      nil
    else
      indent 1 
        (text "<pp file=\"" ++ text loc.file ++ 
           (text "\" line=\"" ++
              text (string_of_int loc.line) ++ text "\" ") ++
           (match parent with
              None -> nil 
            | Some (k) -> text ("parent=\"" ^ 
                                  (Callg.fid_to_string k) ^ "\"")) ++ 
           text "></pp>" ++ line)

end

(*************** "New" comparison operations ***************)

let compare_var v1 v2 =
  match v1, v2 with 
    OrigVar v1, OrigVar v2 -> v1 - v2
  | CreatedVar v1, CreatedVar v2 -> Ciltools.compare_var v1 v2
  | _ -> Pervasives.compare v1 v2

let rec compare_lval (h1, o1) (h2, o2) =
  let ch = compare_host h1 h2 in
  if (ch == 0) then
    Ciltools.compare_offset o1 o2
  else
    ch

and compare_host h1 h2 =
  match h1, h2 with 
    CVar v1, CVar v2 -> compare_var v1 v2

  | CMem e1, CMem e2 ->
      compare_exp e1 e2

  | AbsHost a1, AbsHost a2 ->
      Alias.Abs.compare a1 a2

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



(** Compare lvals in a way where the variable attributes matter *)
let rec compare_lval_attr (h1, o1) (h2, o2) =
  let ch = compare_host_attr h1 h2 in
  if (ch == 0) then
    Ciltools.compare_offset o1 o2
  else
    ch

and compare_host_attr h1 h2 =
  match h1, h2 with 
    CVar v1, CVar v2 ->
      let v1 = var_of_abs v1 in
      let v2 = var_of_abs v2 in
      Ciltools.compare_var_attr v1 v2

  | CMem e1, CMem e2 ->
      compare_exp_attr e1 e2

  | AbsHost a1, AbsHost a2 ->
      Alias.Abs.compare a1 a2

  | _ -> 
      compare h1 h2

and compare_exp_attr x y =
  match x, y with
    CLval l1, CLval l2 
  | CAddrOf l1, CAddrOf l2
  | CStartOf l1, CStartOf l2 ->
      compare_lval_attr l1 l2
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
      compare_exp_attr e1 e2 
  | CUnOp(o1,e1,t1), CUnOp(o2,e2,t2) ->
      let o_comp = compare o1 o2 in
      if (o_comp <> 0) then 
        o_comp
      else
        let e_comp = compare_exp_attr e1 e2 in
        if( e_comp <> 0) then
          e_comp
        else
          Ciltools.compare_type t1 t2

  | CBinOp(o1,l1,r1,t1), CBinOp(o2,l2,r2,t2) ->
      let o_comp = compare o1 o2 in
      if (o_comp <> 0) then
        o_comp
      else
        let l_comp = compare_exp_attr l1 l2 in
        if( l_comp <> 0) then
          l_comp
        else
          let r_comp = compare_exp_attr r1 r2 in
          if ( r_comp<> 0) then 
            r_comp
          else
            Ciltools.compare_type t1 t2

  | CCastE(t1,e1), CCastE(t2,e2) ->
      let t_comp = Ciltools.compare_type t1 t2 in
      if (t_comp <> 0) then 
        t_comp
      else 
        compare_exp_attr e1 e2 

  | _ -> compare x y



(*************** "New" hash functions ***************)
  
let rec hash_lval (h, o) =
  let hh = hash_host h in
  hh lxor (Ciltools.hash_offset o)

and hash_host h =
  match h with 
    CVar v -> Hashtbl.hash v
  | CMem e ->
      hash_exp e
  | AbsHost a ->
      Alias.Abs.hash a
 
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

(** Hash lvals in a way where attributes matter *)
let rec hash_lval_attr (h, o) =
  let hh = hash_host_attr h in
  hh lxor (Ciltools.hash_offset o)

and hash_host_attr h =
  match h with 
    CVar v -> Ciltools.hash_var_attr (var_of_abs v)
  | CMem e ->
      hash_exp_attr e
  | AbsHost a ->
      Alias.Abs.hash a

and hash_exp_attr x =
  match x with
    CLval l ->
      70039403 lxor hash_lval_attr l  
  | CAddrOf l ->
      9083249 lxor hash_lval_attr l
  | CStartOf l ->
      1243789 lxor hash_lval_attr l
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
       0x11237639 lxor (hash_exp_attr e)
  | CAlignOfE(e1)  -> 
      0x19857434 lxor (hash_exp_attr e1)
        
  | CUnOp(o1,e1,t1) ->
      let o_hash = Hashtbl.hash o1 in
      let e_hash = hash_exp_attr e1 in
      let t_hash = Ciltools.hash_type t1 in
      o_hash lxor e_hash lxor t_hash
        
  | CBinOp(o1,l1,r1,t1) ->
      let o_hash = Hashtbl.hash o1 in
      let l_hash = hash_exp_attr l1 in
      let r_hash = hash_exp_attr r1 in
      let t_hash = Ciltools.hash_type t1 in
      o_hash lxor l_hash lxor r_hash lxor t_hash
        
  | CCastE(t1,e1) ->
      let t_hash = Ciltools.hash_type t1 in
      let e_hash = hash_exp_attr e1 in
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

module HashedLvalAttr = struct
  type t = aLval
  let equal a b = compare_lval_attr a b == 0
  let hash x = hash_lval_attr x
end

module LvHash = Weak.Make(HashedLvalAttr)

let goldenLvals = LvHash.create 173

(** Try to reduce the memory footprint of lvals by removing "unneeded" 
    parts of the CIL AST / sharing equivalent parts *)
let rec distillLval lv =
  let host, off = lv in
  let newOff = Cil_lvals.distillOff off in
  match host with
    CVar _  | AbsHost _ -> 
      if off == newOff
      then lv
      else (host, newOff)  
  | CMem(exp)->
      let newE = distillExp exp in
      if off == newOff && exp == newE
      then lv
      else (CMem newE, newOff)
          
and distillExp e =
  match e with 
    CLval (l) ->
      let newLv = mergeLv l in
      if l == newLv 
      then e
      else CLval newLv

  | CAddrOf(l) ->
      let newLv = mergeLv l in
      if l == newLv 
      then e
      else CAddrOf newLv

  | CStartOf(l) -> 
      let newLv = mergeLv l in
      if l == newLv 
      then e
      else CStartOf newLv

  | CConst (CEnum (ce, name, ei)) ->
      Cil_lvals.distillEnuminfo ei;
      let newCE = Cil_lvals.distillExp ce in
      if ce == newCE 
      then e
      else CConst (CEnum (newCE, name, ei))

  | CSizeOfE (exp) ->
      let newE = distillExp exp in
      if exp == newE
      then e
      else CSizeOfE newE

  | CAlignOfE (exp) ->
      let newE = distillExp exp in
      if exp == newE
      then e
      else CAlignOfE newE

  | CConst _
  | CSizeOfStr _ -> e

  | CSizeOf (t) ->
      let newT = Cil_lvals.mergeType t in
      if t == newT 
      then e
      else CSizeOf newT

  | CAlignOf (t) -> 
      let newT = Cil_lvals.mergeType t in
      if t == newT 
      then e
      else CAlignOf newT

  | CCastE (t, exp) ->
      let newE = distillExp exp in
      let newT = Cil_lvals.mergeType t in
      if t == newT && exp == newE 
      then e
      else CCastE (newT, newE)

  | CUnOp(op, exp, t) ->
      let newE = distillExp exp in
      let newT = Cil_lvals.mergeType t in
      if t == newT && exp == newE 
      then e
      else CUnOp(op, newE, newT)

  | CBinOp(op, e1, e2, t) ->
      let newE1 = distillExp e1 in
      let newE2 = distillExp e2 in
      let newT = Cil_lvals.mergeType t in
      if t == newT && e1 == newE1 && e2 == newE2
      then e
      else CBinOp(op, newE1, newE2, newT)


and mergeLv lv = 
  try 
    LvHash.find goldenLvals lv
  with Not_found ->
    let newLv = distillLval lv in
    LvHash.add goldenLvals newLv;
    newLv

let printHashStats () =
  let hashStats = Stdutil.string_of_hashstats LvHash.stats goldenLvals 
    "Golden lvals" in
  logStatus hashStats

(************ "New" type functions **************)


let rec typeOfUnsafe e : typ = 
  match e with
    CLval lv -> typeOfLvalUnsafe lv
  | CAddrOf (lv) -> TPtr (typeOfLvalUnsafe lv, [])
  | CStartOf (lv) ->
      (match Cil_lvals.unrollTypeNoAttrs (typeOfLvalUnsafe lv) with
         TArray (t,_, _) -> TPtr(t, [])
       | _ -> 
           Errormsg.s (Errormsg.bug "typeOfUnsafe: StartOf on a non-array")
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
    CVar vid, off -> 
      Cil_lvals.typeOffsetUnsafe (var_of_abs vid).vtype off
  | CMem addr, off ->
      (match Cil_lvals.unrollTypeNoAttrs (typeOfUnsafe addr) with
         TPtr (t, _) -> 
           Cil_lvals.unrollTypeNoAttrs (Cil_lvals.typeOffsetUnsafe t off)
       | t ->
           (match off with
              NoOffset ->
               TVoid []
            | _ -> 
                Cil_lvals.typeOffsetUnsafe t off 
          )
      )
  | AbsHost h, off ->
      TVoid [] (* Hack to make offset attachment allowed *)
        

let typeAfterDeref exp =
  match Cil_lvals.unrollTypeNoAttrs (typeOfUnsafe exp) with
    TPtr (t,_) ->
      Cil_lvals.unrollTypeNoAttrs t
  | TInt (_,x) -> TVoid x
  | TVoid x -> TVoid x (* Hack to make offset attachment allowed *)
  | _ ->
      raise Cil_lvals.TypeNotFound


let attachOffset host offset =
  let hdOff, tlOff = Cil.removeOffset offset in
  let hostTyp = typeOfLvalUnsafe (host, hdOff) in
  (* Only check if the "last" offset matches *)
  let canAttach, counter = Cil_lvals.canAttachOffset hostTyp tlOff in
  if canAttach then (host, offset)
  else raise (Cil_lvals.OffsetMismatch counter)


let mkMemChecked ptrExp offset =
  try
    let hostTyp = typeAfterDeref ptrExp in
    let canAttach, counter = Cil_lvals.canAttachOffset hostTyp offset in
    if canAttach then mkMem ptrExp offset
    else raise (Cil_lvals.OffsetMismatch counter)
  with 
    Errormsg.Error 
  | Cil_lvals.TypeNotFound ->
      raise (Cil_lvals.OffsetMismatch None)


(*************** "New" Formal -> Actual Substitution ***************)

(** Find instances of the formal within given lval, and substitute w/ 
    the given actual lval. 
    Assumes we are looking for inter-procedural effects *)
let rec substActForm (actual:aExp) (lvalWithFormal:aLval) : aLval =
  match lvalWithFormal with 
    (CVar(vid), formOff) ->
      (* NO, don't substitute! A straight-up var as a formal is always a
         different memory cell from a straight-up var in the other scope! *)
      raise Cil_lvals.SubstInvalidArg

  | (CMem (CLval (CVar(vid), NoOffset)), outerOff) -> begin
      (* Assume any variable encountered is the formal... To fix this,
         should actually add a var attribute to compare against *)
      try
        mkMemChecked actual outerOff
      with 
        Cil_lvals.OffsetMismatch _
      | Errormsg.Error 
      | Failure _ ->
          raise Cil_lvals.SubstInvalidArg
    end
  | (CMem(ptrExp), outerOff) ->
      (try
         let newExp = substActFormExp actual ptrExp in
         mkMemChecked newExp outerOff
       with 
         Cil_lvals.OffsetMismatch _
       | Errormsg.Error 
       | Failure _ ->
           raise Cil_lvals.SubstInvalidArg)

  | AbsHost h, outerOff ->
      logError "substActForm: tried to subs into abs host\n";
      raise Cil_lvals.SubstInvalidArg
             
        
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
      raise Cil_lvals.SubstInvalidArg


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
  let newOff, _ = Cil_lvals.canonicizeOff off in
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
      let newLv = host, Cil_lvals.simplifyOff off in
      let curTyp = typeOfLvalUnsafe newLv in
      (newLv, [(curTyp, newLv)], false)

  | (AbsHost _ as host, off) ->
      (* Base case *)
      let newLv = host, Cil_lvals.simplifyOff off in
      (newLv, [], false)

  | (CMem (ptrExp), off) ->
      let simplifiedPtr, seenTypes, changed = doSimplifyExp ptrExp in
      let curTyp = typeOfLvalUnsafe lv in
      (try 
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
           let newOff = Cil_lvals.simplifyOff off in
           let newLv = (mkMem simplifiedPtr newOff) in
           (newLv, (curTyp, newLv) :: seenTypes, changed))


and doSimplifyExp (exp:aExp) : aExp * (Cil.typ * aLval) list * bool =
  match exp with
    CLval lv ->
      let simplerLv, seenTypes, changed = doSimplifyLv lv in
      (CLval (simplerLv), seenTypes, changed)
  | CAddrOf lv ->
      let simplerLv, seenTypes, changed = doSimplifyLv lv in
      (CAddrOf (simplerLv), seenTypes, changed)
  | CStartOf lv ->
      let simplerLv, seenTypes, changed = doSimplifyLv lv in
      (CStartOf (simplerLv), seenTypes, changed)

  | CCastE (t, e) ->
      (* strip the cast ? *)
      let simplerExp, seenTypes, changed = doSimplifyExp e in
      (simplerExp, seenTypes, changed)

  | CUnOp (op, e, t) ->
      let simplerExp, seenTypes, changed = doSimplifyExp e in
      (CUnOp (op, simplerExp, t), seenTypes, changed)
      

  | CBinOp (op, e1, e2, t) ->
      (match op with
         PlusPI | IndexPI | MinusPI | PlusA ->
           (* drop the offset expression... *)
           let simplerExp, seenTypes, changed = doSimplifyExp e1 in
           (simplerExp, seenTypes, changed)
       | _ ->
           let e1', seenTypes1, changed1 = doSimplifyExp e1 in
           let e2', seenTypes2, changed2 = doSimplifyExp e2 in
           let seenTypes = List_utils.unionEq 
             (fun (t1, l1) (t2, l2) ->
                Ciltools.equal_type t1 t2 && compare_lval l1 l2 == 0)
             seenTypes1 seenTypes2 in
           (CBinOp (op, e1', e2', t), seenTypes, changed1 || changed2)
      )
        
  | CConst _  | CSizeOf _ | CSizeOfE _  | CSizeOfStr _ 
  | CAlignOf _ | CAlignOfE _  ->
      (exp, [], false)

(** May eliminate redundant parts of lval access path. Returns the 
    simplified lval and a flag indicating whether a change is made *)
let simplifyLval (lv:aLval) : aLval * bool =
  let simplerLv, _, changed = doSimplifyLv lv in
  (simplerLv, changed)

let simplifyExp (exp:aExp) : aExp * bool =
  let simplerExp, _, changed = doSimplifyExp exp in
  (simplerExp, changed)

(******************* High-level compare ********************)


(** A measure of imprecision, when checking equality of lvals
    that may involve querying a pointer analysis *)
type imprecision =
    Syntactic                (* Syntactically equal (skip checking context) *)
  | PtsToHostL of int * int  (* LHS pts to RHS *)
  | PtsToHostR of int * int  (* RHS pts to LHS *) 
  | PtsToSame of int * int   (* Ptr alias -- Size of points to set *)
  | RepsSame of int * int    (* Location alias -- Size of label set *)
  | SameType                 (* no AA info, but same type *)
      (* TODO: have to return a common concrete target for the Pts-to stuff 
         (and hopefully that target isn't a function, or something stupid) *)

let string_of_imp imp =
  (match imp with
     Syntactic -> "Syntactic"
   | PtsToHostL (a, b) 
   | PtsToHostR (a, b) -> 
       "used PTA nodes ptSet(" ^ 
         (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")"
   | PtsToSame (a, b) -> 
       "used PTA nodes ptSet(" ^ 
         (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")"
   | RepsSame (a, b) -> 
       "used PTA nodes lblSet(" ^ 
         (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")"
   | SameType -> "Same type")


let compareImpSets (x, y) (x', y') =
  (* smaller is less imprecise *)
  let c = x - x' in
  if c <> 0 then c
  else y - y'
    

(** Rank levels of imprecision. 
    @return negative integer if [i1] is less imprecise than [i2], etc. *)
let compare_imprec i1 i2 =
  match i1, i2 with
  | Syntactic, Syntactic -> 0
  | Syntactic, _ -> -1
  | _, Syntactic -> 1
  | PtsToHostL (x, y), PtsToHostL (x', y') ->
      compareImpSets (x, y) (x', y')
  | PtsToHostL _, _ -> -1
  | _, PtsToHostL _ -> 1
  | PtsToHostR (x, y), PtsToHostR (x', y') ->
      compareImpSets (x, y) (x', y')
  | PtsToHostR _, _ -> -1
  | _, PtsToHostR _ -> 1
  | PtsToSame (x, y), PtsToSame (x', y') ->
      compareImpSets (x, y) (x', y')
  | PtsToSame _, _ -> -1
  | _, PtsToSame _ -> 1
  | RepsSame (x, y), RepsSame (x', y') ->
      compareImpSets (x, y) (x', y')
  | RepsSame _, _ -> -1
  | _, RepsSame _ -> 1
  | SameType, SameType -> 0


let sameLocations n1 n2 =
  if Alias.Abs.location_alias n1 n2 then begin
    let s1 = Alias.Abs.label_size n1 in
    let s2 = Alias.Abs.label_size n2 in
    Some (RepsSame (s1, s2))
  end
  else None
    
let samePtrs p1 p2 =
  if Alias.Abs.may_alias p1 p2 then begin
    let s1 = Alias.Abs.pts_size p1 in
    let s2 = Alias.Abs.pts_size p2 in
    Some (PtsToSame (s1, s2))
  end
  else None

let lPointsTo p t =
  if Alias.Abs.points_to p t then begin
    let s1 = Alias.Abs.pts_size p in
    let s2 = Alias.Abs.label_size t in
    Some (PtsToHostL (s1, s2)) 
  end
  else None

let rPointsTo t p =
  if Alias.Abs.points_to p t then begin
    let s1 = Alias.Abs.label_size t in
    let s2 = Alias.Abs.pts_size p in
    Some (PtsToHostR (s1, s2)) 
  end
  else None


(** Returns Some(imprec) if lv1 may be the same lval as lv2... 
    where imprec is a measure of imprecision... else returns None *)

let rec sameHost h1 h2 =
  (try match h1, h2 with
     CVar v1, CVar v2 -> 
       if compare_var v1 v2 == 0 
       then Some (Syntactic)
       else None

   | CMem ptr1, CMem ptr2 ->
       let node1 = node_of_absExp ptr1 in
       let node2 = node_of_absExp ptr2 in
       samePtrs node1 node2 
         
   | AbsHost a1, AbsHost a2 ->
       sameLocations a1 a2
         
   | AbsHost a, CVar vid ->
       let vNode = Alias.Abs.getNodeVar (var_of_abs vid) in
       sameLocations a vNode

   | CVar vid, AbsHost a ->
       let vNode = Alias.Abs.getNodeVar (var_of_abs vid) in
       sameLocations vNode a

   | AbsHost a, CMem ptr ->
       let ptNode = node_of_absExp ptr in
       rPointsTo ptNode a 

   | CMem ptr, AbsHost a ->
       let ptNode = node_of_absExp ptr in
       lPointsTo ptNode a 
         
   | CMem ptr, CVar vid ->
       let ptrNode = node_of_absExp ptr in
       let vNode = Alias.Abs.getNodeVar (var_of_abs vid) in
       lPointsTo ptrNode vNode

   | CVar vid, CMem ptr ->
       let ptrNode = node_of_absExp ptr in
       let vNode = Alias.Abs.getNodeVar (var_of_abs vid) in
       rPointsTo ptrNode vNode

   with 
     Alias.UnknownLoc ->
       logError "sameHost UnknownLoc\n";
       tryType h1 h2
   | Not_found ->
       logError "sameHost Not_found\n";
       tryType h1 h2
  )


and tryType h1 h2 =
  (* couldn't resolve aliases; try an approximation! *)
  try
    let t1 = typeOf h1 in
    let t2 = typeOf h2 in
    typeBasedCheck t1 t2
  with
    Errormsg.Error
  | Cil_lvals.TypeNotFound ->
      None
        
and typeBasedCheck typ1 typ2 =
  if (Ciltools.equal_type typ1 typ2) then Some (SameType)
  else None

and typeOf = function
    CVar v -> (var_of_abs v).vtype
  | CMem e -> typeAfterDeref e
  | AbsHost _ -> TVoid []
      
(* non-syntactic match or uses Abs alias nodes *)
let sameLvalAbs lv1 lv2 =
  let t1 = typeOfLvalUnsafe lv1 in
  let t2 = typeOfLvalUnsafe lv2 in
  if (isVoidType t1 || isVoidType t2 || 
        Ciltools.equal_type t1 t2 ) then
    let (h1, o1) = lv1 in
    let (h2, o2) = lv2 in
    if (Ciltools.compare_offset o1 o2 == 0) then         
      (* ignores case when ptr points to some offset *)
      sameHost h1 h2 
    else
      None
  else
    None

let sameLval lv1 lv2 : imprecision option =
(*  match isAbsLval lv1, isAbsLval lv2 with *)
  match isAbsVar lv1, isAbsVar lv2 with
    Some _, _ 
  | None, Some _ ->
      sameLvalAbs lv1 lv2
(*
  | Some _, None ->
      let h1, o1  = lv1 in
      if o1 = NoOffset then
        let h2, _ = lv2 in
        sameHost h1 h2
      else 
        sameLvalAbs lv1 lv2
  | None, Some _ -> 
      let h2, o2  = lv2 in
      if o2 = NoOffset then
        let h1, _ = lv1 in
        sameHost h1 h2
      else 
        sameLvalAbs lv1 lv2
*)
  | None, None ->
      if compare_lval lv1 lv2 == 0 then Some (Syntactic)
      else sameLvalAbs lv1 lv2


(**************** Scope Support **************)

(** Scope reading / manipulation for abstract lvals *)
module AL = 
struct

  let scopeVar op vi =
    op vi

  let rec scopeExp op e =
    match e with
      CLval (h,_) 
    | CAddrOf (h,_)
    | CStartOf (h,_) ->
        scopeHost op h
    | CSizeOfE e1
    | CAlignOfE e1
    | CUnOp (_,e1,_) 
    | CCastE (_,e1) -> 
        scopeExp op e1
          
    | CBinOp (_, e1, e2, _) ->
        scopeExp op e2;
        scopeExp op e1
          (* Do e1 last, so that readScope will use the left-most variable  *)

    | CConst _
    | CSizeOf _
    | CSizeOfStr _
    | CAlignOf _ -> ()
        
  and scopeLval op ((h,_):aLval) =
    scopeHost op h
      
  and scopeHost op h =
    match h with
      CVar(vid) ->
        scopeVar op (var_of_abs vid)
    | CMem(ptrExp) ->
        scopeExp op ptrExp
          
    | AbsHost _ ->
        raise IsAbstract

end

(** Find & decipher the scope annotation from within an lval *)
let getScope lv =
  let ret = ref STBD in
  try AL.scopeLval (readScope ret) lv; !ret
  with IsAbstract -> SGlobal

let getScopeParanoid curFunc lv =
  let ret = ref STBD in
  try AL.scopeLval (paranoidReadScope curFunc ret) lv; !ret
  with IsAbstract -> SGlobal


(************* Printing support **********)


(** Convert lval to a string describing it + the scope 
    (wrt to the current function).  *)
let string_of_lvscope (lv:aLval) : string =
  (* Also get the var id for debugging... *)
  let vidStr = 
    try
      let vi = findBaseVarinfoLval lv in
      ":" ^ (string_of_int vi.vid)
    with Cil_lvals.BaseVINotFound -> ""
  in
  try
    let scope = getScope lv in
    (string_of_lval lv) ^ (Scope.string_of_scope scope) ^ vidStr
  with Scope.BadScope ->
    (string_of_lval lv) ^ vidStr
      
(************* Misc... ***********)

let makeFormalWithName name index =
  let baseVar = Cil_lvals.mkVarinfo false name (TVoid []) in
  setScope (SFormal index) baseVar;
  (hostOfVar baseVar, NoOffset)
    

(** Helper function to generate a dummy lval w/ the
    scope of a formal var *)
let makeFormalVar index =
  let name = "$F" ^ (string_of_int index) in
  makeFormalWithName name
