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

(** Utilities to annotate variables w/ their scope 
    (only wrt to the current function or the function summary examined!) *)

open Cil
open Lvals
module CLv = Cil_lvals

module L = Logging

(***************************************************
 * Tracking scope
 ***************************************************)

type scope = 
    SFormal of int (* expression is derived from Nth formal *)
  | SGlobal        (* expression is derived from a global *)
  | SFunc          (* expression is a function *)
  | STBD           (* scope of expression needs to be determined *)


let string_of_scope = function
    SGlobal -> "#g"
  | SFormal n -> ("#f" ^ (string_of_int n))
  | SFunc -> "#func"
  | STBD -> "#tbd"


(*********************************************************
 * Annotate vars w/ scope, as opposed to just pairing an
 * an lval (which only has one var) with its scope. This
 * is done to accomodate expressions
 *********************************************************)

let scopeString = "#s"

(* Use an index > 0 to indicate a formal variable of that index. 
   Use special nums < 0 for other scopes *)

let globalNum = -1

let localNum = -2

let funcNum = -3

exception BadScope

type 'a scopeOp  = 'a -> unit

(********** Operations on scope attr of vars **************)


(** Remove the scope attribute from a variable *)
let removeScope vi =
  vi.vattr <- Cil.dropAttribute scopeString vi.vattr

(** Set the scope annotation for the variable *)
let setScope scope vi =
  let scopeAttr = Attr (scopeString,
                        match scope with
                          SGlobal -> [AInt globalNum]
                        | SFunc -> [AInt funcNum]
                        | SFormal i -> [AInt i]
                        | STBD -> [AInt localNum] 
                            (* Can probably just leave it off *)
                       ) in
  removeScope vi;
  vi.vattr <- Cil.addAttribute scopeAttr vi.vattr


(** Get the actual scope (skip annotation) *)
let decideScopeVar curFunc vi : scope =
  if (vi.vglob) then begin
    if (Cil.isFunctionType vi.vtype) then
      SFunc
    else
      SGlobal
  end
  else match CLv.getIndex curFunc.sformals vi with
    Some (i) ->
      SFormal i
  | None ->
      STBD


(** Annotate variable w/ scope. This is needed to remember the variable's
    scope AFTER the curFunc is analyzed and we've already moved on to
    the next function! *)
let addScope (curFunc:Cil.fundec) ret vi =
  let scope = decideScopeVar curFunc vi in
  setScope scope vi;
  ret := scope

    
(** Decipher the scope annotation that is attached to the var.
    Be careful about alias analysis representative nodes, 
    which do not have varinfos in the first place. *)
let decipherScope vi : scope =
  try 
    let Cil.Attr (_, attArgs) = 
      List.find 
        (fun (Cil.Attr (attName, attArgs)) ->
           attName = scopeString) vi.vattr in
    match attArgs with
      [Cil.AInt i] ->
        if (i == globalNum) then SGlobal
        else if (i >= 0) then SFormal i
        else STBD
    | _ ->
        L.logError "attArgs not a [Cil.AInt]";
        raise BadScope
  with Not_found ->
    L.logError ("decipherScope vi=" ^ vi.vname ^ ": Not_found");
    raise BadScope


(** Read the scope from the variable and store the result in ret *)
let readScope ret vi =
  ret := decipherScope vi



(**************** Search and apply op **************)

(** raised when scoping is applied to an lval with an abstract host *)
exception IsAbstract

(** Scoping for abstract lvals *)
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
      CVar(vi) ->
        scopeVar op vi
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

(** Remove scope annotations from the lval *)
let killScope lv =
  try AL.scopeLval removeScope lv
  with IsAbstract -> ()

(** Annotate the lval w/ a scope, based on the curFun. 
    Return the scope for the left-most variable as well *)
let annotScope (curFun:Cil.fundec) (lv: aLval) : scope =
  let ret = ref STBD in
  try AL.scopeLval (addScope curFun ret) lv;  !ret
  with IsAbstract -> SGlobal


(** Wrapper class that checks/manipulates scope annotations 
    (if you wanna do mixin-type things) *)
class scoper = object

  method getScope = getScope

  method killScope = killScope

  method addScope = addScope

end
