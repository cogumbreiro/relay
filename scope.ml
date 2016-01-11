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


let string_of_scope = function
    SGlobal -> "#g"
  | SFormal n -> ("#f" ^ (string_of_int n))
  | SFunc -> "#func"
  | STBD -> "#tbd"

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
    if (Cil.isFunctionType vi.vtype) then SFunc
    else SGlobal
  end
  else match CLv.getIndex curFunc.sformals vi with
    Some (i) -> SFormal i
  | None -> STBD


let badScopeDebug vi =
  L.logError ("BadScope: " ^ vi.vname ^ ":" ^ string_of_int vi.vid ^ 
                "   loc: " ^ (Cildump.string_of_loc !currentLoc)) 
    
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
        if i == globalNum then SGlobal
        else if i == funcNum then SFunc
        else if i == localNum then STBD
        else if (i >= 0) then SFormal i
        else failwith ("unexpected scope attr: " ^ (string_of_int i))
    | _ ->
        L.logError "attArgs not a [Cil.AInt]";
(*        badScopeDebug vi; *)
        raise BadScope
(*        STBD *)
  with Not_found ->
(*    badScopeDebug vi; *)
    raise BadScope
(*    STBD *)


(** Read the scope from the variable and store the result in ret *)
let readScope ret vi =
  ret := decipherScope vi


(* Warn about scope different... may be different for the 
   locking functions because we don't search for the fundec of
   the locking function when we read in the user-supplied list
   and get the actual formal. We only make up a similar formal *)
let warnScope vi decided =
  badScopeDebug vi;
  L.logError ("Decided: " ^ string_of_scope decided)
  
let paranoidReadScope curFun ret vi =
  let read = decipherScope vi in
  let decided = decideScopeVar curFun vi in
  if read <> decided then
    warnScope vi decided
  ;
  ret := read

(**************** Prepass to annotate scope ********)

(** Must run before the varinfo indexer runs (@see id_fixer.ml) *)

(** A visitor that annotates each varinfo with a scope 
    (if declared in a function, relative to that function). *)
class scopeVisitor = object (self)
  inherit nopCilVisitor 

  val mutable curFunc = Cil.dummyFunDec

  method vfunc finfo =
    L.logStatusF "Scoping function: %s\n" finfo.svar.vname;
    L.flushStatus ();
    curFunc <- finfo;
    DoChildren

  method handleVI (vi:varinfo) =
    try
      let oldScope = decipherScope vi in
      let newScope = decideScopeVar curFunc vi in
      assert (oldScope = newScope)  
    with BadScope ->
      let newScope = decideScopeVar curFunc vi in
      setScope newScope vi
    
  method vvdec (vi:varinfo) =
    self#handleVI vi;
    DoChildren

  method vvrbl (vi:varinfo) =
    self#handleVI vi;
    DoChildren

end

let doAnnotateScope (root : string) =
  L.logStatus "Annotating scopes once and for all.\n";
  L.flushStatus ();
  let vis = new scopeVisitor in
  Filetools.walkDir 
    (fun ast file ->
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       (* Write back the result! *)
       Cil.saveBinaryFile ast file;
    ) root;
  L.logStatus "Scope annotations complete!\n";
  L.flushStatus ()

