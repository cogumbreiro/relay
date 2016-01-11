
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


(** Interface for configuring and querying different global alias analyses *)


open Fstructs
open Cil
open Alias_types

module CilPTA = Myptranal
module Steens = Pta_fi_eq

module D = Cildump
module FC = Filecache
module L = Logging

(***** Structs for CIL PTA *****)

let aliasCache = Hashtbl.create 10 (* alias file-info cache *)


(***** Structs for loading *****)

let rootPath = ref ""

let setRoot path = 
  rootPath := path


(***********************************************************)
(* Wrappers to different analysis engines                  *)


class virtual aliasAnalyzer = object (self)

  (** short string describing the alias analysis *)
  method virtual identity : string

  (* Interface for Cil values *)

  method virtual deref_funptr : Cil.exp -> (fKey list)

  method virtual may_alias : Cil.exp -> Cil.exp -> bool

  method virtual points_to : Cil.exp -> Cil.varinfo -> bool

  method virtual deref_exp : Cil.exp -> Cil.varinfo list

  (* Getting abstract nodes from Cil values *)

  method virtual deref_exp_abs : Cil.exp -> ptaNode list

  method virtual deref_lval_abs : Cil.lval -> ptaNode list

  method virtual getNodeLval : Cil.lval -> ptaNode

  (* Operations on abstract nodes (e.g., convert back) *)
  method virtual represents : ptaNode -> Cil.lval list

  method virtual may_alias_abs : ptaNode -> ptaNode -> bool

  method virtual deref_abs : ptaNode -> ptaNode

  method virtual compare : ptaNode -> ptaNode -> int

  method virtual hash : ptaNode -> int

  method virtual string_of : ptaNode -> string

  method virtual size_of : ptaNode -> int

  method virtual reachableFrom : ptaNode -> ptaNode list -> bool

  method virtual reachableFromG : ptaNode -> bool

end



class dummyAnalyzer = object 
  inherit aliasAnalyzer

  method identity = "dummy"

  val error_msg = "using dummy alias analyzer\n"

  method deref_funptr fexp =
    failwith error_msg 

  method points_to e v =
    failwith error_msg 

  method may_alias e1 e2 =
    failwith error_msg 

  method deref_exp e =
    failwith error_msg 

  method deref_exp_abs e =
    failwith error_msg 

  method deref_lval_abs lv =
    failwith error_msg

  method getNodeLval lv =
    failwith error_msg

  method represents abs =
    failwith error_msg

  method may_alias_abs abs1 abs2 =
    failwith error_msg

  method deref_abs abs =
    failwith error_msg

  method compare abs1 abs2 =
    failwith error_msg

  method hash abs =
    failwith error_msg

  method string_of abs =
    failwith error_msg

  method size_of abs =
    failwith error_msg

  method reachableFrom abs srcs =
    failwith error_msg

  method reachableFromG abs =
    failwith error_msg

end


(********* CIL Analysis interface *********) 

exception UnknownLoc

class cilAnalyzer = object
  inherit dummyAnalyzer

  method identity = "cil"

  method deref_funptr fexp =
    (* Should also match ftypes, since the PTA merges fields *)
    let ftype = Cil.typeOf fexp in
    let ftype_str = D.string_of_ftype ftype in    
    let fdecs = 
      try
        CilPTA.resolve_funptr fexp
      with 
        Not_found
      | CilPTA.UnknownLocation ->
          []
    in
    List.fold_left 
      (fun curList fdec ->
         let ft = fdec.svar.vtype in
         let fts = D.string_of_ftype ft in
         if (fts = ftype_str) then
           fdec.svar.vid :: curList
         else
           curList
      ) [] fdecs

  method deref_exp e =
    try
      CilPTA.resolve_exp e
    with 
      Not_found
    | CilPTA.UnknownLocation ->
        []

  method points_to e v =
    try
      let results = CilPTA.resolve_exp e in
      List.exists 
        (fun otherV ->
           (Ciltools.compare_var v otherV) == 0
        ) results
    with 
      Not_found 
    | CilPTA.UnknownLocation ->
        raise UnknownLoc
               
  method may_alias e1 e2 =
    try
      CilPTA.may_alias e1 e2
    with 
      Not_found 
    | CilPTA.UnknownLocation ->
        raise UnknownLoc

  (* TODO: figure out how to get an abstract rep for CIL nodes *)

end


(********* Steensgaard Analysis interface *********) 



class steensAnalyzer = object (self)
  inherit aliasAnalyzer

  initializer
    L.logStatus "Initializing Steensgaard AA info";
    Steens.analyzeAll !rootPath false

  method identity = "steens"

  method deref_funptr fexp =
    Steens.resolve_funptr fexp

  method deref_exp e =
    try
      Steens.deref_exp e
    with Not_found ->
      L.logError "0 targets from deref_exp";
      []

  method may_alias e1 e2 =
    Steens.may_alias e1 e2

  method points_to e v =
    Steens.points_to e v

  method deref_exp_abs e =
    let nodes = Steens.Abs.deref_exp e in
    List.map (fun n -> SteensNode n) nodes

  method deref_lval_abs lv =
    let nodes = Steens.Abs.deref_lval lv in
    List.map (fun n -> SteensNode n) nodes

  method getNodeLval lv =
    match Steens.Abs.getNodeLval lv with 
      None -> raise UnknownLoc
    | Some n -> SteensNode n 

  method represents abs =
    match abs with
      SteensNode n ->
        Steens.Abs.lvals_of n
    | _ -> raise NodeTypeMismatch


  method may_alias_abs abs1 abs2 =
    match abs1, abs2 with
      SteensNode n1, SteensNode n2 ->
        Steens.Abs.may_alias n1 n2
    | _ -> raise NodeTypeMismatch

  method deref_abs abs =
    match abs with
      SteensNode n ->
        SteensNode (Steens.Abs.deref n)
    | _ -> raise NodeTypeMismatch

  method compare abs1 abs2 =
    match abs1, abs2 with
      SteensNode n1, SteensNode n2 ->
        Steens.Abs.compare n1 n2
    | _ -> raise NodeTypeMismatch

  method hash abs =
    match abs with 
      SteensNode n ->
        Steens.Abs.hash n
    | _ -> raise NodeTypeMismatch


  method string_of abs =
    match abs with 
      SteensNode n -> 
        Steens.Abs.string_of n
    | _ -> raise NodeTypeMismatch


  method size_of abs =
    match abs with 
      SteensNode n -> 
        Steens.Abs.size_of n
    | _ -> raise NodeTypeMismatch

  method reachableFrom abs srcs =
    match abs with 
      SteensNode n ->
        let ss = List.map (fun x -> match x with 
                               SteensNode m -> m
                             | _ -> raise NodeTypeMismatch) srcs in
        Steens.Abs.reachableFrom n ss
    | _ -> raise NodeTypeMismatch

  method reachableFromG abs =
    match abs with 
      SteensNode n ->
        Steens.Abs.reachableFromG n 
    | _ -> raise NodeTypeMismatch

end



(***** singleton analyzers *****)

let dummyAA = new dummyAnalyzer

let cilAA = ref dummyAA (* lazily create *)

let steensAA = ref dummyAA (* lazily create *)

let getCilAA () =
  if (!cilAA == dummyAA) then
    cilAA := new cilAnalyzer
  ;
  !cilAA

let getFiCiEqAA () =
  if (!steensAA == dummyAA) then
    steensAA := new steensAnalyzer
  ;
  !steensAA


(***** the particular analyzer to use for each query *****)

(* TODO: allow separate analyses for different phases, not just query-types *)


let fpAA = ref dummyAA

let expAA = ref dummyAA

let lvalAA = ref dummyAA

let mayAA = ref dummyAA


    
(****************************************************
 * Initializing / Updating CIL PTA info
 ****************************************************)

let setCurrentFile (f:Cil.file) : unit =
  (* TODO: cache PTA state?? *)
  if !cilAA != dummyAA then (* Only do this if it's using the cilAA *)
    if (not (Hashtbl.mem aliasCache f.fileName)) then
      begin
        CilPTA.reset_globals ();
        CilPTA.analyze_file f;
        CilPTA.compute_results false;
        Hashtbl.clear aliasCache;
        Hashtbl.add aliasCache f.fileName true;
      end



(***********************************************************)
(* Set up                                                  *)




let ws = "[ \r\n\t]*"

(* Top level split, between the field name and value  *)
let topSplitter = Str.split_delim (Str.regexp (ws ^ "[:]" ^ ws))

let groupEnd = Str.regexp (ws ^ "[}]" ^ ws)

let assignAnalysis aspect analysis =
  let analysis = String.lowercase analysis in
  let aspect = String.lowercase aspect in
  let chosen = 
    match analysis with
      "cil" -> getCilAA ()
    | "fi_ci_eq" -> getFiCiEqAA ()
    | _ ->
        (L.logError ("bad AA value " ^ analysis ^ " -- defaulting to Cil\n");
         getFiCiEqAA ())
  in
  match aspect with
    "funptrs" -> fpAA := chosen
  | "exps" -> expAA := chosen
  | "lvals" -> lvalAA := chosen
  | "may_alias" -> mayAA := chosen
  | _ ->
      L.logError ("bad AA aspect value " ^ aspect ^ "  -- doing nothing\n")
      

let initSettings settings rootPath = begin
  setRoot rootPath;
  let aSettings = Config.getGroup settings "ALIAS_ANALYSIS" in
  Config.iter 
    (fun fieldName values ->
       assignAnalysis fieldName values
    ) aSettings;
end



(***********************************************************)
(* Wrapper functions around PTA                            *)


(* WARNING: these may return empty lists instead of throwing exceptions *)
 
let may_alias e1 e2 : bool =
  !mayAA#may_alias e1 e2

let points_to e v : bool =
  !mayAA#points_to e v

let deref_funptr fpExp : (fKey list) =
  !fpAA#deref_funptr fpExp

let deref_exp e = 
  !expAA#deref_exp e

(* Given an expression that refers to the address of a function,
   get a list of possible functions. *)
let rec funsFromAddr addrExp : fKey list =
  match addrExp with
    Lval(l) -> (* assume it's a function pointer *)
      deref_funptr addrExp
  | CastE(_, e) ->
      funsFromAddr e
  | AddrOf (Var(va), NoOffset) (* funcs should not have offsets *)
  | StartOf (Var(va), NoOffset) ->
      [va.vid]
  | _ -> failwith "funsFromAddr doesn't understand given fptr"


(** Given a function call expression, find the targets of the call *)
let rec funsForCall callExp : fKey list =
  match callExp with
    Lval(Var(finfo), NoOffset) ->
      [finfo.vid]
  | Lval(Mem(fpExp), _) -> 
      deref_funptr fpExp
  | CastE(_, e) ->
      funsForCall e
  | AddrOf (Var(va), NoOffset) (* funcs should not have offsets *)
  | StartOf (Var(va), NoOffset) ->
      [va.vid]
  | _ -> failwith "funsForCall doesn't understand given fptr"



module Abs =
struct

  module CLv = Cil_lvals
  module Stat = Mystats

  (* Interface for CIL lvals/exps *)

  let deref_lval lv =
    !lvalAA#deref_lval_abs lv
      
  let deref_exp exp =
    !expAA#deref_exp_abs exp

  let getNodeLval lv =
    !lvalAA#getNodeLval lv

  let getNodeVar  var =
    getNodeLval (Var var, NoOffset)

  (* Interface for Abstract nodes *)

  let represents abs =
    !expAA#represents abs

  let deref abs =
    !expAA#deref_abs abs

  let may_alias abs1 abs2 =
    !expAA#may_alias_abs abs1 abs2

  let compare abs1 abs2 =
    !lvalAA#compare abs1 abs2

  let hash abs =
    !lvalAA#hash abs

  let string_of abs =
    !lvalAA#string_of abs

  let size_of abs =
    !lvalAA#size_of abs

  let reachableFrom abs srcs =
    !lvalAA#reachableFrom abs srcs

  let reachableFromG abs =
    !lvalAA#reachableFromG abs

end


(** Simple bit of "reflection" to identify what function pointer 
    analysis is used *)
let identifyFPA () : string =
  !fpAA#identity
