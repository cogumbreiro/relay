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
open Cildump
open Logging

module CilPTA = Myptranal
module Steens = Pta_fi_eq
module Anders = Pta_fs_dir

module Stats = Mystats


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

  (* Deprecated: Use Callg module's functions w/ "current" program point *)
  method virtual deref_funptr : Cil.exp -> (fKey list)

  method virtual may_alias : Cil.exp -> Cil.exp -> bool

  method virtual points_to : Cil.exp -> Cil.varinfo -> bool

  method virtual deref_exp : Cil.exp -> Cil.varinfo list

  (* Getting abstract nodes from Cil values *)

  method virtual deref_exp_abs : Cil.exp -> ptaNode list

  method virtual deref_lval_abs : Cil.lval -> ptaNode list

  method virtual getNodeLval : Cil.lval -> ptaNode

  method virtual getNodeExp : Cil.exp -> ptaNode

  (* Operations on abstract nodes (e.g., convert back) *)
  method virtual represents : ptaNode -> Cil.lval list

  method virtual deref_abs : ptaNode -> ptaNode

  method virtual compare : ptaNode -> ptaNode -> int

  method virtual hash : ptaNode -> int

  method virtual string_of : ptaNode -> string

  method virtual pts_size : ptaNode -> int

  method virtual label_size : ptaNode -> int

  method virtual may_alias_abs : ptaNode -> ptaNode -> bool

  method virtual points_to_abs : ptaNode -> ptaNode -> bool 

  method virtual location_alias_abs : ptaNode -> ptaNode -> bool

  method virtual reachableFrom : ptaNode -> ptaNode list -> bool

  method virtual reachableFromG : ptaNode -> bool

  method virtual writeState : unit -> unit

end



class dummyAnalyzer = object (self)
  inherit aliasAnalyzer

  method identity = "dummy"

  method error_msg = (self#identity ^ " alias analyzer doesn't support this\n")
    
  method deref_funptr fexp =
    failwith self#error_msg 

  method points_to e v =
    failwith self#error_msg 

  method may_alias e1 e2 =
    failwith self#error_msg 

  method deref_exp e =
    failwith self#error_msg 

  method deref_exp_abs e =
    failwith self#error_msg 

  method deref_lval_abs lv =
    failwith self#error_msg

  method getNodeLval lv =
    failwith self#error_msg

  method getNodeExp e =
    failwith self#error_msg

  method represents abs =
    failwith self#error_msg

  method may_alias_abs abs1 abs2 =
    failwith self#error_msg

  method location_alias_abs abs1 abs2 =
    failwith self#error_msg

  method points_to_abs ptr targ =
    failwith self#error_msg

  method deref_abs abs =
    failwith self#error_msg

  method compare abs1 abs2 =
    failwith self#error_msg

  method hash abs =
    failwith self#error_msg

  method string_of abs =
    failwith self#error_msg

  method pts_size abs =
    failwith self#error_msg

  method label_size abs =
    failwith self#error_msg

  method reachableFrom abs srcs =
    failwith self#error_msg

  method reachableFromG abs =
    failwith self#error_msg

  method writeState () =
    failwith self#error_msg

end


(********* CIL Analysis interface *********) 

exception UnknownLoc

class cilAnalyzer = object
  inherit dummyAnalyzer

  method identity = "cil"

  method deref_funptr fexp =
    (* Should also match ftypes, since the PTA merges fields *)
    let ftype = Cil.typeOf fexp in
    let ftype_str = string_of_ftype ftype in    
    let fdecs = 
      try CilPTA.resolve_funptr fexp
      with 
        Not_found
      | CilPTA.UnknownLocation -> []
    in
    List.fold_left 
      (fun curList fdec ->
         let ft = fdec.svar.vtype in
         let fts = string_of_ftype ft in
         if (fts = ftype_str) 
         then fdec.svar.vid :: curList
         else curList
      ) [] fdecs

  method deref_exp e =
    try CilPTA.resolve_exp e
    with 
      Not_found
    | CilPTA.UnknownLocation -> []

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
    try CilPTA.may_alias e1 e2
    with 
      Not_found 
    | CilPTA.UnknownLocation ->
        raise UnknownLoc

  (* TODO: figure out how to get an abstract rep for CIL nodes *)

  method writeState () =
    logStatus "writeState: Not needed for CIL PTA"

end


(********* Steensgaard Analysis interface *********) 

let getSteensNode n =
  match n with
    SteensNode s -> s
  | _ -> raise NodeTypeMismatch

let makeSteensNode s =
  SteensNode s

class steensAnalyzer = object (self)
  inherit dummyAnalyzer

  initializer
    logStatus "Initializing Steensgaard AA info";
    Steens.analyzeAll !rootPath false

  method identity = Steens.aaName

  method deref_funptr fexp =
    Steens.resolve_funptr fexp

  method deref_exp e =
    try
      Steens.deref_exp e
    with Not_found ->
      logError "0 targets from deref_exp";
      []

  method may_alias e1 e2 =
    Steens.may_alias e1 e2

  method points_to e v =
    Steens.points_to e v

  method deref_exp_abs e =
    let nodes = Steens.Abs.deref_exp e in
    List.map makeSteensNode nodes

  method deref_lval_abs lv =
    let nodes = Steens.Abs.deref_lval lv in
    List.map makeSteensNode nodes

  method getNodeLval lv =
    match Steens.Abs.getNodeLval lv with 
      None -> raise UnknownLoc
    | Some n -> makeSteensNode n 
        
  method getNodeExp e =
    match Steens.Abs.getNodeExp e with
      None -> raise UnknownLoc
    | Some n -> makeSteensNode n

  method represents abs =
    let n = getSteensNode abs in
    Steens.Abs.lvals_of n

  method may_alias_abs abs1 abs2 =
    let n1 = getSteensNode abs1 in
    let n2 = getSteensNode abs2 in
    Steens.Abs.may_alias n1 n2
    
  method location_alias_abs abs1 abs2 =
    let n1 = getSteensNode abs1 in
    let n2 = getSteensNode abs2 in
    Steens.Abs.location_alias n1 n2

  method points_to_abs ptr targ =
    let n1 = getSteensNode ptr in
    let n2 = getSteensNode targ in
    Steens.Abs.points_to n1 n2

  method deref_abs abs =
    try
      let n = getSteensNode abs in
      makeSteensNode (Steens.Abs.deref n)
    with Not_found ->
      raise UnknownLoc

  method compare abs1 abs2 =
    let n1 = getSteensNode abs1 in
    let n2 = getSteensNode abs2 in
    Steens.Abs.compare n1 n2
    
  method hash abs =
    let n = getSteensNode abs in
    Steens.Abs.hash n

  method string_of abs =
    let n = getSteensNode abs in
    Steens.Abs.string_of n

  method pts_size abs =
    let n = getSteensNode abs in
    Steens.Abs.pts_size n

  method label_size abs =
    let n = getSteensNode abs in
    Steens.Abs.label_size n
    
  method reachableFrom abs srcs =
    let n = getSteensNode abs in
    let ss = List.map getSteensNode srcs in
    Steens.Abs.reachableFrom n ss
    
  method reachableFromG abs =
    let n = getSteensNode abs in
    Steens.Abs.reachableFromG n 

  method writeState () =
    logStatus "writeState: Not needed for Steens PTA"
    
end

(********* Andersen's Analysis interface *********) 

let getAndersNode n =
  match n with
    AndersNode x -> x
  | _ -> raise NodeTypeMismatch

let makeAndersNode x =
  AndersNode x

class andersAnalyzer = object (self)
  inherit dummyAnalyzer

  initializer
    logStatus "Initializing Andersen's AA info";
    Anders.analyzeAll !rootPath false

  method identity = Anders.aaName

  method deref_funptr fexp =
    Anders.resolve_funptr fexp

  method deref_exp e =
    try
      Anders.deref_exp e
    with Not_found ->
      logError "0 targets from deref_exp";
      []

  method may_alias e1 e2 =
    Anders.may_alias e1 e2

  method points_to e v =
    Anders.points_to e v

  method deref_exp_abs e =
    let nodes = Anders.Abs.deref_exp e in
    List.map makeAndersNode nodes

  method deref_lval_abs lv =
    let nodes = Anders.Abs.deref_lval lv in
    List.map makeAndersNode nodes

  method getNodeLval lv =
    match Anders.Abs.getNodeLval lv with 
      None -> raise UnknownLoc
    | Some n -> makeAndersNode n
        

  method getNodeExp e =
    match Anders.Abs.getNodeExp e with
      None -> raise UnknownLoc
    | Some n -> makeAndersNode n

  method represents abs =
    let n = getAndersNode abs in
    Anders.Abs.lvals_of n

  method may_alias_abs abs1 abs2 =
    let n1 = getAndersNode abs1 in
    let n2 = getAndersNode abs2 in
    Anders.Abs.may_alias n1 n2

  method location_alias_abs abs1 abs2 =
    let n1 = getAndersNode abs1 in
    let n2 = getAndersNode abs2 in
    Anders.Abs.location_alias n1 n2

  method points_to_abs abs1 abs2 = 
    let n1 = getAndersNode abs1 in
    let n2 = getAndersNode abs2 in
    Anders.Abs.points_to n1 n2

  method deref_abs abs =
    try
      let n = getAndersNode abs in
      makeAndersNode (Anders.Abs.deref n)
    with Not_found ->
      raise UnknownLoc

  method compare abs1 abs2 =
    let n1 = getAndersNode abs1 in
    let n2 = getAndersNode abs2 in
    Anders.Abs.compare n1 n2
    
  method hash abs =
    let n = getAndersNode abs in
    Anders.Abs.hash n

  method string_of abs =
    let n = getAndersNode abs in
    Anders.Abs.string_of n

  method pts_size abs =
    let n = getAndersNode abs in
    Anders.Abs.pts_size n

  method label_size abs =
    let n = getAndersNode abs in
    Anders.Abs.label_size n
    
  method reachableFrom abs srcs =
    let n = getAndersNode abs in
    let ss = List.map getAndersNode srcs in
    Anders.Abs.reachableFrom n ss
    
  method reachableFromG abs =
    let n = getAndersNode abs in
    Anders.Abs.reachableFromG n

  method writeState () =
    Anders.writeState !rootPath
      
    
end

(***** Optimistic No-Alias Analysis *****)

class noAliasAnalyzer = object (self)
  inherit dummyAnalyzer

  initializer
    logStatus "Initializing NoAlias AA info"

  method identity = "noalias"
  method deref_funptr fexp = []
  method deref_exp e = []
  method may_alias e1 e2 = false
  method points_to e v = false
  method deref_exp_abs e = []
  method deref_lval_abs lv = []

end

(***** OIC Callgraph *****)

class oicFPAnalysis = object (self)
  inherit dummyAnalyzer as super
    
  initializer
    logStatus "Initializing OIC FP info"
    
  (* Set this so it knows which callgraph file to read *)
  method identity = "oic"

end


(***** DSA Callgraph *****)

class dsaFPAnalysis = object (self)
  inherit dummyAnalyzer as super
    
  initializer
    logStatus "Initializing DSA FP info"
    
  (* Set this so it knows which callgraph file to read *)
  method identity = "llvm_dsa"

end

(***** AndersFS Callgraph *****)

class andersFSFPAnalysis = object (self)
  inherit dummyAnalyzer as super
    
  initializer
    logStatus "Initializing AndersFS FP info"
    
  (* Set this so it knows which callgraph file to read *)
  method identity = "llvm_anders"

end


(***** Finalizer ******)
  
let toFinalize = ref []

let finalize pta =
  pta#writeState ()

let addFinalizer pta =
  toFinalize := List_utils.addOnce !toFinalize pta

let finalizeAll () =
  List.iter finalize !toFinalize

(* ... now each user of alias is responsible for calling finalizeAll() *)


(***** singleton analyzers *****)

let dummyAA = new dummyAnalyzer

(* lazily create these (to avoid required pre-processing when unnecessary) *)
let cilAA = ref dummyAA 
let steensAA = ref dummyAA
let andersAA = ref dummyAA
let noAliasAA = ref dummyAA
let oicFPAA = ref dummyAA 
let dsaAA = ref dummyAA 
let andersFSFPAA = ref dummyAA 

let getCilAA () =
  if (!cilAA == dummyAA) then begin
    cilAA := new cilAnalyzer;
    addFinalizer !cilAA
  end;
  !cilAA

let getFiCiEqAA () =
  if (!steensAA == dummyAA) then begin
    steensAA := new steensAnalyzer;
    addFinalizer !steensAA
  end;
  !steensAA

let getAndersAA () =
  if (!andersAA == dummyAA) then begin
    andersAA := new andersAnalyzer;
    addFinalizer !andersAA
  end;
  !andersAA

let getNoAliasAA () =
  if (!noAliasAA == dummyAA) then 
    noAliasAA := new noAliasAnalyzer;
  !noAliasAA

let getOICFPAA () =
  if (!oicFPAA == dummyAA) then 
    oicFPAA := new oicFPAnalysis;
  !oicFPAA

let getDSAFPAA () =
  if (!dsaAA == dummyAA) then 
    dsaAA := new dsaFPAnalysis;
  !dsaAA

let getAndersFSFPAA () =
  if (!andersFSFPAA == dummyAA) then 
    andersFSFPAA := new andersFSFPAnalysis;
  !andersFSFPAA
    

(* Hmm, could have done the above more easily w/ macros, or
   a language where class types are first class *)


(***** the particular analyzer to use for each query *****)

let fpAA = ref dummyAA
let expAA = ref dummyAA
let lvalAA = ref dummyAA
let mayAA = ref dummyAA


    
(****************************************************
 * Initializing / Updating CIL PTA info
 ****************************************************)

let aliasCache = Hashtbl.create 10 (* alias file-info cache *)

let setCurrentFile (f:Cil.file) : unit =
  (* TODO: cache PTA state?? *)
  if !cilAA != dummyAA then (* Only do this if it's using the cilAA *)
    if (not (Hashtbl.mem aliasCache f.fileName)) then begin
      CilPTA.reset_globals ();
      CilPTA.analyze_file f;
      CilPTA.compute_results false;
      Hashtbl.clear aliasCache;
      Hashtbl.add aliasCache f.fileName true;
    end
    else ()
  else ()

let newFuncListener fkey f : unit =
  setCurrentFile f


(***********************************************************)
(* Set up                                                  *)


let ws = "[ \r\n\t]*"

(* Top level split, between the field name and value  *)
let topSplitter = Str.split_delim (Str.regexp (ws ^ "[:]" ^ ws))

let groupEnd = Str.regexp (ws ^ "[}]" ^ ws)

let assignAnalysis aspect analysis =
  let analysis = Strutil.strip (String.lowercase analysis) in
  let aspect = Strutil.strip (String.lowercase aspect) in
  let chosen = 
    match analysis with
      "cil" -> getCilAA ()
    | "fi_ci_eq" -> getFiCiEqAA ()
    | "anders" -> getAndersAA ()
    | "noalias" -> getNoAliasAA ()
    | "oic" -> getOICFPAA ()
    | "dsa" -> getDSAFPAA ()
    | "andersfs" -> getAndersFSFPAA ()
    | _ ->
        failwith ("bad AA value " ^ analysis ^ " -- defaulting to Cil\n")
  in
  match aspect with
    "funptrs" -> fpAA := chosen
  | "exps" -> expAA := chosen
  | "lvals" -> lvalAA := chosen
  | "may_alias" -> mayAA := chosen
  | _ ->
      failwith ("bad AA aspect value " ^ aspect ^ "  -- doing nothing\n")
      

let initSettings settings rootPath = begin
  Cilinfos.addGetFuncListener newFuncListener;
  setRoot rootPath;
  let aSettings = Config.getGroup settings "ALIAS_ANALYSIS" in
  Config.iter 
    (fun fieldName values ->
       assignAnalysis fieldName values
    ) aSettings;
end

let setFilterFunSig what =
  Steens.setFilter what;
  Anders.setFilter what


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

(** Given an expression that refers to the address of a function,
    get a list of possible functions. 
    E.g., for the first parameter in the call: thread_create(&foo, ...)  *)
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


(** Given a function call expression, find the targets of the call. 
    E.g., for a function pointer call *callExp(...); *)
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
    Stat.time "derefLval" !lvalAA#deref_lval_abs lv
      
  let deref_exp exp =
    Stat.time "derefExp" !expAA#deref_exp_abs exp

  let getNodeLval lv =
    Stat.time "getNodeLval" !lvalAA#getNodeLval lv

  let getNodeExp exp =
    Stat.time "getNodeExp" !expAA#getNodeExp exp

  let getNodeVar var =
    getNodeLval (Var var, NoOffset)

  (* Interface for Abstract nodes *)

  let represents abs =
    !expAA#represents abs

  let deref abs =
    !expAA#deref_abs abs

  let may_alias abs1 abs2 =
    !expAA#may_alias_abs abs1 abs2

  let location_alias abs1 abs2 =
    !expAA#location_alias_abs abs1 abs2

  let points_to abs1 abs2 =
    !expAA#points_to_abs abs1 abs2

  let compare abs1 abs2 =
    !lvalAA#compare abs1 abs2

  let hash abs =
    !lvalAA#hash abs

  let string_of abs =
    !lvalAA#string_of abs

  let pts_size abs =
    !lvalAA#pts_size abs

  let label_size abs =
    !lvalAA#label_size abs

  let reachableFrom abs srcs =
    Stat.time "reachableFrom" (!lvalAA#reachableFrom abs) srcs

  let reachableFromG abs =
    !lvalAA#reachableFromG abs

end


(** Simple bit of "reflection" to identify what function pointer 
    analysis is used *)
let identifyFPA () : string =
  !fpAA#identity
