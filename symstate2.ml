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


(** Symbolic execution module. In the end, provides mapping of 
    lvalues to their value at each program point. Values are in terms
    of the initial values (at function start). @see sym_types.ml
 *)

open Cil
open Pretty
open Fstructs
open Symex_base
open Sym_types
open Scope
open Cildump
open Logging

module DF = Dataflow
module A = Alias
module SS = Symsummary
module BS = Backed_summary
module CLv = Cil_lvals
module Lv = Lvals

module Stat = Mystats
module I = Inspect

(* TODO:
   [*] Be able to summarize lvals w/ negative field offsets

   [*] Use host's basetype for ptr arith instead?
*)

(* flag for DF debug info *)
let debug = false

let inspect = ref false

let setInspect yesno =
  inspect := yesno

(************************************************************
            Access to mod information 
************************************************************)

let modSumms = ref (new Modsummaryi.absModSumm)

let setModSumm (newSummaries) =
  modSumms := newSummaries

(*********** Null checking stuff ***********************************)

module NI = struct

  let isNullHost = isNullHost

end

module NULL = SYMEX_NULL_GEN (NI)

(************************************************************
             Concurrency Adjustments
************************************************************)

module Conc = struct

  let initConcAdjusts cg =
    Shared.initEscapeable cg

  let adjust = ref true

  (** Adjust value (looked up or about to be stored?) for concurrency *)
  let doAdjust addr v =
    if !adjust then
      match v with 
        Vtop -> v
      | _ -> 
          let lv = hostOfAddr addr, NoOffset in
          if Shared.escapeableAbs lv then Vtop else v
    else v

  (** Adjust the values of the symbolic state *)
  let adjustStore state = 
    if !adjust then begin
      state
    end else
      state


  (* TODO: only do the adjust when preparing to propagate out ...
     not on every read / write that may happen during "eval..." 
     at least allows SOME global lock expressions be expressed in
     terms of globals (itself) ... *)

end

(***********************************************************
       Configuration, etc.
************************************************************)


let config settings =
  let handleKeyVal fieldName value =
    let informError fieldName = 
      logError "Corrupt line in symbolic exec. settings file:\n";
      logError (fieldName ^ "\n")
    in
    try
      match fieldName with
        "ADJUST" -> Conc.adjust := bool_of_string value
      | _ -> informError fieldName
    with e ->
      logError ("config: " ^ (Printexc.to_string e));
      informError fieldName;
      raise e
  in
  try
    let mySettings = Config.getGroup settings "SYM_EX" in
    Config.iter handleKeyVal mySettings
  with Not_found ->
    logStatus "No special settings for sym-ex"

let curCG = ref Callg.emptyCG

let init settings cg modSum =
  config settings;
  setModSumm modSum;
  Conc.initConcAdjusts cg;
  curCG := cg


(*********************************************************
 * Cells, values, expressions etc. See symsummary.ml
 *********************************************************)

let curPtrID = ref 0

let freshPtrID () = 
  let result = !curPtrID in
  curPtrID := result + 1;
  result

(*** Sources ***)
type extSource =
    ExtReturn
  | ExtArg of int    (* Collapse all cells extended from arg i to one? *)


(*********************************************************
 * Symbolic store and state
 *********************************************************)


(* Variable store is map from addresses to values *)
type symStore = symVal AddrMap.t

(* per program-point state *)
type symState = {
  store: symStore;
  assumptions: symExp list;   (* things assumed to be true along this path *)
}

(* Expose alias *)
type symSt = symState 

(*********************************************************
 * Special instances of addresses, states, etc.
 *********************************************************)

(* A null pointer is either Vbot, or (Vval e when e is a constant). 
   Only take this interpretation when used as a pointer *)

let bottomExp = makeNullExp ()

let emptySymState = {
  store = AddrMap.empty;
  assumptions = [];
}

let bottomSymState = {
  store = AddrMap.empty;
  assumptions = [bottomExp];
}

(* TODO: use the intraDataflow stuff *)
let curFunc = ref Cil.dummyFunDec
let curFunID = ref Callg.dummyFID

module TopPtrHash = struct
  type t = Cil.typ * Cil.exp * Cil.offset
  let equal (t1, e1, o1) (t2, e2, o2) =
    Ciltools.equal_type t1 t2 &&
      Ciltools.compare_exp e1 e2 == 0 &&
      Ciltools.compare_offset o1 o2 == 0
  let hash (t1, e1, o1) =
    (Ciltools.hash_type t1) lxor 
      (Ciltools.hash_exp e1) lxor 
      (Ciltools.hash_offset o1)
end

module TPH = Hashtbl.Make(TopPtrHash)

let topPtrCache = TPH.create 37

module EH = Hashtbl.Make(CLv.HashedExp)

let ptrExpCache = EH.create 37

(*********************************************************
 * Operations on state 
 *********************************************************)

let expEqual exp otherE = Lvals.compare_exp exp otherE == 0

let lvalEqual lv otherLv = Lvals.compare_lval lv otherLv == 0


(** looks up the value at 'addr' and 'canonOff' from a given store. 
    Assumes offset is canonicized. May raise Not_found *)
let lookupValStore (s:symStore) addr canonOff : symVal =
  let v = AddrMap.find addr s in
  match v with 
    Vstruct offMap -> 
      (match canonOff with
         NoOffset -> v
       | Field _ 
       | Index _ ->
           let truncOff = CLv.simplifyOff canonOff in
           OffsetMap.find truncOff offMap
      )
  | _ -> (* if canonOff is a field offset, but addr evals to TOP,
            the result is TOP *)
      v


(** looks up the value at 'addr' and 'canonOff' from a given state. 
    Assumes offset is canonicized. May raise Not_found *)
let lookupVal (state:symState) addr canonOff : symVal = 
  if isNullAddr addr then begin
    logError ~prior:3 "lookupVal: given null addr";
    Vtop
  end 
  else lookupValStore state.store addr canonOff



(** returns a new store where 'addr.off' has been assigned 'v'
    may update a struct *)
let assignVarStore (oldStore:symStore) addr off v : symStore =
  let v = Conc.doAdjust addr v in
  match off with
    NoOffset ->
      (AddrMap.add addr v oldStore)
  | Field _ 
  | Index _ ->
      let truncOff = CLv.simplifyOff off in
      (* Make a struct thing if it isn't there already *)
      let outerVal = try
        let oldOuterVal = AddrMap.find addr oldStore in
        match oldOuterVal with
          Vstruct offMap -> 
            Vstruct (OffsetMap.add truncOff v offMap)
        | Vbot
        | Vtop   (* even for Vtop? strong update i guess? *)
        | Vval _ ->
            Vstruct (OffsetMap.add truncOff v OffsetMap.empty)
        | Vextptr _
        | Vmustptr _ 
        | Vmayptr _ ->
            (* maybe it was wrong before about being used as a ptr? *)
            Vstruct (OffsetMap.add truncOff v OffsetMap.empty)
      with Not_found ->
        Vstruct (OffsetMap.add truncOff v OffsetMap.empty)
      in
      (AddrMap.add addr outerVal oldStore)


(** returns a new state where 'addr.off' has been assigned 'v' *)
let assignVar (oldState:symState) addr off v : symState =
  if isNullAddr addr then begin
(*    logError "assignVar: given null addr"; *)
    oldState
  end 
  else
    let newStore = assignVarStore oldState.store addr off v in
    { oldState with store = newStore; }
      

let makeSetWithNull targ =
  AddrOffSet.add (nullAddr, NoOffset) (AddrOffSet.singleton targ)

let addNullTo targSet =
  AddrOffSet.add (nullAddr, NoOffset) targSet


(** LUB of two values *)
let rec combineVals v1 v2 =
  match v1, v2 with
      Vmustptr addrOff1, Vmustptr addrOff2 -> 
        if ((compareAddrOff addrOff1 addrOff2) == 0) then v1
        else
          let ptSet = AddrOffSet.add addrOff1 AddrOffSet.empty in
          Vmayptr (freshPtrID (), AddrOffSet.add addrOff2 ptSet)
            
    | Vmayptr (id1, ptset1), Vmayptr (id2, ptset2) ->
        if (AddrOffSet.equal ptset1 ptset2) then (* TODO: use IDs instead *)
          v1
        else 
          Vmayptr (freshPtrID (), AddrOffSet.union ptset1 ptset2)
            
            
    | Vextptr (id1, ptset1), Vextptr (id2, ptset2) -> 
        if (AddrOffSet.equal ptset1 ptset2) then (* TODO: use IDs instead *)
          v1
        else
          Vextptr (freshPtrID (), AddrOffSet.union ptset1 ptset2)

    | Vmustptr mustAddrOff, Vmayptr (mayId, mayPtSet)
    | Vmayptr (mayId, mayPtSet), Vmustptr mustAddrOff ->
        Vmayptr (freshPtrID (), AddrOffSet.add mustAddrOff mayPtSet)

    | Vmustptr mustAddrOff, Vextptr (extId, extPtSet)
    | Vextptr (extId, extPtSet), Vmustptr mustAddrOff ->
        Vextptr (freshPtrID (), AddrOffSet.add mustAddrOff extPtSet)
          
    | Vextptr (extId, extPtSet), Vmayptr (mayId, mayPtSet)
    | Vmayptr (mayId, mayPtSet), Vextptr (extId, extPtSet) ->
        Vextptr (freshPtrID (), AddrOffSet.union mayPtSet extPtSet)
     
    (* Assume constants are null pointers *)   
    | (Vmustptr targ), (Vval e) 
    | (Vval e), (Vmustptr targ) when isConst e ->
        Vmayptr (freshPtrID (), makeSetWithNull targ)
        
    | (Vmayptr (_, targs)), (Vval e)
    | (Vextptr (_, targs)), (Vval e)
    | (Vval e), (Vextptr (_, targs))
    | (Vval e), (Vmayptr (_, targs)) when isConst e ->
        Vextptr (freshPtrID (), addNullTo targs)


(*
    | Vbot, (Vmustptr targ) 
    | (Vmustptr targ), Vbot ->
        Vmayptr (freshPtrID (), makeSetWithNull targ)

    | (Vmayptr (_, targs)), (Vbot)
    | (Vextptr (_, targs)), (Vbot)
    | (Vbot), (Vextptr (_, targs))
    | (Vbot), (Vmayptr (_, targs)) ->
        Vextptr (freshPtrID (), addNullTo targs)
*)

    | Vbot, x
    | x, Vbot ->
        x 

    | Vstruct offmap1, Vstruct offmap2 ->
        (* unify the offset maps *)
        Vstruct (OffsetMap.union combineVals offmap1 offmap2)

    | Vval exp1, Vval exp2 ->
        (* What if concrete versions are eq (x_0 actually == y_0 + c) *)
        if (expEqual exp1 exp2) then
          v1
        else
          Vtop
                    
    | Vval _, Vstruct _ ->
        (* Could happen when no offsets have been touched on one path,
           but then the new value may differ from the uninitialized value *)
        Vtop

    | Vstruct _ , Vval _ ->
        Vtop
          
    | Vtop, _ 
    | _, Vtop ->
        Vtop
        
    | _, _ ->
        (* could happen when ignoring casts, e.g., (x = (unsigned int)ptr) *)
        logError "combineVals: trying to unify different types of vals";
        Vtop
        

(** LUB of the two states *) 
let combineStates st1 st2 =
  if (st1 == bottomSymState) then st2 
  else if (st2 == bottomSymState) then st1 else
    let store1 = st1.store in
    let store2 = st2.store in
    
    (* Combine store *)
    (* TODO: treat entries w/ no matched mapping and create initial val *)
    let newStore = AddrMap.union combineVals store1 store2 in 

    (* TODO update assumptions *)
    { st1 with 
        store = newStore; 
    }
      


    
(*********************************************************
 * Making fresh vars, cells, values, etc.
 *********************************************************)

(***** Vars *****)

let sym_var_counter = ref 0
let sym_pref = "#"
let sym_pref_char = '#'
let vi_name_pool = Hashtbl.create 17

(** Create a symbolic variable associated of a given type
    Optionally pass a base name *)    
let freshVarinfo ?(name:string option) (typ:Cil.typ) =
  let n = match name with
    None ->
      let suff = (string_of_int !sym_var_counter) in
      sym_var_counter := !sym_var_counter + 1;
      sym_pref ^ suff
  | Some n ->
      n
  in
  makeVarinfo false n typ 

let string_of_extSource e = 
  match e with
    ExtReturn ->
      "return:"
  | ExtArg n ->
      ("arg:" ^ (string_of_int n) ^ ":")


(** Make up a varinfo for unknown value of type t, 
    originating from location l *)
let varinfoOfLocType (e:extSource) (t:Cil.typ) (l:Cil.location) : Cil.varinfo =
  (* make sure to be consistent with naming the alloc'ed blocks *)
  let prefix = ("@" ^ (string_of_extSource e)) in
  let name = (prefix ^ (Cildump.string_of_loc l)) in
  try
    Hashtbl.find vi_name_pool name
  with Not_found ->
    let newVi = freshVarinfo ~name:name t in
    Hashtbl.add vi_name_pool name newVi;
    newVi


(** True if this is the name of a symbolic constant *)
let isSymVarname (name:string) =
  name.[0] == sym_pref_char


(***** Cells *****)

(** Get the symbolic cell that represents the given concrete source *)
let getAddr host (isSum:bool) : symAddr =
  (* try to find a cell w/ the same host & return existing 
     cell, or create the cell *)
  let host, _ = Lvals.mergeLv (host, NoOffset) in
  let addr = { saHost = host; saSummary = isSum; } in
  addr.saSummary <- addr.saSummary or isSum; (* update summary flag *)
  addr

(** Specially make a fresh cell to represent an alloc site *)
let getAllocAddr (e:extSource) (t:Cil.typ) (l:Cil.location) : symAddr =
  let host = Lvals.hostOfVar (varinfoOfLocType e t l) in
  getAddr host true


(***** Complex creations *****)

(** Assume that the lval (host, off) holds a pointer value. Give that
    a pointer value that points to *lval *)
let makePtrValue (host, off) : symVal =
  let host, _ = Lvals.mkMem (Lvals.mkLval host off) NoOffset in
  let addr = getAddr host false in (* Check if it's a summary node later *)
  let ptrID = freshPtrID () in
  let ptSet = AddrOffSet.add (addr, NoOffset) AddrOffSet.empty in
  Vextptr (ptrID, ptSet)


(** Assume given offset has been canonicized,
    Under this execution, assume the typ of the addr is baseTyp 
    (an attempt to recover discarded type-cast information) *)
let makeValue baseTyp addr canonOff =
  let v =
    let host = hostOfAddr addr in
    if (Cil.isPointerType baseTyp) then
      makePtrValue (host, canonOff)
    else
      let val1 = Vval (Lvals.mkLval host canonOff) in
      val1
  in
  Conc.doAdjust addr v
(*  v *)


(** Concatenate two offsets for an addr *) 
let concatOffset (addr:symAddr) (outerOff:Cil.offset) (innerOff:Cil.offset)
    : Cil.offset =
  let newOff, isSum = CLv.canonicizeOff (Cil.addOffset outerOff innerOff) in
  addr.saSummary <- addr.saSummary || isSum; (* update flag *)
  CLv.simplifyOff newOff
    

(** Before using / evaluating the value of a cell, make sure it
    won't cause termination issues *)
let limitAddrOff (baseTyp:Cil.typ) (addr:symAddr) 
    (outerOff:Cil.offset) (innerOff:Cil.offset) : symAddr * Cil.offset = 
  (* Check if the lval should be trimmed, and a new addr used *)
  let host = hostOfAddr addr in
  let newOff, summedOff = 
    CLv.canonicizeOff (Cil.addOffset outerOff innerOff) in
  let correspLv = (host, newOff) in
  let (finalHost, fOff), summedLv = Lvals.simplifyLval correspLv in
  let finalAddr = getAddr finalHost (summedLv || summedOff) in
  (finalAddr, fOff)



(*********************************************************
 * Expression Evaluation
 *********************************************************)


(** Returns the symbolic expression associated with 'true' or 'false'. 
    Recall that in  C we have "false == 0" and "true <> 0". *)
let exp_of_bool b = match b with
    true -> Lvals.abs_of_exp (Cil.integer 1)
  | false -> Lvals.abs_of_exp (Cil.integer 0)

(** Assuming the int64 i represents a bool, invert it *)
let not_of_int64 i = 
  if (i == Int64.zero) then
    Int64.one
  else
    Int64.zero

(** Evaluate the value at an addr + offset *)
let evalAddr (s:symState) baseTyp addr canonOff : symVal * symState =
  try
    let value = lookupVal s addr canonOff in
    (value, s)
  with Not_found ->
    (* Initialize to some value *)
    let newVal = makeValue baseTyp addr canonOff in
    let finalStore = assignVar s addr canonOff newVal in
    (newVal, finalStore) 


(** true if the variable is in scope *)
let inScope (vi:Cil.varinfo) =
  (* TODO: probably shouldn't track globals until apply summaries 
     for race checking. Throw away function-typed globals as they 
     are immutable. *)
  ( (vi.vglob && not (Cil.isFunctionType vi.vtype)) ||
      List.exists 
      (fun x ->
         x.vid = vi.vid) !curFunc.sformals ||
      List.exists 
      (fun x ->
         x.vid = vi.vid) !curFunc.slocals 
      (* TODO filter out direct references to functions? *)
  )

let topPtrCacheLookup = TPH.find topPtrCache

(** Get a list of lvals corresponding to a deref of a top ptr *) 
let topPtrDerefLval baseType ptrExp outerOff =
  (* Expand aliases w/ CIL AA, and filter by type and scope. *)
  try
    topPtrCacheLookup (baseType, ptrExp, outerOff)
  with Not_found ->
    (* deref ptrExp *)
    let aliases = 
      try 
        EH.find ptrExpCache ptrExp
      with Not_found ->
        let temp = Stat.time "resolve_exp" A.deref_exp ptrExp in
        EH.add ptrExpCache ptrExp temp;
        temp
    in
    let results = 
      List.fold_left 
        (fun curResults vi ->
           if (inScope vi) then
             try
               let newLval = Lvals.attachOffset (Lvals.hostOfVar vi) outerOff in
               let newType = Lvals.typeOfLvalUnsafe newLval in
               if (Ciltools.equal_type baseType newType) then
                 newLval :: curResults
               else
                 curResults
             with 
               CLv.OffsetMismatch _
             | Errormsg.Error
             | Failure _ ->
                 curResults
           else
             curResults
        ) [] aliases in
    TPH.add topPtrCache (baseType, ptrExp, outerOff) results;
    results
        
(** Get a list of addrs that this pointer may target *) 
let topPtrAddrsLval baseType ptrExp outerOff : (symAddr * Cil.offset) list =
  let results = topPtrDerefLval baseType ptrExp outerOff in
  List.map 
    (fun (host, off) ->
       (* could make up new addrs *)
       (getAddr host false, off)
    ) results
    

(** Get a list of lvals that this pointer expression may target *) 
let topPtrDerefExp (origExp:Cil.exp) =
  (* Filter by type and scope *)
  let origType = CLv.typeOfUnsafe origExp in
  try
    topPtrCacheLookup (origType, origExp, NoOffset)
  with Not_found ->
    let aliases = 
      try 
        EH.find ptrExpCache origExp
      with Not_found ->
        let temp = Stat.time "resolve_exp" A.deref_exp origExp in
        EH.add ptrExpCache origExp temp;
        temp
    in 
    let results = List.fold_left 
      (fun curResults vi ->
         if (inScope vi) then
           let newLval = (Lvals.hostOfVar vi, NoOffset) in
           try
             let newType = Lvals.typeOfLvalUnsafe newLval in
             if (Ciltools.equal_type origType newType) then
               newLval :: curResults
             else
               curResults
           with 
               Errormsg.Error 
           | Failure _ ->
               curResults
         else
           curResults
      ) [] aliases in  
    TPH.add topPtrCache (origType, origExp, NoOffset) results;
    results        

let topPtrAddrsExp (origExp : Cil.exp) : (symAddr * Cil.offset) list =
  let results = topPtrDerefExp origExp in
  List.map (fun (host, off) ->
              (getAddr host false, off)
           ) results  


exception NotPtrConstPair

(** Calculate the new pointer created by ptr arith. Assumes v1 is a pointer
    and v2 is a constant *)
let doPtrArith (expectedTyp:Cil.typ) (ptrTyp:Cil.typ) 
    ((v1, v2):symVal * symVal) : symVal = 

  (* Convert an integer valued pointer offset to a field/array offset.
     May raise UnknownOffset if offset cannot be determined *) 
  let ptrFromOffset (i:int64) (addr, off) =
    let bitsBase, baseWidth = 
      match off with
        Field _ -> Cil.bitsOffset expectedTyp off
      | _ -> 0, 0 (* Ignore array index, and set to 0 offset *)
    in
    let bitsForI = (Cil.bitsSizeOf ptrTyp) * (Int64.to_int i) in
    (addr, Offset.bitsToOffset expectedTyp (bitsBase + bitsForI))     
  in

  match v1, v2 with
    Vtop, _ -> Vtop
  | Vbot, _ -> Vbot

  | _, Vbot 
  | _, Vtop -> (* could be more specific, e.g., all fields in base struct *)  
      (* Vtop *)
      v1

  (* TODO, grab host type from source of ptr target addr instead! *)
  | Vmustptr (addr, off), 
      Vval (Lvals.CConst (CInt64(i,ik,_))) -> begin
      try 
        Vmustptr (ptrFromOffset i (addr, off))
      with Offset.UnknownOffset ->
        Vtop
    end
  | Vmayptr (id, addrOffSet), 
        Vval (Lvals.CConst (CInt64(i,ik,_))) -> begin
      try 
        let newSet = AddrOffSet.fold 
          (fun (addr, off) curSet ->
             AddrOffSet.add (ptrFromOffset i (addr, off)) curSet
          ) addrOffSet AddrOffSet.empty in
        Vmayptr (freshPtrID (), newSet)
      with Offset.UnknownOffset ->
        Vtop
    end
  | Vextptr (id, addrOffSet), 
          Vval (Lvals.CConst (CInt64(i,ik,_))) -> begin
      try 
        let newSet = AddrOffSet.fold 
          (fun (addr, off) curSet ->
             AddrOffSet.add (ptrFromOffset i (addr, off)) curSet
          ) addrOffSet AddrOffSet.empty in
        Vextptr (freshPtrID (), newSet)
      with Offset.UnknownOffset ->
        Vtop
    end
  | _ ->
      raise NotPtrConstPair


(** Out of a list of types, pick a Ptr type that is somewhat specific
    and return the deref'ed type *)
let rec pickDerefedTyp tList =
  match tList with
    t :: [] -> begin
      match t with
        TPtr (TVoid _, _) ->
          TVoid []
      | TPtr (t', _) ->
          t'
      | _ ->
          t
    end
  | t :: tl -> begin
      match t with
        TPtr (TVoid _, _) ->
          pickDerefedTyp tl
      | TPtr (t', _) ->
          t'
      | _ ->
          pickDerefedTyp tl
    end
  | [] -> failwith "pickDerefedTyp: given an empty list!"


(** Reorder a pair of values so that a ptr value comes first, then
    a constant comes second. Also returns a flag if swapped
    May raise NotPtrConstPair if arg isn't a ptr/constant pair *)
let reorderPtrConstPair ((v1, v2):symVal * symVal) : symVal * symVal * bool =
  match (v1, v2) with 
    Vmustptr _, Vval e 
  | Vmayptr _ , Vval e
  | Vextptr _, Vval e 
  | Vtop, Vval e 
  | Vbot, Vval e when isConst e ->
      (v1, v2, false)
  | (Vval e, _) when isConst e ->
      (v2, v1, true)
  | _ ->
      raise NotPtrConstPair
        

exception PointerCastError

(* Don't want Vval expressions to blow up *)
let maxOpsInVval = 24


let lubOverTargets addrOffSet foo s =
  (* TODO: make side-effect free? *)
  let newVal, newState = AddrOffSet.fold
    (fun target (curVal, curSt) ->
       let newVal, newSt = foo target curSt in
       match curVal with
         None -> (Some newVal, newSt)
       | Some v -> (Some (combineVals v newVal), newSt)
    ) addrOffSet (None, s) in
  match newVal with
    None -> (Vtop, newState)
  | Some v -> (v, newState)

        
(** Evaluate a cil expression (ce) to a value, possibly creating
    new bindings in the symState *)
let rec eval (s:symState) exp (assumedTyp:Cil.typ option) : 
    symVal * symState = 
  match exp with
    (* Variable reference *)
    Lvals.CLval(Lvals.CVar(vi) as host, off) ->
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      (* TODO, compare w/ assumed typ *)
      let baseTyp = Lvals.typeOfLvalUnsafe (host, newOff) in
      evalAddr s baseTyp addr newOff

  | Lvals.CLval(Lvals.AbsHost _ as host, off) ->
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      let baseTyp = TPtr (TVoid [], []) in
      evalAddr s baseTyp addr newOff


      
  (* Simple pointer dereference *)
  | Lvals.CLval(Lvals.CMem(ptrExp) as host, off) -> 
      let ptrVal, newState = eval s ptrExp None in
      let newOff, isSum = CLv.canonicizeOff off in
      (* TODO, compare w/ assumed typ *)
      let baseTyp = Lvals.typeOfLvalUnsafe (host, newOff) in
      derefPtrVal ptrExp newOff baseTyp newState ptrVal

  (* Take addr of a cell *)
  | Lvals.CAddrOf (l) ->
      resolveAddrOfLval s l

  (* Start of an array, like taking the addr of the array *)
  | Lvals.CStartOf(l) ->
      resolveAddrOfLval s l

  | Lvals.CCastE(t, e) ->
      eval s e (Some t) (* re-eval w/ an assumed type *)

  (* Pointer arith *)
  | Lvals.CBinOp(PlusPI, ce1, ce2, typ) 
  | Lvals.CBinOp(IndexPI, ce1, ce2, typ) -> begin
      let v1, v2, newSt = eval2 s ce1 ce2 assumedTyp in
      (* Shouldn't need to reorder (ptr should be ce1, offset ce2) *)
      let ptrTyp = pickDerefedTyp [Lvals.typeOfUnsafe ce1] in
      try
        let expectedTyp = 
          match assumedTyp with
            Some (t) ->
              pickDerefedTyp [t; typ]
          | None ->
              pickDerefedTyp [typ]
        in
        (doPtrArith expectedTyp ptrTyp (v1, v2), newSt)
      with NotPtrConstPair ->
        (* Assume it's an array-like access? *)
        eval newSt ce1 assumedTyp
    end

  | Lvals.CBinOp(MinusPI, ce1, ce2, typ) -> begin
      let v1, v2, newSt = eval2 s ce1 
        (Lvals.CUnOp (Neg, ce2, Lvals.typeOfUnsafe ce2)) assumedTyp in
      (* Shouldn't need to reorder (ptr should be ce1, offset ce2) *)
      let ptrTyp = pickDerefedTyp [Lvals.typeOfUnsafe ce1] in
      try
        let expectedTyp = 
          match assumedTyp with
            Some (t) ->
              pickDerefedTyp [t; typ]
          | None ->
              pickDerefedTyp [typ]
        in
        (doPtrArith expectedTyp ptrTyp (v1, v2), newSt)
      with NotPtrConstPair ->
        (* Assume it's an array-like access? *)
        eval newSt ce1 assumedTyp
    end
 
  (* May be pointer arith... check *)
  | Lvals.CBinOp(PlusA, ce1, ce2, typ) -> begin
      let v1, v2, newSt = eval2 s ce1 ce2 assumedTyp in
      match v1, v2 with
        Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
        Vval (Lvals.CConst(CInt64(i2,_,_))) ->
          Vval (Lvals.CConst(CInt64(Int64.add i1 i2, ik1, None))), newSt
            
      (* Non-constant binops *) 
      | Vval y, Vval z -> 
          if ((Lvals.countOpsInExp y) + (Lvals.countOpsInExp z)  
              > maxOpsInVval) then
            Vtop, newSt
          else
            Vval (Lvals.CBinOp(PlusA, y, z, typ)), newSt

      | Vtop, _
      |  _, Vtop ->
           Vtop, newSt
             
      | Vbot, _
      | _, Vbot ->
          Vbot, newSt (* or return bottom state? *)  

      | _, _ ->
          (* Maybe it's ptr arith *)
          try 
            let v1, v2, switched = reorderPtrConstPair (v1, v2) in
            let ptrTyp = 
              if (switched) then
                pickDerefedTyp [Lvals.typeOfUnsafe ce1]
              else
                pickDerefedTyp [Lvals.typeOfUnsafe ce2]
            in
            let expectedTyp =
              match assumedTyp with
                Some (t) ->
                  pickDerefedTyp [t; typ]
              | None ->
                  pickDerefedTyp [typ]
            in
            (doPtrArith expectedTyp ptrTyp (v1, v2), 
             newSt)
          with NotPtrConstPair ->
            logError "eval PlusA, not used as pointer arith";
            (Vval exp, newSt)
    end
      
  (* Sorta pointer arith *)
  | Lvals.CBinOp(MinusPP, ptr1, ptr2, typ) ->
      (Vtop, s) (* Don't know the actual spacing between pt'ed to addresses *)

  (* Ops *)
  | Lvals.CUnOp(unop,ce,t) -> begin
      let v, newSt = eval s ce None in
      match unop, v with
        (* Try to eval constants *)
        Neg, Vval (Lvals.CConst (CInt64(i1,ik1,_))) ->
          Vval (Lvals.CConst (CInt64(Int64.neg i1, ik1, None))), newSt
      | BNot, Vval (Lvals.CConst (CInt64(i1,ik1,_))) ->
          Vval (Lvals.CConst (CInt64(Int64.lognot i1, ik1, None))), newSt
      | LNot, Vval (Lvals.CConst (CInt64(i1, ik1, None))) ->
          let newI = not_of_int64 i1 in
          Vval (Lvals.CConst (CInt64(newI, ik1, None))), newSt
      | _, Vval innerOperand -> 
          if (Lvals.countOpsInExp innerOperand > maxOpsInVval) then
            Vtop, newSt
          else
            Vval (Lvals.CUnOp(unop, innerOperand, t)), newSt

      | _, Vtop ->
          Vtop, newSt
      | _, Vbot ->
          Vbot, newSt
      | _, _ -> 
          (* There was one case where a guy cast a struct of two longs
             to a single long long, then negated that number... 
             why wasn't it caught by the Vval innerOperand case above? *)
          logError ("eval: unop can't const-fold: " ^ (Lvals.string_of_exp exp));
          Vtop, newSt
    end

  | Lvals.CBinOp(bop,ce1,ce2,t) -> begin
      let v1, v2, newSt = eval2 s ce1 ce2 assumedTyp in
      match bop, v1, v2 with
        (* Evaluate constant ops *)
        (PlusA, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.add i1 i2, ik1, None))), newSt
      | (MinusA, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.sub i1 i2, ik1, None))), newSt
      | (Mult, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.mul i1 i2, ik1, None))), newSt
      | (Div, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) -> begin
          try
            Vval (Lvals.CConst(CInt64(Int64.div i1 i2, ik1, None))), newSt
          with Division_by_zero ->
            Vbot, newSt
        end
      | (Mod, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) -> begin
          try
            Vval (Lvals.CConst(CInt64(Int64.rem i1 i2, ik1, None))), newSt
          with Division_by_zero ->
            Vbot, newSt
        end
      | (Shiftlt, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.shift_left i1 
                               (Int64.to_int i2), ik1, None))), newSt
      | (Shiftrt, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.shift_right i1 
                               (Int64.to_int i2), ik1, None))), newSt
      | (BAnd, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.logand i1 i2, ik1, None))), newSt
      | (BOr, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.logor i1 i2, ik1, None))), newSt
      | (BXor, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (Lvals.CConst(CInt64(Int64.logxor i1 i2, ik1, None))), newSt
            
      (* Lvals.CConstant tests *)
      | (Lt, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (exp_of_bool (i1 < i2)), newSt 
      | (Le, Vval (Lvals.CConst(CInt64(i1,ik1,_))),
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (exp_of_bool (i1 <= i2)), newSt
      | (Gt, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (exp_of_bool (i1 > i2)), newSt
      | (Ge, Vval (Lvals.CConst(CInt64(i1,ik1,_))),
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (exp_of_bool (i1 >= i2)), newSt
      | (Eq, Vval (Lvals.CConst(CInt64(i1,ik1,_))),
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (exp_of_bool (i1 == i2)), newSt
      | (Ne, Vval (Lvals.CConst(CInt64(i1,ik1,_))), 
         Vval (Lvals.CConst(CInt64(i2,_,_)))) ->
          Vval (exp_of_bool (i1 != i2)), newSt
            (* What about non-constant versions of these tests? Ask ATP? *)
            
      (* Non-constant binops *) 
      | op, Vval y, Vval z -> 
          if ((Lvals.countOpsInExp y) + (Lvals.countOpsInExp z) > maxOpsInVval) then
            Vtop, newSt
          else
            Vval (Lvals.CBinOp(op, y, z, t)), newSt

      | op, Vtop, _
      | op, _, Vtop ->
          Vtop, newSt
            
      | op, Vbot, _
      | op, _, Vbot ->
          Vbot, newSt (* or return bottom state? *)
            
      | op, _, _ ->
          logError "eval bin op, operands are non-Vval";
          (Vval exp, newSt)
            (* TODO: Make sure this does not create expressions that
               involve more than initial values *)
    end 

  (* Misc stuff *)
  | Lvals.CAlignOfE (e) -> begin
      match eval s e assumedTyp with
        Vval vexp, newSt -> 
          Vval (Lvals.CAlignOfE (vexp)), newSt
      | _ , newSt -> 
          Vval (exp), s
    end

  | Lvals.CSizeOfE (e) -> begin 
      match eval s e assumedTyp with
        Vval vexp, newSt -> 
          Vval (Lvals.CSizeOfE (vexp)), newSt
      | _ , newSt -> 
          Vval (exp), s
    end

  | Lvals.CAlignOf _
  | Lvals.CSizeOf _
  | Lvals.CSizeOfStr _ (* What if the sizeof the str is known? *)
  | Lvals.CConst _ -> 
      Vval exp, s

and eval2 (s:symState) ce1 ce2 (assumedT:Cil.typ option) 
    : (symVal * symVal * symState) =
  let v1, newSt1 = eval s ce1 None in
  let v2, newSt2 = eval newSt1 ce2 None in
  (v1, v2, newSt2)

(** Check if lval should be trimmed for termination *)
and evalPtrTarget outerOff baseTyp (addr, innerOff) s =
  let finalAddr, fOff = limitAddrOff baseTyp addr outerOff innerOff in
  evalAddr s baseTyp finalAddr fOff    
    
(** Assuming ptrVal should be treated as a pointer, deref and read
    the value stored at the target *)
and derefPtrVal ptrExp outerOff baseTyp (s:symState) ptrVal : symVal * symState =
  match ptrVal with
    Vmustptr (addr, innerOff) ->
      evalPtrTarget outerOff baseTyp (addr, innerOff) s 
        
  | Vmayptr (id, addrOffSet) ->
      (* get each value and LUB them *)
      lubOverTargets addrOffSet (evalPtrTarget outerOff baseTyp) s
        
  | Vextptr (id, addrOffSet) ->
      lubOverTargets addrOffSet (evalPtrTarget outerOff baseTyp) s

  | Vstruct _ ->
      logError ("eval: deref'ing a struct? " ^
                    (Lvals.string_of_exp ptrExp));
      (Vtop, s)

  | Vbot ->
      logError ~prior:3 ("eval: nullptr dereference! " ^ 
                      (Lvals.string_of_exp ptrExp));
      (Vtop, s)

  | Vval e when isConst e ->
      logError ~prior:3 ("eval: nullptr dereference! " ^ 
                      (Lvals.string_of_exp ptrExp));
      (Vtop, s)

  | Vtop ->
      (Vtop, s)
      (* Don't do this for now
      let newLvals = topPtrDerefLval baseTyp ptrExp outerOff in 
      List.fold_left 
        (fun (curVal, curSt) anLval ->
           let newVal, newSt =
             eval curSt (Lvals.CLval anLval) (Some baseTyp) in
           match curVal with 
             None -> (Some newVal, newSt)
           | Some v -> (Some (combineVals v newVal), newSt)
        ) (None, s) newLvals
      *)

  | Vval exp ->
      try
        (* convert exp to a pointer first *)
        let newPtrVal, newSt = castAsPointer s exp in
        derefPtrVal ptrExp outerOff baseTyp newSt newPtrVal
      with PointerCastError ->
        logError ("derefPtrVal: Vval not treated as ptr: " ^ 
                      (Lvals.string_of_exp exp));
        (Vtop, s)


(** Try to treat an expression as a pointer value *) 
and castAsPointer (s:symState) exp : symVal * symState = 
  match exp with
    Lvals.CLval(host,off) -> begin
      (* Don't try to evaluate exp... exp SHOULD only be in terms of
         unknown initial values *)
      let newOff, _ = CLv.canonicizeOff off in
      let truncOff = CLv.simplifyOff newOff in
      (makePtrValue (host, truncOff), s)
    end

  | Lvals.CAddrOf (l)  (* Shouldn't get these in the first place... *)
  | Lvals.CStartOf(l) ->
      (* known to happen in linux 2.6.15's 
         __read_page_state : unsigned long (unsigned long ) *)
      logError "AddrOf/StartOf found as part of Vval";
      resolveAddrOfLval s l

  | Lvals.CCastE(_,e) -> 
      castAsPointer s e (* treat as if cast isn't there and retry *)
        
  (* Ops *)
  | Lvals.CBinOp(PlusPI, host, offsets, typ)
  | Lvals.CBinOp(MinusPI, host, offsets, typ)
  | Lvals.CBinOp(IndexPI, host, offsets, typ) 
  | Lvals.CBinOp(PlusA, host, offsets, typ) ->
      (* Assume it is pointer arith, and only use first operand *)
      castAsPointer s host

  | Lvals.CBinOp(_)
  | Lvals.CUnOp(_) -> 
      raise PointerCastError

  (* Constants *)
  | Lvals.CAlignOfE _
  | Lvals.CSizeOfE _
  | Lvals.CAlignOf _
  | Lvals.CSizeOf _
  | Lvals.CSizeOfStr _ 
  | Lvals.CConst _ -> (* Ignore string literals since they are constant? *)
      raise PointerCastError
        

(** Get the ptr value that points to (addr + offset) an lval, and possibly 
    a new state due to lazy creation *)
and resolveAddrOfLval (s:symState) lval : symVal * symState =

  let rec addOffToPointer ptrExp off curState ptrVal =
    match ptrVal, off with
      _ , NoOffset ->      (* Should maybe be more careful if fst not ptr? *)
        (ptrVal, curState)
    | Vmustptr (addr, innerOff), _ ->
        (Vmustptr (addr, concatOffset addr off innerOff), curState)
          
    | Vmayptr (id, addrOffSet), _ ->
        let newSet = AddrOffSet.fold 
          (fun (addr, innerOff) curSet ->
             AddrOffSet.add (addr, concatOffset addr off innerOff) curSet
          ) addrOffSet AddrOffSet.empty in
        (Vmayptr (freshPtrID (), newSet), curState)
          
    | Vextptr (id, addrOffSet), _ ->
        let newSet = AddrOffSet.fold 
          (fun (addr, innerOff) curSet ->
             AddrOffSet.add (addr, concatOffset addr off innerOff) curSet
          ) addrOffSet AddrOffSet.empty in
        (Vextptr (freshPtrID (), newSet), curState)
          
    | Vtop, _ -> 
        (Vtop, curState)

    (* trying to calculate the bits of offset from base of struct *)
    | Vval (Lvals.CConst _), _ 
    | Vbot, _ -> begin
        try
          let t = Lvals.typeOfUnsafe ptrExp in
          let bitsOff, bitsWidth = Cil.bitsOffset t off in
          let bytesOff = bitsOff / 8 in
          (Vval (Lvals.abs_of_exp (Cil.integer bytesOff)), curState)
        with 
          SizeOfError _
        | Errormsg.Error ->
            logError ("resolveAddrOfLval: offsetOf calculation failed" ^ 
                          (Lvals.string_of_exp ptrExp));
            (Vtop, curState)
      end


    (* Shouldn't actually get Vstruct *)
    | Vstruct _, _ ->
        logError ("resolveAddrOfLval: can't deref non-pointer val " ^
                        (Lvals.string_of_exp ptrExp));
        (Vtop, curState)
          
    (* try to treat as a pointer *)
    | Vval exp, _ -> begin
        try
          let newPtrVal, newSt = castAsPointer curState exp in
          addOffToPointer ptrExp off newSt newPtrVal
        with PointerCastError ->
          logError ("resolveAddrOfLval: Vval not treated as ptr: " ^ 
                          (Lvals.string_of_exp ptrExp));
          (Vtop, curState)
      end
  in
  match lval with
    ((Lvals.CVar _) as host, off) 
  | ((Lvals.AbsHost _) as host, off) ->
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      (Vmustptr (addr, newOff), s)
        
  | host, off when isNullHost host ->
      (* If it was a null ptr, leave as a nullptr *)
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      (Vmustptr (addr, newOff), s)

  | Lvals.CMem(ptrExp), off ->
      (* don't need to make up addrs, just reuse the old pointer, 
         and tack on the new outer offset *)
      let ptrVal, newState = eval s ptrExp None in
      let newOff, _ = CLv.canonicizeOff off in
      addOffToPointer ptrExp newOff newState ptrVal



(*********************************************************
 * Test / Debug code
 *********************************************************)

let printSymState ({store = st; assumptions = a;} as state) =
  if (state == bottomSymState) then
    logStatus "State is $BOTTOM\n"
  else
    AddrMap.iter
      (fun addr v ->
         (* Print binding *)
         printAddr addr;
         printVal  v;
      ) st      


(*********************************************************
 * Utility functions on values and state
 *********************************************************)

(** Thrown when two expressions cannot be coaxed into unification *)
exception Not_unifiable


(** Compare the values held in two cells
    If they don't refer to the exact same symbolic variables, check if 
    the symbolic variables are at least equivalent
    @return Some (-1) if v1 is a subset of the values of v2
            Some (0)  if v1 = v2
            Some (1)  if v2 is a subset of the values of v1
            None      if incomparable
*)
let rec compareVals v1 v2 : int option =
  match v1, v2 with
    Vmustptr addrOff1, Vmustptr addrOff2 ->
      if ((compareAddrOff addrOff1 addrOff2) == 0) then
        (* TODO don't ignore summary flag while comparing? *)
        Some(0)
      else
        None

  | Vmayptr (id1, ptset1), Vmayptr (id2, ptset2) ->
      if (AddrOffSet.equal ptset1 ptset2) then (* TODO: use IDs instead *)
        Some (0)
      else if (AddrOffSet.subset ptset1 ptset2) then 
        Some (-1)
      else if (AddrOffSet.subset ptset2 ptset1) then
        Some (1)
      else
        None

  | Vextptr (id1, ptset1), Vextptr (id2, ptset2) ->
      if (AddrOffSet.equal ptset1 ptset2) then (* TODO: use IDs instead *)
        Some (0)
      else if (AddrOffSet.subset ptset1 ptset2) then 
        Some (-1)
      else if (AddrOffSet.subset ptset2 ptset1) then
        Some (1)
      else
        None

  | Vmustptr mustPtr, Vmayptr (id, ptset)
  | Vmustptr mustPtr, Vextptr (id, ptset) ->
      if (AddrOffSet.mem mustPtr ptset) then
        Some(-1)
      else
        None

  | Vmayptr (id, ptset), Vmustptr mustPtr
  | Vextptr (id, ptset), Vmustptr mustPtr ->
      if (AddrOffSet.mem mustPtr ptset) then
        Some(1)
      else
        None

  | Vmayptr (mayID, mayPtSet), Vextptr (extID, extPtSet) ->
      if (AddrOffSet.subset mayPtSet extPtSet) then
        Some(-1)
      else
        None

  | Vextptr (extID, extPtSet), Vmayptr (mayID, mayPtSet) ->
      if (AddrOffSet.subset mayPtSet extPtSet) then
        Some(1)
      else
        None

  (* Assume constants are null pointers *)
  | Vmustptr _, Vval e  
  | Vmayptr _, Vval e
  | Vextptr _, Vval e when isConst e ->
      Some (1)
  | Vval e, Vextptr _
  | Vval e, Vmustptr _ 
  | Vval e, Vmayptr _ when isConst e ->
      Some (-1)


  | Vstruct offmap1, Vstruct offmap2 ->
      if (OffsetMap.subset compareVals offmap1 offmap2) then
        Some (-1)
      else
        None

  | Vval exp1, Vval exp2 -> 
      (* What if concrete versions are EQ, but exps themselves aren't?
         Is that possible? Intuition is no for now... *)
      if(expEqual exp1 exp2) then
        Some (0)
      else
        None

  | Vtop, Vtop -> Some (0) 
      (* are they really compareable? ... 
         for termination, and to match the effects of combineVals *)

  | Vbot, Vbot -> Some (0)

  | Vtop, _ -> Some (1)

  | _, Vtop -> Some (-1)

(* ... *)
  | Vbot, _ -> Some (-1)

  | _, Vbot -> Some (1)

  |  _, Vstruct _ ->
      (* Happens when fields have not been touched in one path, 
         but have in the other *)
      Some (-1)

  | Vstruct _, _ ->
      Some (1)

  | _, _ ->
      logError "compareVals: addrs -> to different types of values";
      None

let equalVals v1 v2 =
  match compareVals v1 v2 with Some 0 -> true | Some _ | None -> false

(** Check if state s1 is a subset of s2 *)
let statesSubset (s1:symState) (s2:symState) : bool =
  if(s1 == bottomSymState) then true
  else if (s2 == bottomSymState) then false
  else 
    let store1 = s1.store in
    let store2 = s2.store in
    (* TODO: treat entries w/ no matched mapping and create initial val *)
    AddrMap.subset compareVals store1 store2
    
let statesEqual (s1:symState) (s2:symState) : bool =
  if(s1 == s2) then true
  else 
    let store1 = s1.store in
    let store2 = s2.store in
    AddrMap.equal equalVals store1 store2
      

(*********************************************************
 * Intra-proc Dataflow Analysis
 *********************************************************)

(**** Summary Support / Substitution ****)


exception NotPointer

let rec lvalOfActual (exp:Cil.exp) : Cil.lval =
  match exp with
    Lval lv -> lv
  | CastE (_, e) -> lvalOfActual e
  | _ ->
(*      logError ("substLval: actual not lval " ^ (string_of_exp exp)); *)
      raise CLv.SubstInvalidArg


(****** Actual -> Formal Substitution ******)
let rec substExp (args:Cil.exp list) (expFromOtherPlanet:Lvals.aExp) : Lvals.aExp =
  match expFromOtherPlanet with
    Lvals.CLval(lv) ->           
      let newLval = substLval args lv in
      (Lvals.CLval(newLval))
        
  | Lvals.CAddrOf (l) ->
      Lvals.mkAddrOf (substLval args l)

  | Lvals.CStartOf(l) ->
      Lvals.CStartOf (substLval args l)
        
  | Lvals.CCastE(t, e) ->
      Lvals.CCastE (t, substExp args e)
        
  | Lvals.CAlignOfE(e) ->
      Lvals.CAlignOfE (substExp args e)
        
  | Lvals.CSizeOfE(e) ->
      Lvals.CSizeOfE (substExp args e)
        
  | Lvals.CUnOp (unop, e, t) ->
      let newExp = substExp args e in
      Lvals.CUnOp (unop, newExp, t)
        
  | Lvals.CBinOp (bop, e1, e2, t) ->
      let newExp1 = substExp args e1 in
      (try
         let newExp2 = substExp args e2 in
         Lvals.CBinOp(bop, newExp1, newExp2, t)
       with CLv.SubstInvalidArg as e ->
         (match bop with
            PlusPI
          | MinusPI
          | IndexPI ->
              logErrorF "substExp: trimming ptrArith offset %s\n" 
                (Lvals.string_of_exp e2) ;
              newExp1 (* allow omission of constant offset *)
          | _ ->
              let expStr = Lvals.string_of_exp expFromOtherPlanet in 
              logError
                ("substExp encountered unknown exp: " ^ expStr);
              raise e
         )
      )
  | Lvals.CAlignOf _
  | Lvals.CSizeOf _
  | Lvals.CSizeOfStr _
  | Lvals.CConst _ ->
      raise CLv.SubstInvalidArg

and substLval (args:Cil.exp list) (lvalFromOtherPlanet:Lvals.aLval) : Lvals.aLval =
  match lvalFromOtherPlanet with 
    Lvals.AbsHost _, _ ->
      (* Treat as global... no subs *)
      lvalFromOtherPlanet

  | (Lvals.CVar(vi), formOff) -> begin
      let scope = decipherScope (Lvals.var_of_abs vi) in
      match scope with
        SGlobal -> 
          lvalFromOtherPlanet
      | SFormal n -> begin
          (* Only substitute if the actual is an lval *)
          let actual = List.nth args n in
          let lv = lvalOfActual actual in 
          let (h, o) = Lvals.abs_of_lval lv in
(*          (h, Cil.addOffset formOff o) *)
          try Lvals.attachOffset h (Cil.addOffset formOff o)
          with
            CLv.OffsetMismatch om ->
              logError ~prior:3 
                ("substLval: " ^ CLv.string_of_offsetMiss om ^ " 1");
              raise CLv.SubstInvalidArg
          | Failure s ->
              logError ("substLval: failure " ^ s);
              raise CLv.SubstInvalidArg
        end
      | _ ->
          (* It was local var of the other function -- can't substitute. *)
          logError ("substLval: local variable " ^ (Lvals.var_of_abs vi).vname
            ^ " stayed in summary?");
          raise CLv.SubstInvalidArg
    end


  | (Lvals.CMem (Lvals.CLval(Lvals.CVar(vi), NoOffset)), outerOff) -> begin
      let scope = decipherScope (Lvals.var_of_abs vi) in
      match scope with
        SGlobal -> 
          lvalFromOtherPlanet
      | SFormal n -> begin
          let actual = Lvals.abs_of_exp (List.nth args n) in
          try
            Lvals.mkMemChecked actual outerOff
          with 
            CLv.OffsetMismatch om ->
              logError ~prior:3
                ("substLval: " ^ CLv.string_of_offsetMiss om ^ " 2");
              raise CLv.SubstInvalidArg
        end
      | _ ->
          logError ("substLval: local variable " ^ (Lvals.var_of_abs vi).vname
                      ^ " stayed in summary?");
          raise CLv.SubstInvalidArg
    end

  | (Lvals.CMem(ptrExp), outerOff) ->
      try
        let newExp = substExp args ptrExp in
        Lvals.mkMemChecked newExp outerOff
      with 
        CLv.OffsetMismatch om ->
          logError ~prior:3
            ("substLval: " ^ CLv.string_of_offsetMiss om ^ " 3");
          raise CLv.SubstInvalidArg


let rec substValue state (args:Cil.exp list) (valFromOtherPlanet:symVal) :
    symVal * symState =
  let handlePtr target curSt =
    let addr, off = target in 
    if isNullAddr addr 
    then (Vmustptr target, state) (* leave null ptrs alone *)
    else try 
      (* Update the target(s) of the pointer (syntactic substitution),
         then get a pointer back by taking the addrOf the new target *)
      let host = hostOfAddr addr in
      let newLval = substLval args (host, off) in
      resolveAddrOfLval state newLval
    with CLv.SubstInvalidArg ->
      (Vtop, state)
  in

  match valFromOtherPlanet with
    Vtop -> (Vtop, state)
  | Vbot -> (Vbot, state)
  | Vval e -> begin
      (* First syntactically substitute formals w/ actuals.
         Then evaluate expression under current state. 
         CHECK: ... or evaluate before substituting or just substitute? *)
      try 
        let newExp = substExp args e in
        eval state newExp None
      with CLv.SubstInvalidArg ->
        (Vtop, state) 
          (* exp may be in terms of other functions locals *)
    end

  | Vstruct offMap ->
      (* Update the individual values of the map *)
      let newMap, newState = OffsetMap.fold 
        (fun k v (curMap, curState) ->
           let newVal, newState = substValue curState args v in
           (OffsetMap.add k newVal curMap, newState)
        ) offMap (OffsetMap.empty, state) in
      (Vstruct newMap, newState)

  | Vmustptr target -> handlePtr target state

  | Vmayptr (_, aoSet) -> lubOverTargets aoSet handlePtr state

  | Vextptr (_, aoSet) -> lubOverTargets aoSet handlePtr state


(** Convert the symbolic state lval (addr, offset) into
    an external lval. May raise CLv.OffsetMismatch   *)
let concretizeLval (ptAddr, ptOff) = 
  let host = hostOfAddr ptAddr in
  let finalOff, _ = CLv.canonicizeOff ptOff in
  (host, finalOff)


(** Convert a pointer that points to an (addr, offset) pair 
    into an expression *)
let concretizePointerTo ptrTarget = 
  (Lvals.mkAddrOf (concretizeLval ptrTarget))



(** Get the abstract PTA nodes that are the targets of this 
    pointer expression *)
let rec ptrExpToAbs ptrExp =
  (try Lvals.deref_absExp ptrExp
   with A.UnknownLoc -> 
     logError ~prior:3 ("mainAliases -- Vtop: unable to deref: " ^
                          (Lvals.string_of_exp ptrExp));
     raise NotPointer
  )
  
(** Get the canonical aliases of the given pointer "origExp" *)
let rec mainAliases origExp state v : (bool * (Lvals.aExp list)) =
  match v with
    Vval symExp -> begin
      (* try to promote to a pointer and retry *)
      try 
        let newVal, newState = castAsPointer state symExp in
        mainAliases origExp newState newVal 
      with PointerCastError ->
        raise NotPointer
    end

  | Vstruct _ -> 
      raise NotPointer

  | Vmustptr target ->
      (true, 
       try [concretizePointerTo target]
       with CLv.OffsetMismatch _ -> [] )

  | Vmayptr (id, addrOffSet) ->
      (false,
       try AddrOffSet.fold 
         (fun target cur ->
            let e = concretizePointerTo target in
            List_utils.addOnceP expEqual cur e)
         addrOffSet []
       with CLv.OffsetMismatch _ -> []
      )

  | Vextptr (id, addrOffSet) ->
      (* TODO See if it should be expanded even more? *)
      (true,
       try AddrOffSet.fold 
         (fun target cur ->
            let e = concretizePointerTo target in
            List_utils.addOnceP expEqual cur e)
         addrOffSet []
       with CLv.OffsetMismatch _ -> []
      )

  | Vtop ->
      (* Get the abstract PTA node that represents the 
         location of the origExp *)
      (match origExp with
         Lvals.CAddrOf (Lvals.CMem innerExp, off)
       | Lvals.CStartOf (Lvals.CMem innerExp, off) ->
           let absTargs = ptrExpToAbs innerExp in
           (true, 
            List.map (fun node -> Lvals.CLval (Lvals.AbsHost node, off)) absTargs)
       | Lvals.CAddrOf _ 
       | Lvals.CStartOf _ -> 
           (true, [origExp])
       | Lvals.CCastE (t, e) ->
           mainAliases e state v
       | _ ->
           let absTargs = ptrExpToAbs origExp in
           (true, List.map 
              (fun node -> 
                (Lvals.mkAddrOf (Lvals.AbsHost node, NoOffset))) absTargs)
      )

  | Vbot -> 
      (false, [])


(** Get the addr of the main lvals associated with the _value_
    of the given exp (or return nothing if it's not an lval).
    Should just call this, getTargets?
*)
let getAliasesExp state (exp:Lvals.aExp) : (bool * (Lvals.aExp list)) =
  try
    let theVal, newSt = eval state exp None in
    mainAliases exp newSt theVal
  with NotPointer ->
    (* if it's not a pointer, then just return the original lval as we
       may have mis-categorized it as an int or whatever *)
    match exp with
      Lvals.CLval _ ->
        (true, [exp])
    | Lvals.CStartOf _
    | Lvals.CAddrOf _ -> 
        logError 
          ("getAliasesExp: AddrOf not pointer? " ^ Lvals.string_of_exp exp);
        (true, [exp])
    | _ ->
        (true, [])

let getAliasesLval state lval valueAlias : (bool * (Lvals.aExp list)) =
  match lval with
    Lvals.CVar _, _ ->
      let lvExp = Lvals.CLval lval in
      if valueAlias then
        let mustAlias, aliases = getAliasesExp state lvExp in
        (mustAlias, aliases)
      else (true, [lvExp])

  | Lvals.AbsHost _, _ ->
      (true, [Lvals.CLval lval])

  | Lvals.CMem ptrExp, off ->
      (* get the main aliases for the ptrExp, and reconstruct *)
      let mustAlias, aliases = getAliasesExp state ptrExp in
      let results = List.fold_left
        (fun cur aliasPtr -> 
           try 
             let canonLv = Lvals.mkMemChecked aliasPtr off in
             let (simpleH, simpleO), _ = Lvals.simplifyLval canonLv in
             List_utils.addOnceP expEqual cur (Lvals.CLval (simpleH, simpleO))
           with
             CLv.OffsetMismatch _ ->
               cur
        ) [] aliases in
      (mustAlias, results)


(** Substitute the formals in lvalWithFormal w/ the actuals, 
    given the current SymEx state. Also,  *)
let substActForm state actuals lvalWithFormal : bool * Lvals.aExp list =
  let mustAlias, results = 
    match Lvals.getScope lvalWithFormal with
      SGlobal -> (true, [Lvals.CLval lvalWithFormal])
    | SFormal n ->
        (try
           let substituted = substLval actuals lvalWithFormal in
           let mustAlias, results = getAliasesLval state substituted false in
           if results = [] then
             logError ~prior:3 ("substActForm returned 0 results for: " ^
                                  (Lvals.string_of_lvscope lvalWithFormal))
           ;
           (mustAlias, results)
         with CLv.SubstInvalidArg -> begin
           let arg = List.nth actuals n in
           logError ~prior:3 ("substActForm unsubstitutable arg: " ^
                                (string_of_exp arg) ^ " formal: " ^ 
                                (Lvals.string_of_lval lvalWithFormal));
           (true, [])
         end
        ) 
    | _ -> 
        (* It was local var of the other function -- can't substitute. *)
        logError ("substActForm: local variable " ^ 
                    (Lvals.string_of_lval lvalWithFormal) 
                  ^ " stayed in summary?");
        (true, [])
  in
  mustAlias, NULL.filterNulls results
  

(** Extract only the lvals from the list of expressions *)
let lvalsOfExps exps =
  List.fold_left 
    (fun cur e -> match e with 
       Lvals.CLval lv -> lv :: cur 
     | _ -> cur) [] exps


(**** Actual Dataflow ****)

module SymStateDF = struct

  let name = "calculate symbolic state"

  let debug = ref debug 

  (** Use symex state *)
  type t = symState

  (** Make a hashtable of dataflow facts for each statement
      Indexed by statement id *)
  let stmtStartData: t Inthash.t = Inthash.create 17

  (* Flow insensitive (within function) version of DF data *)
  let fiDFData: t ref = ref emptySymState

  (** Set all state to $BOTTOM, except the entry stmt is set to INPUT *)
  let initStmtStartData input =
    fiDFData := emptySymState;
    Inthash.clear stmtStartData;
    (* Assume first stmt in the list is the entry stmt *)
    match !curFunc.sallstmts with
      hd :: tl ->
        Inthash.add stmtStartData hd.sid input;
        List.iter 
          (fun stmt ->
             if (stmt.preds == []) then
               Inthash.add stmtStartData stmt.sid input
             else
               Inthash.add stmtStartData stmt.sid bottomSymState
          ) tl
    | _ -> ()

  let copy (d: t) = d
     
  let pretty () (d: t) = Pretty.nil

  let computeFirstPredecessor (s: stmt) (d: t) : t = 
    d

  (** Combine old fact at this statement, w/ newD. Also, detect 
      fixed point *)
  let combinePredecessors (s: stmt) ~(old: t) (newD: t) : t option =
    (* Try to combine FI df data *)
    if not (Stat.time "SS subset test" (statesSubset newD) !fiDFData) then
      fiDFData := Stat.time "SS combineStates" (combineStates !fiDFData) newD
    ;
    (* Now check if FS df data is different *)
    if(Stat.time "SS subset test" (statesSubset newD) old) then
      None
    else begin
      if !inspect then begin
        logStatus "Inspecting SS: state before combining";
        logStatus ((string_of_stmt s));
        printSymState old
      end;
      let comboS = Stat.time "SS combineStates" 
        (combineStates old) newD in
      if !inspect then begin
        logStatus "Inspecting SS: state after combining";
        printSymState comboS
      end;
      Some (comboS)
    end     

  (** Handle an lhsLval = rhsVal, in a given state *) 
  let handleAssign state lhsLval rhsVal =
    let rec assignToPointerTarget ptrExp off baseTyp curState ptrVal =
      match ptrVal with
        Vmustptr (addr, innerOff) ->
          (* TODO, check if the target is a summary node... 
             don't do strong update if that's the case      *)
          let newOff = concatOffset addr off innerOff in
          assignVar curState addr newOff rhsVal
            
      | Vmayptr (id, offAddrSet) ->
          (* set values to LUB of old value and rhs for each addr...
             TODO: can strong update if only 1 target and not summ node? *)
          AddrOffSet.fold 
            (fun (addr, innerOff) curSt ->
               (* Check if lval should be trimmed for termination *)
               let finalAddr, fOff =
                 limitAddrOff baseTyp addr off innerOff in
               let curVal, midState = evalAddr curSt baseTyp finalAddr fOff in
               let newVal = combineVals rhsVal curVal in
               assignVar midState finalAddr fOff newVal
            ) offAddrSet curState
            
      | Vextptr (id, offAddrSet) ->
          (* set values to LUB of old value and rhs for each addr...
             TODO: can strong update if only 1 target and not summ node? *)
          AddrOffSet.fold 
            (fun (addr, innerOff) curSt ->
               (* Check if lval should be trimmed for termination *)
               let finalAddr, fOff =
                 limitAddrOff baseTyp addr off innerOff in
               let curVal, midState = evalAddr curSt baseTyp finalAddr fOff in
               let newVal = combineVals rhsVal curVal in
               assignVar midState finalAddr fOff newVal
            ) offAddrSet curState

      | Vtop -> 
          (* LUB possible old value w/ rhs *)
          (* TODO: Do something different w/ pointers of TOP value...
          let baseType = CLv.typeOfLvalUnsafe lhsLval in
          let addrOffs = topPtrAddrsLval baseType ptrExp off in
          List.fold_left 
            (fun curSt (addr, innerOff) ->
               let finalAddr, fOff =
                 limitAddrOff baseTyp addr off innerOff in
               let curVal, midState = evalAddr curSt baseTyp finalAddr fOff in
               let newVal = combineVals rhsVal curVal in
               assignVar midState finalAddr fOff newVal
            ) curState addrOffs
          *)
          curState

      | Vval e when isConst e ->
          logError ~prior:3 ("handleAssign: nullptr deref " ^
                          (string_of_exp ptrExp));
          curState (* not bottomSymState *)

      | Vbot -> 
          logError ~prior:3 ("handleAssign: nullptr deref " ^
                          (string_of_exp ptrExp));
          curState (* not bottomSymState *)

      | Vstruct _ ->
          logError ("handleAssign: trying to store to a non-ptr: " ^
                          (string_of_exp ptrExp));
          curState

      | Vval exp ->
          (* convert to a pointer *)
          try 
            let newPtrVal, newState = castAsPointer curState exp in
            assignToPointerTarget ptrExp off baseTyp newState newPtrVal
          with PointerCastError ->
            logError ("handleAssign: Vval not treated as ptr: " ^ 
                            (string_of_exp ptrExp));
            curState
    in
    match lhsLval with
      (* [COPY] (vi.off) = rhsVal *)
      (Var(vi), off) ->
        let host = Lvals.hostOfVar vi in
        let lhsOff, isSum = CLv.canonicizeOff off in
        let lhsAddr = getAddr host isSum in
        let newState = assignVar state lhsAddr lhsOff rhsVal in
        newState
        
      (* [STORE] *e.outerOff = rhsVal *)
    | (Mem(ptrExp), outerOff) ->
        let pe = Lvals.abs_of_exp ptrExp in
        let lhs, lhsState = eval state pe None in
        let newOff, _ = CLv.canonicizeOff outerOff in
        let baseTyp = CLv.typeOfLvalUnsafe (Mem ptrExp, newOff) in
        assignToPointerTarget ptrExp newOff baseTyp lhsState lhs


  (** handle the return value portion of a function call *)
  let handleCallRet inState ret_option args fkey =
    (* Assume x = malloc has been converted to x = &allocSiteGlobal *)
    match ret_option with
      Some (lv) ->
        let summaryRetVal = SS.sum#find fkey in
        let rhs, midSt = substValue inState args summaryRetVal in
        handleAssign midSt lv rhs
    | None ->
        inState


  let havoc curSt (targHost, targOff) =
    assignVar curSt targHost targOff Vtop


  (** handle the modifications to actuals in a function call *)
(*

  substitute the actual, then eval and see what target was modded
  
  so if the summary says ( *(f.off1) ).off2 was modded,
  the actual is some exp
  find &(( *(exp + off1) ).off2) and smash the values held there?

  simple examples

  1) *f is modified and f == &x 
     subst gives *(&x) == x, 
     so find the addrOf (x) == ptr (x) 
     smash x

  2) *f is modified and f == NULL
     subst gives *NULL
     so addrOf( *NULL ) gives BOTTOM
     don't smash anything

  3) f is modified and f == &x
     subst / taking addrOf fails, so don't mod anything...
     (actually, f shouldn't have been in the summary in the first place)

*)

  let rec havocPtr s origTarget ptrToTargetAliases =
    match ptrToTargetAliases with
      Vmustptr (addr, off) ->
        havoc s (addr, off)

    | Vmayptr (_, addrOffSet) 
    | Vextptr (_, addrOffSet) ->
        AddrOffSet.fold 
          (fun (addr,off) st ->
             havoc st (addr, off)
          ) addrOffSet s
    | Vtop -> begin
        (* consider that TOP represents a pointer and convert *)
        try
          let host = Lvals.AbsHost (Lvals.node_of_absLval origTarget) in
          let addr = getAddr host true in 
          havoc s (addr, NoOffset)
        with A.UnknownLoc ->
          logErrorF "handleCallArgs: unknownLoc for Vtop %s\n" 
            (Lvals.string_of_lval origTarget);
          s
      end
          
    | Vstruct _ ->
        logErrorF "handleCallArgs: ignoring copied struct %s\n"
          (Lvals.string_of_lval origTarget);
        s
          
    | Vval (Lvals.CConst _) 
    | Vbot ->
        logError ~prior:3 ("handleCallArgs: origTarget constant? " ^
                             (Lvals.string_of_lval origTarget));
        (* passing in a null pointer or non-pointer value 
           leads to no change *)
        s
          
    | Vval exp -> (* Attempt to promote exp into a ptr *)
        try
          let newPtrVal, newState = castAsPointer s exp in
          havocPtr newState origTarget newPtrVal
        with PointerCastError ->
          logError ("handleCallArgs: couldn't get targets to mod" ^
                        (Lvals.string_of_lval origTarget));
          s
      
(*
  let handleCallArgs s actuals loc key =
    try
      let mods = Stat.time "getMods" !modSumms#getMods key in
      List.fold_left
        (fun curSt ((sumHost, sumOffset), sumScope) -> match sumScope with
           SFormal n -> begin
             Stat.time "havocF"
               (fun () ->
                  let arg = List.nth actuals n in
                  let absArg = Lvals.abs_of_exp arg in
                  try
                    let substLv = Lvals.substActForm absArg (sumHost, sumOffset) in
                    let ptrExp = (Lvals.mkAddrOf substLv) in 
                    (* ... messy way of getting targets-to-mod in simple form *)
                    let ptrToMod, evalState = 
                      eval curSt ptrExp None in
                    havocPtr evalState substLv ptrToMod
                  with CLv.SubstInvalidArg ->
                    curSt
               ) ()
           end
         | SGlobal ->
             Stat.time "havocG"
               (fun () ->
                  let newOff, isSum = CLv.canonicizeOff sumOffset in
                  let newAddr = getAddr sumHost isSum in
                  havoc curSt (newAddr, newOff)
               ) ()
               
         | _ ->
             failwith "SS: mod scope not resolved / filtered"
        )
        s mods 
        (* Hmm... another thing that could be done to speed this up
           is to collect all globals separately 
           (if the function call has many targets) *)
    with 
      Not_found ->
        (* All summaries should be initialized *)
        failwith (Printf.sprintf "SS: modSumms#get returned Not_found @ %s\n"
                    (string_of_loc !currentLoc))
    | Modsummaryi.BottomSummary ->
        logErrorF "SS: modSumm is BOTTOM: %s @ %s\n"
          (Callg.fid_to_string key) (string_of_loc !currentLoc);
        bottomSymState
*)

  (** Aggregate modified lvals before doing the mods *)
  let aggregateModSums targFuns =
    List.fold_left
      (fun (gMods, lMods) sumKey ->
         try
           let gMods = 
             Lvals.LvalSet.union gMods (!modSumms#getGlobalMods sumKey) in
           let lMods = 
             (!modSumms#getLocalMods sumKey, sumKey) :: lMods in
           gMods, lMods
         with Modsummaryi.BottomSummary ->
(*
           logErrorF "SS: modSumm is BOTTOM: %s @ %s\n"
             (Callg.fid_to_string sumKey) (string_of_loc !currentLoc);
*)
           gMods, lMods
      ) (Lvals.LvalSet.empty, []) targFuns
      (* TODO: cache aggregateModSums for each pass where the summaries
         have not changed? *)

  let handleOneGlobalMod (sumHost, sumOffset) curSt =
    let newOff, isSum = CLv.canonicizeOff sumOffset in
    let newAddr = getAddr sumHost isSum in
    havoc curSt (newAddr, newOff)

  let handleGlobalMods s gMods =
    Stat.time "havocG"
      (Lvals.LvalSet.fold handleOneGlobalMod gMods) s

  let handleFormalMods actuals s formMods = 
    Stat.time "havocF"
      (List.fold_left
         (fun curSt ((sHost, sOffset), sumScope) -> match sumScope with
            SFormal n -> begin
              (fun () ->
                 let arg = List.nth actuals n in
                 let absArg = Lvals.abs_of_exp arg in
                 try
                   let substLv = Lvals.substActForm absArg (sHost, sOffset) in
                   let ptrExp = (Lvals.mkAddrOf substLv) in 
                   (* ... messy way of getting targets-to-mod in simple form *)
                   let ptrToMod, evalState = 
                     eval curSt ptrExp None in
                   havocPtr evalState substLv ptrToMod
                 with CLv.SubstInvalidArg ->
                   curSt
              ) ()
            end
          | SGlobal ->
              (* May contain a global if the global is accessed 
                 with a local lock *)
              handleOneGlobalMod (sHost, sOffset) curSt

          | _ ->
              failwith "SS: handleFormalMods given local?"
         ) s) formMods

  (** Handle an instruction in a given state *)
  let handleInstr (i:instr) (s: t) =
    if !inspect then begin
      logStatus "Inspecting SS: state before instr";
      logStatus ((string_of_instr i));
      printSymState s
    end;
    let result = 
      match i with
        (* Do assignment *)
        Set(lhsLval, newVal, location) -> 
          let rhs, midState = eval s (Lvals.abs_of_exp newVal) None in
          let finalSt = handleAssign midState lhsLval rhs in
          DF.Done (finalSt)

      (* Function call *)
      | Call(ret_option, callExp, actuals, loc) ->
          let pp = getCurrentPP () in
          let targFuns = Callg.callTargsAtPP !curCG !curFunID pp in
          if (targFuns = []) then begin
            logErrorF "SS call: %s returned 0 fun(s)\n" (string_of_exp callExp);
            (match callExp with Lval (Var (va), _) ->
               logError "SS DIRECT CALL 0 funs!"  | _ -> ());
            DF.Default 
          end else
            (* Handle actuals/globals, then handle return value *)
            let globals, formals = 
              Stat.time "aggMods" aggregateModSums targFuns in
            let nextSt = handleGlobalMods s globals in
            let result = List.fold_left
              (fun curSt (fMod, sumKey) -> 
                 let argged = handleFormalMods actuals nextSt fMod in
                 let retted = handleCallRet argged ret_option actuals sumKey in
                 Stat.time "SS combineStates" (combineStates curSt) retted
              ) bottomSymState formals in
(*            
            let result = List.fold_left 
              (fun curSt sumKey ->
                 let argged = Stat.time "SS modSums" 
                   (handleCallArgs s actuals loc) sumKey in
                 let retted = handleCallRet argged ret_option actuals sumKey in
                 Stat.time "SS combineStates" (combineStates curSt) retted
              ) bottomSymState targFuns
            in
*)
            DF.Done (result)
                       
      | Asm(_) -> DF.Default
    in
    if !inspect then begin
      match result with 
        DF.Default -> 
          logStatus "Inspecting SS: state after instr: no change\n";
      | DF.Done finalSt ->
          logStatus "Inspecting SS: state after instr:";
          printSymState finalSt
      | DF.Post _ ->
          logStatus "Inspecting SS: state after instr: Post?\n"
    end;
    result

  (** Analyze a given instruction, in a given state *)      
  let doInstr (i: instr) (s: t) =
    if (s == bottomSymState) then 
      DF.Default
    else
      handleInstr i s
            
          
  (** Analyze a statement *)
  let doStmt (s: stmt) (d: t) = 
    DF.SDefault 


  (** Analyze the guard of a branch.
      TODO use value of the guard to check if path is feasible, and 
      add to assumptions. *)
  let doGuard (guard: Cil.exp) (d: t) =
    DF.GDefault
      
     
  (** return true if the statement should be analyzed *) 
  let filterStmt _ = true

    
  (** find from DATA, the state preceding statement S,
      returns BOTTOM if not found *)
  let getStmtData (data: t Inthash.t) (s: stmt) : t = 
    try Inthash.find data s.sid
    with Not_found -> 
      bottomSymState
        

  (** Combine states at return stmts and stmts w/ no succesors *)
  let combRetStates (curState:t option) (s:stmt) : t option =
    let combineS () =
      let newState = getStmtData stmtStartData s in
      match curState with
        None -> Some (newState)
      | Some (st) -> Some (combineStates st newState)
    in
    match (s.skind, s.succs) with
      Return (_, _), _ ->
        combineS ()  
    | _, _ -> curState

        
  (**** DEBUG ****)
        
  (** Get the joined state of each return statement *)
  let getExitState () =
    List.fold_left combRetStates None !curFunc.sallstmts
        

  (** Print DF facts of given STMT *)
  let printData allData stmt =
    let data = getStmtData allData stmt in
    begin
      Printf.printf "*** DF info preceding statement: %s\n" 
        (sprint 80 (d_stmt () stmt));
      printSymState data;
    end
            

end


(** Print out per-statement state for current function *)
let printCurFuncState () =
  List.iter (SymStateDF.printData SymStateDF.stmtStartData) !curFunc.sallstmts



module SymStateFwd = DF.ForwardsDataFlow (SymStateDF)

let getSymstate (pp:prog_point) : symSt =
  SymStateFwd.getDataBefore pp

let getFISymstate () : symSt =
  !SymStateDF.fiDFData

(** Combine values at return statements *)
let combRetVals (curVal:symVal option) (s:stmt) : symVal option =
  let combineV exp =
    let newState = SymStateDF.getStmtData SymStateDF.stmtStartData s in
    let newVal, _ = eval newState (Lvals.abs_of_exp exp) None in
    match curVal with
      None -> Some newVal
    | Some v -> Some (combineVals v newVal)
  in
  (* Consider Return statements *)
  match (s.skind, s.succs) with
    Return (Some(exp), _), _ ->
      combineV exp
  | _, _ ->
      curVal
        


(****************** External API (+ helpers) ******************)


(* MUST ALIAS FLAG set to true for now *)

(** Get the canonical pointed-to targets of an lval. 
    E.g., given lv == "*x" and the state at this point says
    that x \mapsto a pointer to y, then return y. *)
let derefALvalAt (pp:prog_point) (lv:Lvals.aLval) : (bool * (Lvals.aLval list)) =

  let rec derefLvalHelper ptrExp outerOff curState ptrVal =
    match ptrVal with 
      Vmustptr (addr, off) -> 
        let results =
          try [ (concretizeLval (addr, Cil.addOffset outerOff off)) ]
          with CLv.OffsetMismatch _ -> [] 
        in
        (true, results)

    | Vmayptr (id, addrOffSet) ->
        let results = 
          try AddrOffSet.fold 
            (fun (addr,off) curList ->
               (concretizeLval (addr, Cil.addOffset outerOff off)) :: curList
            ) addrOffSet [] 
          with CLv.OffsetMismatch _ -> []
        in
        (true, results)
          
    | Vextptr (id, addrOffSet) ->
        let results = 
          try AddrOffSet.fold 
            (fun (addr,off) curList ->
               (concretizeLval (addr, Cil.addOffset outerOff off)) :: curList 
            ) addrOffSet [] 
          with CLv.OffsetMismatch _ -> []
        in
        (true, results)
          
    | Vtop ->
        (* Well pushing it through eval didn't help... *)
        let ptrExp, _ = Lvals.simplifyExp ptrExp in
        (try
(* Delaying deref of abshost had issues?

          let n = Lvals.node_of_absExp ptrExp in
            (true, 
            [Lvals.mkMem (Lvals.CLval ((Lvals.AbsHost n), NoOffset)) outerOff])
*)
           let nodes = Lvals.deref_absExp ptrExp in
           (true, List.map (fun node -> (Lvals.AbsHost node, outerOff)) nodes)

         with A.UnknownLoc ->
           logError ("derefLvalHelper: Vtop has no targets " ^ 
                         Lvals.string_of_exp ptrExp);
           (true, [])
        )

    | Vval (Lvals.CConst _)
    | Vbot -> 
        logError ~prior:3 ("derefLvalAtInstr: deref a nullptr " ^
                        (Lvals.string_of_exp ptrExp));
        (true, [])
          
    | Vval exp -> begin
        (* convert to a pointer *)
        try 
          let newPtrVal, newState = castAsPointer curState exp in
          derefLvalHelper ptrExp outerOff newState newPtrVal
        with PointerCastError ->
          logError ("derefLvalAtInstr: Vval not treated as ptr: " ^ 
                          (Lvals.string_of_exp ptrExp));
          (true, [])
      end
    | Vstruct _ ->
        logError ("derefLvalAtInstr: eval returned non-ptr for ptrexp " ^
                        (Lvals.string_of_exp ptrExp));
        (true, [])
  in
  match lv with
    (Lvals.CMem(ptrExp), outerOff) ->
      let state = getSymstate pp in
      let ptrVal, newSt = eval state ptrExp None in
      let newOff, _ = CLv.canonicizeOff outerOff in
      let must, lvals = derefLvalHelper ptrExp newOff newSt ptrVal in
      (must, List.map (fun lv -> Lvals.mergeLv lv) lvals)
  | _ ->
      (true, [])

let derefLvalAt (pp:prog_point) (lv:Cil.lval) : (bool * (Lvals.aLval list)) =
  derefALvalAt pp (Lvals.abs_of_lval lv)



let getAliasesAt (pp:prog_point) (lv:Lvals.aLval) : (bool * (Lvals.aExp list)) =
  let state = getSymstate pp in
  getAliasesLval state lv true


let getAliasesFI lv = 
  let state = getFISymstate () in
  getAliasesLval state lv true


(**************** New substitution functions *************)


(** Substitute the formals in lvalWithFormal w/ the actuals,
    given the current program point. Also, translate the substituted
    result in terms of formals and globals (or other "main aliases").
    Expect lvals returned. *)
let substActForm2 pp actuals lvalWithFormal : bool * Lvals.aLval list =
  let state = getSymstate pp in
  let mustAlias, results = substActForm state actuals lvalWithFormal in
  (mustAlias, lvalsOfExps results)


(** Use fiState to do substitution *)
let substActForm2FI actuals lvalWithFormal : bool * Lvals.aLval list =
  let state = getFISymstate () in
  let mustAlias, results = substActForm state actuals lvalWithFormal in
  (mustAlias, lvalsOfExps results)  


(** Substitute the formals in lvalWithFormal w/ the actuals,
    given the current program point. DO NOT translate in terms
    of formals and globals. Any expression can be returned. *)
let substActForm3 pp actuals lvalWithFormal : Lvals.aLval list =
  match Lvals.getScope lvalWithFormal with
    SGlobal -> [lvalWithFormal]
  | SFormal n ->
      (try
         let substituted = substLval actuals lvalWithFormal in
         [substituted]
       with CLv.SubstInvalidArg ->
         let arg = List.nth actuals n in
         logError ~prior:3 ("substActForm3 unsubstitutable arg: " ^
                              (string_of_exp arg) ^ " f: " ^ 
                              (Lvals.string_of_lval lvalWithFormal));
         []
       )
  | _ -> 
      (* It was local var of the other function -- can't substitute. *)
      logError ("substActForm3: local variable " ^ 
                    (Lvals.string_of_lval lvalWithFormal) 
                  ^ " stayed in summary?");
      []



(*********************************************************
           Initialization for each run
 *********************************************************)

(** Initialize the state and dataflow facts for given func *)
let initState funID (func:Cil.fundec) : unit =
  curFunc := func;
  curFunID := funID;
  Hashtbl.clear vi_name_pool;
  TPH.clear topPtrCache;
  EH.clear ptrExpCache;
  curPtrID := 0;
  sym_var_counter := 0;
  SymStateDF.initStmtStartData emptySymState;
  SymStateFwd.clearPPData ();
  setInspect (I.inspector#mem !curFunc.svar.vname)
    

(************************************************************
             Run and stuff
************************************************************)


(** Evaluate the symbolic store for a given function.
    Assumes func has CFG info computed (e.g., func.sallstmts is valid)
    Returns true if the return value summary is updated   *)
let doSymState funID (func:Cil.fundec) : unit = begin
  initState funID func;
  (* Can't access per prog point data *)
  Osize.p_size "Symstate(pre) stmtStartData" SymStateDF.stmtStartData;
  flushStatus ();
  SymStateFwd.compute func.sallstmts;
  Osize.p_size "Symstate(post) stmtStartData" SymStateDF.stmtStartData;
end


(************************************************************)
(* Summarization *)

let rec scopeValueCheckExp exp =
  SGlobal (* TODO *)

let scopeShouldPrune scope =
  match scope with
    STBD -> true
  | SFormal _ | SGlobal | SFunc -> false

let doScopePrune v =
  logErrorF "Returning a local? %s\n" (string_of_val v);
  Vtop

(** Maybe it's returning an "uninitialized" pointer because we skipped
    assembly code? Turn those into TOP *)
let rec scopeValue v = 
  match v with
    Vbot | Vtop -> v 
  | Vval exp -> 
      if scopeShouldPrune (scopeValueCheckExp exp) then doScopePrune v
      else v

  | Vmustptr (addr, o) ->
      let host = hostOfAddr addr in
      if scopeShouldPrune (Lvals.getScope (host, o)) then doScopePrune v
      else v

  | Vmayptr (id, addrs) | Vextptr (id, addrs) ->
      if AddrOffSet.exists 
        (fun (addr, o) ->
           let host = hostOfAddr addr in
           scopeShouldPrune (Lvals.getScope (host, o))
        ) addrs 
      then doScopePrune v
      else v

  | Vstruct offMap -> 
      let omap = OffsetMap.mapCh scopeValue offMap in
      if omap == offMap then v else Vstruct omap

(** Update the summary and indicate whether the summary is newer *)
let summarize sumKey (func:Cil.fundec) : bool =
  let outVal = 
    match List.fold_left combRetVals None func.sallstmts with
      None -> Vbot
    | Some v -> scopeValue v
  in
  let oldOut = SS.sum#find sumKey in
  match compareVals outVal oldOut with
    (Some i) when (i <= 0) -> false
  | _ ->
      let combVal = combineVals oldOut outVal in
      SS.sum#addReplace sumKey combVal;
      true


(* Test / Debug stuff *)
let getExitState () =
  SymStateDF.getExitState ()


let printExitState () = 
  match getExitState () with
    Some (s) ->
      printSymState s
  | None ->
      printSymState bottomSymState




(** Package the symstate analysis *)
class symexAnalysis = object (self)
  
  method setInspect yesno =
    inspect := yesno

  method isFinal key = (* don't need to skip *)
    false
      
  method compute (funID: Callg.funID) cfg = begin
    logStatus "doing symstate";
    Stat.time "Computing symstate DF: " (doSymState funID) cfg;
  end
      
  method summarize key cfg =
    if self#isFinal key then begin
      SS.sum#addReplace key Sym_types.Vtop;
      false
    end
    else summarize key cfg
      (* In other phases, can override to not summarize! *)
      
  method flushSummaries () =
    SS.sum#serializeAndFlush 

end

