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


(** Collection of the symbolic types used by the symbolic execution
    analysis (@see symstate2.ml) *)

open Stdutil
open Cil
open Pretty
open Logging

module CLv = Cil_lvals
module Lv = Lvals
module Stat = Mystats

module Lvs = Set.Make (Lvals.OrderedLval)

(*********************************************************
 * Cells, values, expressions etc.
 *********************************************************)

(* symbolic cells that can be looked up in the store *)
type symAddr = 
    { saHost : Lv.aHost;
      mutable saSummary : bool; }

let hostOfAddr (addr:symAddr) =
  addr.saHost

type symExp = Lv.aExp

let compAddrApprox 
    { saHost = v1; } 
    { saHost = v2; } =      
  Lv.compare_host v1 v2     (* ignores saSummary *)

module OrderedAddr = struct
  type t = symAddr
  let compare = compAddrApprox
end

module AddrMap = Mapset.Make(OrderedAddr)

module HashAddr = struct
  type t = symAddr
  let equal x y = compAddrApprox x y == 0
  let hash x = Lv.hash_host x.saHost
end

module AddrHash = Weak.Make(HashAddr)

let compareAddrOff (a1,o1) (a2,o2) =
  let comp_a = compAddrApprox a1 a2 in
  if (comp_a <> 0) then
    comp_a 
  else
    Ciltools.compare_offset o1 o2
      
module OrderedAddrOffset = struct
  type t =  symAddr * Cil.offset
  let compare = compareAddrOff
end

module AddrOffSet = Set.Make(OrderedAddrOffset)


module OrderedOffset = struct
  type t = Cil.offset
  let compare = Ciltools.compare_offset
end

module OffsetMap = Mapset.Make(OrderedOffset)

module ES = Set.Make(
  struct
    type t = symExp
    let compare a b = Lv.compare_exp a b
  end)


(* symbolic value held at a symbolic address *)
type symVal =
    Vtop
  | Vbot
  | Vval of symExp                    (** probably non-pointer value *)
  | Vstruct of symVal OffsetMap.t      (** structure: indexable by offset
                                          (include array indexing here) *)
  | Vmustptr of (symAddr * Cil.offset) (** pointer to another symbolic cell *)
  | Vmayptr of (int * AddrOffSet.t)    (** pointer to multiple cells, 
                                          w/ an id to approx. linearity *)
  | Vextptr of (int * AddrOffSet.t)    (** pointer to external cell, 
                                          which may actually point to 
                                          other existing cells. Be sure 
                                          to ask coarse ptr analysis *)

let isConst e =
  match e with
    Lv.CConst _ -> true
  | _ -> false

let makeNullExp () =
  Lv.CConst (CStr "$SSBOT")

let nullVar = CLv.mkVarinfo true "$NULL" (TVoid [])

let nullAddr =
  { saHost = Lv.CMem (Lv.mkLval 
                        (Lv.hostOfVar nullVar) NoOffset);
    saSummary = true; }

let isNullAddr addr =
  Lv.compare_host nullAddr.saHost addr.saHost == 0

let isNullHost h =
  Lv.compare_host nullAddr.saHost h == 0

let isAbsAddr addr =
  match addr.saHost with
    Lv.AbsHost _ -> true
  | _ -> false



(***** Convert from lvals -> sym addresses -> and back *****)

(** Get the symbolic cell that represents the given concrete source *)
let getAddr host (isSum:bool) : symAddr =
  (* try to find a cell w/ the same host & return existing 
     cell, or create the cell *)
  let host, _ = Lv.mergeLv (host, NoOffset) in
  let addr = { saHost = host; saSummary = isSum; } in
  addr.saSummary <- addr.saSummary or isSum; (* update summary flag *)
  addr


(** Convert the symbolic state lval (addr, offset) into
    an external lval. May raise CLv.OffsetMismatch   *)
let concretizeLval (ptAddr, ptOff) = 
  let host = hostOfAddr ptAddr in
  let finalOff, _ = CLv.canonicizeOff ptOff in
  (host, finalOff)



(*********************************************************
 * Symbolic store and state
 *********************************************************)


(* Variable store is map from addresses to values *)
type symStore = symVal AddrMap.t

(* per program-point state *)
type symState = {
  store: symStore;
  assumptions: symExp list; (* TODO: things assumed to be true along this path *)
  usedSubexps: ES.t
}


(*********************************************************
 * Test / Debug code
 *********************************************************)

let d_addr addr =
  let host = hostOfAddr addr in
  let lval = (host, NoOffset) in
  let str = "lval = " ^ (Lv.string_of_lval lval) in
  let str = if addr.saSummary then str ^  "(sum)"
  else str in
  text str

let string_of_addr (addr:symAddr) : string = 
  sprint 80 (d_addr addr)

let printAddr addr =
  logStatus (string_of_addr addr)

let d_pointerTarg (addr, off) =
  d_offset (d_addr addr) () off

let string_of_pointer (addr, off) =
  sprint 80 (d_pointerTarg (addr, off))


let d_ptrSet header addrOffSet =
  header ++ indent 2 
    (AddrOffSet.fold
       (fun target cur ->
          cur ++ d_pointerTarg target ++ line 
       ) addrOffSet nil)

let rec d_value v =
  match v with
     Vtop -> text "$SSTOP"
   | Vbot -> text "$SSBOT"
       
   | Vval exp -> 
       dprintf "Vval: %s" (Lv.string_of_exp exp)
         
   | Vmustptr target -> 
       text "Mustptr -> " ++  d_pointerTarg target

   | Vmayptr (id, addrOffSet) ->
       let header = text ("Mayptr (" ^ (string_of_int id) ^ "):\n") in
       d_ptrSet header addrOffSet
         
   | Vextptr (id, addrOffSet) ->
       let header = text ("Extptr (" ^ (string_of_int id) ^ "):\n") in
       d_ptrSet header addrOffSet

   | Vstruct offMap ->
       let header = text "Struct:\n"in
       header ++ indent 2
         (OffsetMap.fold 
            (fun off v cur ->
               cur ++ 
                 (d_offset Pretty.nil () off) ++ text " ->" ++
                 (indent 2 (d_value v)) ++ line
            ) offMap nil
         )

let string_of_val v =
  sprint 80 (d_value v)

let prefix = " -> "

let printVal v =
  logStatus prefix;
  logStatusD ((indent 2 (d_value v)) ++ line)

    
(*********************************************************
 * Special instances of addresses, states, etc.
 *********************************************************)

let bottomExp = makeNullExp ()

let emptySymState = {
  store = AddrMap.empty;
  assumptions = [];
  usedSubexps = ES.empty;
}

let bottomSymState = {
  store = AddrMap.empty;
  assumptions = [bottomExp];
  usedSubexps = ES.empty
}

(*********************************************************
 * Operations on state 
 *********************************************************)


let curPtrID = ref 0

let freshPtrID () = 
  let result = !curPtrID in
  curPtrID := result + 1;
  result

let usedInitialVals = ref Lvs.empty


(***** Lazily create values *****)

(** Remember that the initial value of addr was used *)
let recordInitialValUse addr off =
  let lv = concretizeLval (addr, off) in
  usedInitialVals := Lvs.add lv !usedInitialVals


(** Assume that the lval (addr, off) holds a pointer value. Give that
    a pointer value that points to *lval *)
let makePtrValue addr off : symVal =
  let host = hostOfAddr addr in
  let targHost, _ = Lv.mkMem (Lv.mkLval host off) NoOffset in
  if addr.saSummary then
    let targAddr = getAddr targHost true in 
    (* prev was summary then new is *)
    let ptrID = freshPtrID () in
    let ptSet = AddrOffSet.add (targAddr, NoOffset) AddrOffSet.empty in
    Vextptr (ptrID, ptSet)
  else
    let targAddr = getAddr targHost false in
    Vmustptr (targAddr, NoOffset)


(** Assume lval is a struct *)
let makeStructValue () : symVal =
  Vstruct OffsetMap.empty


(** Assume given offset has been canonicized,
    Under this execution, assume the typ of the addr is baseTyp 
    (an attempt to recover discarded type-cast information) *)
let makeValue baseTyp addr canonOff =
  recordInitialValUse addr canonOff;
  if (Cil.isPointerType baseTyp) then makePtrValue addr canonOff
  else if (Cil.isStructType baseTyp) then makeStructValue ()
  else
    let host = hostOfAddr addr in
    (* Refer to the "initial value" of the Lval *)
    let val1 = Vval (Lv.mkLval host canonOff) in
    val1

let makeValueNoTypeInfo addr canonOff =
  let baseTyp = Lv.typeOfLvalUnsafe (hostOfAddr addr, canonOff) in
  makeValue baseTyp addr canonOff


(****** Base lookups *******)


(** looks up the value at 'addr' and 'canonOff' from a given store. 
    Assumes offset is canonicized. May raise Not_found *)
let lookupValStore (s:symStore) baseType addr canonOff : symVal =
  let canonOff = CLv.simplifyOff canonOff in
  match canonOff with
    NoOffset ->
      (try 
         AddrMap.find addr s
       with Not_found ->
         makeValue baseType addr canonOff)
  | Index _
  | Field _ ->
      let outerVal = 
        (try
           (AddrMap.find addr s)
         with Not_found ->
           (makeStructValue ())) 
          (* could have just skipped ahead to making the innerVal -- whatever *)
      in
      let innerVal = match outerVal with
        Vstruct offMap ->
          (try
             OffsetMap.find canonOff offMap 
           with Not_found ->
             makeValue baseType addr canonOff)
        | Vtop -> 
            (* if canonOff is a field offset, but the base addr evals to TOP,
               the result is TOP *)
            Vtop
            
        | _ ->
            logError 
              (Printf.sprintf "Field access %s on non-struct\n%s\n"
                 (Lv.string_of_lval (hostOfAddr addr, canonOff)) 
                 (string_of_val outerVal));
            outerVal
      in
      innerVal


(** looks up the value at 'addr' and 'canonOff' from a given state. 
    Assumes offset is canonicized. May raise Not_found *)
let lookupVal (state:symState) baseType addr canonOff : symVal = 
  if isNullAddr addr then begin
    logError "lookupVal: given null addr";
    Vtop
  end 
  else if isAbsAddr addr then begin
    logError "lookupVal: given abs addr";
    lookupValStore state.store baseType addr canonOff
  end
  else lookupValStore state.store baseType addr canonOff



(****** Writes into store / killing things *******)

(** @return true of the expression refers to the given lval *)
let rec expContainsLval lval exp =
  match exp with
    Lv.CLval lv -> 
      lvalContainsLval lval lv
  | Lv.CAddrOf lv
  | Lv.CStartOf lv -> false

  | Lv.CCastE (_, e) ->
      expContainsLval lval e

  (* Constants *)
  | Lv.CAlignOfE _
  | Lv.CSizeOfE _
  | Lv.CAlignOf _
  | Lv.CSizeOf _
  | Lv.CSizeOfStr _ 
  | Lv.CConst _ -> false

  | Lv.CBinOp(_, e1, e2, _) ->
      expContainsLval lval e1 ||
        expContainsLval lval e2

  | Lv.CUnOp(_, e, _) -> 
      expContainsLval lval e

and lvalContainsLval lvToMatch lvToCheck =
  if Lv.compare_lval lvToMatch lvToCheck == 0 then true
  else
    (match lvToCheck with
       Lv.CVar _ , _ 
     | Lv.AbsHost _, _ -> false
     | Lv.CMem e, _ -> expContainsLval lvToMatch e)


(** Kill old mentions of the lval (in subexpressions, etc.) *)
let rec killSubExpressVal lval oldVal killed : symVal option * ES.t =
  match oldVal with
    Vval e ->
      if expContainsLval lval e then Some (Vtop), ES.add e killed
      else None, killed
  | Vstruct offmap ->
      (match killSubExpressOff lval offmap killed with
         Some newOffMap, moreKilled -> Some (Vstruct newOffMap), moreKilled
       | None, moreKilled -> None, moreKilled)
  | _ -> 
      None, killed
        
and killSubExpressOff lval offMap killed : (symVal OffsetMap.t) option  * ES.t =
  OffsetMap.fold
    (fun oldKey oldVal (newMap, curKilled) -> 
       match newMap, killSubExpressVal lval oldVal curKilled with
         None, (None, moreKilled) -> None, moreKilled
       | Some x, (None, moreKilled) -> Some x, moreKilled
       | None, (Some newVal, moreKilled) ->
           Some (OffsetMap.add oldKey newVal offMap), moreKilled
       | Some newMap, (Some newVal, moreKilled) ->
           Some (OffsetMap.add oldKey newVal newMap), moreKilled
    ) offMap (None, killed)
    
let killSubExpressStore lval oldStore : symStore option * ES.t =
  AddrMap.fold
    (fun oldKey oldVal (newStore, killed) ->
       match newStore, killSubExpressVal lval oldVal killed with
         None, (None, moreKilled) -> None, moreKilled
       | Some x, (None, moreKilled) -> Some x, moreKilled
       | None, (Some newVal, moreKilled)  ->
           Some (AddrMap.add oldKey newVal oldStore), moreKilled
       | Some newMap, (Some newVal, moreKilled) ->
           Some (AddrMap.add oldKey newVal newMap), moreKilled
    ) oldStore (None, ES.empty)
    
let lvalCommonToSubexp lval usedSubexps =
  ES.exists (expContainsLval lval) usedSubexps
    
let killSubExpressState lval oldState =
  if lvalCommonToSubexp lval oldState.usedSubexps then
    match killSubExpressStore lval oldState.store with
      None, _ -> oldState
    | Some (newStore), killed -> 
        { oldState with 
            store = newStore;
            usedSubexps = ES.diff oldState.usedSubexps killed;
        }
  else oldState

(******************* Add mapping to store ******************)

(** returns a new store where 'addr.off' has been assigned 'v'
    may update a struct *)
let assignVarStore (oldStore:symStore) addr off v : symStore =
  let off = CLv.simplifyOff off in
  match off with
    NoOffset ->
      (AddrMap.add addr v oldStore)
  | Field _ 
  | Index _ ->
     (* Make a struct thing if it isn't there already *)
      let outerVal = 
        try
          let oldOuterVal = AddrMap.find addr oldStore in
          match oldOuterVal with
          Vstruct offMap -> 
            Vstruct (OffsetMap.add off v offMap)
          | Vbot
          | Vtop   (* even for Vtop? strong update i guess? *)
          | Vval _ ->
              Vstruct (OffsetMap.add off v OffsetMap.empty)
          | Vextptr _
          | Vmustptr _ 
          | Vmayptr _ ->
              (* maybe it was wrong before about being used as a ptr? *)
              Vstruct (OffsetMap.add off v OffsetMap.empty)
        with Not_found ->
          Vstruct (OffsetMap.add off v OffsetMap.empty)
      in
      (AddrMap.add addr outerVal oldStore)

let assignVarState oldState addr off v =
  let newStore = assignVarStore oldState.store addr off v in
  (* Index any subexpressions *)
  match v with 
    Vval e ->
      { oldState with 
          store = newStore;
          usedSubexps = ES.add e oldState.usedSubexps ; }
  | _ -> 
      { oldState with
          store = newStore; }
        

(** returns a new state where 'addr.off' has been assigned 'v' *)
let assignVar (oldState:symState) addr off v : symState =
  if isNullAddr addr then begin
(*    logError "assignVar: given null addr"; *)
    oldState
  end 
  else if isAbsAddr addr then begin
    logError "assignVar: given abs addr";
    let newStore = assignVarStore oldState.store addr off v in
    { oldState with store = newStore; }    
  end
  else
    let lval = concretizeLval (addr, off) in
    let tempState = Stat.time "killSubexp" 
      (killSubExpressState lval) oldState in
    assignVarState tempState addr off v


let makeSetWithNull targ =
  AddrOffSet.add (nullAddr, NoOffset) (AddrOffSet.singleton targ)

let addNullTo targSet =
  AddrOffSet.add (nullAddr, NoOffset) targSet


(*********************************************************
 * Getting cells / generating initial values, etc.
 *********************************************************)



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
  let (finalHost, fOff), summedLv = Lv.simplifyLval correspLv in
  let finalAddr = getAddr finalHost (summedLv || summedOff) in
  (finalAddr, fOff)



(** Evaluate the value at an addr + offset *)
let evalAddr (s:symState) baseTyp addr canonOff : symVal * symState =
  (* TODO: push limitAddrOff to here / get rid of this 
     baseTyp crap / don't change the state *)
  let value = lookupVal s baseTyp addr canonOff in
  (value, s)
    



(*********************************************************
 * Lattice ops
 *********************************************************)



let compareMayPtrs (id1, ptset1) (id2, ptset2) =
  if (AddrOffSet.equal ptset1 ptset2) then Some (0)
  else if (AddrOffSet.subset ptset1 ptset2) then Some (-1)
  else if (AddrOffSet.subset ptset2 ptset1) then Some (1)
  else None
  

let rec eqVals v1 v2 =
  match v1, v2 with
    Vbot, Vbot
  | Vtop, Vtop -> true

  | Vmustptr addrOff1, Vmustptr addrOff2 ->
      (compareAddrOff addrOff1 addrOff2) == 0

  | Vmayptr (_, s1), Vmayptr (_, s2) ->
      AddrOffSet.equal s1 s2
        
  | Vextptr (_, s1), Vextptr (_, s2) ->
      AddrOffSet.equal s1 s2

  | Vval exp1, Vval exp2 -> 
      (* What if concrete versions are EQ, but exps themselves aren't?
         Is that possible? Intuition is no for now... *)
      (Lv.compare_exp exp1 exp2) == 0

  | Vstruct om1, Vstruct om2 ->
      OffsetMap.equal eqVals om1 om2

  | _, _ ->
      false


let eqStates st1 st2 =
  (st1 == st2) || 
    (AddrMap.equal eqVals st1.store st2.store
     && st1.assumptions = st2.assumptions)

let isBottomState st =
  eqStates bottomSymState st
    
module StrictLatticeOps = struct
  
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
      (* TODO don't ignore summary flag while comparing? *)
      Vmustptr addrOff1, Vmustptr addrOff2 ->
        if ((compareAddrOff addrOff1 addrOff2) == 0) then Some(0)
        else None

    | Vmayptr s1, Vmayptr s2 ->
        compareMayPtrs s1 s2

    | Vextptr s1, Vextptr s2 ->
        compareMayPtrs s1 s2

    | Vmustptr mustPtr, Vmayptr (id, ptset)
    | Vmustptr mustPtr, Vextptr (id, ptset) ->
        if (AddrOffSet.mem mustPtr ptset) then Some(-1)
        else None

    | Vmayptr (id, ptset), Vmustptr mustPtr
    | Vextptr (id, ptset), Vmustptr mustPtr ->
        if (AddrOffSet.mem mustPtr ptset) then Some(1)
        else None

    | Vmayptr (mayID, mayPtSet), Vextptr (extID, extPtSet) ->
        if (AddrOffSet.subset mayPtSet extPtSet) then Some(-1)
        else None

    | Vextptr (extID, extPtSet), Vmayptr (mayID, mayPtSet) ->
        if (AddrOffSet.subset mayPtSet extPtSet) then Some(1)
        else None

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
        if (OffsetMap.subset compareVals offmap1 offmap2) then Some (-1)
        else None

    | Vval exp1, Vval exp2 -> 
        (* What if concrete versions are EQ, but exps themselves aren't?
           Is that possible? Intuition is no for now... *)
        if((Lv.compare_exp exp1 exp2) == 0) then Some (0)
        else None

    | Vtop, Vtop -> Some (0) 

    | Vbot, Vbot -> Some (0)

    | Vtop, _ -> Some (1)

    | _, Vtop -> Some (-1)

    | Vbot, _ -> Some (-1)

    | _, Vbot -> Some (1)

    | Vval _, Vstruct _ ->
         (* TODO: Happens when fields have not been touched in one path, 
            but have in the other. Should compare the new val vs initial *)
         (* Some (-1) *)
         None

    | Vstruct _, Vval _ ->
        (* Some (1) *)
        None

    | _, _ ->
        logError "compareVals: addrs -> to different types of values";
        None

  let equalVals v1 v2 =
    match compareVals v1 v2 with Some 0 -> true | Some _ | None -> false
      

  (** LUB of two values *)
  let rec combineVals v1 v2 =
    match v1, v2 with
      Vmustptr addrOff1, Vmustptr addrOff2 -> 
        if ((compareAddrOff addrOff1 addrOff2) == 0) then v1
        else
          let ptSet = AddrOffSet.add addrOff1 AddrOffSet.empty in
          Vmayptr (freshPtrID (), AddrOffSet.add addrOff2 ptSet)
            
    | Vmayptr (id1, ptset1), Vmayptr (id2, ptset2) ->
        if (AddrOffSet.equal ptset1 ptset2) then v1
        else Vmayptr (freshPtrID (), AddrOffSet.union ptset1 ptset2)
          
          
    | Vextptr (id1, ptset1), Vextptr (id2, ptset2) -> 
        if (AddrOffSet.equal ptset1 ptset2) then v1
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
    | (Vval e), (Vmayptr (_, targs)) when isConst e ->
        Vmayptr (freshPtrID (), addNullTo targs)

    | (Vextptr (_, targs)), (Vval e)
    | (Vval e), (Vextptr (_, targs)) when isConst e ->
        Vextptr (freshPtrID (), addNullTo targs)        

    | Vstruct offmap1, Vstruct offmap2 ->
        (* unify the offset maps *)
        Vstruct (OffsetMap.union combineVals offmap1 offmap2)

    | Vval exp1, Vval exp2 ->
        (* What if concrete versions are eq (x_0 actually == y_0 + c) *)
        if ((Lv.compare_exp exp1 exp2) == 0) then v1
        else Vtop

    | Vtop, _ 
    | _, Vtop ->
        Vtop
          
    | Vbot, x
    | x, Vbot -> 
        x
          
    | Vval _, Vstruct _ ->
        (* Could happen when no offsets have been touched on one path,
           but then the new value may differ from the uninitialized value...
           TODO: should make the first guy a struct w/ the initial
           values and combine with new struct? *)

        Vtop (* See s3_pkt.c in mini_openssl for termination issues.
                It also had something to do w/ pointer arith giving
                things a strange offset. *)

    | Vstruct _ , Vval _ ->
        Vtop

    | _, _ ->
        (* could happen when ignoring casts, e.g., (x = (unsigned int)ptr) *)
        logError "combineVals: unifying different vals";
        Vtop


  (** Check if state s1 is a subset of s2 *)
  let statesSubset (s1:symState) (s2:symState) : bool =
    if(s1 == bottomSymState) then true
    else if (s2 == bottomSymState) then false
    else 
      let store1 = s1.store in
      let store2 = s2.store in
      let cmp = compareVals in
      (* TODO: treat entries w/ no matched mapping and create initial val *)
      AddrMap.subset cmp store1 store2 &&
        ES.subset s1.usedSubexps s2.usedSubexps

  let statesEqual (s1:symState) (s2:symState) : bool =
    if(s1 == s2) then true
    else 
      AddrMap.equal equalVals s1.store s2.store &&
        ES.equal s1.usedSubexps s2.usedSubexps


  (** LUB of the two states *) 
  let combineStates st1 st2 =
    if (st1 == bottomSymState || isBottomState st1) then st2 
    else if (st2 == bottomSymState || isBottomState st2) then st1 else
      let store1 = st1.store in
      let store2 = st2.store in
      
      (* Combine store *)
      (* TODO: treat entries w/ no matched mapping and create initial val ...
         have a mapset that calls a func for generating default values?  *)
      let newStore = AddrMap.union combineVals store1 store2 in 

      (* TODO update assumptions *)
      { st1 with 
          store = newStore;
          usedSubexps = ES.union st1.usedSubexps st2.usedSubexps;
      }

end

(********************* Lazy version ********************)


module LazyLatticeOps = struct

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
      (* TODO don't ignore summary flag while comparing? *)
      Vmustptr addrOff1, Vmustptr addrOff2 ->
        if ((compareAddrOff addrOff1 addrOff2) == 0) then Some(0)
        else None

    | Vmayptr s1, Vmayptr s2 ->
        compareMayPtrs s1 s2

    | Vextptr s1, Vextptr s2 ->
        compareMayPtrs s1 s2

    | Vmustptr mustPtr, Vmayptr (id, ptset)
    | Vmustptr mustPtr, Vextptr (id, ptset) ->
        if (AddrOffSet.mem mustPtr ptset) then Some(-1)
        else None

    | Vmayptr (id, ptset), Vmustptr mustPtr
    | Vextptr (id, ptset), Vmustptr mustPtr ->
        if (AddrOffSet.mem mustPtr ptset) then Some(1)
        else None

    | Vmayptr (mayID, mayPtSet), Vextptr (extID, extPtSet) ->
        if (AddrOffSet.subset mayPtSet extPtSet) then Some(-1)
        else None

    | Vextptr (extID, extPtSet), Vmayptr (mayID, mayPtSet) ->
        if (AddrOffSet.subset mayPtSet extPtSet) then Some(1)
        else None

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
        if (OffsetMap.subset compareVals offmap1 offmap2) then Some (-1)
        else None

    | Vval exp1, Vval exp2 -> 
        (* What if concrete versions are EQ, but exps themselves aren't?
           Is that possible? Intuition is no for now... *)
        if((Lv.compare_exp exp1 exp2) == 0) then Some (0)
        else None

    | Vtop, Vtop -> Some (0) 

    | Vbot, Vbot -> Some (0)

    | Vtop, _ -> Some (1)

    | _, Vtop -> Some (-1)

    | Vbot, _ -> Some (-1)

    | _, Vbot -> Some (1)

    | Vval _, Vstruct _ ->
         (* TODO: Happens when fields have not been touched in one path, 
            but have in the other. Should compare the new val vs initial *)
        None

    | Vstruct _, Vval _ ->
        None

    | _, _ ->
        logError "compareVals: addrs -> to different types of values";
        None

  let equalVals v1 v2 =
    match compareVals v1 v2 with Some 0 -> true | Some _ | None -> false
      

  (** LUB of two values *)
  let rec combineVals v1 v2 =
    match v1, v2 with
      Vmustptr addrOff1, Vmustptr addrOff2 -> 
        if ((compareAddrOff addrOff1 addrOff2) == 0) then v1
        else
          let ptSet = AddrOffSet.add addrOff1 AddrOffSet.empty in
          Vmayptr (freshPtrID (), AddrOffSet.add addrOff2 ptSet)
            
    | Vmayptr (id1, ptset1), Vmayptr (id2, ptset2) ->
        if (AddrOffSet.equal ptset1 ptset2) then v1
        else Vmayptr (freshPtrID (), AddrOffSet.union ptset1 ptset2)
          
          
    | Vextptr (id1, ptset1), Vextptr (id2, ptset2) -> 
        if (AddrOffSet.equal ptset1 ptset2) then v1
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
    | (Vval e), (Vmayptr (_, targs)) when isConst e ->
        Vmayptr (freshPtrID (), addNullTo targs)

    | (Vextptr (_, targs)), (Vval e)
    | (Vval e), (Vextptr (_, targs)) when isConst e ->
        Vextptr (freshPtrID (), addNullTo targs)        

    | Vstruct offmap1, Vstruct offmap2 ->
        (* unify the offset maps *)
        Vstruct (OffsetMap.union combineVals offmap1 offmap2)

    | Vval exp1, Vval exp2 ->
        (* What if concrete versions are eq (x_0 actually == y_0 + c) *)
        if ((Lv.compare_exp exp1 exp2) == 0) then v1
        else Vtop

    | Vtop, _ 
    | _, Vtop ->
        Vtop
          
    | Vbot, x
    | x, Vbot -> 
        x
          
    | Vval _, Vstruct _ ->
        (* Could happen when no offsets have been touched on one path,
           but then the new value may differ from the uninitialized value...
           TODO: should make the first guy a struct w/ the initial
           values and combine with new struct? *)
        Vtop

    | Vstruct _ , Vval _ ->
        Vtop

    | _, _ ->
        (* could happen when ignoring casts, e.g., (x = (unsigned int)ptr) *)
        logError "combineVals: unifying different vals";
        Vtop


  (** Check if state s1 is a subset of s2 *)
  let statesSubset (s1:symState) (s2:symState) : bool =
    if(s1 == bottomSymState) then true
    else if (s2 == bottomSymState) then false
    else 
      let store1 = s1.store in
      let store2 = s2.store in
      let cmp = compareVals in
      (* TODO: treat entries w/ no matched mapping and create initial val *)
      AddrMap.subset cmp store1 store2 && 
        ES.subset s1.usedSubexps s2.usedSubexps
          (* TODO hmm... use something that matches the combine *)

  let statesEqual (s1:symState) (s2:symState) : bool =
    if(s1 == s2) then true
    else 
      AddrMap.equal equalVals s1.store s2.store &&
        ES.equal s1.usedSubexps s2.usedSubexps

        
  (** LUB of the two states, assuming the stores lazily generate values *)  
  let combineStates st1 st2 =
    if (st1 == bottomSymState || isBottomState st1) then st2 
    else if (st2 == bottomSymState || isBottomState st2) then st1 else
      let store1 = st1.store in
      let store2 = st2.store in
      
      (* Combine store, assuming empty slots mean that it's the initial val *)
      let tempStore, remaining2 = 
        AddrMap.fold
          (fun addr v1 (result, remainingStore2) ->
             let v2 = 
               try
                 AddrMap.find addr remainingStore2
               with Not_found ->
                 makeValueNoTypeInfo addr NoOffset
             in
(* TODO: go through assignVar so that usedSubexps gets updated too *)
             let newResult = AddrMap.add addr (combineVals v1 v2) result in
             let newRemaining = AddrMap.remove addr remainingStore2 in
             (newResult, newRemaining)
          ) store1 (store1, store2) in 
      
      let newStore = AddrMap.fold
        (fun addr v2 result ->
           let v1 = makeValueNoTypeInfo addr NoOffset in
           AddrMap.add addr (combineVals v1 v2) result
        ) remaining2 tempStore in
      
      (* TODO update assumptions *)
      { st1 with 
          store = newStore;
          usedSubexps = ES.union st1.usedSubexps st2.usedSubexps; 
          (* TODO search store and update instead... *)
      }

end        
  

(************* More printing ***************)


let printSymState ({store = st; assumptions = a;} as state) =
  if (isBottomState state) then
    logStatus "State is $BOTTOM\n"
  else begin
    logStatus "-------";
    AddrMap.iter
      (fun addr v ->
         printAddr addr;
         printVal v;
      ) st;
    logStatus "-------"
  end


    
