
(*
  Copyright (c) 2008-2009, Regents of the University of California

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


(** Collection of the types used by the OO-related funptr analysis *)

open Cil
open Pretty
open Fstructs
open Type_reach
open Type_utils
open Cildump
open Logging

module Stat = Mystats


(*** User-tunable parameters ***)
let heapMax = ref 5
let offsetOfWeaken = ref false 
let filterNFP = ref true
let heapRecMax = ref 2
let recsMax = ref 64 (* Set to negative if you want no limit *)
let nonUniformPtrArith = ref true (* true to allow funky ptr arith *)
let fullFI = ref false

(*********************************************************
  Variables and addresses
 *********************************************************)

type varID = int
let vidOfVarinfo vi = vi.vid


type offset_val = int
(* int of offset and whether or not it is summary -- 
   used for keeping track of pointer targets *)
type offset_sum = offset_val * bool 
let int_of_off ((i, _) : offset_sum) = i
let isSumOff ((_, s) : offset_sum) = s


(* boolean indicating if variable is a struct + width indicating size *)
type structInfo = bool * int

(* Support for naming heap variables *)
type callstring = (prog_point * fKey) list
type heapID = callstring

type heapVarinfo =
    { hID: heapID; }
      

(* Support for naming input ghost variables based on access path *)
type accElem = 
    AccVar of varID              (* must start at a program variable *)
  | AccDeref
  | AccField of offset_val       (* offset from previous dude *)

type accPath = (accElem list * int) (* list + its length memoized *)

(* cells that can be looked up in the store *)
type fvar = 
    FVar of varID               (* global / stack arrays not summary nodes *)
  | FHeap of heapVarinfo
  | FRet of type_id
  | FInput of accPath

(* addresses -- targets of pointers *)
type flocation = fvar * offset_sum

type vKind = 
    HSingle
  | HSum
  | HGlobal
      
type vAttrib = {
  vKind : vKind;
  vType : type_id; 
} 
      

      
(*** Misc data structures ***)

let compVarID (v1 : varID) v2 =
  v1 - v2

let compOffset (o1 : offset_val) o2 = Pervasives.compare o1 o2
let compOffsetSum (o1 : offset_sum) o2 = Pervasives.compare o1 o2



let closestToZeroOff (v1 : offset_val) v2 : int =
  let c = abs v1 - abs v2 in
  if c == 0 then 
    if v1 = v2 then 0 
    else v1 (* if one is pos and other not, take the sign of v1 *)
  else c

let compTypID (t1 : type_id) t2 =
  t1 - t2

let compHeapID (id1 : heapID) id2 =
  Pervasives.compare id1 id2 

let compareHeapinfo hi1 hi2 =
  compHeapID hi1.hID hi2.hID

let comp_accElem (a: accElem) b =
  Pervasives.compare a b
    
let comp_accPath (ap1, l1) (ap2, l2) =
  (** Keep shorter paths first... (when applying summaries, get 
      types of prefixes earlier?) *)
  let d = l1 - l2 in
  if d == 0 then
    List_utils.compare comp_accElem ap1 ap2
  else d

let hashHeapinfo hi =
  List_utils.hash (Hashtbl.hash) hi.hID 

let hashAccElem x = 
  match x with
    AccDeref -> 129473
  | AccField o -> 1847092 lxor Hashtbl.hash o
  | AccVar vid -> 23904 lxor Hashtbl.hash vid
      
let hashAccPath ap =
  List_utils.hash hashAccElem ap

let hashVar var = 
  match var with
    FVar x -> 930487 lxor (Hashtbl.hash x)
  | FInput (ap,_) -> 43257 lxor (hashAccPath ap)
  | FHeap hi -> 233242 lxor (hashHeapinfo hi)
  | FRet t -> 77214792 lxor (hashTypID t)

let compFVar v1 v2 =
  if v1 == v2 then 0
  else match v1, v2 with
    FVar vi1, FVar vi2 -> compVarID vi1 vi2
  | FInput ap1, FInput ap2 -> comp_accPath ap1 ap2
  | FHeap hi1, FHeap hi2 -> compareHeapinfo hi1 hi2
  | FRet _, FRet _ -> 0 (* ignore type *)
  | _, _ -> Pervasives.compare v1 v2

let eqFVar (v1 :fvar) v2 = v1 == v2 || compFVar v1 v2 == 0

let compFLoc ((v1, o1) as l1 : flocation) ((v2, o2) as l2) =
  if l1 == l2 then 0
  else 
    let co = compOffsetSum o1 o2 in
    if co == 0 then compFVar v1 v2 
    else co

(** Compare locations, ignoring the summary bit on the offset... *)
let compFLocNoSum ((v1, o1) as l1 : flocation) ((v2, o2) as l2) =
  if l1 == l2 then 0
  else 
    let o1 = int_of_off o1 in
    let o2 = int_of_off o2 in
    let co = o1 - o2 in
    if co == 0 then compFVar v1 v2 
    else co
      
module OrderedFVar = struct
  type t = fvar
  let compare x y = compFVar x y
end

module VarMap = Mapset.Make(OrderedFVar)
module VarSet = Iset.Make(OrderedFVar)

module HashableVar = struct
  type t = fvar
  let equal x y = eqFVar x y
  let hash x = hashVar x
end

module VarH = Hashtbl.Make(HashableVar)

module OrderedFLoc = struct
  type t = flocation
  let compare x y = compFLoc x y
end

module FLocSet = Iset.Make (OrderedFLoc)

module OrderedOffset = struct
  type t = offset_val
  let compare x y = compOffset x y
end

module OffsetMap = Mapset.Make(OrderedOffset)
module OffsetSet = Iset.Make(OrderedOffset)


(************************************************************)

type fcontext = Summary_keys.sumKey
let string_of_context c = Summary_keys.string_of_sumKey c

(************************************************************)

type nfpSource = (fKey)
let string_of_nfpsource (fk) = string_of_int fk

module NFPS = Set.Make (
  struct
    type t = nfpSource
    let compare a b = Pervasives.compare a b
  end )

type nfpSources = NFPS.t

let d_nfpsources ts =
  if NFPS.is_empty ts then text "{}"
  else 
    text "{" ++ seq_to_doc (text "; ") 
      NFPS.iter (fun ts -> text (string_of_nfpsource ts))
      ts nil ++ text "}"
      
(* FOR DEBUGGING *)

(*

let rootSource = NFPS.singleton (-4)
let voidNfpSource = NFPS.singleton (-3)
let constNfpSource = NFPS.singleton (-2)
let lookupNfp = NFPS.singleton (-1)

let makeNfpSource fkey =
  NFPS.singleton fkey

*)

let rootSource = NFPS.empty
let voidNfpSource = NFPS.empty
let constNfpSource = NFPS.empty
let lookupNfp = NFPS.empty

let makeNfpSource fkey =
  NFPS.empty


(*********************************************************
  Values
 *********************************************************)

(* Field Map Set ID *)
type fmsID = int

type ptrSet = FLocSet.t

type fvalue = 
    FNFP of nfpSources
  | FInt of Int64.t
  | FpRef of fvar
  | Refs of ptrSet
  | Records of recordPart list
  | NCRecord of recordPart (* non-correlated records *)
  | FIRecs of fvalue (* field insensitive *)

and fieldMap = fvalue OffsetMap.t

and recordPart = (fieldMap * offset_val)

(*********************************************************
  Memory: Heap/Stack/Globals
 *********************************************************)

type memory = fvalue VarMap.t

type varAttrs = vAttrib VarMap.t

(* per program-point state *)
type fpState = {
  bindings : memory;
  vAttrs : varAttrs;
}

let varsOfVarMap vm =
  VarMap.fold (fun v _ cur -> VarSet.add v cur) vm VarSet.empty

type varTypes = type_id VarMap.t


(************************************************************)

(* Operations on ptr sets *)

let emptyPtrSet = FLocSet.empty

(* Is it actually possible to have duplicate summary/non-summary offs? *)
let combinePtrSets (ls1 : ptrSet) ls2 =
  FLocSet.union ls1 ls2 

let intersectPtrSets (ls1 : ptrSet) ls2 =
  FLocSet.inter ls1 ls2 

let addToPtrSet (var, off) (ls : ptrSet) =
  FLocSet.add (var, off) ls 

let removePtrSet (var, off) (ls : ptrSet) =
  FLocSet.remove (var, off) ls 

let inPtrSet (var, off) (ls : ptrSet) =
  FLocSet.mem (var, off) ls


(************ Handling NULL + Extern + NFP **************)


let noOff : offset_val = 0
let topOff : offset_val = -1

let noOffSum : offset_sum = (0, false) (* Hmm... why is it always false? *)


let isNoOffset o = o = noOff
let isTopOffset o = o = topOff

let isNoOffsetSum (o, _) = o = noOff

let string_of_offsetval (i : offset_val) = string_of_int i

let maxOff (i1 : offset_val) (i2) = max i1 i2
let string_of_offsum ((i, s) : offset_sum) = 
  (string_of_int i ^ " " ^ string_of_bool s)

let markSumOff (o, _) : offset_sum =
  (o, true)

let withinBounds (off: offset_val) (maxOff : offset_val) =
  off <= maxOff

let divOff (o1 : offset_val) (o2 : offset_val) =
  if !fullFI then 
    (* JAN HACK *)
    0
  else 
    o1 / o2
      
      
let moduloOff (o1 : offset_val) (o2 : offset_val) =
  if !fullFI then
    (* JAN HACK *)
    0
  else o1 mod o2

class type specialVarType = object

  method getVinfo : varinfo
  method getVID : varID
  method getVar : fvar
  method getLoc : flocation

  method isVID : varID -> bool
  method isVar : fvar -> bool
  method isLoc : flocation -> bool
  method isMem : FLocSet.t -> bool

  method addToSet : FLocSet.t -> FLocSet.t

end

class specialVar tag typ : specialVarType = 
  let specialVinfo = Cil_lvals.mkVarinfo true ("$" ^ tag) typ in
  let vid = vidOfVarinfo specialVinfo in
  let var = FVar vid in
object (self) 

  val loc = (var, (noOff, false))
      
  method getVinfo = specialVinfo
  method getVID = vid
  method getVar = var
  method getLoc = loc

  method isVID x = compVarID vid x == 0
  method isVar x = eqFVar var x
  method isLoc x = compFLoc loc x == 0
  method isMem ls = inPtrSet loc ls
  
  method addToSet targSet = addToPtrSet loc targSet

end

let nullVar = new specialVar "0" voidType
let nfpVar = new specialVar "NFP" voidType
let extVar = new specialVar "EXT" (TFun (voidType, None, false, []))

let nullFP = FpRef (nullVar#getVar)

let initFPVar = extVar (* pick this for now... *)
let initFP = FpRef (initFPVar#getVar)

let isSpecialVar v =
  (nfpVar#isVar v || nullVar#isVar v || extVar#isVar v)

let stripSpecial locs =
  FLocSet.filter 
    (fun (v, o) -> not (isSpecialVar v)) locs
    
let stripSpecialVars vs =
  VarSet.filter (fun v -> not (isSpecialVar v)) vs

let isRetVar var = match var with FRet _ -> true | _ -> false

exception NullException

let varinfoOfVid vid = 
  if nullVar#isVID vid then nullVar#getVinfo
  else if nfpVar#isVID vid then nfpVar#getVinfo
  else if extVar#isVID vid then extVar#getVinfo
  else Cilinfos.getVarinfo vid


(********** Ops on input-based vars ***********)

exception CantStripDeref

let makeInVar (accPath, newLen) =
  FInput (accPath, newLen)

let stripADeref inVar =
  match inVar with 
    FInput (ap, oldLen) ->
      (match ap with 
         AccDeref :: t -> makeInVar (t, oldLen - 1)
       | AccField _ :: _ | AccVar _ :: _ | [] -> 
           raise CantStripDeref)
  | _ -> raise CantStripDeref

(** Always put a some offset (maybe 0) before a deref *)
let attachDeref (ap, oldLen) =
  match ap with
    AccField _ :: _ -> (AccDeref :: ap, oldLen + 1)
  | AccVar _ :: _
  | AccDeref :: _ -> (AccDeref :: AccField noOff :: ap, oldLen + 2)
  | [] -> failwith "attachDeref on to empty AP"

let reattachADeref inVar =
  match inVar with
    FInput ap  -> 
      let newAP, newLen = attachDeref ap in
      makeInVar (newAP, newLen)
  | _ -> failwith "reattachADeref: non input var "

let stripADerefVars inVars =
 List.map stripADeref inVars


let concatOffset outerOff innerOff =
  if !fullFI then (* JAN HACK *)
    noOff
  else 
    if isTopOffset outerOff || isTopOffset innerOff then
      topOff
    else outerOff + innerOff
    (* TODO: be able to check here if you are trying to add
       an offset for a function variable? *)

let concatOffsetSum (oVal, oSum) (iVal, iSum) =
  if !fullFI then
    (* JAN HACK *)
    (noOff, true)
  else if isTopOffset oVal || isTopOffset iVal then
    (topOff, oSum || iSum)
  else (oVal + iVal, oSum || iSum)


let subtractOff  o1 o2 =
  if !fullFI then (* JAN HACK *)
    noOff
  else 
    if isTopOffset o1 || isTopOffset o2 then
      topOff
    else o1 - o2
  

let subtractOffSum (v1, s1) (v2, s2) =
  if !fullFI then 
    (* JAN HACK *)
    (noOff, true)
  else
    if isTopOffset v1 || isTopOffset v2 then
      (topOff, s1 || s2)
    else (v1 - v2, s1 || s2)

let invertOff o =
  if isTopOffset o then topOff else -o

let invertOffSum (v, s) =
  if isTopOffset v then (topOff, s) else (-v, s)

let concatOffsetVar var (outerOff, sum1) (innerOff, sum2) = 
  if !fullFI then
    (* JAN HACK *)
    (noOff, true)
  else 
    if isTopOffset outerOff || isTopOffset innerOff then
      (topOff, true)
    else if isSpecialVar var then (innerOff, sum2)
    else  (outerOff + innerOff, sum1 || sum2)
      
      
let addField (prevAP, oldLen) moreOff =
  match prevAP with
    AccField off :: t -> 
      (AccField (concatOffset moreOff off) :: t, oldLen)
  | AccDeref :: _ -> 
      (AccField moreOff :: prevAP, oldLen + 1)
  | AccVar _ :: _ -> 
      (AccField moreOff :: prevAP, oldLen + 1)
  | [] -> failwith "addField empty path"

let reattachFieldAcc inVar off =
  match inVar with
    FInput ap -> 
      let newAP, newLen = addField ap off in
      makeInVar (newAP, newLen)
  | _ -> failwith "reattachFieldAcc: non input var "

let isInputNode var =
  match var with FInput _ -> true | _ -> false

let rec compressApOffs off t =
  match t with
    AccField o2 :: t2 -> compressApOffs (concatOffset off o2) t2
  | _ -> (off, t)


(********** Offset stuff **********)

let widthModArray typ =
  try bitsSizeOf (typeModArray typ)
  with SizeOfError _ -> 0

module CilO2OCache = Cache.Make (
  struct
    type t = (bool * Cil.typ * Cil.offset)
    let equal (kc1, t1, o1) (kc2, t2, o2) =
      kc1 == kc2 && Type_utils.equal_type t1 t2 
      && Ciltools.compare_offset o1 o2 == 0
    let hash (kc, t, o) =
      (Hashtbl.hash kc) lxor (Type_utils.hash_type t)
      lxor (Ciltools.hash_offset o lsl 8)
  end )

let cilo2oc = CilO2OCache.create 2048
  
(** Convert a cil offset to bits.
    If [keepConst] is true then array indices are not normalized to the
    summary offset if it is a constant *)
let cilOff2Offset keepConst baseType off =
  if !fullFI then
    (* JAN HACK *)
    noOffSum
  else begin 
    try
      CilO2OCache.find cilo2oc (keepConst, baseType, off)    
    with Not_found ->
      let off, summarized = Offset.canonicizeOff keepConst off in
      let bitsOff, _ = Cil.bitsOffset baseType off in
      ignore (CilO2OCache.add cilo2oc 
                (keepConst, baseType, off) (bitsOff, summarized));
      (bitsOff, summarized)
  end


module O2CilOCache = Cache.Make (
  struct 
    type t = (Cil.typ * int)
    let equal (t1, o1) (t2, o2) =
      o1 == o2 && Type_utils.equal_type t1 t2
    let hash (t, o) =
      Hashtbl.hash o lxor Type_utils.hash_type t
  end )


let o2ciloc = O2CilOCache.create 2048

let offset2CilOff baseType bits =
  try O2CilOCache.find o2ciloc (baseType, bits)
  with Not_found ->
    let off = Offset.bitsToOffsetNoWrap baseType bits in
    ignore (O2CilOCache.add o2ciloc (baseType, bits) off);
    off

let o2cilocScalar = O2CilOCache.create 2048

(** Find a cil offset that's actually a scalar *)
let offset2CilOffScalar baseType bits =
  try O2CilOCache.find o2cilocScalar (baseType, bits)
  with Not_found ->
    let allOffs = Offset.bitsToOffsetAll baseType bits in
    try 
      let off = List.find 
        (fun cilOff -> 
           let cilT = typeOffset baseType cilOff in
           not (isStructType cilT)) allOffs in
      ignore (O2CilOCache.add o2cilocScalar (baseType, bits) off);
      off
    with Not_found ->
      raise Offset.UnknownOffset


module WO2CilOCache = Cache.Make (
  struct 
    type t = (Cil.typ * int * int)
    let equal (t1, w1, o1) (t2, w2, o2) =
      w1 = w2 && o1 == o2 && Type_utils.equal_type t1 t2
    let hash (t, w, o) =
      (Hashtbl.hash w lsl 4) lxor Hashtbl.hash o lxor Type_utils.hash_type t
  end )

let o2cilocNonScalar = WO2CilOCache.create 2048

(** Find a cil offset that's NOT a scalar *)
let offset2CilOffNonScalar baseType width bits : Cil.offset =
  try WO2CilOCache.find o2cilocNonScalar (baseType, width, bits)
  with Not_found ->
    let allOffs = Offset.bitsToOffsetAll baseType bits in
    (* just find the "closest" width that is still > than width *)
    let offOpt = 
      List.fold_left
        (fun cur cilOff -> 
           let cilT = typeOffset baseType cilOff in
           if isStructType cilT then
             let w = widthModArray cilT in
             if w >= width then 
               (match cur with 
                  None -> Some (cilOff, w)
                | Some (_, oldW) -> 
                    if oldW <= w then cur else Some (cilOff, w))
             else cur
           else cur
        ) None allOffs in
    match offOpt with 
      None -> raise Offset.UnknownOffset 
    | Some (o, _) -> 
        ignore (WO2CilOCache.add o2cilocNonScalar 
                  (baseType, width, bits) o);
        o


(*********** Compare ************)

let setIsNull ls =
  FLocSet.is_singletonX (nullVar#getLoc) ls
    
let rec compareVal fv1 fv2 =
  if fv1 == fv2 then 0
  else match fv1, fv2 with
    Refs ls1, Refs ls2 -> FLocSet.compare ls1 ls2
  | Records rs1, Records rs2 -> List_utils.compare compareRecords rs1 rs2
        
  | FpRef v1, FpRef v2 -> compFVar v1 v2

  (* Lame special-casing for null *)
  | FpRef v, FInt _
  | FInt _, FpRef v ->
      if nullVar#isVar v then 0 else Pervasives.compare fv1 fv2
  | Refs ls, FInt _ -> if setIsNull ls then 0 else 1
  | FInt _, Refs ls -> if setIsNull ls then 0 else -1

  | FIRecs r1, FIRecs r2 ->
      compareVal r1 r2

  | NCRecord (fm1, m1), NCRecord (fm2, m2) ->
      compareRecords (fm1, m1) (fm2, m2)
        
  | Records [(fm1, m1)], NCRecord (fm2, m2)
  | NCRecord (fm1, m1), Records [(fm2, m2)] ->
      compareRecords (fm1, m1) (fm2, m2)

  | _, _ -> Pervasives.compare fv1 fv2
      
and compareRecords (fm1, m1) (fm2, m2) =
  let c = compOffset m1 m2 in
  if c == 0 then compareFM fm1 fm2
  else c

and compareFM fm1 fm2 =
  if fm1 == fm2 then 0
  else OffsetMap.compare compareVal fm1 fm2

(* Exact equality checks *)

let eqVals v1 v2 = 
  compareVal v1 v2 == 0

let eqFM a b = 
  compareFM a b == 0

let compareAtt a1 a2 = Pervasives.compare a1 a2

let eqAtt a1 a2 = a1 = a2

let hashAtt a = Hashtbl.hash a

let eqTypeID (t1:type_id) t2 = t1 = t2

(***************** Manage single records ******************)

let inlineRecord outerRec innerRec innerRecOff curMax =
  OffsetMap.fold 
    (fun innerOff v (cur, curMax) -> 
       let newOff = concatOffset innerOff innerRecOff in
       OffsetMap.add newOff v cur, maxOff curMax newOff)
    innerRec (outerRec, curMax)


(** Go through the (offset, value) mappings in nonfp and if the value is a 
    record, then flatten that. In the end, we only have one level of offsets *)
let rec flattenRecord (fm, curMax) =

  let inlineInto off (inFM, inMax) (fm, curMax) =
    let fm = OffsetMap.remove off fm in
    let newIn = flattenRecord (inFM, maxOff curMax inMax) in
    List.map (fun (inFM, newMax) ->
                let newFM, newMax = inlineRecord fm inFM off newMax in
                (newFM, newMax)) newIn
  in
  let flattenOne fm cur =    
    OffsetMap.fold 
      (fun off v cur ->
         match v with
           FNFP _ | FpRef _ | FInt _ | Refs _ -> cur
         | Records rs -> 
             List_utils.mapCross 
               (fun innerRec -> 
                  List_utils.mapCross (inlineInto off innerRec) cur) rs
         | FIRecs _ -> failwith "flatten record: inner is FIRec?"
         | NCRecord _ -> failwith "flatten record: inner is NCRecord?"
      ) fm cur
  in
  flattenOne fm [(fm, curMax)]
    (* don't flatten the fp part for now ... *)

(** Take record components and return a list of record components
    where each offset is mod curMax *)
let wrapARecord curMax (fm, m) =
  if withinBounds m curMax then [(fm, m)]
  else 
    (* Bin the elements into an array, based on offset, 
       then merge each of the bins *)
    let fmArray = GrowArray.make 8 (GrowArray.Elem OffsetMap.empty) in
    let wrapOffMap array map =
      OffsetMap.iter 
        (fun o v ->
           let arrayInd = divOff o curMax in 
           let newOff = moduloOff o curMax in
           let oldOffMap = GrowArray.getg array arrayInd in
           GrowArray.setg array arrayInd 
             (OffsetMap.add newOff v oldOffMap)
        ) map
    in
    let arrayToRecs () =
      GrowArray.fold_lefti
        (fun curRecs ind fm ->
           let curMax = 
             try fst (OffsetMap.max_binding fm) with Not_found -> noOff in
           (fm, curMax) :: curRecs
        ) [] fmArray
    in
    let () = wrapOffMap fmArray fm in
    arrayToRecs ()

      
let wrapRecords width recs =
  List_utils.mapCross (wrapARecord width) recs


(***************** Hashing ******************)

let rec hashVal v =
  match v with
    FInt _ | FNFP _ | FpRef _ -> 908324 lxor (Hashtbl.hash v)
  | Records [(fm, m)] -> hashRecord (fm, m) (* make compatible w/ compareVal *)
  | Records rs -> List_utils.hash hashRecord rs
  | FIRecs fir -> hashVal fir
  | NCRecord (fm, m) -> hashRecord (fm, m)
  | Refs locs ->
      FLocSet.fold
        (fun (v, o) cur -> (cur lsl 1) lxor (hashVar v)) locs 63636

and hashRecord (fm, m) =
  (Hashtbl.hash m lsl 10) lxor (hashFM fm)

and hashOffVal o v =
  Hashtbl.hash o lxor hashVal v

and hashFM fm =
  OffsetMap.sampleHash hashOffVal fm

let hashVarVal var v =
  hashVar var lxor hashVal v

let hashVarAtt var att =
  hashVar var lxor hashAtt att

let hashStore st = 
  VarMap.sampleHash hashVarVal st

let hashAttribs atts =
  VarMap.sampleHash hashVarAtt atts

let hashState st = 
  hashStore st.bindings lxor (hashAttribs st.vAttrs lsl 2)


(************************************************************
  Reset global state...
************************************************************)

type nfpOutData = 
    { mutable nfpEmbed : embeddingRelation; 
      mutable nfpOffsetOfs : offsetOfSet; }

let nfpOutData = 
  { nfpEmbed = Inthash.create 0; 
    nfpOffsetOfs = Inthash.create 0; }

let initNfpOutData rootDir =
  nfpOutData.nfpEmbed <- getEmbeddingRelation ();
  nfpOutData.nfpOffsetOfs <- getOffsetOfSet rootDir
    
let gAddrTaken = ref None

let initGAddrTaken rootDir =
  logStatus "Initializing global addr-taken";
  let gt = Global_addr_taken.getGlobalAddrTaken rootDir in
  Global_addr_taken.printAddrTaken gt;
  let gt = Global_addr_taken.toSet gt in
  gAddrTaken := Some gt
    
let globalHadAddrTaken vid =
  match !gAddrTaken with
    Some gt -> IntSet.mem vid gt
  | None -> failwith "didn't initialize global addr-taken"


(*********************************************************
 * Special instances of addresses, states, etc.
 *********************************************************)

let emptyState = {
  bindings = VarMap.empty;
  vAttrs = VarMap.empty;
}

let funkyVar = FInput ([], 0)
let funkyAttrib = { vType = addTyp (TVoid []);
                    vKind = HSingle; }


let bottomState = {
  bindings = VarMap.add funkyVar (FInt (Int64.of_int 12345)) VarMap.empty;
  vAttrs = VarMap.add funkyVar funkyAttrib VarMap.empty;
}
  
let topState = {
  bindings = VarMap.add funkyVar (FInt (Int64.of_int 98765)) VarMap.empty;
  vAttrs = VarMap.add funkyVar funkyAttrib VarMap.empty;
}
  
let eqStores st1 st2 = 
  st1 == st2 || VarMap.equal eqVals st1 st2

let eqAttrs as1 as2 =
  VarMap.equal eqAtt as1 as2

let eqTypeMap tm1 tm2 =
  VarMap.equal eqTypeID tm1 tm2

let eqStates st1 st2 =
  (st1 == st2) || 
    ( eqStores st1.bindings st2.bindings && 
        eqAttrs st1.vAttrs st2.vAttrs )

let isBottomState st =
  eqStates bottomState st

let isTopState st =
  eqStates topState st

(** States that should not go through flow functions *)
let dontFlowFuncState st =
  isBottomState st || isTopState st

let compareStates st1 st2 =
  if st1 == st2 then 0
  else 
    let c = VarMap.compare compareVal st1.bindings st2.bindings in 
    if c == 0 then 
      VarMap.compare compareAtt st1.vAttrs st2.vAttrs
    else c


(************************************************************)

module HashableState = 
struct
  type t = fpState
  let equal a b = eqStates a b
  let hash a = hashState a
end                          


(*********************************************************
 * Test / Debug code
 *********************************************************)

class fp_type_printer = object(self)

  method indent n d =
    indent n d

  method d_pp pp =
    dprintf "(%d,%d)" pp.pp_stmt pp.pp_instr
    
  method d_pp_fk (pp, fk) =
    self#d_pp pp ++ num fk

  method d_heap_id cs () = 
    seq_to_doc (text ":") List.iter self#d_pp_fk cs nil

  method d_typ t () =
    text (string_of_type (findTyp t))

  method d_heap_var id =
    dprintf "HEAP<%t>" (self#d_heap_id id)
      
  method d_varinfo vi =
    let scope = if vi.vglob then "g" else "l" in
    text (vi.vname ^ ":" ^ scope ^ ":" ^ (string_of_int vi.vid))
    
  method d_accElem a =
    match a with
      AccDeref -> text " * "
    | AccVar vid -> let vi = varinfoOfVid vid in self#d_varinfo vi
    | AccField o -> 
        dprintf ".[%s]" (string_of_offsetval o)

  method d_accPath (ap, len) =
    seq_to_doc (text "<") List.iter self#d_accElem ap nil

  method d_var var =
    match var with
      FVar vid -> let vi = varinfoOfVid vid in self#d_varinfo vi
    | FInput ap -> self#d_accPath ap
    | FHeap hi -> self#d_heap_var hi.hID
    | FRet _ -> text "$RET"
        
  method d_rawoff prefix baseOff =
    prefix ++ dprintf ".[%s]" (string_of_offsetval baseOff)

  method d_off prefix typOpt off =
    if isTopOffset off then prefix ++ text ".TOFF"
    else match typOpt with
      Some cilType -> begin
        try
          let cilOffset = offset2CilOff cilType off in
          d_offset prefix () cilOffset ++ 
            dprintf ".[%s]" (string_of_offsetval off)
        with Offset.UnknownOffset -> 
          self#d_rawoff prefix off
      end
    | None -> 
        self#d_rawoff prefix off
        
  method d_pointerTarg (var, off_sum) =
    let bits = int_of_off off_sum in
    let sum = isSumOff off_sum in
    self#d_off (self#d_var var) None bits ++ (text (string_of_bool sum))

  method d_ptrSet header s =
    let header = header ++ text "{" in
    (seq_to_doc (text ", ") FLocSet.iter
       (fun target -> self#d_pointerTarg target) s header) ++ 
      dprintf "} (%d)" (FLocSet.cardinal s)


  method d_value typOpt v =
    match v with
      FNFP ts -> text "NFP: " ++ d_nfpsources ts
    | FInt i64 -> dprintf "Int: %Ld" i64
    | FpRef var -> 
        text "FpRef -> " ++  self#d_var var
    | Refs targets ->
        let header = text ("Refs:") in
        self#d_ptrSet header targets
    | Records rs ->
        dprintf "RecordSet |%d| { " (List.length rs) ++ line ++ 
          self#d_recordSet typOpt rs
        ++ line ++ text "}\n" 
    | FIRecs fir ->
        text "FIRecs: " ++ self#d_value None fir ++ line 
    | NCRecord (fm, maxOff) ->
        dprintf "Record {" ++ line ++
          (self#indent 2 (self#d_fieldMap typOpt fm)) ++ line ++
          dprintf "maxOff: %s\n}\n" (string_of_offsetval maxOff)
          
  method d_fieldMap typOpt fm =
    map_to_doc Pretty.line OffsetMap.iter
      (fun off v ->
         let offDoc = self#d_off Pretty.nil typOpt off in
         offDoc ++ text " -> " ++ (self#d_value typOpt v)
      ) fm Pretty.nil


  method d_record typOpt (fm, maxOff) =
    (self#indent 2 (self#d_fieldMap typOpt fm)) ++ line ++ 
      dprintf "maxOff: %s\n" (string_of_offsetval maxOff)
    
  method d_recordSet typOpt fms =
    seq_to_doc Pretty.line List.iter
      (fun r -> 
         text "<" ++ self#d_record typOpt r ++ text ">")
      fms Pretty.nil
      
  method d_hAtt att =
    let kind = match att.vKind with 
      HSingle -> "i"
    | HSum    -> "s"
    | HGlobal -> "g" in
    let typStr = string_of_type (findTyp att.vType) in
    dprintf "(%s : %d, %s)" typStr att.vType kind

  method d_heapAtts hAtts =
    text "{" ++ 
      map_to_doc (text ", ") VarMap.iter 
      (fun var att -> self#d_var var ++ text " : " ++ self#d_hAtt att)
      hAtts Pretty.nil ++
      text "}\n"

end

let defPrint = new fp_type_printer

let string_of_var var : string = 
  sprint 80 (defPrint#d_var var)

let string_of_var_list l =
  sprint 80 (seq_to_doc (text ", ") List.iter (fun x -> defPrint#d_var x)
               l nil)

let string_of_loc_list l =
  sprint 80 (seq_to_doc (text ", ") List.iter 
               (fun (v,o) -> defPrint#d_pointerTarg (v,o)) l nil)
    
let printVar var =
  logStatusD (defPrint#d_var var)

let string_of_pointer (addr, off) =
  sprint 80 (defPrint#d_pointerTarg (addr, off))

let string_of_val v =
  sprint 80 (defPrint#d_value None v)

let string_of_vAtt vAtt =
  sprint 80 (defPrint#d_hAtt vAtt)

let prefix = " -> "

let string_of_var_val var v =
  sprint 80 (defPrint#d_var var ++ text prefix ++ 
               defPrint#d_value None v)

let printVal v typOpt =
  logStatus prefix;
  logStatusD ((indent 2 (defPrint#d_value typOpt v)) ++ line)

let printHeapAtts atts =
  logStatusD (defPrint#d_heapAtts atts)
  
let printBinding atts var v = 
  try
    (match var with
       FVar vid ->
         printVar var;
         let vi = varinfoOfVid vid in 
         printVal v (Some vi.vtype)

     | FRet typ ->
         printVar var;
         printVal v (Some (findTyp typ))

     | FHeap _ | FInput _ ->
         let att = VarMap.find var atts in
         logStatusD (defPrint#d_var var ++ text ":" ++ defPrint#d_hAtt att);
         printVal v (Some (findTyp att.vType))
    )
  with Not_found ->
    logStatusD (defPrint#d_var var ++ text ": ???"); 
    printVal v None
    
      
let printBindings st =
  VarMap.iter (printBinding st.vAttrs) st.bindings
    
let printStateCore st =
  logStatusF "Bindings: (%d)\n" (Stdutil.mapSize st.bindings VarMap.fold);
  printBindings st;
  logStatusF "Heap atts: (%d)\n" (Stdutil.mapSize st.vAttrs VarMap.fold);
  printHeapAtts st.vAttrs
  

let printState st =
  if (isBottomState st) then
    logStatus "State is $BOTTOM\n"
  else if (isTopState st) then
    logStatus "State is $TOP\n"
  else begin
    logStatus "-------";
    printStateCore st;
    logStatus "-------";
  end


(** Fancier DOT version of state *)
let dotState st =
  failwith "TODO dotState"

(************************************************************)
(* Raw printer for hashing state, etc *)

class rawPrinter = object (self)
  inherit fp_type_printer 

  method indent n d = d

  method d_typ t () =
    num t

  method d_varinfo vi =
    text (string_of_int vi.vid)
    
  method d_var var =
    match var with
      FVar vid -> dprintf "vi(%d)" vid
    | FInput ap -> self#d_accPath ap
    | FHeap hi -> self#d_heap_var hi.hID
    | FRet _ -> text "$RET"
        
  method d_off prefix typOpt off =
    self#d_rawoff prefix off

end

let rawPrint = new rawPrinter

let rawdoc_of_store store curDoc =
  VarMap.fold 
    (fun var v doc -> doc ++ rawPrint#d_var var ++ rawPrint#d_value None v)
    store curDoc


(** String representation of state that is not meant for human readability *)
let rawstring_of_state st =
  if isBottomState st then "$BOTTOM"
  else if isTopState st then "$TOP"
  else
    let doc = text "Bindings:" in
    let doc = rawdoc_of_store st.bindings doc in
    let doc = doc ++ text "Heap atts: " in
    let doc = doc ++ rawPrint#d_heapAtts st.vAttrs in
    sprint 80 doc



(************************************************************)
(* Printer w/ no special chars for making filenames *)

class noSpecialPrinter = object(self)
  inherit fp_type_printer

  (* So far just make printing variables have no special chars *)

  method d_var var =
    match var with
      FVar vid -> 
        let vi = varinfoOfVid vid in
        dprintf "%s_%d" vi.vname vid
    | FInput ap -> self#d_accPath ap
    | FHeap hi -> self#d_heap_var hi.hID
    | FRet _ -> text "RET"

  method d_heap_var id =
    dprintf "HEAP_%t" (self#d_heap_id id)
      
  method d_heap_id cs () = 
    seq_to_doc (text "_") List.iter self#d_pp_fk cs nil

  method d_pp_fk (pp, fk) =
    dprintf "%d_%d_%d__" pp.pp_stmt pp.pp_instr fk
      
  method d_accElem a =
    match a with
      AccDeref -> text "d"
    | AccVar vid -> text ("var_" ^ string_of_int vid)
    | AccField o -> dprintf "f_%s" (string_of_offsetval o)

  method d_accPath (ap, len) =
    seq_to_doc (text "_") List.iter self#d_accElem ap nil

end

let noSpecialP = new noSpecialPrinter

let plain_string_var var =
  sprint 1024 (noSpecialP#d_var var)

(********** Type/size info for vars **********)

let combineCilTypes t1 t2 =
  if t1 == t2 then t1
  else if structSubtype t1 t2 then begin
    t1
  end else if structSubtype t2 t1 then begin
    t2
  end else begin
    let res = pickWidest t1 t2 in
    if not (isPolyInt t1 || isPolyInt t2) then begin
      let ts1 = string_of_type t1 in
      let ts2 = string_of_type t2 in
      logErrorF "combineTypes %s ~ %s -> %s @ %s\n" 
        ts1 ts2 (string_of_type res) (string_of_loc !currentLoc);
    end;
    res
  end

module ComboTH = Hashtbl.Make
  (struct
     type t = type_id * type_id
     let equal (t1, t2) (t1', t2') = (t1 = t1' && t2 = t2')
     let hash (t1, t2) = t1 lxor t2
   end)

let comboTHCache = ComboTH.create 17

let combineTypes tid1 tid2 =
  if tid1 = tid2 then begin
    tid1
  end else 
    try
      let res = ComboTH.find comboTHCache (tid1, tid2) in
      res
    with Not_found ->
      let t1 = findTyp tid1 in
      let t2 = findTyp tid2 in
      let resT = combineCilTypes t1 t2 in
      let res = addTyp resT in
      ComboTH.add comboTHCache (tid1, tid2) res;
      res


let warnVAttH = Hashtbl.create 17 
let warnVAttrNF msg =
  if Hashtbl.mem warnVAttH msg then ()
  else begin
    Hashtbl.add warnVAttH msg ();
    logError msg
  end

let typeIDofNonProgVar atts var =
  try 
    let attr = VarMap.find var atts in
    attr.vType
  with Not_found -> 
(*
    warnVAttrNF (Printf.sprintf "vAttr NF typeID %s @ %s" 
                   (string_of_var var) (string_of_loc !currentLoc));
*)
    unknownMallocIndex

exception UnknownType

let typeOfOff baseT off =
  if isNoOffset off then baseT 
  else try
    let coff = offset2CilOff baseT off in
    canonType (typeOffset baseT coff)
  with Offset.UnknownOffset ->
    raise UnknownType


let typeIDofVarID vid =
  let vi = varinfoOfVid vid in
  addTyp vi.vtype

let rec typeIDOfFvar atts fv =
  match fv with
    FVar vid -> typeIDofVarID vid
  | FRet t -> t
  | FInput ([AccVar vid], _) ->
      typeIDofVarID vid 
        (* Hmm... not necessarily a good reflection of what it's used as...*)
  | FInput (AccField o :: t, _) ->
      let o, t = compressApOffs o t in
      let len = List.length t in 
      let baseTID = typeIDOfFvar atts (makeInVar (t, len)) in
      addTyp (typeOfOff (findTyp baseTID) o)
  | FInput _ | FHeap _ -> 
      typeIDofNonProgVar atts fv

let typeOfFVar atts fv = 
  match fv with 
    FVar vid -> let vi = varinfoOfVid vid in vi.vtype 
  | FRet t -> findTyp t
  | FInput _ | FHeap _-> 
      findTyp (typeIDOfFvar atts fv)
        
let isUnknownMallocVar atts var =
  match var with
    FVar _ | FRet _ -> false
  | FInput _ | FHeap _ -> 
      let tid = typeIDOfFvar atts var in
      isImpreciseMallocID tid

let unknownScalarType = TPtr ((TVoid []), [])

let typeOfOffScalar baseT off =
  try
    let coff = offset2CilOffScalar baseT off in
    canonType (typeOffset baseT coff)
  with Offset.UnknownOffset ->
    if isNoOffset off then
      if isImpreciseMalloc baseT || isStructType baseT
      then unknownScalarType
      else baseT
    else unknownScalarType

let nonInputTypeOfLocScalar atts (var, off) =
  let baseT = typeOfFVar atts var in
  typeOfOffScalar baseT (int_of_off off) 

let typeOfLocScalar atts (var, off) =
  match var with
    FInput ap ->
      let extendedAP = attachDeref (addField ap (int_of_off off)) in
      let baseT = typeOfFVar atts (makeInVar extendedAP) in
      let t = if isImpreciseMalloc baseT then baseT else TPtr (baseT, []) in
      let t2 = nonInputTypeOfLocScalar atts (var, off) in
      findTyp (combineTypes (addTyp t) (addTyp t2))
  | FHeap _ | FVar _ | FRet _ -> nonInputTypeOfLocScalar atts (var, off)

      

let typeOfLocNonScalar atts width (var, off) =
  let baseT = typeOfFVar atts var in
  let bits = int_of_off off in
  try
    let coff = offset2CilOffNonScalar baseT width bits in
    canonType (typeOffset baseT coff)
  with Offset.UnknownOffset ->
    if isNoOffset bits then baseT
    else Trans_alloc.unknownMallocType
      
let typeOfLocChoice atts (isStruct, width) (var, off) =
  if isStruct then typeOfLocNonScalar atts width (var, off)
  else typeOfLocScalar atts (var, off)
      
let typeOfLoc atts (var, off) =
  let baseT = typeOfFVar atts var in
  typeOfOff baseT (int_of_off off)


let typeModArrayFVar atts var =
  typeModArray (typeOfFVar atts var) 

let scalarWidth = (Cil.initCIL (); bitsSizeOf (TInt (ILong, [])))
let scalarSI = (false, scalarWidth)

let isScalar (isStruct, width) =
  not isStruct

let getStructInfo typ =
  let typ = typeModArray typ in
  (isStructType typ, widthModArray typ)

let structInfoFVar atts v = 
  let t = typeModArrayFVar atts v in
  getStructInfo t

let maxOffsetOfRecs recs =
  List.fold_left (fun cur (_, m) -> maxOff m cur) noOff recs

let structInfoVal v = 
  match v with 
    Records recs -> (true, maxOffsetOfRecs recs + scalarWidth)
  | NCRecord (fm, m) -> (true, m)
  | FIRecs _ -> (true, scalarWidth) (* Don't really know the width *)
  | FpRef _ | Refs _ | FInt _ | FNFP _ -> scalarSI 
      

let nonPtrInt t =
  match typeModArray (Cil_lvals.unrollTypeNoAttrs t) with
    TInt (ikind, _) -> 
      (match ikind with
         IChar | ISChar | IUChar | IShort | IUShort -> true (* too short *)
       | IInt | ILong | ILongLong  -> false (* Signed -- leave for now *)
       | _ -> false)
  | _ -> false


let alreadyInPart (var, o) cur =
  List.exists (List.mem (var, o)) cur

(** Partition the vars we may merge based on type? *)
let collectTypeParts atts (var, o1) cur =
  if alreadyInPart (var, o1) cur then cur
  else try
    let t = canonType (typeOfLoc atts (var, o1)) in
    List_utils.addToPartition 
      (fun (otherVar, o2) ->
         (* wasted repeat type generation ... *)
         (int_of_off o1) = (int_of_off o2) &&
          let otherT = canonType (typeOfLoc atts (otherVar, o2)) in
          Type_utils.equal_type t otherT 
      ) (var, o1) cur
  with UnknownType ->
    [(var, o1)] :: cur
      
let partitionByType atts locs = 
  FLocSet.fold (collectTypeParts atts) locs []

(** Partition the vars we may merge based on offset *)
let collectOffsetPart (var, o1) cur =
  if alreadyInPart (var, o1) cur then cur
  else 
    List_utils.addToPartition (fun (otherVar, o2) -> o1 = o2) (var, o1) cur
      
let partitionByOffset locs = 
  FLocSet.fold collectOffsetPart locs []


(********* Default values for non-input variables *********)

let emptyRecordParts = (OffsetMap.empty, noOff)

let emptyRecord = Records [emptyRecordParts]

(* default value for unbound key is NULL / empty record *)
let defaultValNonInputType t =
  if isStructType t then emptyRecord
  else FInt (Int64.zero) 

let defaultValNonInput (key:fvar) = 
  FInt (Int64.zero)

(* sucks that we don't know the width from the raw_offset *)
let defaultRecordVal key = FInt (Int64.zero)

let defaultAttr var = 
  { vKind = HSingle;
    vType = unknownMallocIndex; }

(****** Base lookups *******)

let hasBinding var st =
  VarMap.mem var st.bindings

let getBinding var st =
  VarMap.find var st.bindings

let addBinding oldSt var v =
  { oldSt with bindings = VarMap.add var v oldSt.bindings; }

let getVarAttr var st =
  VarMap.find var st.vAttrs
    
let addVarAttr var att st =
  { st with 
      vAttrs = VarMap.add var att st.vAttrs; }

(************************************************************)

(* Normalize records wrt to FP fields across other records *)

let funToLoc var =
  (var, (noOff, false))

let isFunc (var, off) =
  match var with 
    FVar vid ->
      let vi = varinfoOfVid vid in
      isFunctionType vi.vtype && isNoOffsetSum off
  | FHeap _ | FRet _ -> false
  | FInput _ -> false (* actually unclear... but no access to state... *)


let isFunc2 attribs (var, off) =
  isNoOffsetSum off && 
    isFunctionType (typeOfLocScalar attribs (var, off))

let mentionsFP v =
  match v with
    FpRef _ -> true
  | Refs ls -> FLocSet.exists isFunc ls 
  | FNFP _ | FInt _ | Records _ | FIRecs _ | NCRecord _ -> false
  
(** Determine which offsets hold function pointers *)
let collectIfFP off v cur =
  if mentionsFP v then OffsetSet.add off cur else cur

let collectFPOffsets cur (fm, m) =
  OffsetMap.fold collectIfFP fm cur

let collectOffsets cur (fm, m) =
  OffsetMap.fold (fun off _ cur -> OffsetSet.add off cur) fm cur

let collectAllPlusFPOffs (curFP, curAll) (fm, m) =
  OffsetMap.fold (fun off v (curFP, curAll) ->
                    let fp = collectIfFP off v curFP in
                    let all = OffsetSet.add off curAll in
                    (fp, all)
                 ) fm (curFP, curAll)

let sortRecs recs =
  List.sort (fun (fm1, _) (fm2, _) -> compareFM fm1 fm2) recs

(************************************************************)


exception PromoteFPFail of string

(** Return a list of FPs held by the current value *)
let rec promoteToFP v =
  match v with
    FpRef _ -> [v]
  | FInt _ -> [initFP] 
  | Refs ls ->
      let fpVals, nonfpsLocs = 
        FLocSet.fold 
          (fun (var, o) (fpVals, nonfpLocs) -> 
             (* ignore null for now... *)
             if nullVar#isVar var || nfpVar#isVar var 
             then (fpVals, nonfpLocs)
             else if extVar#isVar var 
             then (List_utils.addOnce fpVals (FpRef extVar#getVar), nonfpLocs)
             else if isFunc (var, o) 
             then (List_utils.addOnce fpVals (FpRef var), nonfpLocs)
             else
               if isInputNode var && isNoOffsetSum o then
                 (* Assume it's an FP for now... maybe we just don't 
                    have the precise type information to show it... *)
                 (List_utils.addOnce fpVals (FpRef var), nonfpLocs)
               else 
                 (fpVals, addToPtrSet (var, o) nonfpLocs)
          ) ls ([], FLocSet.empty) in
      if not (FLocSet.is_empty nonfpsLocs) then begin
        (* Some of them we just can't promote *)
        let nonfpVal = Refs nonfpsLocs in
        nonfpVal :: fpVals
      end else 
        if fpVals = [] then [initFP] else fpVals
  | FNFP ts -> [initFP] (* ... *)
  | Records recs ->
      logErrorF "Promote Record to FP %s\n" (string_of_val v);
      List_utils.mapCross
        (fun (fm, m) ->
           try promoteToFP (OffsetMap.find noOff fm)
           with Not_found -> []) recs
  | FIRecs fir ->
      logErrorF "Promote Record to FP %s\n" (string_of_val v);
      [fir]
  | NCRecord (fm, m) -> 
      logErrorF "Promote Record to FP %s\n" (string_of_val v);
      (try
         [OffsetMap.find noOff fm]
       with Not_found ->
         [initFP])
        

let updateFpOffRecs off curRecs newVs =
  List_utils.mapCross 
    (fun (fm, m) ->
       List.map (fun fpV -> (OffsetMap.add off fpV fm, maxOff m off)) newVs
    ) curRecs
      

let promoteToFPMapping curRecs off oldV =
  try
    let newVs = promoteToFP oldV in
    (* If the newVs is just the oldV, then don't do anything... *)
    match newVs with
      [x] -> 
        if x == oldV then curRecs 
        else updateFpOffRecs off curRecs newVs
    | [] -> failwith ("lost values from promoteToFP: " ^ string_of_val oldV)
    | _ -> updateFpOffRecs off curRecs newVs
  with PromoteFPFail msg ->
    logError msg;
    raise (PromoteFPFail msg)

exception HitMaxRecs

let normalizeFPFields fixKeys (fm, maxoff) =
  OffsetSet.fold 
    (fun k curRecs ->
       let origV = 
         try OffsetMap.find k fm 
         with Not_found -> defaultRecordVal k in
       let nextRecs = List_utils.mapCross 
         (fun (curFM, curM) -> 
            promoteToFPMapping [(curFM, curM)] k origV) curRecs in
       if List.length nextRecs > !recsMax then raise HitMaxRecs
       else nextRecs
    ) fixKeys [(fm, maxoff)]


let getUsedOffsetsFM acc srcFM =
  OffsetMap.fold (fun o _ cur -> OffsetSet.add o cur) srcFM acc 
                 
let getUsedOffsets srcOfOffs =
  List.fold_left (fun acc (fm, _) ->
                    getUsedOffsetsFM acc fm) OffsetSet.empty srcOfOffs

let projectOffsFM offs fm =
  OffsetMap.fold (fun o v (cur, m) -> 
                    if OffsetSet.mem o offs then (OffsetMap.add o v cur, max m o)
                    else (cur, m)) fm (OffsetMap.empty, noOff)
    

(************************************************************)

let getLocation keepConst vi cilOff =
  let newOff = cilOff2Offset keepConst vi.vtype cilOff in    
  let vid = vidOfVarinfo vi in
  FVar vid, newOff

let isGlobalFVar atts v =
  match v with 
    FVar vid -> 
      let vi = varinfoOfVid vid in 
      vi.vglob 
  | FInput ([x], _) -> false
  | FInput _ | FHeap _  -> 
      let att = VarMap.find v atts in
      att.vKind = HGlobal
  | FRet _ -> false

let rec isGlobalDebug desc default atts var =
  let doWarning desc default atts var =
    logErrorF "isGlobal %s: NF %s @ %s\n" desc (string_of_var var)
      (string_of_loc !currentLoc);
    logStatusF "Heap atts: (%d)\n" (Stdutil.mapSize atts VarMap.fold);
    printHeapAtts atts;
    default
  in
  let rec loop curVar =
    try isGlobalFVar atts curVar
    with Not_found ->
      match curVar with 
        FInput ([_], _)
      | FVar _ | FRet _ | FHeap _ -> doWarning desc default atts var
      | FInput _ ->
          (* Otherwise we can use the transitivity / prefix property
             (if a prefix is global then the current must be global as well) *)
          let shorter = stripADeref curVar in
          (match shorter with
             FInput (AccDeref :: _, _)
           | FInput (AccVar _ :: _, _) -> loop shorter
           | FInput (AccField f :: t, _) ->
               let _, t = compressApOffs f t in
               let len = List.length t in
               loop (makeInVar (t, len))
           | FInput ([], _) -> doWarning desc default atts var
           | _ -> failwith "stripADeref changed var kind")
  in
  loop var
             

(************************************************************
 Filter relevant values via FP reachability
************************************************************)

exception CannotWeaken

(** Return true if the typ/typ-based-on-value does not reach a funptr *)
let notFwdReach typ vOpt atts =
  not (hitsFunptrDeep typ) &&
    match vOpt with
      Some (FpRef _) -> false
    | Some (Refs ls) ->         
        let t = FLocSet.fold 
          (fun (var, o) cur -> 
             try combineTypes cur (addTyp (typeOfLoc atts (var, o)))
             with UnknownType -> cur
          ) ls unknownMallocIndex in
        not (hitsFunptrDeep (findTyp t))
    | _ -> true (* Ignore records / unknown vals... *)
        

(** Use offsetOf patterns to determine backwards ancestors *)
let bwdCollectOffsetOf atts (var, baseOff) cur =
  if isUnknownMallocVar atts var then raise CannotWeaken 
  else 
    let baseType = typeOfFVar atts var in
    IS.union cur 
      (possibleAncestors nfpOutData.nfpOffsetOfs (baseType, int_of_off baseOff))
      
(** Use embedding relations to determine backwards ancestors *)
let bwdCollectEmbed atts (var, off) cur =
  if isUnknownMallocVar atts var then raise CannotWeaken 
  else
    let baseType = typeOfFVar atts var in
    match Cil_lvals.unrollTypeNoAttrs baseType with
      TComp (ci, _) -> 
        let cur = IS.add ci.ckey cur in
        IS.union cur (transAncestors nfpOutData.nfpEmbed ci.ckey)
    | _ -> cur
        
let bwdCollectCompsPtr atts (var, off) cur =
  if !offsetOfWeaken 
  then bwdCollectOffsetOf atts (var, off) cur 
    (* how to tell if var is a "HEAD" / sentinel node? *)
  else bwdCollectEmbed atts (var, off) cur

let bwdCollectAncestorsVal atts oldVal =
  match oldVal with
    FpRef _ -> raise CannotWeaken
  | Records _ | NCRecord _ | FIRecs _ -> 
      logError "bwdCollectAncestorsVal given record";
      raise CannotWeaken
  | FNFP _ -> raise CannotWeaken
  | FInt i -> 
      if Int64.compare Int64.zero i == 0 then raise CannotWeaken 
      else IS.empty
  | Refs ls ->
      (* Test against special vars?
         TODO: What if it's a pointer to a var where we know its value 
         as well? E.g., for pointers to pointers. See sqlite3SetString *)
      if setIsNull ls then raise CannotWeaken
      else FLocSet.fold 
        (fun (v, o) cur -> 
           if isSpecialVar v then cur
           else bwdCollectCompsPtr atts (v, o) cur) ls IS.empty
        
let bwdReachAncestors ancestors =
  IS.exists 
    (fun sck -> 
       let srcCi = Cilinfos.getCinfo sck in
       List.exists (fun fi -> hitsFunptrDeep fi.ftype) srcCi.cfields
    ) ancestors

let notBwdReachVal atts oldVal =
  let bwdReaches =
    try
      let ancestors = bwdCollectAncestorsVal atts oldVal in
      bwdReachAncestors ancestors
    with 
      CannotWeaken -> true (* same conservative assumption as above *)
    | Not_found ->
        logErrorF "notBwdReachVal NF: %s\n" (string_of_val oldVal);
        true
  in
  not bwdReaches

let neverReachesFPType atts t oldVal =
  if isImpreciseMalloc t then false
  else nonPtrInt t || (notFwdReach t (Some oldVal) atts
                       && notBwdReachVal atts oldVal)
  
let neverReachesFPVar atts var oldVal =
  let t = typeOfFVar atts var in
  neverReachesFPType atts t oldVal

let notBwdReachField atts valOpt =
  match valOpt with 
    Some v -> notBwdReachVal atts v
  | None -> false
      (* conservatively say that, since we don't know the kinds of
         targets this pointer will point to, we don't know that 
         it won't bwdReach *)

let neverReachesFPField atts valOpt ftype =
  nonPtrInt ftype ||
    (notFwdReach ftype valOpt atts && notBwdReachField atts valOpt)
    
(*********************************************************
 * Misc
 *********************************************************)

let isReference v =
  match v with FpRef _ | Refs _ -> true | _ -> false

let isRecordVal v =
  match v with Records _ | NCRecord _ | FIRecs _ -> true | _ -> false

let isScalarVal v =
  match v with Records _ | NCRecord _ | FIRecs _ -> false | _ -> true


let makeMayref t1 t2 =
  Refs (addToPtrSet t2 (FLocSet.singleton t1))

let promoteToRecVal v =
  (OffsetMap.add noOff v OffsetMap.empty, noOff)
  
let promoteToRecord v =
  match v with
    FInt _  | FNFP _ -> [promoteToRecVal v]
  | FpRef _ -> [promoteToRecVal v]
  | Refs ls ->
      let fps, nonfps = FLocSet.partition isFunc ls in
      let fprecs = FLocSet.fold 
        (fun (var, o) cur ->
           promoteToRecVal (FpRef var) :: cur) fps []  in
      let recs = 
        if not (FLocSet.is_empty (stripSpecial nonfps)) then
          let nonfpVal = Refs nonfps in
          (promoteToRecVal nonfpVal :: fprecs)
        else if fprecs = [] then begin
          logErrorF "empty RecordSet @ %s - %s\n" 
            (string_of_loc !currentLoc) (string_of_val v);
          [promoteToRecVal (Refs nonfps)]
        end else fprecs in
      sortRecs recs
  | Records _ | FIRecs _ | NCRecord _ -> 
      failwith "promoteToRecord given record?"


let isEmptyRec (fm, _) = OffsetMap.is_empty fm

(** Align source record (which is in terms of 0 offset) to the target
    which is offset by [delta] *)
let rec alignRecordOff delta v =
  if isNoOffset delta then v
  else
    let alignFM fm =
      OffsetMap.fold (fun o2 v cur -> 
                        OffsetMap.add (concatOffset o2 delta) v cur) 
        fm OffsetMap.empty in
    match v with
      Records recs ->
        Records (List.map (fun (fm, m) -> 
                             (alignFM fm, concatOffset m delta)) recs)

    | NCRecord (fm, m) -> NCRecord (alignFM fm, concatOffset m delta)

    | FIRecs _ -> v
    | FNFP _ | FInt _ | FpRef _ | Refs _ -> 
        alignRecordOff delta (Records (promoteToRecord v))

let alignModOffs delta mods =
  if isNoOffset delta then mods
  else
    OffsetSet.fold 
      (fun o2 cur -> OffsetSet.add (concatOffset o2 delta) cur) 
      mods OffsetSet.empty


(************************************************************)

let rec foldTargetsVal foo v cur =
  match v with
    FNFP _ | FInt _ -> cur
  | FpRef var -> foo (var, (noOff, false)) cur
  | Refs ls ->
      FLocSet.fold foo ls cur
  | Records recs ->
      List.fold_left (fun cur (fm, _) -> 
                        foldTargetsRecord foo fm cur) cur recs

  | FIRecs fir -> foldTargetsVal foo fir cur

  | NCRecord (fm, _) ->
      foldTargetsRecord foo fm cur

and foldTargetsRecord foo fm cur =
  let folder o v cur = foldTargetsVal foo v cur in
  OffsetMap.fold folder fm cur

let flocsMap foo set =
  FLocSet.fold (fun x cur -> addToPtrSet (foo x) cur) set FLocSet.empty


(************************************************************)

let mapNonFPFIR mapper unchanged fir =
  List.fold_left
    (fun (ch, fir) (fp, nonfp) ->
       let newV = mapper nonfp in
       if unchanged nonfp newV then (ch, (fp, nonfp) :: fir)
       else (true, (fp, newV) :: fir)
    ) (false, []) fir

(************************************************************)


let combineVarKinds a1 a2 =
  match a1, a2 with
    HSingle, HSingle -> a1
  | (HSum as s), HSingle 
  | HSingle, (HSum as s)
  | (HSum as s), HSum -> s
  | (HGlobal as g), _ 
  | _, (HGlobal as g) -> g
        
let combineAttr a1 a2 =
  if a1 == a2 then a1
  else 
    { vKind = combineVarKinds a1.vKind a2.vKind;
      vType = combineTypes a1.vType a2.vType; }

let combineAttributes atts1 atts2 =
  if atts1 == atts2 then atts1 
  else VarMap.union combineAttr atts1 atts2

let combineTypeMap tm1 tm2 =
  if tm1 == tm2 then tm1
  else VarMap.union combineTypes tm1 tm2


let combineModOffs offs1 offs2 = 
  OffsetSet.union offs1 offs2

(************** Misc stuff ****************)

let doFilter atts pT (var, o) cur =
  if isSpecialVar var then cur
  else if isUnknownMallocVar atts var then cur
  else 
    try
      let vt = canonType (typeOfLoc atts (var, o)) in
      if isFunctionType vt then cur
      else if structSubtype vt pT then cur
      else 
        ((* Try just warning and not removing for now *)
          logErrorF "Mess: non-ST: %s :/> %s @ %s w/ %s\n"
           (string_of_type vt) (string_of_type pT) 
           (string_of_loc !currentLoc) (string_of_pointer (var, o));
        (* FLocSet.remove (var, o) cur *) 
          cur
        )
    with UnknownType -> cur


let filterImpreciseWrite atts v t =
  match v with
    FNFP _ | FInt _ | FpRef _ 
  | Records _ | NCRecord _ -> v (* Not comparing fields... checked earlier *)
  | Refs locs ->
      if isPoly t then v
      else 
        (match t with
           TPtr (pT, _) ->
             let newLocs = FLocSet.fold (doFilter atts (canonType pT)) 
               locs locs in
             if newLocs == locs then v
             else Refs newLocs
         | _ -> v)
  | FIRecs _ ->
      v (* Pretty much guaranteed to have imprecise sets *)


let updateType state cilT var off =
  match var with
    FInput _ | FHeap _ ->
      if isNoOffsetSum off then
        try
          let oldAtt = getVarAttr var state in
          let newT = combineTypes oldAtt.vType (addTyp cilT) in
          let newAtt = { oldAtt with vType = newT; } in
          addVarAttr var newAtt state
        with Not_found -> 
          logErrorF "WTF: updateType attr NF %s @ %s\n" (string_of_var var)
            (string_of_loc !currentLoc);
(* don't update, because we could screw up the vKind
   addVarAttr var { vType = addTyp cilT; vKind = HSingle; } state
*)
          state
      else state

  | FVar _ | FRet _ -> state


(************************************************************)

(* Tests / operations on input-based-vars *)

let failOnVar msg var =
  failwith (msg ^ string_of_var var)

    
let getInputVarVID var =
  match var with 
    FInput (ap, _) -> 
      let rec loop ap =
        match ap with
          [AccVar vid] -> Some vid
        | _ :: t -> loop t
        | [] -> failwith ("malformed acc path (1)" ^ string_of_var var)
      in loop ap
  | _ -> None

let extendsFormal formals var =
  match getInputVarVID var with
    Some vid -> List.exists (fun vi -> vi.vid = vid) formals
  | None -> false

let rec getPrevHost ap =
  match ap with
    AccDeref :: t ->
      let _, prevAP = compressApOffs noOff t in
      prevAP
  | AccField _ :: t ->
      getPrevHost t
  | _ -> failwith ("getPrevHost went too far back? " ^ 
                     string_of_var (makeInVar (ap, List.length ap)))

let hasOnlyOneDeref accPath =
  let rec loop alreadySeen rest =
    match rest with
      [] -> alreadySeen
    | AccDeref :: t -> if alreadySeen then false else loop true t
    | AccField _ :: t -> loop alreadySeen t
    | AccVar _ :: t -> loop alreadySeen t
  in
  loop false accPath

let rec hasAnyDeref accPath =
  match accPath with 
    [] -> false 
  | AccDeref :: t -> true
  | _ :: t -> hasAnyDeref t



(*** Basics for making initial values ***)


let addInitialAttrib state targVar targTyp targKind =
  let newAtt = { vKind = targKind; vType = addTyp targTyp; } in
  try 
    let old = VarMap.find targVar state.vAttrs in
    let newAtt = combineAttr old newAtt in
    { state with vAttrs = VarMap.add targVar newAtt state.vAttrs; }
  with Not_found ->
    { state with vAttrs = VarMap.add targVar newAtt state.vAttrs; }


let getPrevVKind attribs (prevAP, len) =
  match prevAP with
  | [AccVar _] -> HSingle
  | _ ->
      let rec loopFind attribs origAP curAP curLen =
        match curAP with
        | AccDeref :: t ->
            let var = FInput (curAP, curLen) in
            (try
               let oldAtt = VarMap.find var attribs in
               oldAtt.vKind
             with Not_found ->
               loopFind attribs origAP t (curLen - 1))
              
        | AccField _ :: t -> loopFind attribs origAP t (curLen - 1)
        | AccVar _ :: _ | [] -> 
            raise Not_found 
      in
      loopFind attribs prevAP prevAP len


let miscDefault expectedT =
  defaultValNonInputType expectedT

exception NonInputVar

let getAccPath curFunc atts var : accPath option =
  match var with
    FVar vid ->
      let vi = varinfoOfVid vid in
      if Ciltools.isFormal curFunc vi then Some ([AccVar vid], 1)
      else None
  | FInput ap -> 
      (* Don't make a default if it's already been turned into a global.
         The backward unification + marking this lookup as a global
         reader should make cells further down the deref chain known. *)
      if not (isGlobalDebug "getAccPath" false atts var) then Some (ap)
      else None
  | FHeap _ -> None
  | FRet _ -> None

(************************************************************)

(** Bring predicted types from atts into tm *)
let combineTypeAttributes tm atts =
  VarMap.fold 
    (fun var att curTM ->
       try
         let oldT = VarMap.find var tm in
         let comboT = combineTypes att.vType oldT in
         if eqTypeID oldT comboT then curTM
         else VarMap.add var comboT curTM
       with Not_found ->
         VarMap.add var att.vType curTM
    ) atts tm


(** Bring predicted types from tm into atts *)
let combineAttrTypes atts tm =
  VarMap.fold
    (fun var t curAtts ->
       try
         let oldAtt = VarMap.find var atts in
         let comboT = combineTypes oldAtt.vType t in
         if eqTypeID oldAtt.vType comboT then curAtts
         else VarMap.add var { oldAtt with vType = comboT; } curAtts
       with Not_found ->
         (match var with
            (* Don't bother to bring it in *)
            FVar _ | FHeap _ | FRet _ -> curAtts 
          | FInput (AccDeref :: _ as ap, _) ->
              let prevKind = 
                try
                  let prevHost = getPrevHost ap in
                  let prevLen = List.length prevHost in
                  getPrevVKind atts (prevHost, prevLen)
                with Not_found ->
                  HSingle
              in
              let att = { vType = t; vKind = prevKind; } in
              VarMap.add var att curAtts
          | FInput (AccVar _ :: _, _) | FInput ([], _) -> curAtts
          | FInput (AccField _ :: _, _) -> failwith "malformed ap"
         )
    ) tm atts
