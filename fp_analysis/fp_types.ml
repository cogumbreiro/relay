
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
let heapMax = ref 2
let offsetOfWeaken = ref false 
let heapRecMax = ref 2

(*********************************************************
  Variables and addresses
 *********************************************************)

type varID = int
type raw_offset = int
type type_id = int

let vidOfVarinfo vi = vi.vid

(* boolean indicating if variable is a struct + width indicating size *)
type structInfo = bool * int

(* Support for naming heap variables *)
type callstring = (prog_point * fKey) list
type heapID = callstring
type heapKind = 
    HSingle
  | HSum
  | HGlobal

type heapVarinfo =
    { hID: heapID; 
      hTyp: type_id; }
      

(* Support for naming input ghost variables based on access path *)
type accElem = 
    AccVar of varID                 (* must start at a program variable *)
  | AccDeref of type_id * raw_offset  (* type of destination! *)
  | AccField of raw_offset          (* offset from previous dude *)


(* cells that can be looked up in the store *)
type fvar = 
    FVar of varID               (* global / stack arrays not summary nodes *)
  | FHeap of heapVarinfo
  | FRet of type_id
  | FInput of accElem list

(* addresses -- targets of pointers *)
type flocation = fvar * raw_offset


(*** Misc data structures ***)

let compVarID v1 v2 =
  v1 - v2

let compOffset o1 o2 =
  o1 - o2

let compTypID t1 t2 =
  t1 - t2

let compHeapID id1 id2 =
  Pervasives.compare id1 id2 

let compareHeapinfo hi1 hi2 =
  let c = compTypID hi1.hTyp hi2.hTyp in
  if c == 0 then compHeapID hi1.hID hi2.hID
  else c

let comp_accElem a b =
  match a, b with 
    AccDeref (t1, o1), AccDeref (t2, o2) ->
      let c = compTypID t1 t2 in
      if c == 0 then compOffset o1 o2
      else c
  | _, _ -> Pervasives.compare a b

let comp_accPath ap1 ap2 =
  List_utils.compare comp_accElem ap1 ap2

let hashHeapinfo hi =
  List_utils.hash (Hashtbl.hash) hi.hID 

let hashAccElem x = 
  match x with
    AccDeref (t, o) -> (hashTypID t) lxor (Hashtbl.hash o)
  | AccField o -> 1847092 lxor Hashtbl.hash o
  | AccVar vid -> 23904 lxor Hashtbl.hash vid

let hashAccPath ap =
  List_utils.hash hashAccElem ap

let hashVar var = 
  match var with
    FVar x -> 930487 lxor (Hashtbl.hash x)
  | FInput ap -> 43257 lxor (hashAccPath ap)
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

let eqFVar v1 v2 = compFVar v1 v2 == 0

let compFLoc ((v1, o1) as l1) ((v2, o2) as l2) =
  if l1 == l2 then 0
  else 
    let co = compOffset o1 o2 in
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
  type t = raw_offset
  let compare x y = Pervasives.compare x y
end

module OffsetMap = Mapset.Make(OrderedOffset)
module OffsetSet = Set.Make(OrderedOffset)


(** true if given fvar refers to the same vid *)
let isVarinfoFVar vi v =
  match v with 
    FVar vi2 -> compVarID vi vi2 == 0
  | FInput _ | FHeap _ | FRet _  -> false

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

type fvalue = 
    FNFP of nfpSources
  | FInt of Int64.t
  | FpRef of varID
  | Refs of FLocSet.t
  | Records of recordPart list
  | FIRecs of (fvalue * fvalue) list (* set of collapsed FP/NonFP records *)

and fieldMap = fvalue OffsetMap.t

and recordPart = fieldMap * fieldMap * int


(*********************************************************
  Memory: Heap/Stack/Globals
 *********************************************************)

type memory = fvalue VarMap.t

type heapAttrs = heapKind VarMap.t

(* per program-point state *)
type fpState = {
  bindings : memory;
  hAttrs : heapAttrs;
(* add mapping generated by aggressive merges? *)

}

(************ Handling NULL + Extern + NFP **************)


let noOff = 0

let topOff = -1

let isNoOffset o = o = noOff
let isTopOffset o = o = topOff
let maxOff curMax baseOff =
  max baseOff curMax


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

  val loc = (var, noOff)
      
  method getVinfo = specialVinfo
  method getVID = vid
  method getVar = var
  method getLoc = loc

  method isVID x = compVarID vid x == 0
  method isVar x = eqFVar var x
  method isLoc x = compFLoc loc x == 0
  method isMem ls = FLocSet.mem loc ls
  
  method addToSet targSet = FLocSet.add loc targSet

end

let nullVar = new specialVar "0" voidType
let nfpVar = new specialVar "NFP" voidType
let extVar = new specialVar "EXT" (TFun (voidType, None, false, []))

let nullFP = FpRef (nullVar#getVID)

let isSpecialVar v =
  (nfpVar#isVar v || nullVar#isVar v || extVar#isVar v)

let stripSpecial locs =
  FLocSet.remove extVar#getLoc 
    (FLocSet.remove nullVar#getLoc
       (FLocSet.remove nfpVar#getLoc locs))

let isRetVar var = match var with FRet _ -> true | _ -> false

exception NullException

let varinfoOfVid vid = 
  if nullVar#isVID vid then nullVar#getVinfo
  else if nfpVar#isVID vid then nfpVar#getVinfo
  else if extVar#isVID vid then extVar#getVinfo
  else Cilinfos.getVarinfo vid



(********** Offset stuff **********)

let rec canonicizeOff (off:Cil.offset) : (Cil.offset * bool) =  
  match off with
    NoOffset -> (NoOffset, false)
  | Field (fi, moreOff) -> 
      let (rest, changed) = canonicizeOff moreOff in
      (Field (fi, rest), changed)
  | Index (indExp, moreOff) ->
      (* Convert the index to 0 if non-constant, but leave if constant *)
      let (rest, changed) = canonicizeOff moreOff in
      (match isInteger indExp with
         Some _ -> (Index (indExp, rest), changed)
       | None -> (Index (Cil_lvals.cZero, rest), true))

let cilOff2Offset baseType off =
  let off, summarized = canonicizeOff off in
  let bitsOff, _ = Cil.bitsOffset baseType off in
  bitsOff

let offset2CilOff baseType bits =
  Offset.bitsToOffsetNoWrap baseType bits

let concatOffset outerOff innerOff =
  if isTopOffset outerOff || isTopOffset innerOff then
    topOff
  else outerOff + innerOff
    
let concatOffsetVar var outerOff innerOff = 
  if isTopOffset outerOff || isTopOffset innerOff then
    topOff
  else if isSpecialVar var then innerOff else outerOff + innerOff

(********** Type/size info for vars **********)

      
let isUnknownMallocVar var =
  match var with
    FVar _ | FRet _ -> false
  | FInput ap -> 
      (match List.hd ap with
         AccDeref (t, _) -> isImpreciseMallocID t
       | AccVar _ | AccField _ -> false)
  | FHeap hi -> isImpreciseMallocID hi.hTyp

exception UnknownType

let typeIDofVarID vid =
  let vi = varinfoOfVid vid in
  addTyp vi.vtype

let typeIDOfFvar fv =
  match fv with
    FVar vid -> typeIDofVarID vid
  | FInput ap ->
      (match List.hd ap with
         AccDeref (t, _) -> t
       | AccVar vid -> typeIDofVarID vid
       | AccField _ -> failwith "typeID on AccField" )
  | FHeap hi -> hi.hTyp
  | FRet t -> t

let rec typeOfFVar fv = 
  match fv with 
    FVar vid -> let vi = varinfoOfVid vid in vi.vtype 
  | FInput ap -> typeOfAccPath ap
  | FHeap hi -> findTyp hi.hTyp 
  | FRet t -> findTyp t

and typeOfAccPath ap =
  (match List.hd ap with 
     AccDeref (t, o) -> findTyp t
   | AccVar vid -> let vi = varinfoOfVid vid in vi.vtype
   | _ -> failwith "accpath didn't end w/ var")

and typeOfLoc (var, off) =
  let baseT = typeOfFVar var in
  typeOfOff baseT off

and typeOfOff baseT off =
  if isNoOffset off then baseT 
  else try
    let coff = offset2CilOff baseT off in
    canonType (typeOffset baseT coff)
  with Offset.UnknownOffset ->
    raise UnknownType

(* Hack to match our modeling of arrays *)
let rec typeModArray typ =
  match typ with
    TComp _ -> typ
  | TNamed (t, _) -> typeModArray t.ttype
  | TArray (t, _, _) -> t
  | _ -> typ
      
let typeModArrayFVar var =
  typeModArray (typeOfFVar var) 

let widthModArray typ =
  try bitsSizeOf (typeModArray typ)
  with SizeOfError _ -> 0

let structInfoFVar v = 
  let t = typeModArrayFVar v in
  (isStructType t, widthModArray t)

let getStructInfo typ =
  let typ = typeModArray typ in
  (isStructType typ, widthModArray typ)

let scalarWidth = (Cil.initCIL (); bitsSizeOf (TInt (ILong, [])))

let isScalar (isStruct, width) =
  not isStruct

let nonPtrInt t =
  match typeModArray (Cil_lvals.unrollTypeNoAttrs t) with
    TInt (ikind, _) -> 
      (match ikind with
         IChar | ISChar | IUChar | IShort | IUShort  (* too short *)
       | IInt | ILong | ILongLong  -> true (* Signed *)
       | _ -> false)
  | _ -> false


(** Partition the vars we may merge based on type? *)
let collectTypeParts var cur =
  let t = canonType (typeOfFVar var) in
  List_utils.addToPartition 
    (fun otherVar ->
       (* wasted repeat type generation ... *)
       let otherT = canonType (typeOfFVar otherVar) in
       Ciltools.compare_type t otherT == 0 
    ) var cur
    
let partitionByType vars = 
  VarSet.fold collectTypeParts vars []    

let emptyRecord = Records [(OffsetMap.empty, OffsetMap.empty, 0)]

(* default value for unbound key is NULL / empty record *)
let defaultValTypeInfo sinfo =
  if isScalar sinfo then FInt (Int64.zero) 
  else emptyRecord

(* this is to be used only by strong update and weak update... *)
let defaultVarVal (key:fvar) = 
  defaultValTypeInfo (structInfoFVar key)

(* sucks that we don't know the width from the raw_offset *)
let defaultRecordVal (key:raw_offset) = FInt (Int64.zero)

let defaultAttr var =
  HSingle


(*********** Compare ************)

let setIsNull ls =
  FLocSet.is_singletonX (nullVar#getLoc) ls
    
let rec compareVal fv1 fv2 =
  if fv1 == fv2 then 0
  else match fv1, fv2 with
    Refs ls1, Refs ls2 -> FLocSet.compare ls1 ls2
  | Records rs1, Records rs2 -> List_utils.compare compareRecords rs1 rs2
        
  | FpRef vi1, FpRef vi2 -> compVarID vi1 vi2
      (* Lame special case comparison for null ... *)
  | FpRef vi, FInt _
  | FInt _, FpRef vi -> 
      if nullVar#isVID vi then 0 else Pervasives.compare fv1 fv2
  | Refs ls, FInt _ -> if setIsNull ls then 0 else 1
  | FInt _, Refs ls -> if setIsNull ls then 0 else -1

  | FIRecs r1, FIRecs r2 ->
      let len1 = List.length r1 in
      let len2 = List.length r2 in
      if len1 == len2 then List.fold_left2 compareFIRec 0 r1 r2
      else len1 - len2
        
  | _, _ -> Pervasives.compare fv1 fv2

and compareRecords (fp1, nonfp1, m1) (fp2, nonfp2, m2) =
  let c = m1 - m2 in
  if c == 0 then
    let c = compareFM fp1 fp2 in
    if c == 0 then compareFM nonfp1 nonfp2 
    else c
  else c

and compareFIRec c (fp1, nonfp1) (fp2, nonfp2) =
  if c == 0 then
    let c = compareVal nonfp1 nonfp2 in
    if c == 0 then compareVal fp1 fp2
    else c
  else c

and compareFM fm1 fm2 =
  if fm1 == fm2 then 0
  else OffsetMap.compare compareVal fm1 fm2

let eqVals v1 v2 = 
  compareVal v1 v2 == 0

let eqFM a b = 
  compareFM a b == 0


let compareAtt a1 a2 = Pervasives.compare a1 a2

let eqAtt a1 a2 = a1 = a2

let hashAtt a = Hashtbl.hash a

let equalFM fm1 fm2 = 
  fm1 == fm2 || OffsetMap.equalC eqVals defaultRecordVal fm1 fm2


(***************** Manage single records ******************)

let inlineRecord outerRec innerRec innerRecOff curMax =
  OffsetMap.fold 
    (fun innerOff v (cur, curMax) -> 
       let newOff = concatOffset innerOff innerRecOff in
       OffsetMap.add newOff v cur, maxOff curMax newOff)
    innerRec (outerRec, curMax)


(** Go through the (offset, value) mappings in nonfp and if the value is a 
    record, then flatten that. In the end, we only have one level of offsets *)
let rec flattenRecord (fp, nonfp, maxOff) =

  let inlineInto off (inFp, inNonfp, inMax) (fp, nonfp, maxOff) =
    let nonfp = OffsetMap.remove off nonfp in
    let newIn = flattenRecord (inFp, inNonfp, max maxOff inMax) in
    List.map (fun (inFp, inNonfp, newMax) ->
                let newFp, newMax = inlineRecord fp inFp off newMax in
                let newNonfp, newMax = inlineRecord nonfp inNonfp off newMax in
                (newFp, newNonfp, newMax)) newIn
  in
  
  OffsetMap.fold 
    (fun off v cur ->
       match v with
         FNFP _ | FpRef _ | FInt _ | Refs _ -> cur
       | Records rs -> 
           List_utils.mapCross 
             (fun innerRec -> 
                List_utils.mapCross (inlineInto off innerRec) cur) rs
       | FIRecs _ -> failwith "genDefault made a FIRec?"
    ) nonfp [(fp, nonfp, maxOff)]

  

(***************** Hashing ******************)

let rec hashVal v =
  match v with
    FInt _ | FNFP _ | FpRef _ -> 908324 lxor (Hashtbl.hash v)
  | Records rs -> List_utils.hash hashRecord rs
  | FIRecs fir ->
      List_utils.hash (fun (fp, nonfp) ->
                         (hashVal fp) lxor (hashVal nonfp)) fir
  | Refs locs ->
      FLocSet.fold
        (fun (v, o) cur ->
           (cur lsl 1) lxor (hashVar v)
        ) locs 63636

and hashRecord (fp, nonfp, m) =
  let h1 = hashFM fp in 
  let h2 = hashFM nonfp in
  1249889 lxor (h1 lsl 5) + h1 lxor h2 lxor (m lsl 8)

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
  hashStore st.bindings lxor (hashAttribs st.hAttrs lsl 2)


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
    
let initState () =
  ()


(*********************************************************
 * Special instances of addresses, states, etc.
 *********************************************************)

let emptyState = {
  bindings = VarMap.empty;
  hAttrs = VarMap.empty;
}

let funkyVar = FHeap ( { hTyp = addTyp (canonType (TVoid []));
                          hID = []; } )

let bottomState = {
  emptyState with bindings = 
    VarMap.add funkyVar (FInt (Int64.of_int 123456789)) VarMap.empty;
}

let topState = {
  emptyState with bindings =
    VarMap.add funkyVar (FInt (Int64.of_int 987654321)) VarMap.empty; }

let eqStores st1 st2 = 
  st1 == st2 || VarMap.equal eqVals st1 st2

let eqAttrs as1 as2 =
  VarMap.equal eqAtt as1 as2

let eqStates st1 st2 =
  (st1 == st2) || 
    ( eqStores st1.bindings st2.bindings && 
        eqAttrs st1.hAttrs st2.hAttrs )
    
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
    if c == 0 then VarMap.compare compareAtt st1.hAttrs st2.hAttrs
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

  method d_heap_var t id =
    dprintf "HEAP<%t:%t>" (self#d_typ t) (self#d_heap_id id)
      
  method d_varinfo vi =
    let scope = if vi.vglob then "g" else "l" in
    text (vi.vname ^ ":" ^ scope ^ ":" ^ (string_of_int vi.vid))
    
  method d_accElem a =
    match a with
      AccDeref (t, o) -> dprintf "(%t).[%d]" (self#d_typ t) o
    | AccVar vid ->
        num vid
    | AccField o -> dprintf ".[%d]" o

  method d_accPath ap =
    text "|" ++ seq_to_doc (text "<") List.iter self#d_accElem ap nil ++ text "|"

  method d_var var =
    match var with
      FVar vid -> let vi = varinfoOfVid vid in self#d_varinfo vi
    | FInput ap -> self#d_accPath ap
    | FHeap hi -> self#d_heap_var hi.hTyp hi.hID
    | FRet _ -> text "$RET"
        
  method d_rawoff prefix baseOff =
    prefix ++ dprintf ".[%d]" baseOff

  method d_off prefix varOpt off =
    if isTopOffset off then prefix ++ text ".TOFF"
    else match varOpt with
      Some var ->
        let cilType = typeOfFVar var in
        let cilType = typeModArray cilType in
        (try
           let cilOffset = offset2CilOff cilType off in
           (match cilOffset with
              NoOffset -> prefix
            | _ ->  d_offset prefix () cilOffset)
         with Offset.UnknownOffset -> 
           self#d_rawoff prefix off)
    | None -> self#d_rawoff prefix off
        
  method d_pointerTarg (var, off) =
    self#d_off (self#d_var var) (Some var) off


  method d_ptrSet header s =
    let header = header ++ text "{" in
    (seq_to_doc (text ", ") FLocSet.iter
       (fun target -> self#d_pointerTarg target) s header) ++ 
      dprintf "} (%d)" (FLocSet.cardinal s)

  method d_offsetSet varOpt os =
    text "[" ++ (seq_to_doc (text ", ") OffsetSet.iter 
                   (fun off -> self#d_off Pretty.nil varOpt off)
                   os Pretty.nil) ++ text "]\n"
      

  method d_value varOpt v =
    match v with
      FNFP ts -> text "NFP: " ++ d_nfpsources ts
    | FInt i64 -> dprintf "Int: %Ld" i64
    | FpRef vid -> 
        let vi = varinfoOfVid vid in 
        text "FpRef -> " ++  self#d_varinfo vi
    | Refs targets ->
        let header = text ("Refs:") in
        self#d_ptrSet header targets
    | Records rs ->
        dprintf "RecordSet |%d| { " (List.length rs) ++ line ++ 
          self#d_recordSet varOpt rs
        ++ line ++ text "}\n" 
    | FIRecs fir ->
        dprintf "FIRecs: (%d)\n" (List.length fir) ++
          self#d_firecords varOpt fir ++ line 
          

  method d_fieldMap varOpt fm =
    map_to_doc Pretty.line OffsetMap.iter
      (fun off v ->
         let offDoc = self#d_off Pretty.nil varOpt off in
         offDoc ++ text " -> " ++ (self#d_value varOpt v)
      ) fm Pretty.nil

  method private checkVarOpt varOpt =
    match varOpt with 
      None -> varOpt 
    | Some v -> 
        if isUnknownMallocVar v || 
          (let cilType = typeOfFVar v in
           isPoly cilType) then None
        else varOpt 

  method d_record varOpt (fp, nonfp, maxOff) =
    let varOpt = self#checkVarOpt varOpt in
    text "FP:\n" ++ (self#indent 2 (self#d_fieldMap varOpt fp)) ++ line ++
      text "NonFP:\n" ++ (self#indent 2 (self#d_fieldMap varOpt nonfp)) ++ 
      line ++ dprintf "maxOff: %d\n" maxOff
    
  method d_recordSet varOpt fms =
    let varOpt = self#checkVarOpt varOpt in
    seq_to_doc Pretty.line List.iter
      (fun (fp, nonfp, maxOff) -> 
         text "<" ++ self#d_record varOpt (fp, nonfp, maxOff) ++ text ">")
      fms Pretty.nil

  method d_firecords varOpt fir =
    let varOpt = self#checkVarOpt varOpt in
    seq_to_doc Pretty.line List.iter
      (fun (fp, nonfp) ->
         text "< FP: " ++ self#d_value varOpt fp ++ line ++
           text " NonFP: " ++ self#d_value varOpt nonfp ++ text " >")
      fir (text "[ ") ++ text "]\n"
      
  method d_hAtt att =
    match att with 
      HSingle -> text "i"
    | HSum    -> text "s"
    | HGlobal -> text "g"

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

let printVar var =
  logStatusD (defPrint#d_var var)

let string_of_pointer (addr, off) =
  sprint 80 (defPrint#d_pointerTarg (addr, off))

let string_of_val v =
  sprint 80 (defPrint#d_value None v)

let string_of_rawoff off =
  Printf.sprintf "[%d]" off

let prefix = " -> "

let string_of_var_val var v =
  sprint 80 (defPrint#d_var var ++ text prefix ++ 
               defPrint#d_value (Some var) v)

let printVal var v =
  logStatus prefix;
  logStatusD ((indent 2 (defPrint#d_value (Some var) v)) ++ line)

let printHeapAtts st =
  logStatusD (defPrint#d_heapAtts st.hAttrs)
    
let printState st =
  if (isBottomState st) then
    logStatus "State is $BOTTOM\n"
  else if (isTopState st) then
    logStatus "State is $TOP\n"
  else begin
    logStatus "-------";
    logStatus "Bindings: ";
    VarMap.iter (fun var v -> printVar var; printVal var v) st.bindings;
    logStatus "Heap atts: ";
    printHeapAtts st;
    logStatus "-------";    
  end

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
    | FHeap hi -> self#d_heap_var hi.hTyp hi.hID
    | FRet _ -> text "$RET"
        
  method d_off prefix varOpt off =
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
    let doc = doc ++ rawPrint#d_heapAtts st.hAttrs in
    sprint 80 doc



(****** Base lookups *******)

let hasBinding var st =
  VarMap.mem var st.bindings

let getBinding var st =
  VarMap.find var st.bindings

let addBinding oldSt var v =
  { oldSt with bindings = VarMap.add var v oldSt.bindings; }

let getHeapAttr var st =
  VarMap.find var st.hAttrs
    
let addHeapAttr var att st =
  { st with hAttrs = VarMap.add var att st.hAttrs; }



(************************************************************)

(* Normalize records wrt to FP fields across other records *)

exception PromoteFPFail of string

let funToLoc vi =
  (FVar vi, noOff)

let isFunc (var, off) =
  match var with 
    FVar vid ->
      let vi = varinfoOfVid vid in
      isFunctionType vi.vtype && isNoOffset off
  | _ -> false

let mentionsFP v =
  match v with
    FpRef _ -> true
  | Refs ls -> FLocSet.exists isFunc ls 
  | FNFP _ | FInt _ | Records _ | FIRecs _ -> false
  

(** Determine which offsets harbor function pointers *)
let collectIfFP off v cur =
  if mentionsFP v then OffsetSet.add off cur else cur

let collectFPOffsets cur (fp, nonfp, m) =
  let cur = OffsetMap.fold collectIfFP fp cur in
  OffsetMap.fold collectIfFP nonfp cur
    
(* But also subsume NULL funptrs w/ EXT funptrs *)

let defaultOFP off =
  nullFP

exception CantComboNullExt

let comboNullExtFP v1 v2 =
  match v1, v2 with
    FpRef vi1, FpRef vi2 ->
      if vi1 = vi2 then v1
      else if nullVar#isVID vi1 then
        if nullVar#isVID vi2 then v1
        else if extVar#isVID vi2 then v2
        else raise CantComboNullExt
      else if extVar#isVID vi1 then
        if nullVar#isVID vi2 then v1
        else if extVar#isVID vi2 then v2
        else raise CantComboNullExt
      else raise CantComboNullExt
  | _, _ -> failwith "nonfps in fp maps"

(** Comparison function that clusters NULL w/ EXT FPs *)
let compareClusterNullExtFP v1 v2 =
  match v1, v2 with
    FpRef vi1, FpRef vi2 ->
      if vi1 = vi2 then 0
      else if nullVar#isVID vi1 then
        if nullVar#isVID vi2 then 0
        else if extVar#isVID vi2 then 0
        else -1
      else if extVar#isVID vi1 then
        if nullVar#isVID vi2 then 0
        else if extVar#isVID vi2 then 0
        else -1
      else vi1 - vi2
  | _, _ -> failwith "nonfps in fp maps"

let trySubsumeFPMap fp1 fp2 =
  OffsetMap.unionC comboNullExtFP defaultOFP fp1 fp2

let clusterNullExtFP recs =
  List.sort 
    (fun (fp1, _, _) (fp2, _, _) ->
       OffsetMap.compareC compareClusterNullExtFP defaultOFP fp1 fp2
    ) recs

(** For Field-insensitive records *)
let trySubsumeFPVal fp =
  match fp with
    Refs ls -> 
      if nullVar#isMem ls && extVar#isMem ls 
      then Refs (FLocSet.remove nullVar#getLoc ls)
      else fp
  | FpRef vi -> (* May as well lift it to the next level *)
      Refs (FLocSet.singleton (funToLoc vi))
  | _ -> fp


let sortFIRecParts (fp1, _) (fp2, _) = 
  compareVal fp1 fp2

let clusterFIRecs fir =
  List.sort sortFIRecParts fir
  

(** Return a list of FPs held by the current value *)
let promoteToFP v =
  match v with
    FpRef _ -> [v]
  | FInt _ -> [nullFP]
  | Refs ls ->
      (* Go ahead and do the subsumption optimization early *)
      let ls = if nullVar#isMem ls && extVar#isMem ls 
      then (FLocSet.remove nullVar#getLoc ls) else ls in
      let fps = 
        FLocSet.fold 
          (fun (var, o) cur ->
             match var with
               FVar vi -> 
                 if nullVar#isVID vi then List_utils.addOnce cur nullFP
                 else if isFunc (var, o) then List_utils.addOnce cur (FpRef vi)
                 else cur
             | _ -> cur ) ls [] in
      if fps = [] then [nullFP] 
      else fps
  | FNFP ts -> 
      [nullFP]
  | Records _ | FIRecs _ -> 
      raise (PromoteFPFail "Can't promote Record to fp")


let updateFpOffRecs off curRecs newVs =
  List_utils.mapCross 
    (fun (fp, nonfp, m) ->
       List.map 
         (fun fpV -> 
            (OffsetMap.add off fpV fp, OffsetMap.remove off nonfp, 
             maxOff m off) ) newVs
    ) curRecs

let promoteToFPMapping curRecs off oldV =
  try
    let newVs = promoteToFP oldV in
    match newVs with
      [x] -> 
        if oldV == x then curRecs
        else updateFpOffRecs off curRecs newVs
    | _ -> updateFpOffRecs off curRecs newVs
  with PromoteFPFail msg ->
    logError msg;
    raise (PromoteFPFail msg)

let normalizeFPFields fixKeys (fp, nonfp, maxoff) =
  OffsetSet.fold 
    (fun k curRecs ->
       if OffsetMap.mem k fp then curRecs
       else 
         List_utils.mapCross 
           (fun (curFP, curNonFP, curM) ->              
              let v = 
                try OffsetMap.find k curNonFP 
                with Not_found -> defaultRecordVal k in
              promoteToFPMapping [(curFP, curNonFP, curM)] k v
           ) curRecs
    ) fixKeys [(fp, nonfp, maxoff)]

(************************************************************)


let getLocation vi cilOff =
  let newOff = cilOff2Offset vi.vtype cilOff in    
  let vid = vidOfVarinfo vi in
  (FVar vid, newOff)

let isGlobalHeapVar st v =
  match VarMap.find v st.hAttrs with
    HGlobal -> true
  | _ -> false 

let isGlobalFVar st v =
  match v with 
    FVar vid -> let vi = varinfoOfVid vid in vi.vglob 
  | FInput _ | FHeap _  -> isGlobalHeapVar st v 
  | FRet _ -> false

let isGlobalDebug desc default st var =
  try isGlobalFVar st var 
  with Not_found ->
    logErrorF "isGlobal %s: NF %s @ %s\n" desc (string_of_var var)
      (string_of_loc !currentLoc);
    default
    

(************************************************************
 Filter relevant values via FP reachability
************************************************************)

exception CannotWeaken

let notFwdReach typ =
  not (hitsFunptrDeep typ)

(** Use offsetOf patterns to determine backwards ancestors *)
let bwdCollectOffsetOf (var, baseOff) cur =
  if isUnknownMallocVar var then raise CannotWeaken 
  else 
    let baseType = typeOfFVar var in
    IS.union cur 
      (possibleAncestors nfpOutData.nfpOffsetOfs (baseType, baseOff))
      
(** Use embedding relations to determine backwards ancestors *)
let bwdCollectEmbed (var, off) cur =
  if isUnknownMallocVar var then raise CannotWeaken 
  else
    let baseType = typeOfFVar var in
    match Cil_lvals.unrollTypeNoAttrs baseType with
      TComp (ci, _) -> 
        let cur = IS.add ci.ckey cur in
        IS.union cur (transAncestors nfpOutData.nfpEmbed ci.ckey)
    | _ -> cur
        
let bwdCollectCompsPtr (var, off) cur =
  if !offsetOfWeaken 
  then bwdCollectOffsetOf (var, off) cur 
    (* how to tell if var is a "HEAD" / sentinel node? *)
  else bwdCollectEmbed (var, off) cur

let bwdCollectAncestorsVal oldVal =
  match oldVal with
    FpRef _ -> raise CannotWeaken
  | Records _ | FIRecs _ -> raise CannotWeaken
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
           else bwdCollectCompsPtr (v, o) cur) ls IS.empty
        
let bwdReachAncestors ancestors =
  IS.exists 
    (fun sck -> 
       let srcCi = Cilinfos.getCinfo sck in
       List.exists (fun fi -> hitsFunptrDeep fi.ftype) srcCi.cfields
    ) ancestors

let notBwdReachVal oldVal =
  let bwdReaches =
    try
      let ancestors = bwdCollectAncestorsVal oldVal in
      bwdReachAncestors ancestors
    with CannotWeaken -> true (* same conservative assumption as above *)
    | Not_found ->
        logErrorF "notBwdReachVal NF: %s\n" (string_of_val oldVal);
        true
  in
  not bwdReaches

let neverReachesFPType t oldVal =
  if isImpreciseMalloc t then false
  else nonPtrInt t || (notFwdReach t && notBwdReachVal oldVal)
  
let neverReachesFPVar var oldVal =
  let t = typeOfFVar var in
  neverReachesFPType t oldVal

let notBwdReachField valOpt =
  match valOpt with 
    Some v -> notBwdReachVal v
  | None -> false
      (* conservatively say that, since we don't know the kinds of
         targets this pointer will point to, we don't know that 
         it won't bwdReach *)

let neverReachesFPField valOpt ftype =
  nonPtrInt ftype ||
    (notFwdReach ftype && notBwdReachField valOpt)
    
(*********************************************************
 * Misc
 *********************************************************)

let isReference v =
  match v with FpRef _ | Refs _ -> true | _ -> false

let makeMayref t1 t2 =
  Refs (FLocSet.add t2 (FLocSet.singleton t1))

let promoteToRecFP v =
  (OffsetMap.add noOff v OffsetMap.empty, OffsetMap.empty, maxOff 0 noOff)
  
let promoteToRecNonfp v =
  (OffsetMap.empty, OffsetMap.add noOff v OffsetMap.empty, maxOff 0 noOff)
    
let promoteToRecord v =
  match v with
    FInt _  | FNFP _ -> 
      [promoteToRecNonfp v]
  | Refs ls ->
      let fps, nonfps = FLocSet.partition isFunc ls in
      if FLocSet.is_empty fps 
      then [promoteToRecNonfp (Refs nonfps)]
      else begin
        FLocSet.fold 
          (fun (var, o) cur ->
             match var with
               FVar vi -> promoteToRecFP (FpRef vi) :: cur
             | _ -> failwith "partition isFunc failed?"
          ) fps []
      end
  | FpRef _ -> 
      [promoteToRecFP v]
  | Records _ | FIRecs _ -> 
      failwith "promoteToRecord given record?"

let isEmptyRec (fp, nonfp, _) =
  OffsetMap.is_empty fp && OffsetMap.is_empty nonfp
    

(************************************************************)

let rec foldTargetsVal foo v cur =
  match v with
    FNFP _ | FInt _ | FpRef _ -> cur
  | Refs ls ->
      FLocSet.fold foo ls cur
  | Records recs ->
      List.fold_left
        (fun cur (fp, nonfp, m) ->
           foldTargetsRecord foo nonfp cur) cur recs
  | FIRecs fir ->
      List.fold_left 
        (fun cur (fp, nonfp) -> foldTargetsVal foo nonfp cur) cur fir

and foldTargetsRecord foo nonfp cur =
  OffsetMap.fold
    (fun o v cur -> foldTargetsVal foo v cur) nonfp cur


(************************************************************
 Renaming variables
************************************************************)

let makeMappings size =
  VarH.create size

(** Find the final new mapping for a heap ID if there is a new mapping *)
let findMapping newMappings oldVar =
  let rec loopFind oldVar =
    try
      let newVar = VarH.find newMappings oldVar in
      if oldVar == newVar || compFVar oldVar newVar == 0 
      then Some newVar
      else match loopFind newVar with
        Some x -> VarH.replace newMappings oldVar x; Some x
      | None -> Some newVar
    with Not_found -> None
  in
  loopFind oldVar

let findMappingNonOpt newMappings var =
  match findMapping newMappings var with 
    None -> var 
  | Some newVar -> newVar

(** Indicate that newHi is now used for oldHi *)
let updateMap newMappings newVar oldVar =
  match findMapping newMappings newVar, findMapping newMappings oldVar with
    None, Some (oRep) -> 
      VarH.replace newMappings oldVar newVar;
      VarH.replace newMappings oRep newVar
  | Some nRep, None ->
      VarH.replace newMappings oldVar nRep;
      VarH.replace newMappings newVar nRep
  | Some nRep, Some oRep ->
      VarH.replace newMappings oldVar nRep;
      VarH.replace newMappings newVar nRep;
      VarH.replace newMappings oRep nRep
  | None, None ->
      VarH.replace newMappings oldVar newVar

let heapVarChanged newMappings var =
  match findMapping newMappings var with
    Some newVar -> 
      if newVar == var then None
      else Some newVar
  | None -> None

let mapNonFPFIR mapper unchanged fir =
  List.fold_left
    (fun (ch, fir) (fp, nonfp) ->
       let newV = mapper nonfp in
       if unchanged nonfp newV then (ch, (fp, nonfp) :: fir)
       else (true, (fp, newV) :: fir)
    ) (false, []) fir

(************************************************************)

(* Stuff shared by generalizer and others... *)

let isInputNode var =
  match var with FInput _ -> true | _ -> false

(** Translate 'a var to a concrete type *)
let makeConcrete var t =
  match var with
    FHeap hi -> FHeap { hi with hTyp = t; }
  | _ -> failwith "makeConcrete given non-heap var"


(** Try to coerce 'a vars mentioned in pointers into being concrete *)
let checkAlpha v t st =
(*
  match v with
    FNFP _ | FInt _ | FpRef _ 
  | Records _ -> 
      (v, st) (* Not comparing fields... checked when first storing *)
  | Refs locs ->
      if isPoly t then (v, st)
      else 
        (match t with
           TPtr (pT, _) ->
             let mappings = makeMappings 1 in
             FLocSet.iter 
               (fun (var, o) -> 
                  if isUnknownMallocVar var && not (isInputNode var) then
                    updateMap mappings (makeConcrete var pT) var) locs;
             if VarH.length mappings > 0 then begin
               logStatusF "Alphas: 'a to %s\n" (string_of_type pT);
               (updatePointersValNonOpt mappings v,
                updateStateMappings st mappings)
             end else (v, st)
         | _ -> (v, st))
*)
  (v, st)

let doFilter pT (var, o) cur =
  if isSpecialVar var then cur
  else if isUnknownMallocVar var then cur
  else 
    try
      let vt = canonType (typeOfLoc (var, o)) in
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


let filterImpreciseWrite v t =
  match v with
    FNFP _ | FInt _ | FpRef _ 
  | Records _ -> v (* Not comparing fields... checked earlier *)
  | Refs locs ->
      if isPoly t then v
      else 
        (match t with
           TPtr (pT, _) ->
             let newLocs = FLocSet.fold (doFilter (canonType pT)) locs locs in
             if newLocs == locs then v
             else Refs newLocs
         | _ -> v)
  | FIRecs _ ->
      v (* Pretty much guaranteed to have imprecise sets *)
