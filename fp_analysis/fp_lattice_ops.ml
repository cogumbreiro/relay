
open Cil
open Logging
open Cildump
open Type_reach
open Type_utils
open Fp_types


(************************************************************)


(** LUB of two values *)
let rec combineVals v1 v2 =
  match v1, v2 with
    Refs s1, Refs s2 ->
      if s1 == s2 || FLocSet.equal s1 s2 then v1
      else Refs (FLocSet.union s1 s2)
        
  | FpRef vi, Refs ts
  | Refs ts, FpRef vi -> Refs (FLocSet.add (funToLoc vi) ts)

  | FpRef a, FpRef b -> 
      if compVarID a b == 0 then v1 
      else makeMayref (funToLoc a) (funToLoc b) 
        (* should assert that this only happens for var bindings, 
           not fields... but one exception is during lookup *)

  (* Assume constants are null pointers *)   
  | (Refs ts as mr), FInt _
  | FInt _, (Refs ts as mr) -> 
      if setIsNull ts then mr
      else Refs (nullVar#addToSet ts)

  | (FpRef p as r), FInt _
  | FInt _, (FpRef p as r) -> 
      if nullVar#isVID p then r
      else makeMayref (funToLoc p) (nullVar#getLoc)
        
  | FInt a, FInt b -> if a = b then v1 else FNFP (constNfpSource)
      
  | FIRecs fir1, FIRecs fir2 ->
      FIRecs (combineFIRecs fir1 fir2)

  | Records rs, FIRecs fir
  | FIRecs fir, Records rs ->
      let fir2 = collapseRecordSet rs in
      FIRecs (combineFIRecs fir fir2)

  | Records rs1, Records rs2 ->
      if rs1 == rs2 then v1
      else Records (combineRecordSets rs1 rs2)

  | FNFP ts1, FNFP ts2 -> 
      if ts1 == ts2 then v1 
      else (* FNFP (NFPS.union ts1 ts2) *) 
        if NFPS.compare ts1 ts2 <= 0 then v1 else v2
        
  | (FNFP ts as theNfp), other
  | other, (FNFP ts as theNfp) -> 
      (match other with
         FNFP _ | FInt _ -> FNFP ts
       | FpRef x -> 
           makeMayref (funToLoc x) (nfpVar#getLoc)
       | Refs x ->
           Refs (nfpVar#addToSet x)

       | Records _ | FIRecs _ ->
           (* just promote the Nfp thing to a record and re-try combineVals ? *)
           combineVals other (Records (promoteToRecord theNfp))
      )
        
  | (Records _ as r), (_ as nonrec)
  | (_ as nonrec), (Records _ as r)
  | (FIRecs _ as r), (_ as nonrec)
  | (_ as nonrec), (FIRecs _ as r) ->
      combineVals r (Records (promoteToRecord nonrec))

and combineRecordSets recs1 recs2 =
  match recs1, recs2 with
    [(fp1, nonfp1, m1)], [(fp2, nonfp2, m2)] ->
      (* Special-case single records *)
      if equalFM fp1 fp2 then
        [(combineRecordPart fp1 fp2, (* Combine anyway *)
          combineRecordPart nonfp1 nonfp2,
          max m1 m2)]
      else if OffsetMap.is_empty fp1 then
        (* Special-case empty records *)
        [(fp2, combineRecordPart nonfp1 nonfp2, max m1 m2)]
      else if OffsetMap.is_empty fp2 then
        [(fp1, combineRecordPart nonfp1 nonfp2, max m1 m2)]
      else
        slowPathCombineRecSet recs1 recs2

  | _, _ ->
      slowPathCombineRecSet recs1 recs2

and slowPathCombineRecSet recs1 recs2 =
  let fpOffs1 = List.fold_left collectFPOffsets OffsetSet.empty recs1 in
  let fpOffs2 = List.fold_left collectFPOffsets OffsetSet.empty recs2 in
  (* Assume they are normalized within each set, so only normalize again
     if the two sides disagree on FP Keys! *)
  let norm1, norm2 = 
    if OffsetSet.equal fpOffs1 fpOffs2 then
      recs1, recs2
    else
      let allFPKeys = OffsetSet.union fpOffs1 fpOffs2 in
      let norm1 = List_utils.mapCross (normalizeFPFields allFPKeys) recs1 in
      let norm2 = List_utils.mapCross (normalizeFPFields allFPKeys) recs2 in
      norm1, norm2
  in
  (* TODO: play tricks assuming norm1 is sorted and norm2 is sorted?
     I.e., just call List.merge cmp l1 l2 instead of List.sort cmp (l1 @ l2)*)
  tryReduceRecordSet (norm1 @ norm2)

and combineRecordPart part1 part2 = 
  OffsetMap.unionC combineVals defaultRecordVal part1 part2

(** Try to reduce the number of combinations by merging records with
    FP configurations that subsume each other (e.g., if they are
    exactly equal, or they are only different in that one is
    NULL and the other is "EXT" *)
and tryReduceRecordSet recs =
  let recs = clusterNullExtFP recs in
  List.fold_left mergeSubsumeable [] recs

and mergeSubsumeable cur (fp, nonfp, m) =
  match cur with
    [] -> (fp, nonfp, m) :: cur
  | (fp2, nonfp2, m2) :: t ->
      try
        let newFP = trySubsumeFPMap fp fp2 in
        let newNonfp = combineRecordPart nonfp nonfp2 in
        let newM = max m m2 in
        (newFP, newNonfp, newM) :: t
      with CantComboNullExt ->
        (fp, nonfp, m) :: cur
  
(** Collapse records that use TOP as offset, or to be more scalable *)
and collapseRecVal off v cur =
  match cur with None -> Some v | Some (old) -> Some (combineVals v old)
    
and collapseRecord fp nonfp =
  let newNonFp = match OffsetMap.fold collapseRecVal nonfp None with
      None -> defaultRecordVal 0
    | Some x -> x
  in
  let newFp = match OffsetMap.fold collapseRecVal fp None with
      None -> defaultRecordVal 0
    | Some x -> trySubsumeFPVal x 
  in
  newFp, newNonFp
    
and collapseRecordSet recs =
  List.map (fun (fp, nonfp, _) -> collapseRecord fp nonfp) recs

and combineFIRecs fir1 fir2 =
  let fir = List.rev_append fir1 fir2 in
  (* merge those w/ the same FP sets *)
  let fir = clusterFIRecs fir in
  List.fold_left combineFIRecParts [] fir

and combineFIRecParts cur (fp, nonfp) =
  match cur with
    [] -> (fp, nonfp) :: cur
  | (fp2, nonfp2) :: t -> 
      if compareVal fp fp2 == 0 then (fp2, combineVals nonfp nonfp2) :: t
      else (fp, nonfp) :: cur


let partitionFPNonFP v =
  let merge v cur =
    match cur with
      None -> Some (v)
    | Some (old) -> Some (combineVals old v)
  in
  let extract cur =
    match cur with
      Some x -> x
    | None -> FInt (Int64.zero)
  in
  let rec mergeFpNonFps v (curFP, curNonFP) =    
    match v with
      FpRef _ -> (merge v curFP, curNonFP)
    | FNFP _ | FInt _ -> (curFP, merge v curNonFP)
    | Refs ls ->
        let fp, nonfp = FLocSet.partition isFunc ls in
        let newFP = if FLocSet.is_empty fp then curFP 
        else merge (Refs fp) curFP in
        let newNFP = if FLocSet.is_empty nonfp then curNonFP 
        else merge (Refs nonfp) curNonFP in
        (newFP, newNFP)
          
    | Records recs ->
        List.fold_left (fun x (fp, nonfp, _) -> mergeRecs fp nonfp x) 
          (curFP, curNonFP) recs

    | FIRecs fir ->
        List.fold_left 
          (fun (curFP, curNonFP) (fp, nonfp) ->
             merge fp curFP, merge nonfp curNonFP
          ) (curFP, curNonFP) fir
  and mergeRecs fp nonfp x =
    let x = OffsetMap.fold 
      (fun o v cur -> mergeFpNonFps v x) fp x in
    OffsetMap.fold (fun o v cur -> mergeFpNonFps v x) nonfp x
  in
  let x, y = mergeFpNonFps v (None, None) in
  extract x, extract y

let combineAttrs a1 a2 =
  match a1, a2 with
    HSingle, HSingle -> a1
  | (HSum as s), HSingle 
  | HSingle, (HSum as s)
  | (HSum as s), HSum -> s
  | (HGlobal as g), _ 
  | _, (HGlobal as g) -> g

(* combineStates moved lower because it needs access to other crap *)


(************************************************************)
(* Make states to refer to representatives variables *)


let rec updatePointersVal newMappings v =
  match v with
    FpRef _ | FInt _ | FNFP _ -> None
  | Refs ls -> 
      let changed, newLS = 
        FLocSet.fold 
          (fun (v,o) (ch, ls) ->
             match heapVarChanged newMappings v with
               None -> (ch, ls)
             | Some newV -> 
                 (true, FLocSet.add (newV,o) (FLocSet.remove (v,o) ls))
          ) ls (false, ls) in
      if changed then Some (Refs newLS) else None
  | Records recs ->
      let changed, newRecs = List.fold_left
        (fun (ch, cur) (fp, nonfp, m)  ->
           match updatePointersRecord newMappings nonfp with
             Some newnonfp -> (true, (fp, newnonfp, m) :: cur)
           | None -> (ch, (fp, nonfp, m) :: cur)
        ) (false, []) recs in
      if changed 
      then Some (Records (tryReduceRecordSet newRecs)) 
      else None
  | FIRecs fir ->
      let changed, newFIR = List.fold_left
        (fun (ch, fir) (fp, nonfp) ->
           match updatePointersVal newMappings nonfp with
             None -> (ch, (fp, nonfp) :: fir)
           | Some newV -> (true, (fp, newV) :: fir)
        ) (false, []) fir in
      if changed then Some (FIRecs newFIR) else None

and updatePointersRecord newMappings fm =
  let changed, newFM =
    OffsetMap.fold 
      (fun o v (ch, fm) ->
         match updatePointersVal newMappings v with
             Some newV -> (true, OffsetMap.add o newV fm)
         | None -> (ch, fm)
      ) fm (false, fm) in
  if changed then Some (newFM) else None 

let updatePointersValNonOpt newMappings v =
  match updatePointersVal newMappings v with None -> v | Some x -> x


(** Update the bindings and the values based on the new mapping of 
    heap var -> heap var *)
let updateStateMappings mappings state =
  let swapNewVarVal newVar newVal oldVar store =
    let store = VarMap.remove oldVar store in
    try 
      let newV2 = VarMap.find newVar store in
      let newV2 = combineVals newVal newV2 in
      VarMap.add newVar newV2 store
    with Not_found -> 
      VarMap.add newVar newVal store    
  in
  let swapNewVarAtts newVar newAtt oldVar atts =
    let atts = VarMap.remove oldVar atts in
    try 
      let newA  = VarMap.find newVar atts in
      let newA  = combineAttrs newAtt newA in
      VarMap.add newVar newA atts
    with Not_found -> 
      VarMap.add newVar newAtt atts
  in
  let updateBindingsStore store =
    VarMap.fold
      (fun var v newStore ->
         let newVar, chVar = match heapVarChanged mappings var with
             Some newVar -> newVar, true
           | None -> var, false in
         let newVal, chVal = match updatePointersVal mappings v with
             Some (newV) -> newV, true
           | None -> v, false in
         if chVar || chVal then swapNewVarVal newVar newVal var newStore
         else newStore
      ) store store
  in
  let updateVarAtts atts =
    VarMap.fold
      (fun var att newAtts ->
         match heapVarChanged mappings var with
           Some newVar -> swapNewVarAtts newVar att var newAtts
         | None -> newAtts
      ) atts atts
  in
  if VarH.length mappings == 0 then state (* common case? *)
  else 
    (* Assume the renaming doesn't affect global-ness *)
    let newB = updateBindingsStore state.bindings in
    let newA = updateVarAtts state.hAttrs in
    { bindings = newB;
      hAttrs = newA; 
      (* if we carried mappings from aggMerge, how would that be updated? *) }


let stUseReps st =
  updateStateMappings Fp_unify_globals.globalUnifTable st

let valUseReps v =
  updatePointersValNonOpt Fp_unify_globals.globalUnifTable v

(*********** More lenient equality matches *************)

let eqAttrModSum att1 att2 =
  match att1, att2 with
    HSingle, HSingle
  | HSingle, HSum
  | HSum, HSingle
  | HSum, HSum -> true
  | _, _ -> att1 = att2
  
(*********** More More lenient equality matches *************)

type ckinds =
    GlobAsClass of varID (* Cover functions and global variables *)
  | StructAsClass of typ 
  | NonFPKind

let fpKind = function GlobAsClass _ -> true | _ -> false

let compareCKinds c1 c2 =
  match c1, c2 with
    GlobAsClass vi1, GlobAsClass vi2 -> vi1 - vi2
  | StructAsClass t1, StructAsClass t2 -> Ciltools.compare_type t1 t2
  | _, _ -> Pervasives.compare c1 c2

module CKS = Iset.Make
  (struct 
     type t = ckinds
     let compare a b = compareCKinds a b
   end)

let locToKind (var, o) cur =
  if nullVar#isVar var || extVar#isVar var then cur
  else if nfpVar#isVar var then CKS.add NonFPKind cur
  else if isUnknownMallocVar var then cur
  else match var with
    FVar vid ->
      let vi = varinfoOfVid vid in
      if vi.vglob (* isFunctionType vi.vtype *) 
      then CKS.add (GlobAsClass vid) cur
      else CKS.add (StructAsClass (canonType (vi.vtype))) cur
  | _ -> 
      (* TODO: What if heap / input var is global? *)
      CKS.add (StructAsClass (canonType (typeOfFVar var))) cur


let locsToKinds locs = 
  FLocSet.fold locToKind locs CKS.empty

let strip_nonfp_kinds kinds =
  CKS.filter fpKind kinds

let canComboNullExtFP v1 v2 = 
  compareClusterNullExtFP v1 v2 == 0
      
let canComboFP fp1 fp2 =
  OffsetMap.equalC canComboNullExtFP defaultOFP fp1 fp2

let isCompatibleKinds k1 k2 =
  CKS.subset k1 k2 || CKS.subset k2 k1

let rec eqModFpRefs v1 v2 =
  match v1, v2 with
    Refs ls1, Refs ls2 -> 
      let ls1 = locsToKinds ls1 in
      let ls2 = locsToKinds ls2 in
      isCompatibleKinds ls1 ls2

  | FpRef vi, Refs ls
  | Refs ls, FpRef vi ->
      FLocSet.is_singleton ls && FLocSet.mem (funToLoc vi) ls

  | Records recs1, Records recs2 ->
      if recs1 == recs2 then true
      else List_utils.eq eqModFpRecord recs1 recs2

  | FInt _, FInt _
  | FInt _, FNFP _ 
  | FNFP _, FInt _ 
  | FNFP _, FNFP _ -> true

  | FInt _, Refs ls
  | Refs ls, FInt _ ->
      let ls = locsToKinds ls in
      isCompatibleKinds (CKS.empty) ls
  | FNFP _, Refs ls
  | Refs ls, FNFP _ ->
      let ls = locsToKinds ls in
      let other = CKS.singleton NonFPKind in
      isCompatibleKinds other ls
        
  | ((Records _) as r), ((FInt _) as non)
  | ((FInt _) as non), ((Records _) as r)
  | ((Records _) as r), ((FNFP _) as non)
  | ((FNFP _) as non), ((Records _) as r)
  | ((Records _) as r), ((FpRef _) as non) 
  | ((FpRef _) as non), ((Records _) as r)
  | ((Records _) as r), ((Refs _) as non)
  | ((Refs _) as non), ((Records _) as r) ->
      let r2 = Records (promoteToRecord non) in
      eqModFpRefs r r2
        
  | FIRecs fir1, FIRecs fir2 -> eqModFpFIRecs fir1 fir2
  | _, _ -> eqVals v1 v2

and eqModFpRecord (fp1, nonfp1, m1) (fp2, nonfp2, m2) =
  canComboFP fp1 fp2 && eqModFpRefsFM nonfp1 nonfp2

and eqModFpRefsFM fm1 fm2 =
  OffsetMap.equalC eqModFpRefs defaultRecordVal fm1 fm2

and eqModFpFIRecs fir1 fir2 =
  (* Check that the FP sets equiv, and that's all *)
  let len1 = List.length fir1 in
  let len2 = List.length fir2 in
  if len1 == len2 then
    let fir1 = List.sort sortFIRecParts fir1 in
    let fir2 = List.sort sortFIRecParts fir2 in
    List_utils.eq 
      (fun (fp1, _) (fp2, _) -> eqModFpRefs fp1 fp2) fir1 fir2
  else false


let eqStModFP st1 st2 =
  st1 == st2 ||
    let st1 = stUseReps st1 in
    let st2 = stUseReps st2 in
    (VarMap.equalC eqModFpRefs defaultVarVal st1.bindings st2.bindings &&
       VarMap.equalC eqAttrModSum defaultAttr st1.hAttrs st2.hAttrs)


let eqStModRep st1 st2 =
  st1 == st2 ||
    let st1 = stUseReps st1 in
    let st2 = stUseReps st2 in
    eqStates st1 st2



(************************************************************
 Low level reads and writes for store
************************************************************)

(************************************************************)

let debugRecRead var off (isStruct, width) v =
  logStatusF "GOTCHA: read from %s %b %d\n" (string_of_pointer (var, off)) 
    isStruct width;
  logStatusF "v: %s\n" (string_of_val v)

(* Full record copies *)

let readFromOffMap fm off width =
  OffsetMap.fold 
    (fun off2 v (skipped, curFM, curMax) ->
       let off2 = off2 - off in (* Make result start from 0 *)
       if off2 >= 0 && off2 < width
       then (skipped, OffsetMap.add off2 v curFM, maxOff curMax off2)
       else (true, curFM, curMax)
    ) fm (false, OffsetMap.empty, 0)
    
let readFromRecord (fp, nonfp, m) off (_, width) =
  if isNoOffset off && m < width then 
    (* Would have read it all *)
    false, fp, nonfp, m
  else
    let skippedFp, newfp, mFP = readFromOffMap fp off width in
    let skippedNon, newnonfp, mNFP = readFromOffMap nonfp off width in
    (skippedFp || skippedNon, newfp, newnonfp, max mFP mNFP)

(* Single value lookups *)

let lookupValRecord (fp, nonfp, m) off =
  try OffsetMap.find off fp 
  with Not_found -> 
    try OffsetMap.find off nonfp 
    with Not_found ->
      logError ("lookupValRecord: not found " ^ string_of_rawoff off ^ " " ^
                    string_of_loc !currentLoc);
      defaultRecordVal off

let lookupValRecordSet records off =
  let v = List.fold_left
    (fun cur (fp, nonfp, m) -> 
       match cur with 
         None -> Some (lookupValRecord (fp, nonfp, m) off)
       | Some (x) -> Some (combineVals x (lookupValRecord (fp, nonfp, m) off))
    ) None records in
  match v with Some x -> x | None -> failwith "empty RecordSet"


let lookupValFIRecord fir off =
  match List.fold_left
    (fun cur (fp, nonfp) ->
       match cur with
         None -> 
           Some (combineVals fp nonfp) (* Ugh... *)
       | Some (oldV) ->
           Some (combineVals oldV (combineVals fp nonfp))
    ) None fir with
      Some x -> x 
    | None -> failwith "empty FIRecord"


(************************************************************)

(* TODO: detect when lookup / store uses TOPOFF somewhere? *)

let copyRecords v off sinfo =
 match v with
   Records [(fp, nonfp, w)] -> 
     let changed, fp, nonfp, w = readFromRecord (fp, nonfp, w) off sinfo in
     if changed then Records [(fp, nonfp, w)]
     else v
 | Records recs ->
     let newRecs = List.map 
       (fun (fp, nonfp, w)  ->
          let changed, newFP, newNonfp, newW = 
            readFromRecord (fp, nonfp, w) off sinfo in
          if changed then (newFP, newNonfp, newW)
          else (fp, nonfp, w)
       ) recs in
     Records (tryReduceRecordSet newRecs)
 | _ -> failwith "copyRecords given non-record"


(** Looks up the value at [var] and [off] from a given store. 
    Assumes array offset is canonicized ? *)
let lookupValStore s var off sinfo =
  let v = VarMap.find var s in
  if !currentLoc.line = 44991 then
    debugRecRead var off sinfo v;
  if isScalar sinfo then
    (match v with
       Records [r] ->
         lookupValRecord r off
     | Records recs ->
         lookupValRecordSet recs off
     | FIRecs fir ->
         lookupValFIRecord fir off
     | FNFP _ -> v
     | FpRef _  | Refs _ | FInt _ ->
         if isNoOffset off then v
         else defaultRecordVal off)
  else 
    (match v with
       Records _ -> 
         copyRecords v off sinfo
     | FIRecs fir -> (* Just copy the whole thing... *)
         v
     | FNFP _ | FpRef _ | Refs _ | FInt _ -> 
         let recs = promoteToRecord v in
         copyRecords (Records recs) off sinfo
    )

(******************* Add/Update mapping in store ******************)

let debugRecCopy var off (_, width) v oldVal =
  logStatusF "GOTCHA: write to %s %d\n" (string_of_pointer (var, off)) width;
  logStatusF "v: %s\n oldV: %s\n" (string_of_val v) (string_of_val oldVal)


(** Copy all values in range [targO, targO + width) from src to target.
    Shift all offsets from src by targO before assigning into target *)
let writeToOffMap srcFM targFM targMax targO width =
  OffsetMap.fold 
    (fun off2 v (curFM, curMax) ->
       if off2 < width
       then
         let off2 = concatOffset targO off2 in
         (OffsetMap.add off2 v curFM, maxOff curMax off2)
       else (curFM, curMax)
    ) srcFM (targFM, targMax)
    (* TODO: go through offsets in targFM that are within the bounds of
       srcFM and write 0's whenever srcFM didn't already write a value in *)


    
(** Copy all values from [targO, targO + width) from src to target *)
let writeToRecord 
    (srcFP, srcNFP, srcM) 
    (targFP, targNFP, targM) 
    targO (_, width) =
  if isNoOffset targO && targM < width then 
    (* src would have written over all of target *)
    false, srcFP, srcNFP, srcM
  else
    let newfp, mFP = 
      writeToOffMap srcFP targFP targM targO width in
    let newnonfp, mNFP = 
      writeToOffMap srcNFP targNFP targM targO width in
    (true, newfp, newnonfp, max mFP mNFP)

      
let strongUpdateRecord (fp, nonfp, m) off v =
  match v with
    FpRef _ ->
      let newfp = OffsetMap.add off v fp in
      let newnonfp = OffsetMap.remove off nonfp in
      (newfp, newnonfp, maxOff m off)
  | _ -> 
      let newfp = OffsetMap.remove off fp in
      let newnonfp = OffsetMap.add off v nonfp in
      (newfp, newnonfp, maxOff m off)

let rec strongUpdateNonScalar st var off sinfo v oldVal =
  match v, oldVal with
    Records [src], Records [targ] ->
      let partial, newfp, newnfp, neww = 
        writeToRecord src targ off sinfo in
      let newVal = if partial then Records [(newfp, newnfp, neww)] else v in
      VarMap.add var newVal st

  | Records srcs, Records targs ->
      logError "strongUpdate recs N x M";
      let newRecs = 
        List_utils.mapCross
          (fun src ->
             List.map
               (fun targ ->
                  let partial, fp, nonfp, m =
                    writeToRecord src targ off sinfo in
                  if partial then (fp, nonfp, m) else src
               ) targs
          ) srcs in
      let v = Records (tryReduceRecordSet newRecs) in
      VarMap.add var v st

  (* Hmm... sucks because we don't know the width and so cannot
     know if the new Record will overwrite the old one completely*)
  | FIRecs _, FIRecs _
  | FIRecs _, Records _
  | Records _, FIRecs _ ->
      logError "not strongUpdating FIRec";
      let newVal = combineVals v oldVal in
      VarMap.add var newVal st

  | Records _, x 
  | FIRecs _, x ->
      let newX = Records (promoteToRecord x) in
      strongUpdateNonScalar st var off sinfo v newX

  | x, Records _
  | x, FIRecs _ ->
      let newX = Records (promoteToRecord x) in
      strongUpdateNonScalar st var off sinfo newX oldVal

  | _, _ ->
      let src = Records (promoteToRecord v) in
      let targ = Records (promoteToRecord oldVal) in
      strongUpdateNonScalar st var off sinfo src targ


let weakUpFIRec st var v fir =
  let vfp, vnonfp = partitionFPNonFP v in
  let fir = List.map 
    (fun (fp, nonfp) ->
       let newFP = trySubsumeFPVal (combineVals vfp fp) in
       let newNonFP = combineVals vnonfp nonfp in
       (newFP, newNonFP)
    ) fir in
  VarMap.add var (FIRecs fir) st


let strongUpdateRecordScalar st var off oldRecs v =
  (* Should check if the value written has funptrs, 
     and split on that if needed *)
  match oldRecs with
    [fm] ->
      if mentionsFP v then
        let fps = promoteToFP v in
        let recs = List.map (fun v -> strongUpdateRecord fm off v) fps in
        (* Don't need to try-reduce record, because we know they differ *)
        VarMap.add var (Records recs) st
      else
        let recs = [strongUpdateRecord fm off v] in
        VarMap.add var (Records recs) st
  | _ ->
      let vals = if mentionsFP v then promoteToFP v else [v] in
      let recs = List_utils.mapCross 
        (fun r ->
           List.map (fun v -> strongUpdateRecord r off v) vals)
        oldRecs in
      VarMap.add var (Records (tryReduceRecordSet recs)) st


let strongUpdate st var off v sinfo =
  (* Could still be assigning into a record either way,
     so check cur value first *)
  let oldVal = 
    try VarMap.find var st 
    with Not_found -> defaultVarVal var in
  if isScalar sinfo then begin
    (match oldVal with
       Records recs ->
         strongUpdateRecordScalar st var off recs v

     | FIRecs fir ->
         (* can't really do a strong update ... *)
         weakUpFIRec st var v fir

     | FNFP _ | FInt _ | Refs _ | FpRef _ ->
         if isNoOffset off 
         then VarMap.add var v st
         else 
           let recs = promoteToRecord oldVal in
           strongUpdateRecordScalar st var off recs v)
  end else
    strongUpdateNonScalar st var off sinfo v oldVal
      

let rec helpWeakUpdateRecord (fp, nonfp, m) off v oldV =
  (* Double-check to see if we should keep them split or not *)
  match v, oldV with
    FpRef a, FpRef b -> 
      if compVarID a b == 0 then [(fp, nonfp, m)]
      else
        let newFP, newNonFP, newM = strongUpdateRecord (fp, nonfp, m) off v in
        combineRecordSets [(fp, nonfp, m)] [(newFP, newNonFP, newM)]
  | FpRef _, _ ->
      (* actually the same case as above where the FpRefs do not agree! *)
      let newFP, newNonFP, newM = strongUpdateRecord (fp, nonfp, m) off v in
      combineRecordSets [(fp, nonfp, m)] [(newFP, newNonFP, newM)]
  | _, FpRef _ ->
      (* should promote the new value first *)
      let newVs = promoteToFP v in
      List.fold_left 
        (fun cur newV ->
           if cur = [] 
           then helpWeakUpdateRecord (fp, nonfp, m) off newV oldV
           else combineRecordSets cur 
             (helpWeakUpdateRecord (fp, nonfp, m) off newV oldV)
        ) [] newVs
  | _, _ ->
      (* doesn't matter, just merge and update *)
      let newV = combineVals oldV v in
      [(strongUpdateRecord (fp, nonfp, m) off newV)]
  

let weakUpdateRecord (fp, nonfp, m) off v =
  (* Lookup failure possible? *)
  let oldV = lookupValRecord (fp, nonfp, m) off in
  helpWeakUpdateRecord (fp, nonfp, m) off v oldV


(** Align source record (which is in terms of 0 offset) to the target
    which is offset by [off] *)
let rec alignRecordOff off v =
  if isNoOffset off then v
  else
    let alignFM fm =
      OffsetMap.fold (fun o2 v cur -> OffsetMap.add (o2 + off) v cur) 
        fm OffsetMap.empty
    in
    match v with
      Records [(fp, nonfp, m)] ->
        Records [(alignFM fp, alignFM nonfp, m + off)]

    | Records recs ->
        Records 
          (List.map (fun (fp, nonfp, m) ->
                       (alignFM fp, alignFM nonfp, m + off)) recs)
    | FIRecs _ -> v
    | FNFP _ | FInt _ | FpRef _ | Refs _ -> 
        alignRecordOff off (Records (promoteToRecord v))
          
        
let weakUpdate st var off v sinfo =
  let oldVal = 
    try VarMap.find var st with Not_found -> defaultVarVal var in
  if isScalar sinfo then
    (match oldVal with
       Records [fm] ->
         (* Special case the update because we may need to split *)
         let newrecs = weakUpdateRecord fm off v in
         VarMap.add var (Records newrecs) st

     | Records recs ->
         (* Do the weak update ourselves to avoid the funky lookup *)
         let newrecs = List_utils.mapCross
           (fun r -> weakUpdateRecord r off v) recs in
         VarMap.add var (Records (tryReduceRecordSet newrecs)) st

     | FIRecs fir ->
         weakUpFIRec st var v fir

     | Refs _ | FNFP _ | FInt _ | FpRef _->
         if isNoOffset off then begin
           let newval = combineVals oldVal v in
           VarMap.add var newval st
         end else begin
           let recs = promoteToRecord oldVal in
           let newrecs = List_utils.mapCross
             (fun r -> weakUpdateRecord r off v) recs in
           VarMap.add var (Records (tryReduceRecordSet newrecs)) st  
         end 
    )
  else 
    ( (* TODO: Read only necessary slice of old record and 
         combine w/ new record... *)
      let v = alignRecordOff off v in
      let newval = combineVals oldVal v in
      VarMap.add var newval st)



let makeFieldInsens st =
  let makeFieldInsensVar var v cur =
    match v with
      Records recs ->
        let fir = collapseRecordSet recs in
        VarMap.add var (FIRecs fir) cur  
    | FIRecs _ | FNFP _ | FInt _ | FpRef _ | Refs _ -> cur
  in
  let makeFieldInsensStore st =
    VarMap.fold makeFieldInsensVar st st
  in
  { st with bindings = makeFieldInsensStore st.bindings; }
