
open Cil
open Pretty
open Logging
open Cildump
open Type_utils
open Type_reach
open Fp_rci_types
open Fp_rci_flow_insens
open Fp_rci_unify_structs


(************************************************************)

(* Info used to generate default field vals on the fly *)

(** Identify where a value came from (if it comes from an input location) *)
type valBase = {
  mutable vBaseAttrs : varAttrs; (* Known types/attribs *)
  vBaseAP : accPath; (* Base acc path that is source of current values *)
  vBaseEqs : unifMap;   (* set of equalities for limiting acc path extension 
                          (so that we don't have as much the "MERGED LATER") *)
}
   
let valBaseNone = None

let valBaseSomeAP st ap =
  Some { vBaseAttrs = st.vAttrs;
         vBaseAP = ap;
         vBaseEqs = ShortAPU.makeMappings 0; } (* TODO: get this *)

let getBaseAPOpt st var =
  match var with
    FInput ap ->
      if not (isGlobalDebug "valBase" false st.vAttrs var) 
      then Some ap
      else None
  | FVar _ | FHeap _ | FRet _ -> None

let valBaseOfAPOpt st baseAP =
  match baseAP with 
    Some ap -> valBaseSomeAP st ap 
  | None -> None

let valBaseSome st var = 
  valBaseOfAPOpt st (getBaseAPOpt st var)


(************************************************************)
(* Limit length of access paths generated *)

(* This is one place where we assume we have all shorter accPaths
   already in the store... *)

let rec findCollapseMatches findOldAcc (origPath: accPath) : accPath * bool =
  let rec loop limP (curHead, curHL) curTail (vis : (accElem * accPath) list) =
    match curTail with
      h :: t ->
        (match h with
           (AccVar _) 
         | (AccField _) ->
             (* Don't count first var / field accs *)
             loop limP ((h :: curHead), curHL + 1) t vis
         | AccDeref ->
             (try
                let oldAP = findOldAcc h (curHead, curHL) vis in
                loop true oldAP t vis
              with Not_found ->
                let newHead = h :: curHead in
                let newHL = curHL + 1 in
                let newVis = vis @ [(h, (newHead, newHL))] in
                loop limP (newHead, newHL) t newVis)
        )
    | [] -> (curHead, curHL), limP
  in
  let op, _ = origPath in
  loop false ([], 0) (List.rev op) []
    

(* Don't collapse if it's using types that are commonly used as 
   void *, etc. (up to a limit)  *)
let isPolyType t =
  ( isImpreciseMalloc t || Type_reach.isPolyInt t )

let isPolyTypeID id =
  ( isImpreciseMallocID id || Type_reach.isPolyInt (findTyp id) )

(** Usually don't want to fold up the first occurance of a recursive type,
    because the first occurance may be a dummy head node. I.e., want to
    allow "head" and "head->next" nodes instead of just the "head" node
    even though they are all of type list *)
let maxSameNonPoly = ref 2
let maxPoly = ref 5

(** First entry is for poly * count, tail entries are type-id * count *)
type typeCount = (int * int) list

let newTypeCount = [(-1, 0)]

exception EmptyTC

let numPoly typCount =
  match typCount with
    (_, cnt) :: _ -> cnt
  | [] -> raise EmptyTC

let numNonPoly typCount tid =
  match typCount with
    h :: t ->
      (try List.assoc tid t
       with Not_found -> 0)
  | [] -> raise EmptyTC

let incrPoly typCount =
  match typCount with
    (oid, oldcount) :: t -> (oid, oldcount + 1) :: t
  | [] -> raise EmptyTC

let incrNonPoly typCount tid =
  match typCount with
    h :: t ->
      (try 
         let old = List.assoc tid t in
         let temp = List.remove_assoc tid t in
        h :: (tid, old + 1) :: temp
       with Not_found ->
         h :: (tid, 1) :: t)
  | [] -> raise EmptyTC

let incrTypeCountID tid counter =
  if isPolyTypeID tid then incrPoly counter else incrNonPoly counter tid

let shouldCollapseByTypeID typCount tid1 tid2 =
  (isPolyTypeID tid1 && isPolyTypeID tid2 && numPoly typCount > !maxPoly) ||
    ((tid1 = tid2)
     && not (isPolyTypeID tid1) 
     && numNonPoly typCount tid1 >= !maxSameNonPoly)


(** Make another pass at collapsing all access paths for recursive structs 
    (vs collapsing just the NEWEST part w/ an older part) *)
let findOldAcc atts h (restHead, restL) vis =
  let curVar = makeInVar ((h :: restHead), restL + 1) in
  let curT = typeIDOfFvar atts curVar in
  let typCount = ref newTypeCount in
  let _, oldAP = 
    List.find 
      (fun (acc, oldAP) ->
         let oldVar = makeInVar oldAP in
         let oldT = typeIDOfFvar atts oldVar in
         typCount := incrTypeCountID oldT !typCount;
         shouldCollapseByTypeID !typCount curT oldT
      ) vis in
  oldAP
    
let findOldAccMatchingTarg atts targPath targT h restHead vis =
  (* Should it try to compare the type of the cur path w/ visited
     stuff and not just the targT? *)
  let typCount = ref newTypeCount in
  let targTID = addTyp targT in
  try
    let _, oldAP = 
      List.find 
        (fun (acc, oldAP) ->
           let oldVar = makeInVar oldAP in
           try
             comp_accElem h acc == 0 && 
               (let oldTID = typeIDOfFvar atts oldVar in
                typCount := incrTypeCountID oldTID !typCount;
                shouldCollapseByTypeID !typCount oldTID targTID)
           with Not_found ->
             logErrorF "type of old unknown %s\n" (string_of_var oldVar);
             false 
        ) vis in
    oldAP
  with Not_found ->
    raise Not_found
      

(** Limit access paths for recursive data structures *)
let mergeAccPathCycles atts (targPath : accPath) targT : (accPath * bool) =
(*
  let findOldAcc = findOldAccMatchingTarg atts targPath targT in
*)
  let findOldAcc = findOldAcc atts in
  let path, didCollapse = findCollapseMatches findOldAcc targPath in
  path, didCollapse



let markSummary var curAtts =
  let newAtt = 
    try 
      let old = VarMap.find var curAtts in
      { old with vKind = combineVarKinds old.vKind HSum; }
    with Not_found ->
      logErrorF "vAttr NF: mergeAPAll %s\n" (string_of_var var);
      { (defaultAttr var) with vKind = HSum; } 
  in
  VarMap.add var newAtt curAtts


(************************************************************)

exception CantComboNullExt


(** LUB of two values *)
let rec combineVals base v1 v2 =
  if v1 == v2 then v1
  else match v1, v2 with
    Refs s1, Refs s2 ->
      if FLocSet.equal s1 s2 then v1
      else 
        let combo = combinePtrSets s1 s2 in
        Refs combo
        
  | FpRef var, Refs ts
  | Refs ts, FpRef var -> Refs (addToPtrSet (funToLoc var) ts)

  | FpRef a, FpRef b -> 
      if eqFVar a b then v1 
      else makeMayref (funToLoc a) (funToLoc b) 

  (* Assume constants are null pointers *)   
  | (Refs ts as mr), FInt _
  | FInt _, (Refs ts as mr) -> 
      if setIsNull ts then mr
      else Refs (nullVar#addToSet ts)

  | (FpRef p as r), FInt _
  | FInt _, (FpRef p as r) -> 
      if nullVar#isVar p then r
      else makeMayref (funToLoc p) (nullVar#getLoc)
        
  | FInt a, FInt b -> if a = b then v1 else FNFP (constNfpSource)
      
  | FIRecs fir1, FIRecs fir2 ->
      FIRecs (combineVals base fir1 fir2) 
        (* Unclear how to generate same initial offset values *)

  | Records rs, FIRecs fir
  | FIRecs fir, Records rs ->
      let fir2 = collapseRecordSet rs in
      FIRecs (combineVals base fir fir2)

  | NCRecord (fm, _), FIRecs fir
  | FIRecs fir, NCRecord (fm, _) ->
      let v = collapseRecord fm in
      FIRecs (combineVals base fir v)

  | Records rs, NCRecord (fm, m)
  | NCRecord (fm, m), Records rs ->
      let fm2, m2 = recSetToNCRec rs in
      let fm3, m3 = combineNRecs base (fm, m ) (fm2, m2) in 
      NCRecord (fm3, m3)

  | Records rs1, Records rs2 ->
      if rs1 == rs2 then v1
      else 
        (try
           let recs = combineRecordSets base rs1 rs2 in
           limitRecords recs
         with HitMaxRecs ->
           let r1 = recSetToNCRec rs1 in
           let r2 = recSetToNCRec rs2 in
           NCRecord (combineNRecs base r1 r2))

  | NCRecord (r1, m1), NCRecord (r2, m2) ->
      let r, m = combineNRecs base (r1, m1) (r2, m2) in
      NCRecord (r, m)

  | FNFP ts1, FNFP ts2 -> 
      if NFPS.compare ts1 ts2 <= 0 then v1 else v2
        
  | (FNFP ts as theNfp), other
  | other, (FNFP ts as theNfp) -> 
      (match other with
         FNFP _ | FInt _ -> FNFP ts
           (* JAN TEMP CHANGE *)
       | FpRef x -> makeMayref (funToLoc x) (initFPVar#getLoc) (* nfpVar#getLoc *)
       | Refs x -> Refs (nfpVar#addToSet x)

       | Records _ | FIRecs _ | NCRecord _ ->
           (* just promote the Nfp thing to a record and re-try combineVals *)
           combineVals base other (Records (promoteToRecord theNfp))
      )

  | (Records _ as r), (_ as nonrec)
  | (_ as nonrec), (Records _ as r)
  | (FIRecs _ as r), (_ as nonrec)
  | (_ as nonrec), (FIRecs _ as r)
  | (NCRecord _ as r), (_ as nonrec)
  | (_ as nonrec), (NCRecord _ as r) ->
      combineVals base r (Records (promoteToRecord nonrec))

and combineRecordPart base part1 part2 = 
  (* TODO: make default from base!!! *)
  OffsetMap.unionC (combineVals base) defaultRecordVal part1 part2

and combineRecordSets base recs1 recs2 =
  if recs1 == recs2 then recs1
  else 
    let fpOffs1, offs1 = List.fold_left collectAllPlusFPOffs 
      (OffsetSet.empty, OffsetSet.empty) recs1 in
    let fpOffs2, offs2 = List.fold_left collectAllPlusFPOffs
      (OffsetSet.empty, OffsetSet.empty) recs2 in
    (* Assume they are normalized within each set, so only normalize again
       if the two sides disagree on FP Keys! *)
    let allOffs = OffsetSet.union offs1 offs2 in
    let allFPKeys = OffsetSet.union fpOffs1 fpOffs2 in
    let norm1, norm2 = 
      if OffsetSet.is_empty allFPKeys then recs1, recs2
      else
        let norm1 = List_utils.mapCross (normalizeFPFields allFPKeys) recs1 in
        let norm2 = List_utils.mapCross (normalizeFPFields allFPKeys) recs2 in
        norm1, norm2
    in
    let both = norm1 @ norm2 in
    let both = fillUnknownIns base allOffs both in
    tryReduceRecordSet allFPKeys both

and fillUnknownIns base allOffs recs =
  match base with 
    None -> recs 
  | Some baseInfo ->
      let typ = typeOfFVar baseInfo.vBaseAttrs (makeInVar baseInfo.vBaseAP) in
      let recs = List.map (fillUnknownInsARec baseInfo typ allOffs) recs in
      (* Ignore the attributes generated for the targets? 
         Would need the "current state" updated etc... *)
      recs

and fillUnknownInsARec baseInfo baseTyp allOffs (fm, m) =
  let baseAP = baseInfo.vBaseAP in
  OffsetSet.fold 
    (fun o (curfm, curm) ->
       if OffsetMap.mem o curfm then (curfm, curm)
       else 
         let ftype = typeOfOffScalar baseTyp o in
         let nextAP = addField baseAP o in
         let newV, st = makeDefault nextAP 
           { emptyState with vAttrs = baseInfo.vBaseAttrs; } ftype in  
   (*         (fillInitialOffset baseAP emptyState ftype o) (curfm, curm) in *)
         baseInfo.vBaseAttrs <- combineAttributes baseInfo.vBaseAttrs st.vAttrs;
         let newFM, newm = OffsetMap.add o newV curfm, max o curm in
         if isRecordVal newV then 
           (match flattenRecord (newFM, newm) with
              [x] -> x
            | _ -> failwith "fillInitial inner rec > 1?")
         else (newFM, newm)
    ) allOffs (fm, m)
    

(** Try to reduce the number of combinations by merging records with
    FP configurations that subsume each other (e.g., if they are
    exactly equal, or they are only different in that one is
    NULL and the other is "EXT" *)
and tryReduceRecordSet fpOffs recs =
  match recs with
    [x] -> recs
  | _ ->
      let recs = List.fold_left (mergeSubsumeable fpOffs) [] recs in
      sortRecs recs 

and mergeSubsumeable fpOffs cur (fm, m) =
  try 
    snd (List_utils.listFindReplace 
           (subsumeableFPMap fpOffs)
           (fun ((fm1, m1) as rec1) ((fm2, m2) as rec2) ->
              if rec1 == rec2 then rec1
              else 
                (* If we merge the fp and nonfp parts, then we need to
                   run "trySubsumeFPMap" on just the fp offsets and
                   run "combineRecordPart" on just the non-fp offsets *)
                try 
                  let fm = trySubsumeFPMap fpOffs fm1 fm2 in
                  let m = max m1 m2 in
                  (fm, m)
                with CantComboNullExt -> 
                  logErrorF "CantComboNULL:\n%s\n%s\n"
                    (sprint 80 (defPrint#d_fieldMap None fm1))                  
                    (sprint 80 (defPrint#d_fieldMap None fm2));
                  failwith "Couldn't subsume FP map")
           (fm, m) cur)
  with Not_found ->
    (fm, m) :: cur

(********************)

(** Determines when two values at an fp offset are compatible for merging *)
and subsumeableFPVal v1 v2 =
  match v1, v2 with
    FpRef vi1, FpRef vi2 ->
      if eqFVar vi1 vi2 then true
      else if nullVar#isVar vi1 || nullVar#isVar vi2
        || extVar#isVar vi1 || extVar#isVar vi2  then true
      else false
  | FInt _, FpRef _ -> true
  | FpRef _, FInt _ -> true
  | FInt _, FInt _ -> true

  (* Group non-fp stuff together *)
  | Refs _, Refs _ 
  | FNFP _, Refs _ 
  | Refs _, FNFP _ -> true
      
  | _, _ -> eqVals v1 v2


and subsumeableFPMap fpOffs ((fm1, _) as rec1) ((fm2, _) as rec2) =
  rec1 == rec2 ||
    (* Only care that the FP projected versions are "subsets"... *)
    (let proj1, _ = projectOffsFM fpOffs fm1 in
     let proj2, _ = projectOffsFM fpOffs fm2 in
     OffsetMap.subsetB subsumeableFPVal proj1 proj2 ||
       OffsetMap.subsetB subsumeableFPVal proj2 proj1)



(** Merge NULL, etc. FPs w/ others... Use subset instead?
    for now, also merge EXT (though that probably shouldn't be hidden) *)
and comboNullExtFP fpOffs o v1 v2 =
  if OffsetSet.mem o fpOffs then
    match v1, v2 with
      FpRef vi1, FpRef vi2 ->
        if eqFVar vi1 vi2 then v1
        else 
          if nullVar#isVar vi1 then v2
          else if nullVar#isVar vi2 then v1
          else if extVar#isVar vi1 then v2
          else if extVar#isVar vi2 then v1
          else raise CantComboNullExt
            
    | FInt _, FpRef _ -> v2
    | FpRef _, FInt _ -> v1
    | FInt _, FInt _ -> FpRef (nullVar#getVar)
    | Refs l1, Refs l2 -> Refs (combinePtrSets l1 l2)
    | _, _ -> 
        logErrorF "nonfps in fp maps:\n%s\n%s\n"
          (string_of_val v1) (string_of_val v2);
        failwith "nonfps in fp maps" 
  else combineVals valBaseNone v1 v2


and trySubsumeFPMap fpOffs fp1 fp2 =
  OffsetMap.unionCK (comboNullExtFP fpOffs) (defaultOFP fpOffs) fp1 fp2

(** Generate a default field values *)
and defaultOFP fpKeys off = 
  if OffsetSet.mem off fpKeys then initFP
  else defaultRecordVal off (* TODO: extend from input var accpath *)
    

(** Collapse records that use TOP as offset, or to be more scalable *)
and collapseRecVal off v cur =
  match cur with 
    None -> Some v 
  | Some (old) -> Some (combineVals valBaseNone v old)
      (* shouldn't need the base because records aren't embedded *)

and collapseRecord fm = 
  match OffsetMap.fold collapseRecVal fm None with
    None -> defaultRecordVal noOff
  | Some x -> x
    
and collapseRecordSet recs =
  match recs with
    (fm, _) :: t ->
      let seed = collapseRecord fm in
      List.fold_left (fun cur (fm, _) -> 
                        combineVals valBaseNone cur (collapseRecord fm)) seed t
        (* shouldn't need the base because records aren't embedded *)        
  | [] -> failwith "collapseRecords given empty set"

and recSetToNCRec rs =
  match rs with 
    [] -> OffsetMap.empty, noOff
  | (fm, m) :: t ->
      (* assume already extended from base ? *)
      List.fold_left
        (fun (curfm, curm)  (fm, m) ->
           combineNRecs valBaseNone (curfm, curm) (fm, m) ) (fm, m) t
 
and combineNRecs base (fm1, m1) (fm2, m2) =
  let newfm = combineRecordPart base fm1 fm2 in
  let newm = max m1 m2 in
  (newfm, newm)

and tooManyRecords recs = 
 !recsMax > 0 && List.length recs > !recsMax 

and limitRecords recs =
  if tooManyRecords recs
  then NCRecord (recSetToNCRec recs) 
  else Records recs


(************************************************************
 Generate initial values for formal-based vars and others
************************************************************)

and getInitialVar prevAPOpt state var expectedT =
  match prevAPOpt with
    Some (prevAP) -> 
      let v, state = makeDefault prevAP state expectedT in
      v, addBinding state var v
  | None -> 
      let v = miscDefault expectedT in
      v, addBinding state var v

and makeDefault prevAP state expectedT =
  if isImpreciseMalloc expectedT then
    (* Assume it is an important pointer *)
    let targT = expectedT in
    makePointer prevAP state targT
  else
    let expectedT = typeModArray expectedT in
    match expectedT with
      TPtr (targT, _)
    | TArray (targT, _, _) -> 
        let targT = canonType targT in
        if isFunctionType targT then 
          makeFunPointer prevAP state targT
        else makePointer prevAP state targT
          
    | TComp (ci, _) ->
        (* Fill the record up! *)
        (*
          let recs, state = 
          fillInitialStruct prevAP noOff state ci (OffsetMap.empty, noOff) in
          Records recs, state
*)
        (* Delay filling it up *)
        miscDefault expectedT, state
          
    | TNamed (ti, _) -> makeDefault prevAP state ti.ttype
        
    | TInt _ | TFloat _ | TEnum _ | TBuiltin_va_list _ | TFun _ | TVoid _ -> 
        (miscDefault expectedT, state)
          
and makePointer prevAP state targT =
  let v, targVar, lim = getInitialPointer state prevAP targT in
  (* Var-kind of new guy is also dependent on predecessor *)
  let prevKind = 
    try
      getPrevVKind state.vAttrs prevAP
    with Not_found ->
      logErrorF "vAttr NF getPrevVKind %s @ %s\n" 
        (string_of_var (FInput prevAP)) (string_of_loc !currentLoc);
      HSingle
  in
  let kind = 
    if lim then combineVarKinds prevKind HSum
    else combineVarKinds prevKind HSingle in
(* 
  (* Some problem w/ freshclam? 
   Loses "old value" of this->d->state before call to decompress
   in cabd_extract line 40383... *)
  let kind = combineVarKinds prevKind HSum in
*)
  let state = addInitialAttrib state targVar targT kind in
  (v, state)


and getInitialPointer state prevAP targT =
  let newAccP = attachDeref prevAP in
  let newAccP, lim = Stat.time "mergeAP1" 
    (mergeAccPathCycles state.vAttrs newAccP) targT in
  let ghostVar = makeInVar newAccP in
(*
  if List.length newAccP > 20 then begin
    logErrorF "getInitialPointer LONG AP? %s\n" (string_of_var ghostVar);
  end;
*)
  (makeMayref (ghostVar, (noOff, !fullFI)) nullVar#getLoc, ghostVar, lim)


and makeFunPointer prevAP state targT =
  (* Hmm... are we guaranteed to have cycle-collapsing happen 
     to the prevAP before we get here? *)
  let newAP = attachDeref prevAP in
  let targVar = makeInVar newAP in
  (* Do we really need the attrib for this target? 
     Yes, to help w/ substitution *)
  let v = FpRef targVar in
  let prevKind = 
    try
      getPrevVKind state.vAttrs prevAP 
    with Not_found ->
      logErrorF "vAttr NF getPrevVKind %s @ %s\n" 
        (string_of_var (FInput prevAP)) (string_of_loc !currentLoc);
      HSingle
  in
  let kind = combineVarKinds prevKind HSingle in
  let state = addInitialAttrib state targVar targT kind in
  (v, state)

and getInitialOffset prevAP state expectedT moreOff =
  let nextAP = addField prevAP moreOff in
  makeDefault nextAP state expectedT


and fillInitialOffset prevAP state expectedT moreOff ((fm, m) as curRec) =
  match expectedT with
    TComp _ ->
      (* Watch out if it's an embedded structure. Some of the offsets
         may be filled while others are not! *)
      let newV, state = getInitialOffset prevAP state expectedT moreOff in   
      (* Inline the inner record to a temporary then copy new things into old *)
      let tempRecords = flattenRecord 
        (OffsetMap.add moreOff newV OffsetMap.empty, moreOff) in
      if List.length tempRecords > 1 then
        logErrorF "fillInitial inner recordset size > 1\n %s\n"
          (string_of_val (Records tempRecords))
      ;
      let newRec = 
        List.fold_left 
          (fun curRec (innerFM, innerM) ->
             OffsetMap.fold
               (fun off v (curFM, curM) ->
                  if OffsetMap.mem off curFM then (curFM, curM)
                  else (OffsetMap.add off v curFM, max curM off)
               ) innerFM curRec
          ) curRec tempRecords in 
      newRec, state
  | _ ->
      if OffsetMap.mem moreOff fm 
      then curRec, state
      else 
        let newV, state = getInitialOffset prevAP state expectedT moreOff in   
        (OffsetMap.add moreOff newV fm, max m moreOff), state
      

and fillInitialStruct prevAP baseOff state compinfo curRec =
  let record, state = List.fold_left
    (fun (curRec, state) fi ->
       let moreOff, _ = 
         cilOff2Offset false (TComp (compinfo, [])) (Field (fi, NoOffset)) in
       let moreOff = concatOffset moreOff baseOff in
       let ftype = canonType (Cil_lvals.unrollTypeNoAttrs fi.ftype) in
       let newRec, state = 
         fillInitialOffset prevAP state ftype moreOff curRec in
       newRec, state
    ) (curRec, state) (!Cil.getCfields compinfo) in
  if curRec == record then [curRec], state
  else flattenRecord record, state


let fillInitialNC prevAP baseOff state compinfo curRec =
  let recs, st = fillInitialStruct prevAP baseOff state compinfo curRec in
  match recs with
    [r] -> r, st
  | _ -> recSetToNCRec recs, st



(* combineStates moved lower because it needs access to other crap *)

let normalizeRecs recs =
  let keys = List.fold_left collectFPOffsets OffsetSet.empty recs in
  if OffsetSet.is_empty keys then
    tryReduceRecordSet keys recs
  else
    let norm = List_utils.mapCross (normalizeFPFields keys) recs in
    tryReduceRecordSet keys norm

let makeNormalizedRec newrecs = 
  try limitRecords (normalizeRecs newrecs) 
  with HitMaxRecs -> NCRecord (recSetToNCRec newrecs) 
    (* Already combined / generated initial vals *)
        

(** Finish up combineVals by merging gathered attributes from the baseInfo *)

let mergeBaseInfoSt curSt base =
  match base with
    None -> curSt
  | Some baseInfo ->
      { curSt with vAttrs = 
          combineAttributes curSt.vAttrs baseInfo.vBaseAttrs; }

let combineBaseValsSt curSt base v1 v2 =
  let v = combineVals base v1 v2 in
  v, mergeBaseInfoSt curSt base

let combineVarValsSt curSt var v1 v2 =
  let base = valBaseSome curSt var in
  combineBaseValsSt curSt base v1 v2
  
let combineAPValsSt curSt baseAPOpt v1 v2 =
  let base = valBaseOfAPOpt curSt baseAPOpt in
  combineBaseValsSt curSt base v1 v2        


(************************************************************)

let projectOffs toProj offs =
  try
    normalizeRecs 
      (List.map (fun (fm, m) -> projectOffsFM offs fm) toProj)
  with HitMaxRecs ->
    logErrorF "HitMaxRecs from projectOffs: %s @ %s\n"
      (string_of_val (Records toProj)) (string_of_loc !currentLoc);
    raise HitMaxRecs


let demoteFromARec (fm, m) =
  try OffsetMap.find noOff fm 
  with Not_found -> defaultRecordVal noOff (* TODO: generate input val *)

let trackVal cur v =
  match cur with
    None -> Some v
  | Some v2 -> Some (combineVals valBaseNone v v2) 
      (* Should be non-record val *)

let demoteFromRecord recs =
  let vopt = List.fold_left 
    (fun cur (fm, m) ->
       try
         let v = OffsetMap.find noOff fm in
         trackVal cur v
       with Not_found -> cur ) None recs in
  match vopt with
    None -> defaultRecordVal noOff  (* TODO: generate input val *)
  | Some x -> x
    
let demoteFromRecVal v = 
  match v with
    Records recs -> demoteFromRecord recs
  | NCRecord r -> demoteFromARec r
  | FIRecs fir -> fir
  | FInt _ | Refs _ | FNFP _ | FpRef _ -> v


(************************************************************)
(* Make values, state maps, etc. refer to representatives variables.
   I.e., uses a equalities represented as a map (var -> var) to
   rename variables in any unit of state *)

let updateElem newMappings doUpdate ch elem =
  match doUpdate newMappings elem with
    Some newelem -> (true, newelem)
  | None -> (ch, elem)



let rec updatePointersVal newMappings v =
  match v with
    FpRef var -> 
      (match UnifLocs.varChanged newMappings var with
         None -> None
       | Some (newVar, delta) -> 
           if not (isNoOffset delta) then begin
             logErrorF "updateFP delta %s --(%s)--> %s\n" 
               (string_of_var var) (string_of_offsetval delta) 
               (string_of_var newVar)  ;
             None
           end else Some (FpRef newVar))
        
  | FInt _ | FNFP _ -> None 
  | Refs ls ->
      let changed, newLS = 
        FLocSet.fold 
          (fun (v,o) (ch, ls) ->
             match UnifLocs.varChanged newMappings v with
               None -> (ch, ls)
             | Some (newV, delta) -> 
                 (true, addToPtrSet (newV, UnifLocs.computeNewOff o delta) 
                    (FLocSet.remove (v,o) ls))
          ) ls (false, ls) in
      if changed then Some (Refs newLS) else None
  | Records recs ->
      let changed, newRecs = List.fold_left
        (fun (ch, cur) (fm, m)  ->
           let ch, newfm = 
             updateElem newMappings updatePointersRecord ch fm in
           (ch, (newfm, m) :: cur)
        ) (false, []) recs in
      if changed 
      then Some (makeNormalizedRec newRecs)
      else None

  | NCRecord (fm, m) ->
      (match updatePointersRecord newMappings fm with
         Some newfm -> Some (NCRecord (newfm, m))
       | None -> None)

  | FIRecs fir ->
      (match updatePointersVal newMappings fir with
         None -> None
       | Some x -> Some (FIRecs x))

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


(************************************************************)

let mergeWithOld combiner newVar newVal oldVar map =
  let map = VarMap.remove oldVar map in
  try 
    let newV2 = VarMap.find newVar map in
    let newV2 = combiner newVal newV2 in
    VarMap.add newVar newV2 map
  with Not_found -> 
    VarMap.add newVar newVal map


let swapNewVarVal = mergeWithOld (combineVals valBaseNone) 
  (* should have already generated initial vals *)
let swapNewVarAtts = mergeWithOld combineAttr

(** Update a varmap w/ value bindings given the merger/update-function *)
let updateValBindings merger unifMap bindings =
  VarMap.fold
    (fun var v newBindings ->
       let newVar, delta, chVar = match UnifLocs.varChanged unifMap var with
           Some (newVar, delta) -> newVar, delta, true
         | None -> var, noOff, false in
       let newVal, chVal = match updatePointersVal unifMap v with
           Some (newV) -> newV, true
         | None -> v, false in
       if chVar || chVal then begin
         let newVal = alignRecordOff (invertOff delta) newVal in
         merger newVar newVal var newBindings
       end else newBindings
    ) bindings bindings


let updateModBindings merger unifMap bindings =
  VarMap.fold
    (fun var mods newBindings ->
       match UnifLocs.varChanged unifMap var with
         Some (newVar, delta) -> 
           let mods = alignModOffs (invertOff delta) mods in
           merger newVar mods var newBindings
       | None -> newBindings
    ) bindings bindings


(** Update a varmap where the only mention of a var is the key *)
let updateAttrBindings merger unifMap bindings =
  VarMap.fold
    (fun var att newBindings ->
       match UnifLocs.varChanged unifMap var with
         Some (newVar, delta) -> 
           let att = 
             if isNoOffset delta then att 
             else { att with vType = unknownMallocIndex; } (* no longer known *)
           in
           merger newVar att var newBindings
       | None -> newBindings
    ) bindings bindings


(** Update the bindings and the values based on the new mapping of 
    heap var -> heap var *)
let updateStateMappings (unifMap : unifLocMap) state =
  if VarH.length unifMap == 0 then state (* common case? *)
  else 
    { bindings = updateValBindings swapNewVarVal unifMap state.bindings;
      vAttrs = updateAttrBindings swapNewVarAtts unifMap state.vAttrs; }

(** Update a varmap where the only mention of a var is the key *)
let updateTypeBindings merger unifMap bindings =
  VarMap.fold
    (fun var otherT newBindings ->
       match UnifLocs.varChanged unifMap var with
         Some (newVar, delta) -> 
           let otherT = 
             if isNoOffset delta then otherT
             else  unknownMallocIndex in
           merger newVar otherT var newBindings
       | None -> newBindings
    ) bindings bindings


let swapNewVarMods = mergeWithOld combineModOffs
let swapNewVarTypes = mergeWithOld combineTypes

let updateSideMappings unifMap sidecond =
  if VarH.length unifMap == 0 then sidecond (* common case? *)
  else 
    { sidecond with
        initialFP = updateValBindings swapNewVarVal unifMap sidecond.initialFP;
        modIn = updateModBindings swapNewVarMods unifMap sidecond.modIn; 
        svTypes = updateTypeBindings swapNewVarTypes unifMap sidecond.svTypes;
    }


(************************************************************)
      
let stUseReps curFormals st =
  updateStateMappings (Fp_rci_unify_globals.globMappings curFormals st) st

let valUseReps curFormals st v =
  updatePointersValNonOpt (Fp_rci_unify_globals.globMappings curFormals st) v

(** Only use this if it's certain that the value references globals *)
let valUseRepsTrusted v =
  updatePointersValNonOpt Fp_rci_unify_globals.globalUnifTable v
    

let eqStModRep st1 st2 =
  st1 == st2 ||
(*
    let st1 = stUseReps st1 in
    let st2 = stUseReps st2 in *)
    eqStates st1 st2


(************************************************************)    

let eqInitialFP fp1 fp2 =
  fp1 == fp2 || VarMap.equal (=) fp1 fp2

let eqModIn mod1 mod2 =
  mod1 == mod2 || VarMap.equal OffsetSet.equal mod1 mod2
    
let eqSideCond sid1 sid2 =
  VarSet.equal sid1.nonAliasG sid2.nonAliasG &&
    eqInitialFP sid1.initialFP sid2.initialFP &&
    eqModIn sid1.modIn sid2.modIn &&
    eqTypeMap sid1.svTypes sid2.svTypes

let eqSideModAttrs sid1 sid2 = 
  VarSet.equal sid1.nonAliasG sid2.nonAliasG &&
    eqInitialFP sid1.initialFP sid2.initialFP &&
    eqModIn sid1.modIn sid2.modIn

(************************************************************
 Find repeat access paths (after the fact) for recursive-types
************************************************************)

let seekMerges unifMap var _ curAtts =
  match var with
    FInput ap ->
      if not (isGlobalDebug "mergeAPAll" true curAtts var) then begin
        (* If shorterAP is global then longerAP should be global too.
           should be enough to just check the original var then. 
           Still, kind of sucks that shorterAPs may not be known... *)
        let newp, collapsed = findCollapseMatches (findOldAcc curAtts) ap in
        if collapsed then begin
          let newvar = makeInVar newp in
          UnifLocs.updateMap unifMap newvar (var, noOff);
          markSummary newvar curAtts
        end
        else curAtts
      end else curAtts
  | FVar _ | FHeap _ | FRet _ -> curAtts


(** Accumulate vars mentioned in values too, and close equality relation *)
let mayFoldVarMap foldVal varMap atts =
  let addEligibleLoc (var, _) cur =
    match var with
      FInput _ -> 
        if not (isGlobalDebug "mergeableVar" true atts var)
        then VarSet.add var cur else cur
    | FVar _ | FHeap _ | FRet _ -> cur
  in   
  VarMap.fold 
    (fun var v cur ->
       let cur = foldVal addEligibleLoc v cur in
       addEligibleLoc (var, noOff) cur
    ) varMap VarSet.empty

let nopFoldVal foo v cur = cur


let mergeAccPathCyclesBindings state =
  let map = UnifLocs.makeMappings 7 in
  let atts = VarMap.fold (seekMerges map) state.bindings state.vAttrs in
  let allVars = mayFoldVarMap foldTargetsVal state.bindings atts in
  UnifLocs.closeMerges map allVars;
  if VarH.length map <> 0 then begin
    logStatusF "MERGED later (b) @ %s\n" (string_of_loc !currentLoc);
  end;
  Stat.time "update" (updateStateMappings map) { state with vAttrs = atts; }


let mergeAccPathCyclesAll state =
  let map = UnifLocs.makeMappings 7 in
  let atts = VarMap.fold (seekMerges map) state.bindings state.vAttrs in
  let atts = VarMap.fold (seekMerges map) state.vAttrs atts in
  let allVars = VarSet.union 
    (mayFoldVarMap foldTargetsVal state.bindings atts) 
    (mayFoldVarMap nopFoldVal state.vAttrs atts) in
  UnifLocs.closeMerges map allVars;
  if VarH.length map <> 0 then begin
    logStatusF "MERGED later (a) @ %s\n" (string_of_loc !currentLoc);
  end;
  Stat.time "update" (updateStateMappings map) { state with vAttrs = atts; }

let makeDummyAttrs types =
  VarMap.map (fun t -> { vType = t; vKind = HSingle; }) types

let mergeAccPathCyclesAllSide side =
  let map = UnifLocs.makeMappings 7 in
  let dummyAtts = makeDummyAttrs side.svTypes in
  let atts = VarMap.fold (seekMerges map) side.initialFP dummyAtts in
  let atts = VarMap.fold (seekMerges map) side.modIn atts in
(*  let atts = VarMap.fold (seekMerges map) side.svAttrs atts in *)
(*  let side = updateSideMappings map { side with svAttrs = atts; } in *)
  (* Actually, keep pre-collapsed old atts around *)
  let allVars = VarSet.union 
    (mayFoldVarMap nopFoldVal side.initialFP atts) 
    (mayFoldVarMap nopFoldVal side.modIn atts) in
  UnifLocs.closeMerges map allVars;
  if VarH.length map <> 0 then begin
    logStatusF "MERGED later (s) @ %s\n" (string_of_loc !currentLoc);
    UnifLocs.printMap map;
  end;

  updateSideMappings map side
(*
  { side with svAttrs = combineAttributes side.svAttrs atts; } 
*)


(************************************************************
 Low level reads and writes for store
************************************************************)

(* TODO: detect when lookup / store uses TOPOFF somewhere? *)

(**** Multi-offset reads ****)

let debugRecRead var off (isStruct, width) v =
  logStatusF "GOTCHA: read from %s %b %d\n" (string_of_pointer (var, off)) 
    isStruct width;
  logStatusF "v: %s\n" (string_of_val v)

let readFromOffMap fm bits1 width =
  OffsetMap.fold 
    (fun bits2 v (changed, curFM, curMax) ->
       let newOff = bits2 - bits1 in (* Make result start from 0 *)
       if newOff >= 0 && newOff < width
       then 
         (true, OffsetMap.add newOff v curFM, maxOff curMax newOff)
       else 
         (changed, curFM, curMax)
    ) fm (false, OffsetMap.empty, noOff)
    
let readFromRecord (fm, m) off (_, width) =
  if isNoOffset off && m < width then 
    (* Would have read it all *)
    false, fm, m
  else readFromOffMap fm off width


let copyRecords recs off sinfo =
 match recs with
   [(fm, m)] -> 
     let changed, fm, m = readFromRecord (fm, m) off sinfo in
     if changed then Records [(fm, m)] else Records (recs)

 | _ ->
     let newRecs = List.map 
       (fun (fm, m)  ->
          let changed, newFM, newM = 
            readFromRecord (fm, m) off sinfo in
          if changed then (newFM, newM) else (fm, m)) recs in
     makeNormalizedRec newRecs


let copyNCRecord (fm, m) off (_, width) =
  if isNoOffset off && m < width then
    (* would have read it all *)
    NCRecord (fm, m)
  else
    let partial, fm2, m2 = readFromOffMap fm off width in
    if partial then NCRecord (fm2, m2)
    else NCRecord (fm, m)  


let fillRecs st baseAP compinfo var off recs =
  match baseAP with
    Some prevAP ->
      (* Make sure all fields are filled in... *)
      let recs, st = List.fold_left 
        (fun (curR, curSt) r ->
           let r, st = fillInitialStruct prevAP off curSt compinfo r in
           List.rev_append r curR, st
        ) ([], st) recs in
      let v = makeNormalizedRec recs in
      v, addBinding st var v
  | None -> Records recs, st 

let fillNCRec st baseAP compinfo var off (fm, m) =
  match baseAP with
    Some prevAP -> 
      let (fm, m), st = fillInitialNC prevAP off st compinfo (fm, m) in
      let v = NCRecord (fm, m) in
      v, addBinding st var v
  | None -> NCRecord (fm, m), st

  
(* Single value lookups *)

let rec lookupValRecord baseAP expectedT st ((fm, m) as curRec) off =
  try OffsetMap.find off fm, curRec, st
  with Not_found -> 
    (match baseAP with
       Some (prevAP) ->
         let newRec, state = 
           fillInitialOffset prevAP st expectedT off curRec in
         lookupValRecord baseAP expectedT state newRec off 
     | None ->
         miscDefault expectedT, curRec, st)


let rec lookupValRecordSet baseAP expectedT st records off =
  (* TODO: detect if newRecs is actually new... *)
  let v, newRecs, st = List.fold_left
    (fun (curVal, newRecs, st) (fm, m) -> 
       let x2, newRec, st = 
         lookupValRecord baseAP expectedT st (fm, m) off in
       let newVal = match curVal with 
           None -> Some (x2)
         | Some (x) -> Some (combineVals valBaseNone x x2) in
       newVal, newRec :: newRecs, st) (None, [], st) records in
  match v with 
    Some x -> x, newRecs, st
  | None -> 
      (match baseAP with
         Some ap ->
           logErrorF "empty RecordSet @ %s, %s -> %s\n" 
             (string_of_loc !currentLoc)
             (string_of_pointer (makeInVar ap, (off, false))) 
             (string_of_val (Records records));
           logErrorF "expectedT : %s\n" (string_of_type expectedT);
       | None -> 
           logErrorF "empty RecordSet @ %s, %s\n"
             (string_of_loc !currentLoc)
             (string_of_val (Records records));
           logErrorF "expectedT : %s\n" (string_of_type expectedT)
      );
      failwith "Empty record"
(*
      lookupValRecordSet baseAP expectedT st [(OffsetMap.empty, 0)] off
*)

(************************************************************)

let doRecCopy v off sinfo =
  match v with
    Records recs ->
      copyRecords recs off sinfo
  | NCRecord (fm, m) ->
      copyNCRecord (fm, m) off sinfo
  | _ -> failwith "Doing rec copy on non record"



(** Looks up the value at [var] and [off] from a given store. 
    Assumes array offset is canonicized ? *)
let lookupValStore curFunc expectedT st var off =
  let baseAP = getAccPath curFunc st.vAttrs var in
  let baseVal, st = 
    try getBinding var st, st
    with Not_found ->
      let t = if isNoOffset off 
      then findTyp (combineTypes (addTyp expectedT) 
                      (typeIDOfFvar st.vAttrs var))
      else (typeOfFVar st.vAttrs var) in
      getInitialVar baseAP st var t
  in
  (* Eh... special case here to get the "full" value *)
  if isNoOffset off && isImpreciseMalloc expectedT then
    baseVal, st
  else 
    match Cil_lvals.unrollTypeNoAttrs (typeModArray expectedT) with
      TComp (ci, _) ->
        let sinfo = getStructInfo expectedT in
        (match baseVal with
           Records recs ->
             let v, st = fillRecs st baseAP ci var off recs in
             let res = Stat.time "recCopy" (doRecCopy v off) sinfo in
             res, st
               
         | NCRecord (fm, m) ->
             let v, st = fillNCRec st baseAP ci var off (fm, m) in
             let res = Stat.time "recCopy" (doRecCopy v off) sinfo in
             res, st
               
         | FIRecs fir -> 
             logError "TODO: lookupVal FIRecs, generate initial vals";
             baseVal, st

         | FNFP _ | FpRef _ | Refs _ | FInt _ -> 
             let recs = promoteToRecord baseVal in
             let v, st = fillRecs st baseAP ci var off recs in
             let res = Stat.time "recCopy" (doRecCopy v off) sinfo in
             res, st
        )
    | _ ->
        (match baseVal with
           Records recs ->
             let v, newRecs, st = 
               lookupValRecordSet baseAP expectedT st recs off in
             if recs == newRecs then v, st
             else 
               let st = addBinding st var (makeNormalizedRec newRecs) in
               v, st
         | NCRecord (fm, m) ->
             let v, (fm2, m2), st = 
               lookupValRecord baseAP expectedT st (fm, m) off in
             if fm == fm2 then v, st
             else 
               let st = addBinding st var (NCRecord (fm, m)) in
               v, st
         | FIRecs fir -> 
             logError "TODO: lookupVal FIRecs, generate initial vals";
             fir, st
               
         | FNFP _ ->
             baseVal, st (* Hmm... *)
         | FpRef _  | Refs _ | FInt _ ->
             if isNoOffset off then baseVal, st
             else 
               let recs = promoteToRecord baseVal in
               let v, newRecs, st = lookupValRecordSet baseAP expectedT st
                 recs off in
               let st = addBinding st var (makeNormalizedRec newRecs) in
               v, st
        )


(******************* Add/Update mapping in store ******************)

let debugRecCopy var off (_, width) v oldVal =
  logStatusF "GOTCHA: write to %s %d\n" (string_of_pointer (var, off)) width;
  logStatusF "v: %s\n oldV: %s\n" (string_of_val v) (string_of_val oldVal)


(** Copy all values in range [targO, targO + width) from src to target.
    Shift all offsets from src by targO before assigning into target *)
let writeToOffMap srcFM targFM targMax targO width =
  OffsetMap.fold 
    (fun bits2 v (curFM, curMax) ->
       if bits2 < width
       then
         let off2 = concatOffset targO bits2 in
         (OffsetMap.add off2 v curFM, maxOff curMax off2)
       else (curFM, curMax)
    ) srcFM (targFM, targMax)
    (* TODO: go through offsets in targFM that are within the bounds of
       srcFM and write some initial value whenever srcFM didn't 
       already write a value in *)


    
(** Copy all values from [targO, targO + width) from src to target *)
let writeToRecord (srcFM, srcM) (targFM, targM) targO (_, width) =
  if isNoOffset targO && targM < width then 
    (* src would have written over all of target *)
    false, srcFM, srcM
  else
    let newFM, m = writeToOffMap srcFM targFM targM targO width in
    (true, newFM, m)


(************************************************************)
      
let strongUpdateRecord (fm, m) off v =
  let newm = OffsetMap.add off v fm in
  (newm, maxOff m off)

let strongUpdateRecordScalar bindings var off oldRecs v =
  (* Should check if the value written has funptrs, 
     and split on that if needed *)
  match oldRecs with
    [fm] ->
      if mentionsFP v then
        let fps = promoteToFP v in
        let recs = List.map (fun v -> strongUpdateRecord fm off v) fps in
        VarMap.add var (makeNormalizedRec recs) bindings
      else
        let recs = [strongUpdateRecord fm off v] in
        VarMap.add var (Records recs) bindings
  | _ ->
      let recs = List.map (fun r -> strongUpdateRecord r off v) oldRecs in
      VarMap.add var (makeNormalizedRec recs) bindings


let rec strongUpdateNonScalar st var off sinfo v oldVal =
  match v, oldVal with
    Records [src], Records [targ] ->
      let partial, newfm, newm = writeToRecord src targ off sinfo in
      let newVal = if partial then Records [(newfm, newm)] else v in
      VarMap.add var newVal st

  | Records srcs, Records targs ->
      logError "strongUpdate recs N x M";
      let newRecs = 
        List_utils.mapCross
          (fun src ->
             List.map
               (fun targ ->
                  let partial, fm, m = writeToRecord src targ off sinfo in
                  if partial then (fm, m) else src
               ) targs
          ) srcs in
      let v = makeNormalizedRec newRecs in
      VarMap.add var v st

  (* Hmm... sucks because we don't know the width and so cannot
     know if the new Record will overwrite the old one completely *)
  | FIRecs _, FIRecs _
  | FIRecs _, Records _
  | Records _, FIRecs _ ->
      logError "not strongUpdating FIRec";
      let newVal = combineVals valBaseNone v oldVal in
      VarMap.add var newVal st

  | NCRecord _, Records recs ->
      (* Ugh... just force result to be an NCRecord *)
      let fm, m = recSetToNCRec recs in
      strongUpdateNonScalar st var off sinfo v (NCRecord (fm, m))

  | Records recs, NCRecord _ ->
      (* Ugh... just force result to be an NCRecord *)
      let fm, m = recSetToNCRec recs in
      strongUpdateNonScalar st var off sinfo (NCRecord (fm, m)) oldVal

  | NCRecord (srcFM, srcM), NCRecord (targFM, targM) ->
      let _, fm, m = 
        writeToRecord (srcFM, srcM) (targFM, targM) off sinfo in
      let v = NCRecord (fm, m) in
      VarMap.add var v st

  | Records _, x 
  | FIRecs _, x
  | NCRecord _, x ->
      let newX = Records (promoteToRecord x) in
      strongUpdateNonScalar st var off sinfo v newX

  | x, Records _
  | x, FIRecs _ 
  | x, NCRecord _ ->
      let newX = Records (promoteToRecord x) in
      strongUpdateNonScalar st var off sinfo newX oldVal

  | _, _ ->
      let src = Records (promoteToRecord v) in
      let targ = Records (promoteToRecord oldVal) in
      strongUpdateNonScalar st var off sinfo src targ


let weakUpFIRec st var v fir =
  let newval = combineVals valBaseNone v fir in
  VarMap.add var (FIRecs newval) st


let strongUpdateStore expectedT st var off v oldBaseVal =
  match Cil_lvals.unrollTypeNoAttrs expectedT with
    TComp (ci, _) ->
      (* Don't need this because writeToOffMap already accounts for it... *)
      (* let v = alignRecordOff off v in *)
      strongUpdateNonScalar st var off (getStructInfo expectedT) v oldBaseVal
  | _ ->
      (match oldBaseVal with
         Records recs ->
           strongUpdateRecordScalar st var off recs v
       | NCRecord (fm, m) ->
           let fm, m = strongUpdateRecord (fm, m) off v in
           VarMap.add var (NCRecord (fm, m)) st

       | FIRecs fir -> weakUpFIRec st var v fir
             
       | FNFP _ | FInt _ | Refs _ | FpRef _ ->
           if isNoOffset off 
           then VarMap.add var v st
           else 
             let recs = promoteToRecord oldBaseVal in
             strongUpdateRecordScalar st var off recs v)


let strongUpdateNonInput expectedT store var off v =
  let oldBaseVal =
    try VarMap.find var store 
    with Not_found -> defaultValNonInput var 
  in
  strongUpdateStore expectedT store var off v oldBaseVal


let strongUpdate curFunc expectedT st var off v =
  (* Could still be assigning into a record either way, 
     so check cur value first *)
  let oldBaseVal, st = 
    try (getBinding var st, st)
    with Not_found -> 
      let baseAP = getAccPath curFunc st.vAttrs var in
      getInitialVar baseAP st var (typeOfFVar st.vAttrs var) in
  let newStore = strongUpdateStore expectedT st.bindings var off v oldBaseVal in
  { st with bindings = newStore; }
      

(************************************************************)

let rec helpWeakUpdateRecord base (fm, m) off v oldV =
  (* Double-check to see if we should keep them split or not *)
  match v, oldV with
    FpRef a, FpRef b -> 
      if eqFVar a b then [(fm, m)]
      else
        let newFM, newM = strongUpdateRecord (fm, m) off v in
        combineRecordSets base [(fm, m)] [(newFM, newM)]
  | FpRef _, _ 
  | _, FpRef _ ->
      (* actually the same case as above where the FpRefs do not agree! *)
      let newFM, newM = strongUpdateRecord (fm, m) off v in
      combineRecordSets base [(fm, m)] [(newFM, newM)]

  | _, _ ->
      (* doesn't need to split at all (nonFPs all around) --
         just merge and update *)
      let newV = combineVals base oldV v in
      [(strongUpdateRecord (fm, m) off newV)]

let weakUpdateRecord baseAP expectedT st (fm, m) off v =
  let oldV, newRec, st = 
    lookupValRecord baseAP expectedT st (fm, m) off in
  let base = valBaseOfAPOpt st baseAP in
  let newv = helpWeakUpdateRecord base newRec off v oldV in
  newv, mergeBaseInfoSt st base


let weakUpdateRecordNC baseAP expectedT st (fm, m) off v =
  let oldV, newRec, st = 
    lookupValRecord baseAP expectedT st (fm, m) off in
  let base = valBaseOfAPOpt st baseAP in
  let newV = combineVals base oldV v in
  (strongUpdateRecord (fm, m) off newV, mergeBaseInfoSt st base)

         
let doWeakUpdate expectedT baseAP oldBaseVal st var off v =
  match Cil_lvals.unrollTypeNoAttrs expectedT with
    TComp (ci, _) ->
      (* TODO: Read only necessary slice of old record and 
         combine w/ new record... *)
      let alignv = alignRecordOff off v in
      let newval, st = combineAPValsSt st baseAP oldBaseVal alignv in

      let (_, width1) = structInfoVal alignv in
      let (_, width2) = structInfoVal oldBaseVal in
      if width1 > width2 then
        logStatusF "GOTCHA: WEAKup %s|%d| for (%s,%s) @ %s\n%s vs (old)\n%s\n"
          (string_of_type expectedT) (widthModArray expectedT)  
          (string_of_var var) (string_of_offsetval off)
          (string_of_loc !currentLoc)
          (string_of_val alignv) (string_of_val oldBaseVal);      
      addBinding st var newval
        
  | _ ->
      let doUpdateAsNCRec recs =
        let (fm, m) = recSetToNCRec recs in
        let (newfm, newm), st = 
          weakUpdateRecordNC baseAP expectedT st (fm, m) off v in
        addBinding st var (NCRecord (newfm, newm))              
      in

      let doUpdate recs =
        (* Do the weak update ourselves to avoid the funky lookup *)
        let curSt = ref st in
        (try
           let newrecs = List_utils.mapCross
             (fun r -> 
                let newrecs, st = 
                 weakUpdateRecord baseAP expectedT !curSt r off v in
                curSt := st;
                newrecs
             ) recs in
           let v = makeNormalizedRec newrecs in
           addBinding !curSt var v
         with HitMaxRecs -> doUpdateAsNCRec recs )
      in
      (match oldBaseVal with
         Records [r] ->
           (try
              let newrecs, st = weakUpdateRecord baseAP expectedT st r off v in
              let v = makeNormalizedRec newrecs in
              addBinding st var v
            with HitMaxRecs -> doUpdateAsNCRec [r] )

       | Records recs -> doUpdate recs

       | NCRecord (fm, m) ->
           let (newfm, newm), st = 
             weakUpdateRecordNC baseAP expectedT st (fm, m) off v in
           addBinding st var (NCRecord (newfm, newm))

       | FIRecs fir -> 
           { st with bindings = weakUpFIRec st.bindings var v fir; }

       | Refs _ | FNFP _ | FInt _ | FpRef _->
           if isNoOffset off then begin
             let newval, st = combineAPValsSt st baseAP oldBaseVal v in
             addBinding st var newval
           end else
             let recs = promoteToRecord oldBaseVal in
             doUpdate recs
      )

let weakUpdateNonInput expectedT store var off v =
  let oldBaseVal =
    try VarMap.find var store
    with Not_found -> defaultValNonInput var 
  in
  let fakeSt = { emptyState with bindings = store } in
  let newSt = doWeakUpdate expectedT None oldBaseVal fakeSt var off v in
  newSt.bindings


let weakUpdate curFunc expectedT st var off v =
  let baseAP = getAccPath curFunc st.vAttrs var in
  let oldBaseVal, st = 
    try (getBinding var st, st)
    with Not_found -> 
      getInitialVar baseAP st var (typeOfFVar st.vAttrs var) in
  doWeakUpdate expectedT baseAP oldBaseVal st var off v
    

(************************************************************)

(* Stuff for adding required initialFPs *) 

let delayFPRequest inputVar curDelay =
  VarSet.add inputVar curDelay

let combineDelayedFP d1 d2 =
  VarSet.union d1 d2

let emptyDelayedFP = VarSet.empty


(** Replace FpRefs that mention "input" vars with initFPVar and collect 
    input vars that were replaced. *)
let rec replaceInputFPsVal filterNonFP attribs v curDelay = 
  match v with
    FpRef (FInput _ as inVar) -> 
      (initFP, delayFPRequest inVar curDelay)
  | Refs locs ->
      let newLocs, curDelay =
        FLocSet.fold (replaceInputFPsRefs filterNonFP attribs) 
          locs (locs, curDelay) in
      let newLocs = 
        if filterNonFP then FLocSet.remove nullVar#getLoc newLocs 
        else newLocs in
      if filterNonFP && FLocSet.is_empty (stripSpecial newLocs) then
        (initFP, curDelay)
      else 
        (Refs newLocs, curDelay)
        
  | FpRef (FVar _) -> (v, curDelay)
  | FpRef (FHeap _) | FpRef (FRet _) -> 
      logErrorF "heap or ret var acting as FP %s\n" (string_of_val v);
      (initFP, curDelay)
  | FNFP _ | FInt _ ->
      if filterNonFP then (initFP, curDelay)
      else (v, curDelay)
  | Records recs ->
      let recs, curDelay = 
        List.fold_left (replaceInputFPsRec filterNonFP attribs) 
          ([], curDelay) recs in
      (makeNormalizedRec recs, curDelay)
        
  | NCRecord (fm, m) ->
      let (newfm, newm), curDelay = 
        replaceInputFPsFM filterNonFP attribs curDelay (fm, m) in
      (NCRecord (newfm, newm), curDelay)
        
  | FIRecs fir -> replaceInputFPsVal filterNonFP attribs fir curDelay

and replaceInputFPsRec filterNonFP attribs (recs, delay) (fm, m) =
  let newRec, delay = replaceInputFPsFM filterNonFP attribs delay (fm, m) in
  (newRec :: recs, delay)

and replaceInputFPsFM filterNonFP attribs curDelay (fm, m) =
  let delay = ref curDelay in
  let newFM = OffsetMap.mapCh
    (fun v ->
       let newV, newdelay = replaceInputFPsVal filterNonFP attribs v !delay in
       delay := newdelay;
       newV) fm in
  (newFM, m), !delay


and replaceInputFPsRefs filterNonFP attribs (v, o) (newLocs, delay) =
  match v with
    FInput _ ->
      if isFunc2 attribs (v, o)  then
        (FLocSet.remove (v, o) newLocs, delayFPRequest v delay)
      else if filterNonFP then begin
        let t = typeOfLocScalar attribs (v, o) in
        if isImpreciseMalloc t && isNoOffsetSum o then
          (FLocSet.remove (v, o) newLocs, delayFPRequest v delay)
        else begin
          if !fullFI then
            (FLocSet.remove (v, o) newLocs, delayFPRequest v delay)
          else begin
            logErrorF ~prior:3 "replaceInputFP: dropping %s : %s\n" 
              (string_of_pointer (v, o)) (string_of_type t);
            (FLocSet.remove (v, o) newLocs, delay)
          end
        end
      end
      else (newLocs, delay)
  | FVar _ ->
      if filterNonFP && not (isFunc2 attribs (v, o))
      then (FLocSet.remove (v, o) newLocs, delay)
      else (newLocs, delay)
  | FHeap _ | FRet _ ->
      if filterNonFP then begin
        logErrorF ~prior:3 "replaceInputFP: dropping %s : %s\n" 
          (string_of_pointer (v, o)) 
          (string_of_type (typeOfLocScalar attribs (v, o)));
        (FLocSet.remove (v, o) newLocs, delay)
      end else (newLocs, delay)
        
        

(** Check that the old initial fp rec doesn't already have a value
    bound at "off" that is not a pointer to the special "EXT" func *)
let rec checkOldOff off oldInitialFPRec =
  match oldInitialFPRec with
    FpRef _ | FInt _ | FNFP _ | Refs _ -> 
      checkOldOff off (limitRecords (promoteToRecord oldInitialFPRec))
  | Records recs ->
      List.exists (fun (fm, _) -> OffsetMap.mem off fm) recs
  | NCRecord (fm, _) ->
      OffsetMap.mem off fm
  | FIRecs _ -> 
      failwith "FIRecs checkOldOff"
        

let announceNewRelFP varOff =
  logStatusF "Found relevant context FP: %s @ %s\n" 
    (string_of_var varOff) (string_of_loc !currentLoc)


let rec doAddFpAcc side srcVar off old =
  match old with
    Records recs ->
      let newInitFP = strongUpdateRecordScalar side.initialFP 
        srcVar off recs initFP in
      if eqInitialFP newInitFP side.initialFP then None
      else begin
        let var = reattachFieldAcc srcVar off in
        announceNewRelFP var;
        Some { side with initialFP = newInitFP; }
      end
  | NCRecord (fm, m) ->
      let newFM, newM = strongUpdateRecord (fm, m) off initFP in
      let newInitFP = 
        VarMap.add srcVar (NCRecord (newFM, newM)) side.initialFP in
      if eqInitialFP newInitFP side.initialFP then None
      else begin
        let var = reattachFieldAcc srcVar off in
        announceNewRelFP var;
        Some { side with initialFP = newInitFP; }
      end

  | FpRef _ | FInt _ | FNFP _ | Refs _ -> 
      doAddFpAcc side srcVar off (limitRecords (promoteToRecord old))
  | FIRecs _ -> 
      failwith "FIRecs doAddFpAcc"

let defaultFPAccRec () =
  emptyRecord

(** Add UNKNOWN input-based FPs that need to be known.
    Otherwise it is provided when creating a new context... *)
let addFpAccs side accVar =
  let repVar, delta = UnifLocs.findMappingNonOpt side.aliasedIn accVar in
  assert (delta = noOff);
  let srcVar = stripADeref repVar in
  (match srcVar with
     FInput (ap, _) -> 
       (match ap with
          [AccVar _] 
        | AccDeref :: _ -> (* srcVar is a scalar *)
            if VarMap.mem srcVar side.initialFP then None
            else begin
              announceNewRelFP srcVar;
              Some { side with 
                       initialFP = VarMap.add srcVar initFP side.initialFP; }
            end
        | AccField o :: t -> (* srcVar is a record *)
            let off, t = compressApOffs o t in
            let srcVar = makeInVar (t, List.length t) in
            (try
               (* How bad can this get if old value is set by input, and 
                  there were a ton of recs on input (at a different offset)? *)
               let old = VarMap.find srcVar side.initialFP in
               if checkOldOff off old then None
               else doAddFpAcc side srcVar off old 
             with Not_found ->
               let old = defaultFPAccRec () in
               doAddFpAcc side srcVar off old)
        | _ -> failwith ("malformed accPath (4)" ^ string_of_var srcVar) 
       )
   | _ -> failwith "addFpAccs given non-input")
      


let addModGlob st side var =
  let addMod side var =
    if not (VarSet.mem var side.nonAliasG) then begin
      logStatusF "Mod glob: %s\n" (string_of_var var);
      Some { side with nonAliasG = VarSet.add var side.nonAliasG; }
    end else None
  in
  match var with
    FVar vid ->
      if VarSet.mem var side.nonAliasG then None
      else if globalHadAddrTaken vid && 
        let vi = varinfoOfVid vid in vi.vglob 
      then addMod side var
      else None
  | FInput _ | FHeap _ ->
      if isGlobalDebug "addMods" true st var then
        if VarSet.mem var side.nonAliasG then None
        else addMod side var
      else None
  | FRet _ -> None


let getOffsToMod baseOff lhsT =
  let baseOff = int_of_off baseOff in
  let lhsT = typeModArray (Cil_lvals.unrollTypeNoAttrs lhsT) in
  match lhsT with
    TComp (ci, _) -> 
      List.fold_left 
        (fun cur fi -> 
           let off2, _ = cilOff2Offset false lhsT (Field (fi, NoOffset)) in
           OffsetSet.add (concatOffset off2 baseOff) cur
        ) OffsetSet.empty (!Cil.getCfields ci)
  | _ -> OffsetSet.singleton baseOff

let doAddModIn side var newOffs =
  let oldOffs = 
    try VarMap.find var side.modIn with Not_found -> OffsetSet.empty in
  let diff = OffsetSet.diff newOffs oldOffs in
  if OffsetSet.is_empty diff then None
  else 
    let comboOffs = OffsetSet.union diff oldOffs in
    Some ( { side with modIn = VarMap.add var comboOffs side.modIn; } )

let addModIn st side var off lhsT =
  match var with
    FVar _ | FHeap _ | FRet _ -> None
  | FInput _ ->
      if isGlobalDebug "addModIn" true st var then None
      else 
        let newOffs = getOffsToMod off lhsT in
        doAddModIn side var newOffs
            
            
(** Track mods 
    - to global variables that have had their address taken 
    - to offsets of non-global input variables *)
let addMods st side var off lhsTyp =
  let side = match addModGlob st side var with
      None -> side | Some x -> x in
  addModIn st side var off lhsTyp

(************************************************************)

(** diff on vals that checks which offsets differ (main use-case) *)
let rec diffVals newV oldV =
  if newV == oldV then None
  else 
    (match newV, oldV with
       FInt _, FInt _
     | FpRef _, FpRef _
     | Refs _, Refs _
     | FNFP _, FNFP _ -> if eqVals newV oldV then None else Some newV

     | Records [(fm1, m1)], Records [(fm2, m2)]
     | NCRecord (fm1, m1), NCRecord (fm2, m2) ->
         let fm = diffRecs fm1 fm2 in
         if not (OffsetMap.is_empty fm) then
           let m, _ = OffsetMap.max_binding fm in
           Some (NCRecord (fm, m))
         else None

     | Records recs1, Records recs2 ->
         (* No clean way to do correlate them (well they are sorted) *)
         if eqVals newV oldV then None
         else diffVals
           (NCRecord (recSetToNCRec recs1))
           (NCRecord (recSetToNCRec recs2))
           
     | NCRecord _, Records recs2 ->
         diffVals newV (NCRecord (recSetToNCRec recs2))

     | Records recs, NCRecord _ ->
         diffVals (NCRecord (recSetToNCRec recs)) oldV

     | FIRecs v1, FIRecs v2 -> if eqVals v1 v2 then None else Some newV

     | x, Records _
     | x, NCRecord _
     | x, FIRecs _ -> 
         diffVals (Records (promoteToRecord x)) oldV

     | Records _, x 
     | NCRecord _, x
     | FIRecs _, x-> 
         diffVals oldV (Records (promoteToRecord x))

     | _, _ -> if eqVals newV oldV then None else Some newV
    )

and diffRecs fm1 fm2 =
  OffsetMap.diffBoth eqVals fm1 fm2


(** Determine which offsets are different between newV and oldV  *)
let calcDiffOffsets newV oldV = 
  match diffVals newV oldV with
    None -> OffsetSet.empty
  | Some diffV ->
      (match diffV with
         FInt _ | FpRef _ | Refs _ | FNFP _ -> 
           OffsetSet.singleton 0
       | Records recs -> 
           List.fold_left collectOffsets OffsetSet.empty recs
       | NCRecord (fm, m) -> 
           collectOffsets OffsetSet.empty (fm, m)
       | FIRecs _ -> failwith "TODO: calcDiffOffsets FIRecs"
      )


(************************************************************)

let warnDroppedOffsetSameVar var =
  logErrorF "DROPPED var diff off %s @ %s\n" (string_of_var var)
    (string_of_loc !currentLoc)


(** Hack where if we have a pointer value referencing the same location, 
    but at different offsets, only keep the "lowest" offset.
    E.g., if p = Ptr { &x->f1, or &x->f2 }, only keep p = Ptr { &x->f1 }...
*)
let rec keepOneOffsetSame st = 
  if not (!nonUniformPtrArith) then
    let filteredStore = 
      VarMap.fold keepOneOffsetSameVarVal st.bindings st.bindings in
    { st with bindings = filteredStore; }
  else 
    st

and keepOneOffsetSameVarVal var v st =
  match keepOneOffsetSameVal v with
    None -> st
  | Some newV -> 
      warnDroppedOffsetSameVar var;
      VarMap.add var newV st

and keepOneOffsetSameVal v =
  match v with
    FInt _ | FpRef _ | FNFP _ -> None
  | FIRecs v2 ->
      (match keepOneOffsetSameVal v2 with
         None -> None
       | Some v2' -> Some (FIRecs v2'))
  | Refs locs ->
      if not (!nonUniformPtrArith) then
        let _, newLocs, changed = 
          FLocSet.fold keepOneOffsetLocs locs (VarMap.empty, locs, false) in
        if changed then Some (Refs newLocs)
        else None
      else None

  | Records recs -> 
      let ch = ref false in
      let newrecs = 
        List.map (fun (fm, m) ->
                    let newfm, ch2 = keepOneOffsetRecord (fm, m) in
                    ch := !ch || ch2;
                    (newfm, m)) recs in
      if !ch then Some (makeNormalizedRec newrecs) else None

  | NCRecord (fm, m) ->  
      let newfm, ch = keepOneOffsetRecord (fm, m) in
      if ch then Some (NCRecord (newfm, m))
      else None
          

and keepOneOffsetLocs (var, off) (lowestOffPerVar, newLocs, changed) =
  try
    let off2 = VarMap.find var lowestOffPerVar in
    if closestToZeroOff (int_of_off off2) (int_of_off off) <= 0 then
      (lowestOffPerVar, FLocSet.remove (var, off) newLocs, true)
    else 
      (VarMap.add var off lowestOffPerVar, 
       FLocSet.remove (var, off2) newLocs, true)
  with Not_found ->
    (VarMap.add var off lowestOffPerVar, newLocs, changed)

and keepOneOffsetRecord (fm, m) =
  OffsetMap.fold keepOneOffsetSameOffVal fm (fm, false)

and keepOneOffsetSameOffVal o v (curfm, ch) =
  match keepOneOffsetSameVal v with
    None -> (curfm, ch)
  | Some newv -> (OffsetMap.add o newv curfm, true)


(************************************************************)

let makeFieldInsens st =
  let makeFieldInsensVar var v cur =
    match v with
      Records recs ->
        let fir = collapseRecordSet recs in
        VarMap.add var (FIRecs fir) cur
    | NCRecord (fm, m) ->
        let fir = collapseRecord fm in
        VarMap.add var (FIRecs fir) cur
    | FIRecs _ | FNFP _ | FInt _ | FpRef _ | Refs _ -> cur
  in
  let makeFieldInsensStore st =
    VarMap.fold makeFieldInsensVar st st
  in
  { st with bindings = makeFieldInsensStore st.bindings; }
    
    
(************************************************************)


(* Hack to test how much compressing by type will help reduce
   the number of summaries will need to be generated *)
let hashStToType st =

  let newSt = ref emptyState in

  let makeTypeVar tid =
    FHeap { hID = [ ({pp_instr = tid; pp_stmt = 0;}, 0) ]; }
  in
  
  let addTypVar tid typVar =
    if VarMap.mem typVar !newSt.vAttrs then ()
    else 
      let newAtt = { vType = tid; vKind = HSum; } in
      newSt := { !newSt with vAttrs = VarMap.add typVar newAtt !newSt.vAttrs; }
  in

  let addVal var v =
    try
      let oldv = getBinding var !newSt in
      let newv, st = combineVarValsSt !newSt var oldv v in
      newSt := addBinding st var newv
    with Not_found ->
      newSt := addBinding !newSt var v
  in
  
  let hashVar var =
    let t = typeIDOfFvar st.vAttrs var in
    let var2 = makeTypeVar t in
    addTypVar t var2;
    var2
  in    

  let rec hashToTypeVal v =
    match v with
      FpRef var -> FpRef (hashVar var)
    | FInt _ | FNFP _ -> v
    | Refs locs ->
        let newlocs = FLocSet.fold 
          (fun (var, o) cur -> addToPtrSet (hashVar var, o) cur) 
          locs FLocSet.empty in
        Refs newlocs
    | Records recs ->
        let newrecs = List.map hashRecord recs in
        makeNormalizedRec newrecs
          
    | NCRecord (fm, m) ->
        let fm, m = hashRecord (fm, m) in
        NCRecord (fm, m)
    | FIRecs v -> FIRecs (hashToTypeVal v)

  and hashRecord (fm, m) =
    (hashOffmap fm, m)

  and hashOffmap fm =
    OffsetMap.map hashToTypeVal fm
  in
  
  VarMap.iter 
    (fun var v ->
       let newvar = hashVar var in
       let newval = hashToTypeVal v in
       addVal newvar newval
    ) st.bindings;
  VarMap.iter 
    (fun var att ->
       let _ = hashVar var in
       () (* the act of hashing it should have add the attribute *)
    ) st.vAttrs;
  !newSt
  


(* Also convert the initialFP access paths to some type-based equivalent *)
let hashSideToTyp sideCond =
  (* Hmmm... preserve final field access? *)
  sideCond

