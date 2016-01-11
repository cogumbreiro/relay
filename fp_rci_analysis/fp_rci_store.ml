
open Cil
open Cildump
open Pretty
open Stdutil
open Logging
open Fp_rci_types
open Fp_rci_unify_structs
open Fp_rci_flow_insens 
open Fp_rci_lattice_ops
open Fp_rci_malloc
open Fp_rci_unify
open Fp_rci_globals
open Fp_rci_unify_globals
open Type_reach


(************************************************************)


(* TODO: just use the accPath we already have...
   - Loses the targets actually deref'ed though *)

type access =
    AReadVar of fvar
  | AReadField of offset_val
  | AConcatOff of offset_val
  | ADeref of FLocSet.t

let d_acc acc =
  match acc with
    AReadVar fv -> defPrint#d_var fv
  | AReadField off -> text "F " ++ defPrint#d_off nil None off
  | AConcatOff off -> text "+" ++ defPrint#d_off nil None off
  | ADeref ls -> 
      let sz = FLocSet.cardinal ls in
      if sz > 4 then dprintf "*{MAY} (%d)" sz
      else defPrint#d_ptrSet (text "*") ls 

let d_path path =
  (seq_to_doc (text " ->\n") List.iter d_acc path nil) ++ line

let printAccPath path =
  logStatusD (d_path path)
  
type accessSeq = access list

type lookupHook = access -> unit

let nilLookupHook (acc:access) = ()

(** Hook to have eval record the access path. For focus, we can then 
    just use that access path to slice the state *)
let captureAccessPath curPath acc =
  curPath := acc :: !curPath


(*********************************************************
  High-level operations on state 
 *********************************************************)

let doublecheckWidth var (isStruct, width)  = 
  if width == 0 then 
    (logErrorF "lookup/store given 0 width: %s @ %s\n" 
       (string_of_var var) (string_of_loc !currentLoc); 
     (isStruct, scalarWidth))
  else (isStruct, width)

(******* Bring the next level of global heap vars into bindings *******)

let addIfHeap (var, _) cur =
  match var with 
    FInput _ | FHeap _ 
  | FVar _ ->
      (* allow locals w/ addr taken too, even though it should have been
         heapified already ... *) 
      VarSet.add var cur 
  | FRet _ -> cur 
      
let collectHeapInVal v cur =
  foldTargetsVal addIfHeap v cur

let addGlobalAttr var vtyp cur =
  match var with 
    FHeap _ | FInput _ -> 
      let newAtt = 
        try
         let old = VarMap.find var cur in
         combineAttr old { vKind = HGlobal; vType = vtyp;}
        with Not_found -> { vKind = HGlobal; vType = vtyp; } in
      VarMap.add var newAtt cur
  | FRet _ | FVar _ -> cur

(** Bring in the binding for this global, and attributes for its targets *)
let bringInGlobal var state readerOpt =
  (* Pull in val and type for var, but also pull in type for the next 
     level of targets *)
  let gval, gType, newVar, diffOff = getGlobalBinding var readerOpt in
  let newStore = VarMap.add newVar gval state.bindings in
  let newAttrs = addGlobalAttr newVar gType state.vAttrs in
  (* Next level *)
  let newTargets = collectHeapInVal gval VarSet.empty in
  let newAttrs = VarSet.fold 
    (fun var2 newAttrs -> 
       let _, gType2, newVar2, _ = getGlobalBinding var2 GNone in
       addGlobalAttr newVar2 gType2 newAttrs
    ) newTargets newAttrs in
  { bindings = newStore; vAttrs = newAttrs; }, newVar, diffOff
    

(** Bring in the binding for this global, do not refer to representative *)
let bringInGlobalNoRep var state readerOpt =
  (* Pull in val and type for var, but also pull in type for the next 
     level of targets *)
  let gval, gType = getGlobalBindingNoRep var readerOpt in
  let newStore = VarMap.add var gval state.bindings in
  let newAttrs = addGlobalAttr var gType state.vAttrs in
  (* Next level *)
  let newTargets = collectHeapInVal gval VarSet.empty in
  let newAttrs = VarSet.fold 
    (fun var2 newAttrs -> 
       let _, gType2, newVar2, _ = getGlobalBinding var2 GNone in
       addGlobalAttr newVar2 gType2 newAttrs
    ) newTargets newAttrs in
  { bindings = newStore; vAttrs = newAttrs; }
    


(**** Mark heap reachable from globals as global *****)

module ValQ = Queueset.Make (
  struct
    type t = fvalue
    let compare a b = compareVal a b
  end )


(** Collect vars transitively reachable from the given value [v]
    that may have been allocated on the heap (i.e., include input vars)
    Assume the given set [cur] has already been grep'ed for transitive reach. *)
let collectTransitiveHeap st v cur =
  let valsToGrep = ValQ.create () in
  let cur = ref cur in
  let doCollect var =
    if VarSet.mem var !cur then ()
    else begin
      cur := VarSet.add var !cur;
      try 
        let v = getBinding var st in
        ValQ.addOnce v valsToGrep
      with Not_found ->
        match var with
          FInput _ -> () (* may not have been generated yet *)
        | FVar _ | FRet _ -> (* may not have been set yet *)
            if not (isGlobalDebug "collectTrans" true st.vAttrs var) then begin
              logErrorF "collectTrans no binding for local? %s\n"
                (string_of_var var);
              ()
            end
        | FHeap _ ->
            if not (isGlobalDebug "collectTrans" true st.vAttrs var) then begin
              let str = "collectTrans no binding for local? " ^
                (string_of_var var) ^ "\n" in
              logError str;
              printState st;
              failwith str
            end
    end
  in

  ValQ.addOnce v valsToGrep;
  while not (ValQ.is_empty valsToGrep) do
    let nextV = ValQ.pop valsToGrep in
    let moreVars = collectHeapInVal nextV VarSet.empty in
    VarSet.iter doCollect moreVars;
  done;
  !cur

(** The value is captured by a global. Adjust to reflect escapage *)
let markGlobal gvar var (newGlobals, newSt) =
  match var with
    FHeap _ | FInput _ ->
      (try
         let oldAtt = VarMap.find var newSt.vAttrs in
         if oldAtt.vKind <> HGlobal then
           let newAtt = { oldAtt with vKind = HGlobal; } in
           (VarSet.add var newGlobals, 
            { newSt with vAttrs = VarMap.add var newAtt newSt.vAttrs; })
         else 
           (newGlobals, newSt) (* already known *)
       with Not_found ->
         (* ... check if it's already in the global summary *)
         if hasGlobalBinding var then (newGlobals, newSt)
         else begin
           logErrorF "isGlobal NF: markGlobal %s @ %s\n"
             (string_of_var var) (string_of_loc !currentLoc);
           let newAtt = { vKind = HGlobal; 
                          vType = Type_utils.unknownMallocIndex; } in
           (VarSet.add var newGlobals,
            { newSt with vAttrs = VarMap.add var newAtt newSt.vAttrs; })
         end )
  | FVar _ -> 
      (* may go through locals... should get heapify to work on
         these dudes separately *)
      if not (isGlobalFVar newSt.vAttrs var) then
        logStatusF "ERR: Mark local as global: %s\n" (string_of_var var);
      (newGlobals, newSt)
        
  | FRet _ ->
      (newGlobals, newSt)



let checkEscape gvar st v =
  let heapVars = collectTransitiveHeap st v VarSet.empty in
  if not (VarSet.is_empty heapVars) then
    let newGlobals, st = 
      VarSet.fold (markGlobal gvar) heapVars (VarSet.empty, st) in
    (* Delay notification / merging of values of newGlobals
       until dropGlobalAssume *)
    st
  else st


(************************************************************)

exception LookupNF
    
(** Looks up the value at [var] and [off] from a given state. 
    Assumes offset is canonicized. 
    May raise NullException *)
let lookupVal lookupHook curFunc expectedT state var off readerOpt = 
  if nullVar#isVar var || extVar#isVar var then begin
    (* raise NullException *)
    (FNFP lookupNfp, state)
  end else if nfpVar#isVar var then 
    (FNFP lookupNfp, state)
  else 
    let state, newVar, newOff = 
      if not (VarMap.mem var state.bindings) && 
        isGlobalDebug "lookup" true state.vAttrs var then begin
          (* Bad idea to change the variable names around at this point... *)
(*
          let newSt, newVar, diff = bringInGlobal var state readerOpt in
          let newOff = concatOffset off diff in
*)
          let newSt = bringInGlobalNoRep var state readerOpt in
          let newVar = var in 
          let newOff = off in
          newSt, newVar, newOff
        end else state, var, off in
    lookupHook (AReadVar newVar);
    lookupHook (AReadField newOff);
    try lookupValStore curFunc expectedT state newVar newOff
    with Not_found ->
      failwith "WTF"


let assignVarStore curFunc expectedT oldState var off v strong =
  if strong then strongUpdate curFunc expectedT oldState var off v 
  else weakUpdate curFunc expectedT oldState var off v 


let isSummary state var =
  try
    let att = VarMap.find var state.vAttrs in
    match att.vKind with
      HSingle -> false | HGlobal | HSum -> true
  with Not_found ->
    logErrorF "isSummary can't find attr for: %s\n" (string_of_var var);
    printState state;
    failwith "isSummary not found"


let isFunctionFVar st var =
  isFunctionType (typeOfFVar st var)

let canStrongUpdVar curSt var off =
  match var with
    FVar _ | FRet _ -> not (isSumOff off)
  | FHeap _ | FInput _ -> not (isSumOff off) && not (isSummary curSt var) 


(** returns a new state where [var.off] has been assigned [v] *)
let assignVar curFunc expectedT oldSt var off v strong =
  if nullVar#isVar var then begin
    (* raise NullException *)
    logErrorF "write to null @ %s\n" (string_of_loc !currentLoc);
    (oldSt, emptyNotifyGR)
  end else if nfpVar#isVar var || extVar#isVar var then
    (oldSt, emptyNotifyGR)
  else if isFunctionFVar oldSt.vAttrs var then
    (logErrorF "write to function2: %s <- %s @ %s\n" 
       (string_of_val v) (string_of_var var) (string_of_loc !currentLoc);
     (oldSt, emptyNotifyGR))
  else begin
    let strong = strong && canStrongUpdVar oldSt var off in
    let bitsOff = int_of_off off in
    if isGlobalDebug "assign" true oldSt.vAttrs var then begin
      let oldSt = checkEscape var oldSt v in
      (* Keep a "local" copy of the global -- but don't strong upd it *)
      let strong = false in
      let newState = 
        assignVarStore curFunc expectedT oldSt var bitsOff v strong in
      (* May have gotten a points to set w/ > 1 var, chained up some nodes *)
      let newState = 
        if not strong 
        then !myMerger#aggressiveMerge newState [var] else newState in
      
      (* Delay writing back to global summary until dropGlobalAssume *)
      newState, emptyNotifyGR
    end else 
      let newState = 
        assignVarStore curFunc expectedT oldSt var bitsOff v strong in
      let newState = 
        if not strong 
        then !myMerger#aggressiveMerge newState [var] else newState in
      newState, emptyNotifyGR
  end


(************************************************************)


let dropGlobalAssume curFunc state =

  let dropBindings () = 
    VarMap.fold
      (fun var v (curB, gside) ->
         if isGlobalDebug "dropG" false state.vAttrs var 
         then begin
           let newT = typeIDOfFvar state.vAttrs var in 
           let newGSide = updateGlobalBinding state var v newT in
           (VarMap.remove var curB, combineGSide gside newGSide)
         end else (curB, gside)
      ) state.bindings (state.bindings, freshGSide ())
  in

  let dropAttribs atts gside =
    VarMap.fold
      (fun var att (curA, gside) ->
         if att.vKind = HGlobal && 
           extendsFormal curFunc.sformals var && 
           isFunctionType (Type_utils.findTyp att.vType) then
             ( VarMap.remove var curA, 
               { gside with gInitialFP = VarSet.add var gside.gInitialFP; } )
         else (curA, gside)
      ) atts (atts, gside)
  in

  let newB, gside = dropBindings () in
  let newA, gside = dropAttribs state.vAttrs gside in

  let newState = { bindings = newB; vAttrs = newA; } in
  (newState, gside)



(************************************************************)

(** Diff that compares bindings according to the eq relation *)
let makeDiffMap eq inM outM =
  VarMap.fold 
    (fun var v cur -> 
       try 
         let oldVal = VarMap.find var inM in
         if eq v oldVal then cur
         else VarMap.add var v cur
       with Not_found -> VarMap.add var v cur
    ) outM VarMap.empty
    
(** Return a mapping that reflects the new values of variables at
    the outSt as compared to the input state inSt  *)
let diffStore inSt outSt =
  makeDiffMap eqVals inSt outSt

(** Return map of attributes for new variables and attributes for old 
    variables that changed *)
let diffAttrs inSt outSt =
  makeDiffMap eqAtt inSt outSt

(** Diff that only checks for existence of new bindings *)
let makeDiffMapNF inM outM =
  VarMap.fold
    (fun var v cur -> 
       if VarMap.mem var inM then cur
       else VarMap.add var v cur
    ) outM VarMap.empty


let diffState inSt outSt =
  (* Assume outSt has mappings for everything WE CARE ABOUT in inSt. I.e.,
     if something no longer has a mapping in outSt that had a mapping in
     inSt, it should be because it is no longer reachable in outSt, BUT
     the fact that it is unreachable should show up in the diff 
     so that it will be unreachable when the summary is applied.
     
     Or, we need to be able to indicate when a mapping disappeared /
     keep track of renamings *)
  { bindings = diffStore inSt.bindings outSt.bindings;
    vAttrs = diffAttrs inSt.vAttrs outSt.vAttrs; }


(** Only bring in NEW bindings and associated attributes that refer to 
    newly materialized input variables *)
let mergeNewInBindings oldSt newishSt =

  let getNewOffs olderm newerm =
    OffsetMap.fold 
      (fun o2 v2 cur ->
         if OffsetMap.mem o2 olderm then cur else (o2, v2) :: cur
      ) newerm []
  in

  let collectNewOffs newOffs cur =
    List.fold_left 
      (fun cur (o, v) ->
         try
           let old = OffsetMap.find o cur in
           let newv = combineVals valBaseNone old v in
           OffsetMap.add o newv cur
         with Not_found -> OffsetMap.add o v cur
      ) cur newOffs
  in

  let mergeARec newstuff (old, oldm) =
    OffsetMap.fold 
      (fun o v (res, newm) ->
         OffsetMap.add o v res, max newm o) newstuff (old, oldm)
  in

  let mergeNewOffs newOffMaps recs =
    List.map (mergeARec newOffMaps) recs
  in

  let rec checkRecords var v oldVal cur : memory =
    match v, oldVal with
      FInt _, FInt _ 
    | FNFP _, FNFP _
    | FpRef _, FpRef _
    | Refs _, Refs _ -> cur
    | FIRecs _, FIRecs _ -> cur
    | Records recs1, Records recs2 ->
        (* Find (offset, value) pairs where offset was bound in 
           SOME record of recs1, but NOT in any record of recs2 *)
        let newOffVals = List.fold_left 
          (fun newOffVals (fm1, _) ->
             List.fold_left
               (fun newOffVals (fm2, _) ->
                  collectNewOffs (getNewOffs fm1 fm2) newOffVals
               ) newOffVals recs2
          ) OffsetMap.empty recs1 in
        (* Take the newoffset value pairs and add them to all recs in recs2.
           Be careful to update the maxoff of each too *)
        let newv = (makeNormalizedRec (mergeNewOffs newOffVals recs2)) in
        VarMap.add var newv cur

    | NCRecord (fm1, m1), NCRecord (fm2, m2) ->
        let newOffVals = collectNewOffs (getNewOffs fm1 fm2) OffsetMap.empty in
        let newfm, newm = mergeARec newOffVals (fm2, m2) in
        VarMap.add var (NCRecord (newfm, newm)) cur

    | NCRecord _, _ | _, NCRecord _ 
    | FIRecs _, _ | _, FIRecs _ ->
        failwith (Printf.sprintf "TODO: NCRecord, FIRecs %s vs %s\n" 
                    (string_of_val v) (string_of_val oldVal))

    | Records _, _ ->
        checkRecords var v (Records (promoteToRecord oldVal)) cur
    | _, Records _ ->
        checkRecords var (Records (promoteToRecord v)) oldVal cur

    | _, _ ->
        failwith (Printf.sprintf 
                    "mergeNewInBindings has val mismatches\n%s\n%s\n"
                    (string_of_val v) (string_of_val oldVal))
  in

  let doBindings oldBind newishBind : memory =
    VarMap.fold
      (fun var v cur ->
         try
           let oldVal = VarMap.find var oldBind in
           checkRecords var v oldVal cur
         with Not_found -> VarMap.add var v cur
      ) newishBind oldBind
  in

  let doAttrs oldAtts newAtts =
    VarMap.fold 
      (fun var att cur ->
         if VarMap.mem var cur then cur
         else VarMap.add var att cur
      ) newAtts oldAtts
  in

  { bindings = doBindings oldSt.bindings newishSt.bindings;
    vAttrs = doAttrs oldSt.vAttrs newishSt.vAttrs; }



(* Debug... *)
let checkDiff descr st1 st2 =
  let checkDiffSub gainloss a b =
    let diff = diffState a b in
    if not (VarMap.is_empty diff.vAttrs) then begin
      logErrorF "GOTCHA %s attrs %s?\n" gainloss descr;
      printState diff
    end
  in
  if !currentLoc.line = (-2) then begin
    checkDiffSub "gained" st1 st2;
    checkDiffSub "lost" st2 st1
  end

let makeOverrideMaps fold add inM diffM =
  fold (fun var v cur -> add var v cur) diffM inM

let makeOverrideVarM (inM : 'a VarMap.t) (diffM : 'a VarMap.t) = 
  makeOverrideMaps VarMap.fold VarMap.add inM diffM

let makeOverrideOM (inM : 'a OffsetMap.t) (diffM : 'a OffsetMap.t) = 
  makeOverrideMaps OffsetMap.fold OffsetMap.add inM diffM

(*
let makeOverrideVal oldV diffV =
  (** also need offset... from diffV's perspective... *)

  old NR, new NR, fine
  old rec, new rec, fine
  old rec, new NR ...
  old NR, new rec ...

*)


(************************************************************)

let combineFpAccs a1 a2 =
  VarMap.union (combineVals None) a1 a2
    
let combineGlobalMods s1 s2 =
  let repItUp s cur =
    VarSet.fold 
      (fun var cur -> 
         let repvar, off = globRep var in
         VarSet.add repvar cur
      ) s cur
  in
  repItUp s1 (repItUp s2 VarSet.empty)

let combineModIns mi1 mi2 =
  VarMap.union combineModOffs mi1 mi2

let combineAliasIn a1 a2 =
  if a1 == a2 then a1
  else UnifLocs.joinMaps a1 a2

(*
let combineStores curFunc s1 s2 =
  let firstOnly = VarMap.diff s1.bindings s2.bindings in
  let secondOnly = VarMap.diff s2.bindings s1.bindings in
  let nthWithKth nth kth kthOnly =
    VarMap.fold 
      (fun var _ curSt ->
         let baseAP = getAccPath curFunc curSt.vAttrs var in
         let _, newSt = 
           getInitialVar baseAP curSt var (typeOfFVar kth.vAttrs var) in
         newSt
      ) kthOnly nth
  in
  let s1 = nthWithKth s1 s2 secondOnly in
  let s2 = nthWithKth s2 s1 firstOnly in
  { bindings = VarMap.union (combineVals valBaseNone) s1.bindings s2.bindings; 
    vAttrs = combineAttributes s1.vAttrs s2.vAttrs; }
    (* may have gotten more attributes from getInitialVar. E.g.,

       st1[x] = not tracked, st2[x] = ref y
       we get stCombo[x] = ref {*x, y}  
       so we now need to bring in the attrib for *x.  *)

*)

let combineStores curFunc s1 s2 =
  let curSt = ref s1 in 
  let makeDefaultVal var = 
    (* Assume same attrs in both *)
    let baseAP = getAccPath curFunc !curSt.vAttrs var in
    let v, st = 
      getInitialVar baseAP !curSt var (typeOfFVar !curSt.vAttrs var) in
    curSt := st;
    v
  in
  let combineVarVals var v1 v2 =
    let v, st = combineVarValsSt !curSt var v1 v2 in
    curSt := st;
    v
  in
  let bindings = VarMap.unionCK combineVarVals makeDefaultVal 
    s1.bindings s2.bindings in
  let attrs = combineAttributes !curSt.vAttrs s2.vAttrs in
  { bindings = bindings;
    vAttrs = attrs; }


(************************************************************)


(** LUB of the two states *) 
let combineStates curFunc st1 st2 =
  if st1 == st2 then st1 
  else if (isBottomState st1) then st2 
  else if (isBottomState st2) then st1 
  else if (isTopState st1) then st1
  else if (isTopState st2) then st2
  else begin
    let st1 = stUseReps curFunc.sformals st1 in
    let st2 = stUseReps curFunc.sformals st2 in

    let allAttrs = combineAttributes st1.vAttrs st2.vAttrs in

    let st1 = { st1 with vAttrs = allAttrs; } in
    let st2 = { st2 with vAttrs = allAttrs; } in
    let st = combineStores curFunc st1 st2 in

    (* Hacks *)
    let st = keepOneOffsetSame st in
    let st = !myMerger#aggressiveMergeAll st in

    let st = Stat.time "mergeAPAll" mergeAccPathCyclesAll st in
    st
  end


let combineSideInfo s1 s2 =
  { initialFP = combineFpAccs s1.initialFP s2.initialFP;
    nonAliasG = combineGlobalMods s1.nonAliasG s2.nonAliasG; 
    modIn = combineModIns s1.modIn s2.modIn; 
    svTypes = combineTypeMap s1.svTypes s2.svTypes;
    aliasedIn = combineAliasIn s1.aliasedIn s2.aliasedIn;
  }

(** Not using the curFunc to make up missing entries from st1 st2
    because we assume the are the same in the first place *)
let combineInitialSt (st1, sid1) (st2, sid2) =
  assert (eqStModRep st1 st2);
  (st1, combineSideInfo sid1 sid2)

