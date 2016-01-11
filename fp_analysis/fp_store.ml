open Cil
open Cildump
open Pretty
open Stdutil
open Logging
open Fp_types
open Fp_lattice_ops
open Fp_malloc
open Fp_agg_merge
open Globals_ref
open Type_reach

(*********************************************************
  High-level operations on state 
 *********************************************************)

(**** Mark heap reachable from globals as global *****)

module ValQ = Queueset.Make (
  struct
    type t = fvalue
    let compare a b = compareVal a b
  end )

let addIfHeap (var, _) cur =
  match var with 
    FInput _ | FHeap _ -> VarSet.add var cur 
  | FVar _ | FRet _ -> cur 

let collectHeapInVal v cur =
  foldTargetsVal addIfHeap v cur

(** Collect vars transitively reachable from the given value [v]
    that may have been allocated on the heap (i.e., include input vars)
    Assume the given set [cur] has already been grep'ed for transitive reach. *)
let collectTransitiveHeap st v cur =
  let valsToGrep = ValQ.create () in
  let cur = ref cur in
  ValQ.addOnce v valsToGrep;
  while not (ValQ.is_empty valsToGrep) do
    let nextV = ValQ.pop valsToGrep in
    let moreVars = collectHeapInVal nextV VarSet.empty in
    VarSet.iter
      (fun var ->
         if VarSet.mem var !cur then ()
         else begin
           cur := VarSet.add var !cur;
           try 
             let v = getBinding var st in
             ValQ.addOnce v valsToGrep
           with Not_found ->
             if not (isGlobalDebug "collectTrans" true st var) then
               failwith "collectTrans can't find binding for local?"
         end
      ) moreVars;
  done;
  !cur


let doublecheckWidth var (isStruct, width)  = 
  if width == 0 then 
    (logErrorF "lookup/store given 0 width: %s @ %s\n" 
       (string_of_var var) (string_of_loc !currentLoc); 
     (isStruct, scalarWidth))
  else (isStruct, width)

(** Bring in the binding for this global, and attributes for its targets *)
let addGlobalAttr var cur =
  match var with 
    FHeap _ | FInput _ -> VarMap.add var HGlobal cur
  | FRet _ | FVar _ -> cur
  

(* TODO: be careful about globals that are unified... *)

let bringInGlobal var state readerOpt =
  let gval = getGlobalBinding var readerOpt in
  let newStore = VarMap.add var gval state.bindings in
  let newTargets = collectHeapInVal gval VarSet.empty in
  let newAttrs = VarSet.fold addGlobalAttr newTargets state.hAttrs in
  { bindings = newStore; hAttrs = newAttrs; }


(** The value is captured by a global. Adjust to reflect escapage *)
let markGlobal gvar var (newGlobals, newSt) =
  match var with
    FHeap _ | FInput _ ->
      if isGlobalDebug "mark" true newSt var then (newGlobals, newSt)
      else begin
        logStatusF "Heap escaped: %s |-> %s @ %s\n" 
          (string_of_var gvar) (string_of_var var) (string_of_loc !currentLoc);
        
        ( VarSet.add var newGlobals,
          { newSt with hAttrs = VarMap.add var HGlobal newSt.hAttrs; } )
          (* Leave the binding in the state for now... *)
      end
  | FRet _ | FVar _ -> failwith "collectHeap returned non-malloc"

let mergeGlobalVals var (curSt, curR) =
  let v = getBinding var curSt in
  let readers, newV = updateGlobalBinding var v in
  (* Leave the binding in the state for now... *)
  curSt, readers

    
let checkEscape gvar st v =
  let heapVars = collectTransitiveHeap st v VarSet.empty in
  let heapVars = 
    VarSet.filter (fun var -> VarMap.mem var st.bindings) heapVars in
  if not (VarSet.is_empty heapVars) then begin
    let newGlobals, st = 
      VarSet.fold (markGlobal gvar) heapVars (VarSet.empty, st) in

    (* Delay this until dropGlobalAssume *)
    let newSt, readers = st, RS.empty in
(*    let newSt, readers = 
      VarSet.fold mergeGlobalVals newGlobals (st, RS.empty) in
*)  
    newSt, readers
  end else
    st, RS.empty


(************************************************************)
    
(** Looks up the value at [var] and [off] from a given state. 
    Assumes offset is canonicized. 
    May raise NullException *)
let lookupVal state var off sinfo readerOpt = 
  if nullVar#isVar var || extVar#isVar var then begin
    raise NullException
  end else if nfpVar#isVar var then 
    (FNFP lookupNfp, state)
  else 
    let state = stUseReps state in
    let var = Fp_unify_globals.gRep var in
    let state = 
      if not (VarMap.mem var state.bindings) && 
        isGlobalDebug "lookup" true state var then begin
          bringInGlobal var state readerOpt
        end else state in
    let sinfo = doublecheckWidth var sinfo in
    try (lookupValStore state.bindings var off sinfo, state)
    with Not_found ->
      logError ("lookupVal: not found " ^ string_of_var var ^ " " ^
                  string_of_loc !currentLoc);
      (defaultValTypeInfo sinfo, state)


(** returns a new store where [var.off] has been assigned [v] *)
let assignVarStore oldStore var off v strong width =
  if strong then strongUpdate oldStore var off v width
  else weakUpdate oldStore var off v width

let isSummary state var =
  try
    match VarMap.find var state.hAttrs with
      HSingle -> false | HGlobal | HSum -> true
  with Not_found ->
    logErrorF "isSummary can't find attr for: %s\n" (string_of_var var);
    printState state;
    failwith "isSummary not found"

(** returns a new state where [var.off] has been assigned [v] *)
let assignVar oldSt var off v strong sinfo =
  if nullVar#isVar var then begin
    raise NullException
  end else if nfpVar#isVar var || extVar#isVar var then
    (oldSt, RS.empty)
      (* TODO: check if it's a function... don't want to write to them! *)
  else begin
    let oldSt = stUseReps oldSt in
    let var = Fp_unify_globals.gRep var in
    let v = valUseReps v in
    let strong = strong &&
      ( match var with
          FVar _ | FRet _ -> true
        | FHeap _ | FInput _ -> not (isSummary oldSt var) ) in
    let sinfo = doublecheckWidth var sinfo in
    if isGlobalDebug "assign" true oldSt var
(*      || (match var with FHeap _ -> true | _ -> false) *)
    then begin
      let oldSt, escNotify = checkEscape var oldSt v in
      (* Keep a "local" copy of the global *)
      let newState = 
        { oldSt with
            bindings = assignVarStore oldSt.bindings var off v strong sinfo;   
        } in      
      let newState = 
        if not strong 
        then !myMerger#aggressiveMerge newState [var] else newState in
      (* May have gotten a points to set w/ > 1 var, chained up some nodes *)
      
      (* Woh, what if it was a weak update... wouldn't we get a new value? *)
      
      (* Delay this until dropGlobalAssume *)
      let toNotify = RS.empty in
      (*      let toNotify = updateGlobal var off v sinfo in *)
      
      newState, (RS.union toNotify escNotify)
    end else 
      let newState = 
        { oldSt with 
            bindings = assignVarStore oldSt.bindings var off v strong sinfo; } 
      in
      let newState = 
        if not strong 
        then !myMerger#aggressiveMerge newState [var] else newState in
      newState, RS.empty
  end

(*********************************************************
 * Focusing state based on a target 
 *********************************************************)

type access =
    AReadVar of fvar
  | AReadField of raw_offset
  | AConcatOff of raw_offset
  | ADeref of FLocSet.t

let d_acc acc =
  match acc with
    AReadVar fv -> defPrint#d_var fv
  | AReadField off -> defPrint#d_off nil None off
  | AConcatOff off -> text "+" ++ defPrint#d_off nil None off
  | ADeref ls -> 
      if FLocSet.cardinal ls > 3 then text "*{MAY}" 
      else defPrint#d_ptrSet (text "*") ls 

let printAccPath path =
  logStatusD ((seq_to_doc (text " -> ") List.iter d_acc path nil) ++ line)
  
type accessSeq = access list

type lookupHook = access -> unit

let nilLookupHook (acc:access) = ()


(** Hook to have eval record the access path. For focus, we can then 
    just use that access path to slice the state *)
let captureAccessPath curPath acc =
  match acc with
    AReadField off -> 
      if isNoOffset off then ()  (* Filter out reads of NoOffset *)
      else curPath := acc :: !curPath
  | _ -> curPath := acc :: !curPath

exception UnreachableState
exception FilterMismatch

let getAccGlobalBindings accPath startState =
  let addGlobal cur var = 
    if nullVar#isVar var || nfpVar#isVar var || extVar#isVar var then cur
    else if not (hasBinding var cur) && 
      (isGlobalDebug "getAccGlob" true startState var) then 
        bringInGlobal var cur GNone 
          (* already registered the reader while getting accPath... *)
    else cur
  in
  let collect (var, o) cur = addGlobal cur var in
  List.fold_left 
    (fun cur acc ->
       match acc with
         AReadVar var -> addGlobal cur var
       | AReadField _ -> cur
       | AConcatOff _ -> cur
       | ADeref locs ->
           FLocSet.fold collect locs cur
    ) startState accPath


(* TODO: w/ all these summary nodes... we need to be careful!!! 
   E.g., if x is a summary node with
   x->fp = {f1, f2} and x->next = {null, x}, then
   x->fp(x) should yield a call to 
   - f1 w/ (y->fp = f1, and y->next = {null, x})
   - f2 w/... similar
   instead of 
   - f1 w/ ... and y->next = {null, y}
*)

let filterReach accPath (targV, targO) startState shouldReach =
  logStatusF "filterReach w/ accessPath: ";
  printAccPath accPath;
  logStatusF "Target (%b): %s\n" shouldReach (string_of_pointer (targV, targO));
  flushStatus ();
    
  let startState = getAccGlobalBindings accPath startState in
  let curState = ref startState in

  let goalReached reaches terminal = 
    (shouldReach && reaches) || 
      (not shouldReach && (not terminal || not reaches)) 
      (* Only prune the "not shouldReach" at the terminal case *)
  in

  let rec filterReachVar curVar curPath : bool =
    if nullVar#isVar curVar then raise UnreachableState
    else if nfpVar#isVar curVar then true 
    else if extVar#isVar curVar then true
      (* TODO: Nfp only means any non-FP... what if reach-target is func??? 
         Ugh... totally need a 3rd value *)
    else
      try
        let oldVal = getBinding curVar startState in
        let newVal, reaches = filterReachVal oldVal curPath in
        if goalReached reaches false then  (* others check terminal *)
          ( if oldVal == newVal then ()
            else curState := addBinding !curState curVar newVal;
            reaches )
        else raise UnreachableState
      with Not_found ->
        logErrorF "WTF: filterReachVar var NF %s" (string_of_var curVar);
        raise UnreachableState
          
  and consumeConcats o curPath =
    match curPath with
      AConcatOff off :: rest -> consumeConcats (concatOffset off o) rest
    | _ -> o, curPath

  and filterReachPtrTarget (v,o) curPath =
    let o, curPath = consumeConcats o curPath in
    (* Make up a field read first if the offset is non-zero, 
       or the accessed var is a record *)
    let newPath = 
      if isScalar (structInfoFVar v) && (isNoOffset o)
      then curPath else AReadField o :: curPath in
    filterReachVar v newPath
      
  and filterReachVal curVal curPath : (fvalue * bool) =
    match curVal with
      FNFP _ -> logStatus ("filterReachVal hit Nfp?"); (curVal, true)
        (* TODO: need a 3-value thing ... *)

    | FInt _ -> 
        let reaches, terminal = 
          if curPath = [] then (nullVar#isVar targV, true)
          else
          (logStatus ("filterReachVal hit Null?"); 
           (false, false)) in
        if goalReached reaches terminal then (curVal, reaches)
        else raise UnreachableState

    | FpRef vi -> 
        if curPath = [] then 
          let reaches = isVarinfoFVar vi targV && isNoOffset targO in
          if goalReached reaches true then (curVal, reaches)
          else (logErrorF "unreach: hit other func: %d\n" vi; 
                raise UnreachableState)
            (* TODO: what if reach-target is EXT?
               Ugh... totally need a 3rd value *)

        else (logError ("filterReachVal hit Funptr?"); (curVal, false))
          
    | Refs ls ->
        let checkReach, ls, terminal = 
          (match curPath with
             ADeref ls2 :: rest ->
               if not (FLocSet.equal ls ls2) then
                 (logError "filterReachVal Mayref targets don't match";
                  let diff = FLocSet.diff ls2 ls in
                  logErrorD (defPrint#d_ptrSet (text "Diff+: ") diff ++ line);
                  let diff = FLocSet.diff ls ls2 in
                  logErrorD (defPrint#d_ptrSet (text "Diff-: ") diff ++ line)
                 );
               (fun l2 -> filterReachPtrTarget l2 rest), ls2, false
           | [] -> (fun l2 -> compFLoc (targV, targO) l2 == 0), ls, true
           | h :: rest -> 
               logError "filterReachVal path Mayref mismatch";
               logErrorD (defPrint#d_ptrSet (text "LS1: ") ls ++ line);
               logErrorD (text "Acc2: " ++ d_acc h ++ line);
               (fun l2 -> filterReachPtrTarget l2 rest), ls, false
          ) in
        let newLS, newReaches = FLocSet.fold 
          (fun l2 (curSet, curReach) ->
             try
               let reaches = checkReach l2 in
               let newSet = 
                 if goalReached reaches terminal then FLocSet.add l2 curSet
                 else curSet in
               (newSet, curReach || reaches)
             with UnreachableState ->
               curSet, curReach
          ) ls (FLocSet.empty, false) in

        let newSize = FLocSet.cardinal newLS in
        if newSize == 0 then raise UnreachableState
        else (Refs newLS, newReaches)

    | Records recs ->
        (try
           let o, rest, terminal = checkPathRecord curPath in
           let newrecs, reaches = List.fold_left
             (fun (curRecs, curReach) r -> try
                let newfp, newnonfp, newm, reaches = 
                  checkReachRecord r o rest terminal in
                let newRecs = 
                  if goalReached reaches terminal
                  then (newfp, newnonfp, newm) :: curRecs
                  else curRecs in
                (newRecs, curReach || reaches)
              with UnreachableState -> 
                (curRecs, curReach)
             ) ([], false) recs in
           
           if newrecs = [] then raise UnreachableState
           else Records (tryReduceRecordSet newrecs), reaches
         with FilterMismatch ->
           curVal, true) (* TODO: shouldn't happen in the first place *)

    | FIRecs fir ->
        (try
           let o, rest, terminal = checkPathRecord curPath in
           let newFIR, reaches = 
             if terminal then 
               List.fold_left
                 (fun (newFIR, reached) (fp, nonfp) ->
                    (* check what type the target is... *)
                    if isFunc (targV, targO) then
                      let newFP, reaches = filterReachVal fp rest in
                      if goalReached reaches terminal then
                        ((newFP, nonfp) :: newFIR, reached || reaches)
                      else
                        (newFIR, reached || reaches)
                    else
                      let newNonFP, reaches = filterReachVal nonfp rest in
                      if goalReached reaches terminal then
                        ((fp, newNonFP) :: newFIR, reached || reaches)
                      else
                        (newFIR, reached || reaches)
                 ) ([], false) fir
             else 
               List.fold_left
                 (fun (newFIR, reached) (fp, nonfp) ->
                    (* only search nonfp *)
                    try
                      let newnonfp, reaches = filterReachVal nonfp rest in
                      ((fp, newnonfp) :: newFIR, reached || reaches)
                    with UnreachableState -> (newFIR, reached)
                 ) ([], false) fir
           in
           if newFIR = [] then raise UnreachableState
           else (FIRecs newFIR, reaches)
         with FilterMismatch ->
           curVal, true)  (* TODO: shouldn't happen in the first place *)

  and checkPathRecord curPath =
    match curPath with
      AReadField o :: rest -> (o, rest, false)
    | [] -> (noOff, [], true)
    | h :: rest -> 
        logError "filterReachVal path Record mismatch";
        logErrorD (text "Acc2: " ++ d_acc h ++ line); 
        raise FilterMismatch
          
  and checkReachRecord (fp, nonfp, m) off curPath terminal =
    (* Need to decide which OffsetMap to work with *)
    if OffsetMap.mem off fp then
      let newFP, reaches = checkReachOffmap fp off curPath terminal in 
      (newFP, nonfp, m, reaches)
    else if OffsetMap.mem off nonfp then
      let newNonFP, reaches = checkReachOffmap nonfp off curPath terminal in
      (fp, newNonFP, m, reaches)
    else raise UnreachableState
        
  and checkReachOffmap fm off curPath terminal =
    let oldVal = OffsetMap.find off fm in
    let newVal, reaches = filterReachVal oldVal curPath in
    if goalReached reaches terminal then 
      ( if oldVal == newVal then (fm, reaches)
        else (OffsetMap.add off newVal fm, reaches ) )
    else raise UnreachableState
  in
  match accPath with
    AReadVar fv :: rest ->
      let reaches = filterReachVar fv rest in
      (!curState, reaches)
  | ADeref ls :: rest ->
      if FLocSet.is_singleton ls then
        let v, o = FLocSet.choose ls in
        (* May hit this case with *(array + i) since we get the address of 
           the "array" w/out reading a pointer variable, and jumping
           straight into the derefence. *)
        let reaches = filterReachPtrTarget (v, o) rest in
        if goalReached reaches false then (!curState, reaches)
        else raise UnreachableState
      else 
        failwith "filterReach accessPath doesn't begin with Var?"

  | _ -> failwith "filterReach accessPath doesn't begin with Var?"


(** return a new state where the ptrExp MUST point to the given target *)
let focus revAccPath (v, o) st =
  try
    let newState, reaches = filterReach (List.rev !revAccPath) (v, o) st true in
    newState
  with UnreachableState ->
    logError ("focus: hit unreachable state");
    bottomState


(** return a new state where the ptrExp MUST NOT point to the given target *)
let focusNot revAccPath (v, o) st =
  try 
    let newState, reaches = filterReach (List.rev !revAccPath) (v, o) st false in
    (* can't assert that it doesn't reach at all... *)
    newState
  with UnreachableState ->
    logError ("focus: hit unreachable state");
    bottomState

(************************************************************)


let dropGlobalAssume state =
  let newB, toNotify = VarMap.fold
    (fun var v (curB, toNotify) ->
       if isGlobalDebug "dropG" false state var 
       then begin
(*         logStatusF "Dropped global %s @ %s\n"
           (string_of_var var) (string_of_loc !currentLoc);
*)
         let newNotify, newGVal = updateGlobalBinding var v in
         (VarMap.remove var curB, RS.union toNotify newNotify)
       end else (curB, toNotify)
    ) state.bindings (state.bindings, RS.empty) in
  let newState = { state with bindings = newB; } in
  let newState = stUseReps newState in
  (newState, toNotify)
    (* don't drop indication that heap var is global from hAttrs *)

(************************************************************)

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

let diffState inSt outSt =
  (* Assume outSt has mappings for everything WE CARE ABOUT in inSt. I.e.,
     if something no longer has a mapping in outSt that had a mapping in
     inSt, it should be because it is no longer reachable in outSt, BUT
     the fact that it is unreachable should show up in the diff 
     so that it will be unreachable when the summary is applied.
     
     Or, we need to be able to indicate when a mapping disappeared /
     keep track of renamings *)
  { bindings = diffStore inSt.bindings outSt.bindings;
    hAttrs = (* outSt.hAttrs; *)
      diffAttrs inSt.hAttrs outSt.hAttrs;
  }

let makeOverrideMaps inM diffM =
  VarMap.fold (fun var v cur -> VarMap.add var v cur) diffM inM

let makeOverride inSt diffSt =
  { bindings = makeOverrideMaps inSt.bindings diffSt.bindings;
    hAttrs = makeOverrideMaps inSt.hAttrs diffSt.hAttrs; }


(************************************************************)

(** Check against given state to determine the globalness  *)
let defaultVarValHighLevel st (key:fvar) = 
  if isGlobalDebug "defaultVHL" true st key then
    try getGlobalBinding key GNone
    with Not_found ->
      (logError "defaultVarValHighLevel for unknown global";
       defaultValTypeInfo (structInfoFVar key))
  else
    defaultValTypeInfo (structInfoFVar key)

let defaultValFrom st key =
  try getBinding key st
  with Not_found ->
    defaultVarValHighLevel st key

let combineStores defaultVal s1 s2 =
  VarMap.unionC combineVals defaultVal s1 s2


let countNonSpecial (var, o) cur =
  if not (isSpecialVar var) then cur + 1 else cur

let combineValsCheck v1 v2 =
  let v = combineVals v1 v2 in
  if v == v1 then (v1, false)
  else 
    let shouldTryMerge = 
      (match v with
         FInt _ | FNFP _ | FpRef _ -> false
       | Refs locs -> 
           FLocSet.fold countNonSpecial locs 0 > 1
       | Records _ | FIRecs _ ->
           true (* Don't bother over analyzing *)
      ) in
    v, shouldTryMerge


let combineStoresN defaultVal s1 s2 =
  VarMap.unionCN combineValsCheck defaultVal s1 s2

let defaultAttrFrom st var =
  try VarMap.find var st.hAttrs
  with Not_found -> 
(*    logErrorF "defaultAttrFrom: NF %s @ %s\n" 
      (string_of_var var) (string_of_loc !currentLoc);
*) 
    (* Not going to have it if it's fresh! *)
    defaultAttr var

let combineAttributes defaultAttr atts1 atts2 =
  VarMap.unionC combineAttrs defaultAttr atts1 atts2


(************************************************************)

(** Copy any values that have newly escaped into the global summary *)
let mergeNewEscapes st st1 st2 =
  let findNew () =
    VarMap.fold 
      (fun var att cur ->
         match att with 
           HGlobal ->
             (try 
                let old1 = VarMap.find var st1.hAttrs in
                let old2 = VarMap.find var st2.hAttrs in
                (match old1, old2 with
                   HGlobal, HGlobal -> cur
                 | _ -> VarSet.add var cur)
              with Not_found ->
                VarSet.add var cur)
         | _ -> cur
      ) st.hAttrs VarSet.empty
  in
  let mergeVals vars =
    () (* TODO: merge the final combined val for the global vars into 
          global summary *)
  in
  let newlyEscVar = findNew () in
  mergeVals newlyEscVar;
  st


(** LUB of the two states *) 
let combineStates st1 st2 =
  if st1 == st2 then st1 
  else if (isBottomState st1) then st2 
  else if (isBottomState st2) then st1 
  else if (isTopState st1) then st1
  else if (isTopState st2) then st2
  else
    let st1 = stUseReps st1 in
    let st2 = stUseReps st2 in
    let st1WithAttrs = 
      { st1 with hAttrs = 
          combineAttributes defaultAttr st1.hAttrs st2.hAttrs; } in
    let defaultVal = defaultVarValHighLevel st1WithAttrs in
    let newStore, newKeys = 
      combineStoresN defaultVal st1.bindings st2.bindings in
    let st = 
      { bindings = newStore;
        hAttrs = st1WithAttrs.hAttrs; } in
    let st = mergeNewEscapes st st1 st2 in
    let st = !myMerger#aggressiveMerge st newKeys in
    st


