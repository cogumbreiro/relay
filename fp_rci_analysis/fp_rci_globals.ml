
(** Pre-pass to store all the globals referenced directly by a function 
    Also, visit all initializers and set up the initial global store. *)

open Cil
open Fstructs
open Type_utils
open Fp_rci_types
open Fp_rci_flow_insens 
open Fp_rci_lattice_ops
open Fp_rci_unify_structs
open Fp_rci_unify_globals
open Logging
open Cildump

(************************************************************)

(** Keep track of where a global cell is read *)
type greader = Summary_keys.sumKey * prog_point

(** Keep track of base access paths that global represent *)
type inputReps = (accPath * Summary_keys.sumKey)

type readerOpt = 
    GReader of greader * offset_val * structInfo
  | GNone 

module RS = Set.Make
  (struct
     type t  = greader
     let compare (sk1, pp1) (sk2, pp2) =
       let c = Pervasives.compare sk1 sk2 in
       if c == 0 then 
         Pervasives.compare pp1 pp2
       else c
   end)

(* Side-effects from updating the binding of a global 
   (1) set of readers that need to be notified
   (2) set of "input" fun ptrs that need to be concretized *)

type notifyGR = RS.t
let emptyNotifyGR = RS.empty
let combineNotifyGR note1 note2 = RS.union note1 note2

type gSide = 
    { gNotify : notifyGR;
      gInitialFP : collectedFPReq; (** Initial acc paths required for 
                                      from particular functions *)
    } 


let freshGSide () =
  { gNotify = emptyNotifyGR; gInitialFP = emptyDelayedFP; }

let combineGSide gs1 gs2 =
  { gNotify = combineNotifyGR gs1.gNotify gs2.gNotify;
    gInitialFP = combineDelayedFP gs1.gInitialFP gs2.gInitialFP; }

(************************************************************)


(** TODO: add flow constraints, instead of unifying globals that were
    passed in as actuals to args... 

    If A \flows B, and A is written to, then update B / notify its readers.
    The update of B... may need some renaming abstraction / renaming?
    
    It's okay to rename instances of A to B, in the values held by A / B,
    but there is no need to rename other elements?

    What if the reference global B was written to within the function?
    How to flow back to A?

    Hmm... One thing to notice is that A was not a global before the call.
    B became a global FIRST. Then A realizes it should be a global.
    At that point, A can grab updates from B...

    Unfortunately, writes to B do not show up in the summary so 
    we wouldn't know to update A.
    Ugh. The whole subtyping w/ pointers argument...   *)


(* Make this an IntMap since we don't care about the summary-ness 
   of the offset, just the raw value... *)

type globalInfo =
    { gReaders : notifyGR OffsetMap.t; 
      gVal : fvalue; 
      gType : type_id;  (* usually for global heap nodes, 
                           but keep around for global vars too? *)
    }

module FPGlobalSum = struct
  
  type t = globalInfo option

  type simpleSum = t
  let simplify sum = sum
  let desimplify simp = simp
  let initVal = None
  let unknownSummary = None

end


module FPGS = Backed_summary.Make (FPGlobalSum)

let globalSum = new FPGS.data (Backed_summary.makeSumType "fpgs")
let () = Backed_summary.registerType globalSum
  
let flushGlobals () : unit =
  globalSum#serializeAndFlush

let getVarSumKey var =
  (* eh... just leave it 0 for now *)
  (0, plain_string_var var)


(*** Cached-lookup / store ***)


module GCache = Cache.Make 
  (struct

     type t = Summary_keys.sumKey
     let equal a b = a = b
     let hash a = Hashtbl.hash a
     let evictHook k v =
       globalSum#addReplace k v;
       globalSum#flushOne k

   end)
  
let gcache = GCache.create 512

let getGInfo sumKey = 
  try GCache.find gcache sumKey
  with Not_found ->
    let v = globalSum#find sumKey in
    ignore (GCache.add gcache sumKey v);
    v

let setGInfo sumKey info =
  ignore (GCache.replace gcache sumKey info);
  globalSum#addReplace sumKey info


let hasGlobalBinding var =
  let sumKey = getVarSumKey var in
  match globalSum#find sumKey with
    Some _ -> true
  | None -> false


(*** Misc gInfo operations ***)

let freshGlobal gval gtyp =
  { gReaders = OffsetMap.empty;
    gVal = gval; 
    gType = gtyp; }

(** Use a store to wrap up the value and allow re-use of update routines *)
let singletonStore var info =
  VarMap.add var info.gVal VarMap.empty



(** Drop values at offsets less than 0 and greater than the width *)
let filterOutOfBoundsRec width (fm, m) =
  let newfm, newm = 
    OffsetMap.fold 
      (fun off v (curfm, curm) ->
         if off < 0 || off >= width then (curfm, curm)
         else (OffsetMap.add off v curfm, max curm off)
      ) fm (OffsetMap.empty, noOff) in
  newfm, newm

let filterOutOfBounds width v =
  match v with
    FNFP _ | Refs _ | FInt _ | FpRef _ | FIRecs _ -> v
  | Records recs ->
      let newrecs = List.map (filterOutOfBoundsRec width) recs in
      makeNormalizedRec newrecs
  | NCRecord (fm, m) ->
      NCRecord (filterOutOfBoundsRec width (fm, m))

(** Filter out writes to offsets BEYOND this static global's mem *)
let filterGOutOfBounds var v =
  (* Also do that hack dropping offsets of same var... *)
  let v = 
    match keepOneOffsetSameVal v with
      Some v -> 
        warnDroppedOffsetSameVar var;
        v 
    | None -> v in
  match var with
    FVar vid ->
      let vinfo = varinfoOfVid vid in
      let width = widthModArray vinfo.vtype in
      filterOutOfBounds width v

  | FHeap _ | FInput _ -> v
  | FRet _ -> failwith "filterMemUnsafe given ret"

exception WriteToFunc

let setGVal var info v =
  match var with
    FVar vid ->
      let varinfo = varinfoOfVid vid in
      if isFunctionType varinfo.vtype then begin
        logErrorF "Set gval for func: %s\n" (string_of_var var);
        info
       (* raise WriteToFunc *)
      end else 
        { info with gVal = v; }

  | FHeap _ | FInput _ -> { info with gVal = v; }
  | FRet _ -> failwith "Set gval for ret"


let unwrapFromStore newBinding var info =
  let newVal = VarMap.find var newBinding in
  setGVal var info newVal


let addReaderAtOffset reader bits info = 
  let oldRS =
    try OffsetMap.find bits info.gReaders
    with Not_found -> emptyNotifyGR in
  let newRS = RS.add reader oldRS in
  { info with gReaders = OffsetMap.add bits newRS info.gReaders; }


let stride = 8 (* Read at every stride bits... *)

let rec addReaderUpTo reader maxOff curOff info =
  if curOff < maxOff then
    let nextInfo = addReaderAtOffset reader curOff info in
    addReaderUpTo reader maxOff (curOff + stride) nextInfo
  else info

let addReader info readerOpt =
  match readerOpt with
    GNone -> info
  | GReader (reader, bits, (isStruct, width)) ->
      if isStruct then begin
        let maxOff = bits + width in
        addReaderUpTo reader maxOff bits info
      end 
      else addReaderAtOffset reader bits info

let getAllReaders rdsPerOff = 
  OffsetMap.fold (fun _ rs cur -> combineNotifyGR rs cur) 
    rdsPerOff emptyNotifyGR


let getReadersFor offs rdsPerOff = 
  OffsetMap.fold (fun o rs cur ->
                    if OffsetSet.mem o offs then combineNotifyGR rs cur
                    else cur) rdsPerOff emptyNotifyGR

let updateGType info var off newT = 
  if isNoOffset off 
  then { info with gType = combineTypes info.gType newT; }
  else info

let getGInfoWithDefaults var =
  let sumKey = getVarSumKey var in
  let ginfo = getGInfo sumKey in
  let info = match ginfo with 
      None ->
        logStatusF "getGInfo: fresh global %s @ %s\n" 
          (string_of_var var) (string_of_loc !currentLoc);
        let defValue = defaultValNonInput var in
        let defType = unknownMallocIndex in
        let info = freshGlobal defValue defType in
        setGInfo sumKey (Some info);
        info
    | Some info -> info
  in
  (sumKey, info)


let defaultReaders off = emptyNotifyGR

let combineReaders r1 r2 =
  OffsetMap.unionC combineNotifyGR defaultReaders r1 r2


let numReaders info =
  OffsetMap.fold (fun o rs cur -> cur + RS.cardinal rs) info.gReaders 0

(************************************************************)

let collectUnifiable (var, o1) cur =
  match var with 
    FHeap _ | FInput _ -> 
      let _, info1 = getGInfoWithDefaults var in
      let t1 = info1.gType in
      List_utils.addToPartition 
        (fun (otherVar, o2) ->
           (* wasted repeat type generation ... *)
           o1 = o2 &&
            let _, info2 = getGInfoWithDefaults otherVar in
            let otherT = info2.gType in
            eqTypeID t1 otherT
        ) (var, o1) cur
  | FVar _
  | FRet _ -> cur

(** Of the two vars, which would be keep its name around *)
let cheaperMerge var1 var2 =
  let _, info1 = getGInfoWithDefaults var1 in
  let _, info2 = getGInfoWithDefaults var2 in
  if numReaders info1 > numReaders info2 then -1
  else 1

let pickCheapestRep head tail =
  List.fold_left (fun ((curR, curO), rest) (v, o) -> 
                    if cheaperMerge curR v < 0 then
                      ((curR, curO), (v, o) :: rest)
                    else 
                      ((v, o), (curR, curO) :: rest)
                 ) (head, []) tail

let rec unifyGlobalTargets gv notifyGR : notifyGR =
  match gv with
    Refs locs ->
      (** Collect what can be unified based on type and offset *)
      let unifVars = FLocSet.fold collectUnifiable locs [] in
      List.fold_left unifyPartition notifyGR unifVars

  | Records recs -> List.fold_left unifyGlobalOfRec notifyGR recs
  | NCRecord (fm, m) -> unifyGlobalOfRec notifyGR (fm, m)
  | FIRecs fir -> unifyGlobalTargets fir notifyGR
  | FInt _ | FpRef _ | FNFP _ -> notifyGR

and unifyPartition notifyGR unifVars =
  match unifVars with
    [] -> notifyGR
  | h :: t ->
(*
      let (repVar, repOff), rest = pickCheapestRep h t in
*)
      let (repVar, repOff), rest = pickShortestRepLoc unifVars in
      List.fold_left (fun notifyGR (next, nexto) -> 
                        (* Assume only merge w/ same offset *)
                        let delta = noOff in
                        doUnify notifyGR repVar next delta
                     ) notifyGR rest
       (* TODO don't merge globals of different types... *)


and unifyGlobalOfRec notifyGR (fm, m) =
  OffsetMap.fold (fun off v notifyGR -> 
                    unifyGlobalTargets v notifyGR) fm notifyGR

and checkNotify newv oldV oldReaders notifyGR =
  if eqVals oldV newv then 
    notifyGR 
  else 
    let diffOffs = calcDiffOffsets newv oldV in
    let readers = getReadersFor diffOffs oldReaders in
    combineNotifyGR notifyGR readers


(* TODO: If choice is arbitrary, pick the var that minimizes notifies *)
and doUnify notifyGR v1 v2 diff =
  let v1, off1 = globRep v1 in
  let v2, off2 = globRep v2 in
  if eqFVar v1 v2 then notifyGR
  else begin
    (* Originally wanted v1 = v2 + diff, but we now have 
       v1 = v1' + off1
       v2 = v2' + off2

       so v1' + off1 = v2' + off2 + diff -->
          v1' = v2' + (off2 - off1 + diff) *)
    let newDiff = concatOffset 
      (subtractOff (int_of_off off2) (int_of_off off1)) diff in
    unifyGlobal v1 v2 newDiff;

    (* Combine vals and readers if they had that info before *)
    let sumKey1, info1 = getGInfoWithDefaults v1 in
    let sumKey2, info2 = getGInfoWithDefaults v2 in
    let oldV1 = valUseRepsTrusted info1.gVal in
    let oldV2 = valUseRepsTrusted info2.gVal in

    (* Shift the value from v2' to match that of v1' ... *)
    let oldV2 = alignRecordOff (invertOff newDiff) oldV2 in

(*
    if newDiff <> 0 then begin
      logStatusF "GOTCHA: Debug combine v1:%s = v2:%s + %d"
        (string_of_var v1) (string_of_var v2) newDiff;
      printVal oldV1 (Some (findTyp info1.gType));
      printVal oldV2 (Some (findTyp info2.gType));
    end;
*)

    let newv = combineVals valBaseNone oldV1 oldV2 in

    (** Get closure on equalities from this value before valUseRep'ing it up *)
    closeGlobalMerges newv;

    let newv = valUseRepsTrusted newv in
    let oldV1 = valUseRepsTrusted oldV1 in
    let oldV2 = valUseRepsTrusted oldV2 in

    (* ... *)
    let oldV1 = filterGOutOfBounds v1 oldV1 in
    let oldV2 = filterGOutOfBounds v2 oldV2 in
    let newv = filterGOutOfBounds v1 newv in

    (* Only notify the guys that haven't known about the new value *)
    let newNotify = checkNotify newv oldV1 info1.gReaders RS.empty in
    let newNotify = checkNotify newv oldV2 info2.gReaders newNotify in

    if not (RS.is_empty newNotify) then begin
      logStatusF "requeuing readers from unification of %s %s\n"
        (string_of_var v1) (string_of_var v2);
      logStatus "New val";
      printVal newv (Some (findTyp info1.gType));
      logStatus "Old val 1";
      printVal oldV1 (Some (findTyp info1.gType));
      logStatus "Old val 2";
      printVal oldV2 (Some (findTyp info2.gType));
    end;

    let newNotify = RS.union newNotify notifyGR in

    let newrd = combineReaders info1.gReaders info2.gReaders in
    let newtyp = 
      if isNoOffset newDiff then combineTypes info1.gType info2.gType
      else info1.gType in
    let info = { info1 with gReaders = newrd; gType = newtyp; } in
    let info = setGVal v1 info newv in
    setGInfo sumKey1 (Some info);
    setGInfo sumKey2 (Some info);
    unifyGlobalTargets newv newNotify (* Trigger more *)
  end

    
let makeReaderOfAll readerLoc info =
  let isStruct, width = structInfoVal info.gVal in
  GReader (readerLoc, noOff, (isStruct, width))


(** Copy from v2 into v1 ... todo, record in info, inclusion constraints
    on LOCATIONS (e.g., this location is representative and it
    represents locations that include S) instead of this copy *)
let doCopy readerLoc notifyGR v1 v2 =
  let v1, off1 = globRep v1 in
  let v2, off2 = globRep v2 in
  if eqFVar v1 v2 then notifyGR
  else begin
    let sumKey1, info1 = getGInfoWithDefaults v1 in
    let sumKey2, info2 = getGInfoWithDefaults v2 in
    let oldV1 = valUseRepsTrusted info1.gVal in
    let oldV2 = valUseRepsTrusted info2.gVal in

    (* Originally though v1 == v2

       v1 = v1' + off1
       v2 = v2' + off2

       so v1' + off1 = v2' + off2 -->
          v1' = v2' + (off2 - off1) *)
    let delta = subtractOff (int_of_off off2) (int_of_off off1) in
    let oldV2 = alignRecordOff (invertOff delta) oldV2 in
    
(*
    if delta <> 0 then begin
      logStatusF "GOTCHA: copyDelta %s <- %s + %d\n"
        (string_of_var v1) (string_of_var v2) delta;
      printVal oldV1 (Some (findTyp info1.gType));
      printVal oldV2 (Some (findTyp info2.gType));
    end;
*)
  
    let newv = combineVals valBaseNone oldV1 oldV2 in
    let newv = valUseRepsTrusted newv in

    (* ... *)
    let newv = filterGOutOfBounds v1 newv in

    (* Only notify the guys that haven't known about the new value *)
    let newNotify = checkNotify newv oldV1 info1.gReaders RS.empty in

    if not (RS.is_empty newNotify) then begin
      logStatusF "requeuing readers from copy %s <- %s\n"
        (string_of_var v1) (string_of_var v2);
    end;
    let newNotify = RS.union notifyGR newNotify in

    let newtyp = 
      if isNoOffset delta
      then combineTypes info1.gType info2.gType 
      else  info1.gType in

    let info = { info1 with gType = newtyp; } in
    let info = setGVal v1 info newv in
    let info = addReader info (makeReaderOfAll readerLoc info)  in

    setGInfo sumKey1 (Some info);

    newNotify
  end


(************************************************************)

let shiftReaderOpt readerOpt shiftOff = 
  match readerOpt with
    GNone -> readerOpt
  | GReader (readPP, oldOff, sinfo) ->
      GReader (readPP, concatOffset oldOff shiftOff, sinfo)

let offFromReadOpt readerOpt =
  match readerOpt with
    GNone -> None
  | GReader (_, off, _) -> Some off


let addShiftedReader sumKey readerOpt info diff =
  let newReadOpt = 
    if not (isNoOffset diff) then begin
(*
      logStatusF "GOTCHA: getGlobal %s vs (new) %s + %d\n" 
        (string_of_var var) (string_of_var var2) off;
*)
      let newReadOpt = shiftReaderOpt readerOpt diff in
(*
      (match offFromReadOpt newReadOpt, offFromReadOpt readerOpt with
         None, None -> logStatus "Didn't have off"
       | Some newo, Some oldo -> logStatusF "New off %d vs %d\n" newo oldo
       | _, _ -> failwith "lost off");
*)
      newReadOpt
    end
    else readerOpt in  
  let info = addReader info newReadOpt in
  setGInfo sumKey (Some (info));
  info


(** Look up the value from a global. May look up from a representative.
    Returns the identity and difference in offset between original var and
    the representative var. *)
let getGlobalBinding var readerOpt =
  let var2, off = globRep var in
  let diff = int_of_off off in
  let sumKey, info = getGInfoWithDefaults var2 in
  (* If there is an offset difference, shift the readerOpt offset! *)
  let info = addShiftedReader sumKey readerOpt info diff in
  (* Not shifting value... shifting the read offset instead *)
  let v = valUseRepsTrusted info.gVal in
  v, info.gType, var2, diff

    
(** Get the global binding (may look at the representative for the values
    and add readers to the representative, but will not expose the "name"
    of the representative) *)
let getGlobalBindingNoRep var readerOpt =
  let var2, off = globRep var in
  let diff = int_of_off off in
  let sumKey, info = getGInfoWithDefaults var2 in
  (* If there is an offset difference, shift the readerOpt offset! *)
  let info = addShiftedReader sumKey readerOpt info diff in
  (* Shift the value... *)
  let v = valUseRepsTrusted info.gVal in
  let v = alignRecordOff (invertOff diff) v in
  let t = 
    if isNoOffset diff then info.gType
    else 
      let _, origInfo = getGInfoWithDefaults var in
      origInfo.gType in (* Use the old var's info... *)
  v, t

(*  
  let sumKey, info = getGInfoWithDefaults var in
  let info = addReader info readerOpt in
  setGInfo sumKey (Some (info));
  let v = valUseRepsTrusted info.gVal in
  v, info.gType
*)

(**************************************************)

(** Only use before any reads -- does not notify readers / merge w/ old value *)
let strongUpdateGlobal var off rhs expectedT : unit =
  let gside = freshGSide () in
  let rhs, newdelay = 
    replaceInputFPsVal false emptyState.vAttrs rhs gside.gInitialFP in
  let gside = { gside with gInitialFP = newdelay; } in

  assert (VarSet.is_empty gside.gInitialFP);

  let sumKey, info = getGInfoWithDefaults var in
  let oldVarBinding = singletonStore var info in
  let newVarBinding = 
    strongUpdateNonInput expectedT oldVarBinding var off rhs in
  let info = unwrapFromStore newVarBinding var info in
  let info = updateGType info var off (addTyp expectedT) in
  setGInfo sumKey (Some info)


(** Only use before any reads -- does not notify readers *)
let weakUpdateGlobal var off rhs expectedT : unit =
  let gside = freshGSide () in
  let rhs, newdelay = 
    replaceInputFPsVal false emptyState.vAttrs rhs gside.gInitialFP in
  let gside = { gside with gInitialFP = newdelay; } in

  assert (VarSet.is_empty gside.gInitialFP);

  let sumKey, info = getGInfoWithDefaults var in
  let oldVarBinding = singletonStore var info in
  let newVarBinding = 
    weakUpdateNonInput expectedT oldVarBinding var off rhs in
  let info = unwrapFromStore newVarBinding var info in
  let info = updateGType info var off (addTyp expectedT) in
  setGInfo sumKey (Some info)



(** Only use before any reads -- does not notify readers / merge w/ old value *)
let strongUpdateGlobalBinding var rhs expectedT : unit =
  let gside = freshGSide () in
  let rhs, newdelay = 
    replaceInputFPsVal false emptyState.vAttrs rhs gside.gInitialFP in
  let gside = { gside with gInitialFP = newdelay; } in

  assert (VarSet.is_empty gside.gInitialFP);

  let sumKey, info = getGInfoWithDefaults var in
  let info = setGVal var info rhs in
  let info = updateGType info var noOff (addTyp expectedT) in
  setGInfo sumKey (Some info)



(** Do an update on the variable as a whole (vs var + off). Also, do
    not merge values if there was no binding previously *)
let updateGlobalBinding state var rhs rhsTyp : gSide =
  let gside = freshGSide () in
  let rhs, delay = 
    replaceInputFPsVal false state.vAttrs rhs gside.gInitialFP in
  let gside = { gside with gInitialFP = delay; } in

  let doUpdate gside finalVar oldVal finalVal finalTyp = 
    let sumKey, info = getGInfoWithDefaults finalVar in
    let info = { info with gType = finalTyp; } in
    let info = setGVal finalVar info finalVal in

    setGInfo sumKey (Some info);
    
    let diffOffs = calcDiffOffsets finalVal oldVal in
    let rd = getReadersFor diffOffs info.gReaders in
    
    logStatusF "New global @ %s: %s: %s\n" 
      (string_of_loc !currentLoc) (string_of_var finalVar) 
      (string_of_type (findTyp info.gType));
    printVal info.gVal (Some (findTyp info.gType));
    
    { gside with gNotify = combineNotifyGR gside.gNotify rd; }
  in

  let rec checkAndUpdate gside var rhs rhsTyp =
    let rhs = valUseRepsTrusted rhs in
    let var2, off2 = globRep var in
    let sumKey, info = getGInfoWithDefaults var2 in

    let diff2 = int_of_off off2 in
    let rhs2 = alignRecordOff diff2 rhs in
(*
    if off2 <> 0 then begin
      logStatusF "GOTCHA: updateBinding off: %s = (new) %s + %d\n"
        (string_of_var var) (string_of_var var2) off2;
      printVal rhs (Some (findTyp info.gType));
      printVal rhs2 (Some (findTyp info.gType));
    end;
*)

    let oldVal = info.gVal in
    let oldVal = valUseRepsTrusted oldVal in
    let comboVal = combineVals valBaseNone oldVal rhs2 in
    let newT = 
      if isNoOffset diff2
      then combineTypes info.gType rhsTyp
      else info.gType in
    let newNotify = unifyGlobalTargets comboVal gside.gNotify in
    let gside = { gside with gNotify = newNotify; } in

    let comboVal = valUseRepsTrusted comboVal in
    let oldVal = valUseRepsTrusted oldVal in

    (* filter out of bounds for oldVal too? *)
    let oldVal = filterGOutOfBounds var2 oldVal in
    let comboVal = filterGOutOfBounds var2 comboVal in

    if eqVals oldVal comboVal && newT = info.gType 
    then (gside)
    else begin
      (* Unification may have changed the representative... check it again! *)
      let var3, off3 = globRep var2 in
      assert (isNoOffsetSum off3);
      if eqFVar var2 var3 then doUpdate gside var2 oldVal comboVal newT
      else checkAndUpdate gside var3 comboVal newT
    end
  in
  checkAndUpdate gside var rhs rhsTyp


(************************************************************)
      
let printGlobalStats () =
  logStatus "Printing init global bindings:\n";
  let numBindings = globalSum#fold
    (fun key info cur ->
       match info with
         None -> cur
       | Some info ->
           logStatusF "GV: %s\n" (Summary_keys.string_of_sumKey key); 
           printVal info.gVal (Some (findTyp info.gType));
           cur + 1
    ) 0 in
  logStatusF "Have initial values for %d globals\n" numBindings  
    
