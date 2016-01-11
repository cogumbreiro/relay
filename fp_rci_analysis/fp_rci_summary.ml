
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

(** Function summaries that track (input -> output) effects in FP analysis *)

open Cil
open Pretty
open Type_utils
open Fp_rci_types
open Fp_rci_unify_structs
open Fp_rci_flow_insens 
open Fp_rci_lattice_ops
open Fp_rci_store
open Fp_rci_focus
open Fp_rci_hashcons
open Fp_rci_utils
open Fp_rci_unify_globals
open Cildump
open Logging


(************************************************************)

type sumval = {
  fpIn : fpState;   (* Just the initial bindings for what's equal + attribs *)
  fpSide : fpSideCond;
  fpOut : fpState;
}


(* Generic var just for doing lookups and removes *)
let retVar = FRet (addTyp (TVoid []))

exception NoRetValue

let getRetValue state = 
  try getBinding retVar state
  with Not_found ->
    logError "getRetValue can't find ret value";
    raise NoRetValue

let assignRetValue state retVar v = 
  addBinding state retVar v

let removeRetVal state =
  { state with bindings = VarMap.remove retVar state.bindings; }

let bottomSummary = 
  { fpIn = bottomState; fpSide = emptySide (); fpOut = bottomState; }

let isBottomSummary v = 
  isBottomState v.fpOut

let topSummary = { fpIn = topState; fpSide = emptySide (); fpOut = topState; }
 
let isTopSummary v =
  isTopState v.fpOut

let makeUnknownSummary retTyp =
  (* TODO use the gen_default_val thing instead? But that could reference
     existing vars or generate new ones? ...  *)
  let retVar = FRet (addTyp retTyp) in
  let stWithRetNfp = assignRetValue emptyState retVar (FNFP lookupNfp) in
  { fpIn = stWithRetNfp; fpSide = emptySide (); fpOut = stWithRetNfp; }

let isWildcardSum sum =
  VarMap.mem retVar sum.fpIn.bindings



(************ Track all known summaries for each function ***************)

module FPSum = struct

  type t = (sumval * int) list

  type simpleSum = t

  let simplify sums = 
    List.map 
      (fun (v, id) -> 
         ({ fpIn = hc_state v.fpIn; 
            fpSide = v.fpSide;
            fpOut = hc_state v.fpOut; }, id) ) sums

  let desimplify sums = simplify sums
    
  let initVal = []

  (* Hmm... may want the real type... *)
  let unknownSummary = [ (makeUnknownSummary (TVoid []), 0) ]

end

module FPS = Cache_sum.Make (FPSum)
  
let cacheSize = 384

let sum = new FPS.data cacheSize (Backed_summary.makeSumType "fpsum")
let () = Backed_summary.registerType sum

type sumdb = FPS.data
class data = FPS.data


(************************************************************)

let topSumID = "top"

let getOldSummary (sumdb:sumdb) (fkey, id) =
  if id = topSumID then topSummary
  else 
    let sumKey = Summary_keys.inputFreeSumKey fkey in
    let idInt = int_of_string id in
    let sumList = sumdb#find sumKey in
    try fst (List.find (fun (v, id2) -> id2 = idInt) sumList)
    with Not_found ->
      failwith ("getOldSummary can't find: " ^ 
                  Summary_keys.string_of_sumKey (fkey, id))



(* Hmm... should an update of one summary cause us to normalize
   the set of initialFPs across all the others? 
   But that means we need to notify all the callers if this
   normalization changes things.
   Could also cause some serious shuffling of caller-notify-sets.
   (One caller may choose a different callee context) 

   Instead of normalizing, could also have some measure of "best-match",
   but after introducing a new context would still need all callers to
   find its best-match again...
*)
let replaceSummary (sumdb:sumdb) (fkey, id) sum =
  if id = topSumID then failwith "replaceSummary on top sum"
  else 
    let sumKey = Summary_keys.inputFreeSumKey fkey in
    let idInt = int_of_string id in
    let sumList = sumdb#find sumKey in
    try
      let (_, newSumList) = List_utils.listFindReplace 
        (fun (s, id) (x, idInt) -> id = idInt)
        (fun o n -> n) (sum, idInt) sumList in
      sumdb#addReplace sumKey newSumList
    with Not_found ->
      failwith ("replaceSummary can't find: " ^ 
                  Summary_keys.string_of_sumKey (fkey, id))


(*********************************************************
 * Test / Debug code
 *********************************************************)

let printSummary sumdb funID =
  let sum = getOldSummary sumdb funID in
  printState sum.fpOut

let dotSummary sum =
  logStatus "Input portion:";
  failwith "TODO dotSummary"

let makeTopSumKey fkey =
  (fkey, topSumID)

let makeSumKey fkey id =
  (fkey, string_of_int id)

(************************************************************)

let combineSummaries curFunc s1 s2 =
  if isBottomSummary s1 then s2 
  else if isBottomSummary s2 then s1 
  else if isTopSummary s1 then s1
  else if isTopSummary s2 then s2
  else begin
    if not (eqStModRep s1.fpIn s2.fpIn) then begin
      logErrorF "Diff in summary inputs?\n";
      printState (diffState s1.fpIn s2.fpIn);
    end;
    (* Hmm.. inputs should be the same *)
    { fpIn = s1.fpIn;
      fpSide = combineSideInfo s1.fpSide s2.fpSide ;
      fpOut = combineStates curFunc s1.fpOut s2.fpOut ; }
  end

let eqSummaries s1 s2 =
    eqStModRep s1.fpOut s2.fpOut &&
      eqStModRep s1.fpIn s2.fpIn &&
(*
      eqSideCond s1.fpSide s2.fpSide
*)
      eqSideModAttrs s1.fpSide s2.fpSide

(************************************************************)

module CMemo = Hashtbl.Make (
  struct 
    type t = structInfo * fvar * offset_val option
    let equal (sc1, v1, o1) (sc2, v2, o2) =
      sc1 = sc2 && o1 = o2 && eqFVar v1 v2
    let hash (sc, v, o) =
      (Hashtbl.hash sc lsl 22) lxor (Hashtbl.hash o) lxor (hashVar v)
  end )

type contextMatchInfo =
    { cState : fpState;
      cEqIns : fvar VarH.t;
      cEqGlobals : FLocSet.t VarH.t;
      cFpAccs : funPtrRequire; 
      cFunc : fundec;                (* caller *)
      cCalleeName : string;
      cFormals : varinfo list;          (* callee formals *)
      cActuals : exp list; 
      cGlobReaders : Fp_rci_globals.greader option;
      cDelayFpReqs : collectedFPReq;
      cLookHook : lookupHook;
      cMemo : fvalue CMemo.t;

      t_sumAtts : varAttrs; (* Temporary type attribs from summary *)
    }

let makeMatchContext curFunc readerOpt curState calleeName formals actuals =
  { cState = curState;
    cEqIns = ShortAPU.makeMappings 10;
    cEqGlobals = VarH.create 10;
    cFpAccs = VarMap.empty; 
    cFunc = curFunc;
    cCalleeName = calleeName;
    cFormals = formals;
    cActuals = actuals;
    cGlobReaders = readerOpt;
    cDelayFpReqs = emptyDelayedFP;
    cLookHook = nilLookupHook;
    cMemo = CMemo.create 7;

    t_sumAtts = VarMap.empty;
  }


exception CantAddOffsetRead

(** Attach an offset to the given expression *)
let addOffRead exp offOpt =
  match offOpt with
    None -> exp
  | Some off ->
      let rec findLvalAndAdd exp =
        match exp with 
          Lval (x, oldOff) ->
            let baseT = Cil_lvals.typeOfUnsafe exp in
            if isNoOffset off && not (isStructType baseT) then exp
            else 
              (try
                 logStatusF "Had to add off: %s %s\n" 
                   (string_of_exp exp) (string_of_offsetval off);
                 let cilOff = offset2CilOffScalar baseT off in
                 Lval (x, addOffset cilOff oldOff)
               with Offset.UnknownOffset ->
                 logErrorF "Can't add off @ %s\n" (string_of_loc !currentLoc);
                 exp)
        | CastE (t, e2) -> CastE (t, findLvalAndAdd e2)
        | BinOp _ | UnOp _ | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _
        | AlignOf _ | AlignOfE _ | AddrOf _ | StartOf _ ->
            if not (isNoOffset off) then
              logErrorF "Can't add off @ %s\n" (string_of_loc !currentLoc);
            exp
      in
      findLvalAndAdd exp

(** Specialized version of the typical lookupVal given that this is only used 
    for eval'ing input vars... *)
let lookupForEvalDeref srcType (isStruct, width) mContext var o =
  if not (isStruct) then begin
    let bits = int_of_off o in
    (* ctype guesses: 
       (1) srcType is from summary
       (2) locs are in terms of caller... so we could also try to
       look them up from caller *)
    let t = typeOfLocScalar mContext.cState.vAttrs (var, o) in
    let t = findTyp (combineTypes (addTyp srcType) (addTyp t)) in 

    let (isStruct, width) = getStructInfo t in
    let t = if isStruct then begin
      logErrorF "Became struct? %s vs %s for %s [%s]\n"
        (string_of_type srcType) (string_of_type t) 
        (string_of_var var) (string_of_offsetval bits);
      (* Trans_alloc.unknownMallocType *) (* This should force it to read all *)
      srcType (* just try the srcType *) 
    end else t in
    
    let readHook = 
      makeReaderOpt mContext.cGlobReaders bits (isStruct, width) in
    let v, state = 
      Stat.time "lookV"
        (lookupVal nilLookupHook mContext.cFunc t mContext.cState var bits) 
        readHook in
    ( v, { mContext with cState = state; } )
  end else begin
    (* Callee may have read BEYOND the bounds of local structure around 
       (var, o) so we should just read ALL of var, starting at offset 0,
       then shift the offsets of the value from the caller's perspective 
       to the callee's perspective.
       Reading ALL of the struct is okay since we ONLY use this when
       matching FPs, and for that project out the unneccesary offsets *)
    let bits = int_of_off o in
    let t = 
      if isNoOffset bits then
        findTyp (combineTypes (addTyp srcType) 
                   (typeIDOfFvar mContext.cState.vAttrs var))
      else (typeOfFVar mContext.cState.vAttrs var) 
    in
    let (isStruct, _) as sinfo = getStructInfo t in
    let t = if not isStruct then begin
      logErrorF "Lost struct? %s vs %s for %s [%s]\n"
        (string_of_type srcType) (string_of_type t) 
        (string_of_var var) (string_of_offsetval bits);
      (* Trans_alloc.unknownMallocType *) (* This should force it to read all *)
      srcType (* just try the srcType *) 
    end else t 
    in
    let readHook = makeReaderOpt mContext.cGlobReaders noOff sinfo in
    let v, state =
      Stat.time "lookV"
        (lookupVal nilLookupHook mContext.cFunc t mContext.cState var noOff) 
        readHook in
    let v = alignRecordOff (invertOff bits) v in
    ( v, { mContext with cState = state; } )
  end
    
    
(** Note that these values should be in terms of the caller already *)
let rec evalInputDeref srcType sinfo tailVal outerOffOpt mContext =
  let outerOff = match outerOffOpt with Some off -> off | None -> noOff in
  match tailVal with
    Refs locs -> 
      mContext.cLookHook (ADeref locs);
      mContext.cLookHook (AReadField outerOff);
      let locs = stripSpecial locs in (* Hmm... *)
      let locs = flocsMap 
        (fun (v, o) -> (v, concatOffsetVar v (outerOff, false) o)) locs in
      (match FLocSet.fold 
         (fun (var, o) (curVal, mContext) ->
            try
              let res, mContext = 
                lookupForEvalDeref srcType sinfo mContext var o in
              match curVal with
                None -> Some res, mContext
              | Some (oldVal) -> 
                  Some (combineVals valBaseNone oldVal res), mContext
            with NullException -> (curVal, mContext)
         ) locs (None, mContext)
       with
         None, mContext -> FInt (Int64.zero), mContext
       | Some x, mContext -> x, mContext)

  | FInt _ -> FInt (Int64.zero), mContext
  | FNFP _ -> tailVal, mContext
  | FpRef _ ->
      logErrorF "evalInputDeref given fp %s\n" (string_of_val tailVal);
      FInt (Int64.zero), mContext
  | Records _ | NCRecord _ | FIRecs _ ->
      let v = demoteFromRecVal tailVal in
      evalInputDeref srcType sinfo v outerOffOpt mContext


let evalInputVarOff (findC, addC) structInfo mContext var off =
  let rec recurse structInfo var off mContext =
    match findC (structInfo, var, off) mContext with
      Some v -> v, mContext
    | None ->
        let v, mContext = 
          match var with
            FInput (ap, len) ->
              (match ap with
                 [AccVar vid] ->
                   (* Convert to actual from formal *)
                   (try
                      let formalIndex = List_utils.indexOf 
                        (fun v -> v.vid = vid) mContext.cFormals in
                      let act = List.nth mContext.cActuals formalIndex in
                      let actWithOff = addOffRead act off in
                      let eMisc = { (emptyMisc mContext.cFunc) with
                                      lookupHook = mContext.cLookHook; 
                                      readerOpt = mContext.cGlobReaders;
                                  } in
                      let v, state = eval mContext.cState actWithOff eMisc in
                      ( v, { mContext with cState = state; } )
                    with
                      Not_found ->
                        failOnVar "evalInputVar for non-formal " var
                          
                    | NullException ->
                        logErrorF "evalInputVar NULL %s @ %s\n"
                          (string_of_var var) (string_of_loc !currentLoc);
                        FInt (Int64.zero), mContext
                   )
                     
               | AccField newOff :: t ->
                   let fullOff, t = compressApOffs newOff t in
                   let len = List.length t in
                   assert (off = None); (* same struct info? *)
                   let v, mContext = 
                     recurse structInfo (makeInVar (t, len)) (Some fullOff) mContext in
                   v, mContext
                     
               | AccDeref :: t ->
                   let tailVar = makeInVar (t, len - 1) in
                   let tailVal, mContext = 
                     recurse scalarSI tailVar None mContext in
                   let o = match off with 
                       None -> noOffSum 
                     | Some x -> (x, false) in

                   let sumType = 
                     typeOfLocChoice mContext.t_sumAtts structInfo (var, o) in
                   
                   let v, mContext = 
                     Stat.time "evalDeref" 
                       (evalInputDeref sumType structInfo tailVal off)
                       mContext in
                   ( v, mContext )
                     
               | _ -> failOnVar "evalInputVar malformed ap " var )
                
          | _ -> failwith "evalInputVar: non-input var"
        in
        addC (structInfo, var, off) v mContext;
        v, mContext
  in
  recurse structInfo var off mContext

  
let noCacheFind _ mContext = None

let noCacheAdd _ _ mContext = ()

let noCacheOps = (noCacheFind, noCacheAdd)

let evalInputVar structInfo mContext var =
  evalInputVarOff noCacheOps structInfo mContext var None

let cacheFind (structInfo, var, offOpt) mContext =
  try Some (CMemo.find mContext.cMemo (structInfo, var, offOpt))
  with Not_found -> None

let cacheAdd (structInfo, var, offOpt) v mContext =
  CMemo.replace mContext.cMemo (structInfo, var, offOpt) v

let cacheOps = (cacheFind, cacheAdd)

let evalInputVarCached structInfo mContext var =
  evalInputVarOff cacheOps structInfo mContext var None
    

exception SubstNonLval

let rec ptrSetOfVal callerVal inVar =
  match callerVal with
    Refs callerLocs -> callerLocs
  | FInt _ | FNFP _ -> FLocSet.empty
  | FpRef var -> FLocSet.singleton (funToLoc var) 
  | Records _ | NCRecord _ | FIRecs _ -> 
      let v = demoteFromRecVal callerVal in
      ptrSetOfVal v inVar

let substVarBase doEval sum inVar mContext =
  match inVar with
    FVar _ | FHeap _ | FRet _ -> 
      (FLocSet.singleton (inVar, noOffSum), mContext)
        
  | FInput ap ->
      if extendsFormal mContext.cFormals inVar then
        let reps = UnifLocs.getEqMembers sum.fpSide.aliasedIn inVar in
        let locs, mContext = FLocSet.fold
          (fun (var, off) (locs, mContext) ->
             if not (isNoOffsetSum off) then
               failwith "TODO: substVar rep. w/ offset"
             ;
             try
               let callerVal, mContext = 
                 doEval scalarSI mContext (stripADeref var) in
               let locs2 = ptrSetOfVal callerVal var in
               combinePtrSets locs locs2, mContext
             with CantStripDeref -> 
               (*               raise SubstNonLval *)
               locs, mContext
          ) reps (FLocSet.empty, mContext) in
        locs, mContext
      else FLocSet.singleton (inVar, noOffSum), mContext


let substVar sum inVar mContext =
  substVarBase evalInputVarCached sum inVar mContext

let substVarUncached sum inVar mContext =
  substVarBase evalInputVar sum inVar mContext
  

(** Indicate needed case-splits on FPs if we encounter any during context
    matching or summary application *)
exception SubstFPImprecise of (accessSeq * FLocSet.t * contextMatchInfo)
  
let setLookupHook mContext hook =
  { mContext with cLookHook = hook; }

let removeLookupHook mContext =
  setLookupHook mContext nilLookupHook
    
(******* Filter out summary values that are essential unchanged *******)

let getBaseOfSumVar sumSt sumVar : accElem list option =
  match sumVar with
    FInput (ap, _) ->
      if isGlobalDebug "filterNop" false sumSt.vAttrs sumVar 
      then None else Some ap
  | FVar _ | FHeap _ | FRet _ -> None


let extendsBaseAP baseAP extAP off =
  let rec loop baseRest extRest =
    match baseRest, extRest with
      [], erest ->
        (match erest with
           [AccDeref] -> isNoOffset off
         | [AccField o2; AccDeref] -> off = o2
         | _ -> false)      
    | _, [] -> false
    | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then loop t1 t2
        else false
  in
  loop (List.rev baseAP) (List.rev extAP)


let pruneUnchangedOffmap offs offmap curM =
  OffsetMap.fold 
    (fun o _ (curMap, curMax) ->
       if OffsetSet.mem o offs 
       then (OffsetMap.remove o curMap, curM)
       else (curMap, max curMax o)
    ) offmap (offmap, curM)

let pruneUnchangedRec offs (fm, _) =
  pruneUnchangedOffmap offs fm noOff

let pruneUnchangedRecs unchOffs recs =
  match unchOffs with
    None -> recs
  | Some offs -> List.map (pruneUnchangedRec offs) recs

let rec extendsBaseVarVal sumSt baseAP off v =
  (* Check if the target location is an extension of baseAP from off *)
  let checkLoc locAP =
    if extendsBaseAP baseAP locAP off then None
    else Some v
  in

  match v with
    FInt _ | FNFP _ -> Some v
  | FpRef fpVar -> 
      (match getBaseOfSumVar sumSt fpVar with
         None -> Some v
       | Some fpAP -> checkLoc fpAP)
  | Refs locs ->
      (* Must be the same as original if (locs - {NULL} = { v }) where
         v is some var extending baseAP. If # non-null targets <> 1 
         it must be different *)
      let stripped = FLocSet.remove nullVar#getLoc locs in
      if FLocSet.is_singleton stripped then
        let (var, o) = FLocSet.choose stripped in
        if isNoOffsetSum o then
          (match getBaseOfSumVar sumSt var with
             None -> Some v
           | Some ap -> checkLoc ap)
        else Some v
      else Some v
        
  | Records recs ->
      (match recs with
         [r] ->  
           (* If no offsets have changed values, return None.
              If only a subset of offsets are changed, return that subset *)
           let chOffs, unchOffs = offValUnchanged sumSt baseAP off 
             (OffsetSet.empty, None) r in
           if OffsetSet.is_empty chOffs then None
           else Some (makeNormalizedRec (pruneUnchangedRecs unchOffs [r]))
       | _ ->
           (* This is more complicated. It's impossible for all offsets
              to be unchanged (otherwise there would only be 1 record).
              If only a subset of offsets are unchanged, and that subset
              is common same across all recs, then take out that subset *)
           let chOffs, unchOffs = List.fold_left 
             (offValUnchanged sumSt baseAP off) (OffsetSet.empty, None) recs in
           if OffsetSet.is_empty chOffs then begin
             let baseVar = makeInVar (baseAP, List.length baseAP) in
             logErrorF "why are all offsets unchanged (yet |R| > 1) %s?"
               (string_of_pointer (baseVar, (off, false)));
             printVal v None;
           end;
           Some (makeNormalizedRec (pruneUnchangedRecs unchOffs recs))
      )

  | NCRecord (fm, m) ->
      let chOffs, unchOffs = offValUnchanged sumSt baseAP off
        (OffsetSet.empty, None) (fm, m) in
      if OffsetSet.is_empty chOffs then None
      else 
        (match unchOffs with 
           None -> Some (NCRecord (fm, m))
         | Some offs -> Some (NCRecord (pruneUnchangedRec offs (fm, m))))

  | FIRecs _ -> Some v


and offValUnchanged sumSt baseAP baseOff (chOffs, unchOffs) (fm, _) =
  let collectChangedUnchanged offmap (ch, unch) =
    OffsetMap.fold 
      (fun o v (ch, unch) ->
         match extendsBaseVarVal sumSt baseAP (concatOffset baseOff o) v with
           None -> (ch, OffsetSet.add o unch)
         | Some _ -> (OffsetSet.add o ch, unch)
      ) offmap (ch, unch)
  in
  let finalCh, tUnchOff = 
    collectChangedUnchanged fm (chOffs, OffsetSet.empty) in
  let finalUnch = match unchOffs with 
      None -> Some (tUnchOff) 
    | Some x -> Some (OffsetSet.inter x tUnchOff) in
  (finalCh, finalUnch)


(** Also filter based on mod summary *)
let filterFM modOffs fm =
  OffsetMap.fold
    (fun o v (cur, m) ->
       if OffsetSet.mem o modOffs then (OffsetMap.add o v cur, max m o)
       else (cur, m)
    ) fm (OffsetMap.empty, noOff)

(* Assume input-based sumVars are never merged with other input-based sumVars.
   Otherwise, the modIn sets need to be merged wherever the vars *)
let filterUnchangedVal modIn sumVar sumVal =
  try
    let modOffs = VarMap.find sumVar modIn in
    match sumVal with
      FInt _ | FNFP _ | FpRef _ | Refs _ ->
        if OffsetSet.is_singletonX noOff modOffs then (Some sumVal)
        else begin
          logErrorF "sumVal is scalar, but mod not at 0: %s = %s\n"
            (string_of_var sumVar) (string_of_val sumVal);
          logErrorF "%s\n" (string_of_offsetset modOffs);
          None
        end
          
    | FIRecs _  -> Some sumVal
    | NCRecord (fm, m) ->
        let newfm, m = filterFM modOffs fm in 
        if (OffsetMap.is_empty newfm) then None
        else Some (NCRecord (newfm, m))
          
    | Records recs ->
        (* Detect when all recs end up empty *)
        let allEmpty = ref true in
        let newrecs = List.map 
          (fun (fm, _) ->
             let newfm, m = filterFM modOffs fm in 
             if not (OffsetMap.is_empty newfm) then
               allEmpty := false;
             (newfm, m)
          ) recs in
        if !allEmpty then None
        else Some (Records newrecs)

  with Not_found -> None


let filterUnchangedVarVal sum sumVar sumVal filteredOut =
  (match getBaseOfSumVar sum.fpOut sumVar with
     None -> filteredOut (* Don't touch the non-input guys *)
   | Some ap ->
       (* Finally check for mods *)
       (match filterUnchangedVal sum.fpSide.modIn sumVar sumVal with
          None -> VarMap.remove sumVar filteredOut
        | Some sumVal ->
            (match extendsBaseVarVal sum.fpOut ap noOff sumVal with
               None -> VarMap.remove sumVar filteredOut
             | Some sumVal ->
                 VarMap.add sumVar sumVal filteredOut)
       )
  )
    
let filterUnchanged sum =
  let newBind = VarMap.fold (filterUnchangedVarVal sum) 
    sum.fpOut.bindings sum.fpOut.bindings in
  (* Also remove unnecessary attribs (for targets that no longer exist).
     DO keep the attribs for input locations that were read *)
  let mentioned = VarMap.fold 
    (fun sumVar sumVal cur ->
       let cur = VarSet.add sumVar cur in
       foldTargetsVal (fun (v, o) cur -> VarSet.add v cur) sumVal cur) 
    newBind VarSet.empty in
  let newAtts = VarMap.fold
    (fun sumVar att cur ->
       if (not (VarSet.mem sumVar mentioned) 
           && att.vKind <> HGlobal 
           && not (VarMap.mem sumVar sum.fpOut.bindings))
       then VarMap.remove sumVar cur
       else cur
    ) sum.fpOut.vAttrs sum.fpOut.vAttrs in
  { sum with fpOut = { bindings = newBind; vAttrs = newAtts; }; }


(****** Do substitutitions and such *********)

let rec substVal preciseFP sum calleeVal mContext : (fvalue * contextMatchInfo) =
  match calleeVal with
    FInt _ | FNFP _ -> (calleeVal, mContext)
  | FpRef var ->
      let accessPath = ref [] in
      let mContext = setLookupHook mContext (captureAccessPath accessPath) in
      let varLocs, mContext = substVarUncached sum var mContext in
      let numLocs = FLocSet.cardinal varLocs in
      let mContext = removeLookupHook mContext in
      if numLocs = 1 then
        let var, o = FLocSet.choose varLocs in
        (FpRef var, mContext)
      else if numLocs = 0 then
        FInt (Int64.zero), mContext
      else if preciseFP then
        raise (SubstFPImprecise (!accessPath, varLocs, mContext))
      else (* Actually, don't need the accessPath and substUncached stuff *)
        Refs varLocs, mContext
          
  | Refs locs -> 
      substRefs sum locs mContext
  | FIRecs fir ->
      let newFIR, newContext = substVal preciseFP sum fir mContext in
      (FIRecs newFIR, newContext)

  | Records recs ->
      let newRecs, mContext = substRecords true sum mContext recs in
      makeNormalizedRec newRecs, mContext

  | NCRecord (fm, m) ->
      let newFM, newContext = substRecPart false sum fm mContext in
      NCRecord (newFM, m), newContext

and substRecords preciseFP sum mContext recs =
  (* Don't bother focus'in if the given summary record was already imprecise *)
  let preciseFP = preciseFP && (List.length recs <= 1) in
  try
    let curContext = ref mContext in
    let newRecs = List.map
      (fun (fm, m) -> 
         let newFM, newContext = substRecPart preciseFP sum fm !curContext in
         curContext := newContext;
         (newFM, m)
      ) recs in
    (newRecs, !curContext)
  with SubstFPImprecise (revAccs, targs, cont) ->
    (* Temporarily split state here to do the substitution *)
    logStatusF "Summary must focus substituting:\n";
    printVal (Records recs) None;
    checkDiff "substFPImp" mContext.cState cont.cState;
    let recs, cont = FLocSet.fold
      (fun (v, o) (curRecs, curCont) ->
         let startC = 
           { cont with
               cState = focus revAccs (v, o) cont.cState; 
               cMemo = CMemo.create 7;
           } in
         if isBottomState startC.cState then
           (curRecs, curCont)
         else begin
           let preciseFP = preciseFP &&
             if eqStores mContext.cState.bindings startC.cState.bindings 
             then begin
               logError "focus didn't change?" ;
               false
             end else preciseFP
           in
           let newRecs, newCont = substRecords preciseFP sum startC recs in
           (* assume only the state changes in context... 
              (maybe from evaluating more input parameters) *)
           let newCont = 
             { newCont with 
                 cState = mergeNewInBindings curCont.cState newCont.cState; 
                 cMemo = CMemo.create 7; } in
           checkDiff "substFPImp 2" curCont.cState newCont.cState;
           (List.rev_append newRecs curRecs, newCont)
         end
      ) targs ([], cont) in
    
    (* The context returned by SubstFPImprecise and used above may 
       have been focused... de-focus it before returning *)
    let newContext = 
      { mContext with 
          cState = mergeNewInBindings mContext.cState cont.cState;
          cMemo = CMemo.create 7;
      } in
    checkDiff "substFPImp 3" mContext.cState newContext.cState;
    recs, newContext



and substRecPart preciseFP sum offmap mContext =
  let curContext = ref mContext in
  let newPart = 
    OffsetMap.mapCh 
      (fun v -> 
         let newV, newContext = substVal preciseFP sum v !curContext in
         curContext := newContext;
         newV) offmap in
  newPart, !curContext  


and substRefs sum locs mContext =
  let newContext, newLocs = 
    FLocSet.fold 
      (fun (v, o) (curContext, locs) ->
         let varLocs, curContext = substVar sum v curContext in
         let newLocs = FLocSet.fold 
           (fun (v2, o2) locs ->
              addToPtrSet (v2, concatOffsetVar v2 o o2) locs) varLocs locs in
         (curContext, newLocs)
      ) locs (mContext, FLocSet.empty) in
  Refs newLocs, newContext

(************************************************************)

(* Helper functions for matching contexts *)
          
(** Get all input locations that are relevant (to check if they alias) *)
let collectInputVars sum =
  let fromSt = 
    if isBottomState sum.fpOut then []
    else VarMap.fold 
      (fun var _ cur ->
         match var with 
           FInput _ -> 
             if not (isGlobalDebug "collectInVars" false sum.fpOut.vAttrs var) 
             then var :: cur else cur
         | _ -> cur ) 
      sum.fpOut.bindings [] in
  VarMap.fold
    (fun var _ cur ->
       match var with 
         FInput (p, _) -> if hasAnyDeref p then (var :: cur) else cur
       | _ -> cur
    ) sum.fpSide.initialFP fromSt
    

let atLeastOneMod sum var1 var2 = 
  VarMap.mem var1 sum.fpSide.modIn || VarMap.mem var2 sum.fpSide.modIn
(*  true (* faster to aggressively merge or not? *) *)
    

let checkInter var1 var2 common = 
  match FLocSet.fold (fun (v, o) (curNoOff, curOff) ->
                        match curNoOff, curOff with
                          Some _, Some _ -> (curNoOff, curOff)
                        | _, _ -> 
                            if isNoOffsetSum o then (Some (v, o), curOff)
                            else (curNoOff, Some (v, o))
                     ) common (None, None) with
    None, None -> false
  | Some _, None -> true
  | Some _, Some _ ->
      (* TODO: Hmm, what to do w/ non-triv offsets? *)
(*      logErrorF "mayAliasIn non-triv off: (%s, %s) has locs %s @ %s\n"
        (string_of_var var1) (string_of_var var2)
        (string_of_val (Refs common))
        (string_of_loc !currentLoc); *)
      true
        
  | None, Some _ ->
      (* TODO: Hmm, what to do w/ non-triv offsets ignore for now? *)
(*
      logErrorF "mayAliasIn non-triv off: (%s, %s) has locs %s @ %s\n"
        (string_of_var var1) (string_of_var var2)
        (string_of_val (Refs common))
        (string_of_loc !currentLoc); *)
      true

        
let rec mayAliasInputVal var1 var2 v1 v2 =
  match v1, v2 with
    Refs locs1, Refs locs2 ->
      let common = intersectPtrSets locs1 locs2 in
      let common = stripSpecial common in
      checkInter var1 var2 common

  (* Don't care if one is null... *)
  | FInt _, Refs _
  | Refs _, FInt _
  | FInt _, FInt _ -> false

  | FNFP _, Refs _
  | Refs _, FNFP _ 
  | FNFP _, FInt _
  | FInt _, FNFP _ -> false (* not if we get the partitioning 
                               between FP and nonFP right... *)
  | FNFP _, FNFP _ -> false (* if we are correct about it being NonFP,
                               we wouldn't care that they alias *)

  | _, FpRef _
  | FpRef _, _ -> false

  | Records _, _
  | NCRecord _, _ 
  | FIRecs _, _ ->
      let v1 = demoteFromRecVal v1 in
      mayAliasInputVal var1 var2 v1 v2

  | _, Records _
  | _, NCRecord _
  | _, FIRecs _ -> 
      let v2 = demoteFromRecVal v2 in
      mayAliasInputVal var1 var2 v1 v2


let rec mayAliasInputVarGlob inVal globVar =
  match inVal with
    Refs locs -> 
      let locs = FLocSet.filter 
        (fun (v, o) -> 
           let eq = eqFVar v globVar in
           if eq && not (isNoOffsetSum o) then begin
             logErrorF "mayAliasInputVarGlob nontriv offset %s\n"
               (string_of_pointer (v, o));
             eq
           end else eq
        ) locs in
      if not (FLocSet.is_empty locs)
      then Some locs else None
  | FInt _ 
  | FpRef _ -> None (* Couldn't have written to that function *)
  | FNFP _ -> None (* assume that if it does alias, we don't care *)
  | Records _ | NCRecord _ | FIRecs _ -> 
      let v = demoteFromRecVal inVal in
      mayAliasInputVarGlob v globVar

(************************************************************)



(** If the caller has unknown FPs also, convert those to EXT and request them *)
let convertFPs mContext callerVal = 
  (* Convert relevant FPs and drop any irrelevant non-function targets... *)
  let newval, moreDelay = 
    replaceInputFPsVal true mContext.cState.vAttrs callerVal 
      mContext.cDelayFpReqs in
  ({ mContext with cDelayFpReqs = moreDelay; }, newval)


let possibleBaseLocs atts srcLocs =
  FLocSet.filter 
    (fun (var, o) ->
       match var with
         FVar _ | FHeap _ | FRet _ -> false
       | FInput _ -> not (isGlobalDebug "possibleBL" false atts var)
    ) srcLocs


let captureDelays moreOff (srcVar, srcOff) cont =
  let baseVar = reattachADeref 
    (reattachFieldAcc srcVar (concatOffset moreOff (int_of_off srcOff))) in
(*  logStatusF "wanting to delay: %s\n" (string_of_var baseVar); *)
  { cont with cDelayFpReqs = 
      delayFPRequest baseVar cont.cDelayFpReqs; }


let fillUnknownOffOfRec srcLocs r2Offs mContext (fm, m) =
  OffsetSet.fold 
    (fun o ((newfm, newm), newcont) ->
       if OffsetMap.mem o newfm then ((newfm, newm), newcont)
       else
         let newcont = 
           FLocSet.fold (captureDelays o) srcLocs newcont in
         ((OffsetMap.add o initFP newfm, max newm o), newcont) 
    ) r2Offs ((fm, m), mContext)

let fillUnknownOffs srcLocs r2Offs mContext recs1 =
  let cont = ref mContext in
  let recs = List.map 
    (fun (fm, m) ->
       let (newfm, newm), newcont = 
         fillUnknownOffOfRec srcLocs r2Offs !cont (fm, m) in
       cont := newcont;
       (newfm, newm)   
    ) recs1 in
  recs, !cont

(** Debug duplicate contexts, etc... *)
let inspectLines = ref [151152]
let addInspectLine line = 
  if List.mem line !inspectLines then ()
  else inspectLines := line :: !inspectLines

let shouldInspect () =
  List.mem !currentLoc.line !inspectLines

let checkCompatRecsFP sum srcVar calleeVal
    cont coll proj fill reconstitute r1 r2 =
  (* Project recs1 down to offsets available in recs2
     and fill in unknown offsets so that they have the same offsets *)
  let r2Offs = coll r2 in
  let r1 = proj r1 r2Offs in
  
  let srcLocs, cont = 
    try substVar sum srcVar cont
    with SubstNonLval ->
      (* may not get the location here, but will catch it when
         comparing the values themselves ... *)
      logErrorF "matchFP subst failure: %s\n" (string_of_var srcVar);
      FLocSet.empty, cont
      in
  let srcLocs = possibleBaseLocs cont.cState.vAttrs srcLocs in
  let r1, cont = fill srcLocs r2Offs cont r1 in
  
  (* Finally, convert input-based FPs into something comparable *)
  let cont, callerVal = convertFPs cont (reconstitute r1) in
  let compat = eqVals callerVal calleeVal in
  (cont, compat, callerVal)


let collOffsRecs recs =
  (* We know all the offsets in these initialFP recs are fp offsets! *)
(*  List.fold_left collectFPOffsets OffsetSet.empty recs *)
  List.fold_left collectOffsets OffsetSet.empty recs

let collOffsRecSingle (fm, m) =
  (* collectFPOffsets OffsetSet.empty (fm, m) *)
  collectOffsets OffsetSet.empty (fm, m)

let projOnRecs recs1 r2Offs =
  let recs1 = projectOffs recs1 r2Offs in
  (* normalize wrt r2Offs, not own fp keys! *)
  let norm = List_utils.mapCross (normalizeFPFields r2Offs) recs1 in
  tryReduceRecordSet r2Offs norm

let projOnRecSingle (fm, m) r2Offs =
  projectOffsFM r2Offs fm

let makeRecords recs =
  Records recs

let makeNCRecord (fm, m) =
  NCRecord (fm, m)




(** Check if the FP-containing value in side condition is compatible
    with the caller value, and return the relevant part of the caller value *)
let rec compatibleFPAcc sum srcVar mContext callerVal calleeVal =
  match callerVal, calleeVal with
    FpRef _, FpRef _ 
  | Refs _, Refs _ ->
      let mContext, callerVal = convertFPs mContext callerVal in
      (mContext, eqVals callerVal calleeVal, callerVal)

  | FpRef var, Refs locs -> 
      let locs = stripSpecial locs in
      let mContext, callerVal = convertFPs mContext callerVal in
      (match callerVal with
         FpRef var2 ->
           (mContext, 
            FLocSet.is_singletonX (var, noOffSum) locs, 
            callerVal)
       | _ -> failwith "convertFPs changed FpRef into something else")
        
  | Refs locs, FpRef var ->
      let mContext, callerVal = convertFPs mContext callerVal in
      (match callerVal with
         Refs locs ->
           let locs = stripSpecial locs in
           if FLocSet.is_empty locs then
             (mContext, initFPVar#isVar var, initFP)
           else
             (mContext, FLocSet.is_singletonX (var, noOffSum) locs, callerVal)
       | FpRef var2 ->
           (mContext, eqVals callerVal calleeVal, callerVal)
       | _ -> failwith "convertFPs changed Refs into something else")
        
  | FInt _, _ ->
      compatibleFPAcc sum srcVar mContext initFP calleeVal
  | _, FInt _ -> 
      compatibleFPAcc sum srcVar mContext callerVal initFP

  | FNFP _, _ ->
      logErrorF "matchFP caller NFP %s @ %s\n"
        (string_of_var srcVar) (string_of_loc !currentLoc);
      compatibleFPAcc sum srcVar mContext initFP calleeVal

  | _, FNFP _ ->
      logErrorF "matchFP callee NFP @ %s\n"
        (string_of_loc !currentLoc);
      compatibleFPAcc sum srcVar mContext callerVal initFP

  | Records recs1, Records recs2 ->
      (try
         checkCompatRecsFP sum srcVar calleeVal mContext
           collOffsRecs
           projOnRecs
           fillUnknownOffs
           makeRecords
           recs1 recs2
       with HitMaxRecs ->
         let (fm1, m1) = recSetToNCRec recs1 in
         compatibleFPAcc sum srcVar mContext
           (NCRecord (fm1, m1)) calleeVal
      )


  | NCRecord (fm1, m1), NCRecord (fm2, m2) ->
      checkCompatRecsFP sum srcVar calleeVal mContext
        collOffsRecSingle
        projOnRecSingle
        fillUnknownOffOfRec
        makeNCRecord 
        (fm1, m1) (fm2, m2)

  | NCRecord (fm1, m1), Records recs2 ->
      checkCompatRecsFP sum srcVar calleeVal mContext
        collOffsRecs
        projOnRecSingle
        fillUnknownOffOfRec
        makeNCRecord 
        (fm1, m1) recs2

  | Records recs1, NCRecord (fm2, m2) ->
      (try
         checkCompatRecsFP sum srcVar calleeVal mContext
           collOffsRecSingle
           projOnRecs
           fillUnknownOffs
           makeRecords
           recs1 (fm2, m2)
       with HitMaxRecs ->
         let (fm1, m1) = recSetToNCRec recs1 in
         compatibleFPAcc sum srcVar mContext
           (NCRecord (fm1, m1)) calleeVal
      )

  | x, y when (isRecordVal x && isScalarVal y) ->
      compatibleFPAcc sum srcVar mContext 
        callerVal (Records (promoteToRecord calleeVal))
  | x, y when (isRecordVal y && isScalarVal x) ->
      compatibleFPAcc sum srcVar mContext 
        (Records (promoteToRecord callerVal)) calleeVal
        
  | _, _ ->
      failwith (Printf.sprintf "Unexpected vals matchFP: %s %s\n"
                  (string_of_val callerVal) (string_of_val calleeVal))


(************************************************************)

type matchResult =
    ContextMatch of contextMatchInfo
  | NoContextMatch of contextMatchInfo


let getMappingKind mContext srcVar =
  (* Hacky way to detect that there are others mapped to this guy? *)
  match ShortAPU.findMapping mContext.cEqIns srcVar with
    Some x -> x, HSum
  | None -> srcVar, HSingle
  
let markAsMapped mContext var =
  (match ShortAPU.findMapping mContext.cEqIns var with
     None -> ShortAPU.updateMap mContext.cEqIns var var; (* map to mark node *)
   | Some _ -> ())


let varAliasesGlob context var globVar =
  try 
    let old = VarH.find context.cEqGlobals var in
    FLocSet.mem (globVar, noOffSum) old
  with Not_found -> false


let globAlreadyMapped mContext inVar globVar sumIn =
  (* Assumes all unification constraints between inputs have been
     generated already... *)
  let parentVar = stripADeref inVar in
  let rec checkVal v o =
    match v with 
      Refs locs ->
        if isNoOffset o then
          FLocSet.exists 
            (fun (var, o) -> 
               let var2, o2 = globRep var in
               if not (isNoOffsetSum o) || not (isNoOffsetSum o2) then begin
                 logErrorF "globAlreadyMapped non-triv: %s %s\n"
                   (string_of_pointer (var, o)) 
                   (string_of_pointer (var2, o2))
                 end;
               eqFVar var2 globVar) locs
        else false
    | FInt _ | FNFP _ | FpRef _ -> false
    | Records recs -> 
        List.exists (fun (fm, _) ->
                       try 
                         let v = OffsetMap.find o fm in
                         checkVal v noOff
                       with Not_found -> false) recs
    | NCRecord (fm, m) ->
        (try 
           let v = OffsetMap.find o fm in
           checkVal v noOff
         with Not_found -> false)
    | FIRecs fir -> checkVal fir noOff
  in
  
  let checkVarOff pVar o =
    try 
      let v = getBinding pVar sumIn in
      checkVal v o
    with Not_found -> false
  in

  match parentVar with
    FInput ([AccVar vid], _) ->
      let pVar = FVar vid in
      checkVarOff pVar noOff
      
  | FInput (AccDeref :: t, _) -> 
      let pVar, _ = getMappingKind mContext parentVar in
      checkVarOff pVar noOff

  | FInput (AccField o :: t, _) -> 
      let o, t = compressApOffs o t in
      let pVar = 
        (match t with 
           [AccVar vid] -> FVar vid
         | AccDeref :: t2 -> 
             let origVar = makeInVar (t, List.length t) in
             let pVar, _ = getMappingKind mContext origVar in
             pVar
         | _ -> failOnVar "malformed invar " parentVar) in
      checkVarOff pVar o
  | _ -> failOnVar "malformed inVar" parentVar

  
(* TODO: 

   (1) Cache the id of the last sum that matched at each callsite 
   and fast-forward to that. Need to be careful that at least
   the counter examples used to set up the last sum are incorporated
   if it isn't used / if the output summary isn't ready yet.

*)


let closeEqualities mContext =
  let allVars = VarMap.fold 
    (fun v _ cur -> VarSet.add v cur) mContext.cFpAccs VarSet.empty in
  let allVars = VarH.fold 
    (fun v _ cur -> VarSet.add v cur) mContext.cEqGlobals allVars in
  let allVars = ShortAPU.foldSrcVars VarSet.add mContext.cEqIns allVars in
  ShortAPU.closeMerges mContext.cEqIns allVars

let withOldValVarMap var v oldMap =
  try
    let old = VarMap.find var oldMap in
    VarMap.add var (combineVals valBaseNone old v) oldMap
  with Not_found ->
    VarMap.add var v oldMap


(** Hack to clean up values in a callee initialFP maps. 
    Don't use this for CALLER initialFP maps because it drops possible
    input-based vars on the floor (which for the caller should be recorded) *)
let cleanupInitialFP inState (initialFP : funPtrRequire) : funPtrRequire =
  VarMap.mapCh
    (fun v ->
       let newV, newD = 
         replaceInputFPsVal true inState.vAttrs v emptyDelayedFP in
       assert (VarSet.is_empty newD);
       newV) initialFP


(* Merge all aliased fpAccess paths + values first *)
let mergeAliasedFPAccs mContext initialFP =
  VarMap.fold 
    (fun accVar calleeV cur ->
       let repVar, _ = getMappingKind mContext accVar in
       withOldValVarMap repVar calleeV cur
    ) initialFP VarMap.empty


let initialFPFromReps sum =
  VarMap.fold
    (fun repVar calleeV cur ->
       let eqClass = UnifLocs.getEqMembers sum.fpSide.aliasedIn repVar in
       FLocSet.fold
         (fun (var, off) cur ->
            if isNoOffsetSum off then 
              withOldValVarMap var calleeV cur
            else failwith "TODO"
         ) eqClass cur
    ) sum.fpSide.initialFP VarMap.empty


(************************************************************)
(* Steps of matching *)

let doArtificialAlias = ref true
let artificialLimit = 4

let matchAContext mContext id sum =
  let tempSumAtts = combineAttrTypes sum.fpOut.vAttrs sum.fpSide.svTypes in

  let curContext = ref { mContext with t_sumAtts = tempSumAtts; } in
  let isOkay = ref true in
  let inVars = collectInputVars sum in
  
  let addEqInCounterExample context var1 var2 =
    let var1, var2 = ShortAPU.orderVars var1 var2 in
    ShortAPU.updateMap context.cEqIns var1 var2;
    markAsMapped context var1;
    curContext := context;
    isOkay := false;
  in

  let addEqGlobalCounterExample context var globs =
    let old = 
      try VarH.find context.cEqGlobals var 
      with Not_found -> FLocSet.empty in
    VarH.replace context.cEqGlobals var (combinePtrSets globs old);
    curContext := context;
    isOkay := false;
  in

  let addFpAccCE counterEx context accVar v =
    (* Always add the fpVal, even if this particular accVar matches *)
    let context = 
      try
        let old = VarMap.find accVar context.cFpAccs in
        if not (eqVals old v) then begin
          let combo = combineVals valBaseNone old v in
          { context with cFpAccs = VarMap.add accVar combo context.cFpAccs ; }
        end
        else context
      with Not_found ->
        { context with cFpAccs = VarMap.add accVar v context.cFpAccs ; }
    in
    if counterEx then
      isOkay := false;
    context
  in

  (* Check non-aliasing between inputs + inputs *)
  let checkNonAliasIn inVars =
    if shouldInspect () then begin
      logStatus "GOTCHA aliasIn\n";
      logStatusD (seq_to_doc (text ", ") List.iter 
                    (fun var -> defPrint#d_var var) inVars nil ++ line);
    end;
    List_utils.listIterPairs
      (fun var1 var2 ->
         let var1 = ShortAPU.findMappingNonOpt !curContext.cEqIns var1 in
         let var2 = ShortAPU.findMappingNonOpt !curContext.cEqIns var2 in
         if not (eqFVar var1 var2) then
           if atLeastOneMod sum var1 var2 then begin
             let vs1 = stripADeref var1 in
             let vs2 = stripADeref var2 in
             let v1, context = 
               evalInputVarCached scalarSI !curContext vs1 in
             let v2, context = 
               evalInputVarCached scalarSI context vs2 in
             let doesAlias = mayAliasInputVal var1 var2 v1 v2 in
             if doesAlias then begin
               addEqInCounterExample context var1 var2
             end
             else begin
               curContext := context
             end
           end
      ) inVars
  in

  (* Check if artificial aliasing should be added to speed up convergence *)
  let makeArtificialAliasing inVars = 
    (* build map of types -> vars with the same type *)
    let typeToVars = Hashtbl.create 13 in
    let addVarToType var =
      try
        let att = VarMap.find var !curContext.t_sumAtts in
        (* if type is actually unknown, forget about it *)
        let t = att.vType in
        if isImpreciseMallocID t then ()
        else begin
          let otherVars = 
            try Hashtbl.find typeToVars t 
            with Not_found -> [] in
          Hashtbl.replace typeToVars t (var :: otherVars) 
        end
      with Not_found ->
        ()
    in
    List.iter addVarToType inVars;
    (* if there are any types with # vars > artificialLimit, 
       merge some of them until it is within the limit. 
       For now, merge all of them! *)
    let mergeOverflows t vars =
      let numVars = List.length vars in
      if numVars >= artificialLimit then
        let rep, rest = pickShortestRepVar vars in
        List.iter (addEqInCounterExample !curContext rep) rest;
        logStatusF "mergeOverflows %d -> 1\n" numVars
    in
    Hashtbl.iter mergeOverflows typeToVars
  in

  (* Check non-aliasing between inputs + globals *)
  let checkNonAliasG inVars =
    List_utils.listIterOrderedPairs
      (fun inVar globVar ->
         let context = !curContext in
         if not ( varAliasesGlob context inVar globVar ||
                    globAlreadyMapped context inVar globVar sum.fpIn) then
           let globVar, off = globRep globVar in
           if not (isNoOffsetSum off) then begin
             logErrorF "checkNonAliasG: non-triv offset %s\n"
               (string_of_pointer (globVar, off))
           end;
           let stripped = stripADeref inVar in
           let inVal, context = 
             evalInputVarCached scalarSI context stripped in 
           match mayAliasInputVarGlob inVal globVar with
             Some glocs -> addEqGlobalCounterExample context inVar glocs
           | None -> curContext := context
      ) inVars (VarSet.elements sum.fpSide.nonAliasG);
  in 

  (* Match fpAccess paths with actual given FP *)
  let matchFPAccs () =
    let calleeInitialFP =
      cleanupInitialFP sum.fpIn (initialFPFromReps sum) in

    if shouldInspect () then begin
      logStatusF "GOTCHA matchFPs (callee) %d\n" id;
      printInitialFP calleeInitialFP
    end;

    let callerInitialFP, context =
      VarMap.fold
        (fun accVar calleeV (curCallerVals, context) ->
           let structInfo = structInfoVal calleeV in
           let callerV, context = 
             evalInputVarCached structInfo context accVar in
           (withOldValVarMap accVar callerV curCallerVals, context)
        ) calleeInitialFP (VarMap.empty, !curContext) in

    if shouldInspect () then begin
      logStatusF "GOTCHA matchFPs (caller) %d\n" id;
      printInitialFP callerInitialFP
    end;

    (* ^^^ pass for each sum can have different callerInitialFP?
       Each summary individually has different aliasing so they keep
       different set of initialFP in the sum itself.

       However, each var in such conflated sums do not represent
       a single variable (so a simple evalInputVarCached on just
       that variable won't work)
       
         - can seed the callerInitialFP w/ the fpCounterExamples 
           accumulated thus far in the match state...  

       Hmm... now, what if a LATER summary has initialFP that is
       NOT a subset of the previous initialFPs? We could end up using
       the aliasing constraints accumulated thus far and compress the
       summary's initialFP -- but ONLY for this compatibility checking
       and not for summary application... Is that okay?
    *)

    let context = VarMap.fold
      (fun calleeVar calleeV context ->
         let callerV = 
           try VarMap.find calleeVar callerInitialFP 
           with Not_found -> failwith "callerVal not found?" in
         
         (* For now require them to be exactly the same *)
         let context, compat, callerV = 
           compatibleFPAcc sum calleeVar context callerV calleeV in

         if not compat && shouldInspect () then begin
           logStatusF "GOTCHA matchFP2 %d: %s ->\ncallee %s vs caller %s\n"
             id (string_of_var calleeVar) (string_of_val calleeV) 
             (string_of_val callerV)
         end;

         if compat 
         then addFpAccCE false context calleeVar callerV
         else addFpAccCE true context calleeVar callerV
      ) calleeInitialFP context in
    curContext := context;

  in

  (* Main body *)
  Stat.time "nonAliasIn" checkNonAliasIn inVars;
  if !doArtificialAlias then
    Stat.time "makeArt" makeArtificialAliasing inVars;
  Stat.time "nonAliasG" checkNonAliasG inVars;
  Stat.time "matchFPAcc" matchFPAccs ();

  if !isOkay then (ContextMatch !curContext)
  else (NoContextMatch !curContext)


(************************************************************)


(** When initializing initial state, don't need to actually set
    "EXT" funptrs to EXT *)
let rec filterExt v =
  match v with
    FInt _ | FNFP _ -> Some v
  | FIRecs v2 -> filterExt v2
  | FpRef var -> if initFPVar#isVar var then None else Some v
  | Refs locs -> 
      if FLocSet.is_empty (stripSpecial locs) 
      then None else Some v (* leave EXT in here if it's an option... *)
  | Records recs ->
      let newrecs = filterExtRecs recs in
      if newrecs = [] then None 
      else Some (makeNormalizedRec newrecs)
  | NCRecord (fm, m) ->
      (match filterExtFM fm m with
         Some (fm2, m2) -> Some (NCRecord (fm2, m2))
       | None -> None)

and filterExtRecs recs =
  List.fold_left 
    (fun cur (fm, m) ->
       match filterExtFM fm m with
         Some (fm2, m2) -> (fm2, m2) :: cur
       | None -> cur) [] recs

and filterExtFM fm m =
  let fm2, m2 = OffsetMap.fold 
    (fun o v (curFM, curM) ->
       match filterExt v with
         None -> curFM, curM
       | Some v2 -> (OffsetMap.add o v2 curFM, max curM o)
    ) fm (OffsetMap.empty, noOff) in
  if OffsetMap.is_empty fm2 then None
  else Some (fm2, m2)


let lubTypeSums sumList =
  List.fold_left
    (fun cur (sum, _) ->
       (* Really, it should check that it's a subset of the control flow
          compared to the context we are currently constructing *)
(*       if hasNoControlFlow sum.fpSide 
       then combineTypeMap cur sum.fpSide.svTypes
       else cur
*)
       (* Just try getting all of them for now? *)
       combineTypeMap cur sum.fpSide.svTypes
    ) VarMap.empty sumList
    
let suggestSide initialFP eqIns svTypes = 
  { (emptySide ()) with 
      initialFP = initialFP; 
      aliasedIn = UnifLocs.copyNoOffMap eqIns;
      svTypes = svTypes;
  }

(** Actually create the new context *)
let suggestANewContext calleeKey sumList mContext =
  let newIn = ref emptyState in
  let initialFP = ref VarMap.empty in
  let lubbedTypes = lubTypeSums sumList in

  let getTypeOfInVar pVar =
    try VarMap.find pVar lubbedTypes 
    with Not_found -> unknownMallocIndex
  in

  let assignInputAttrib kind pVar =
    (* seed type from old summaries? not current state? *)
    let typ = getTypeOfInVar pVar in
    if typ = unknownMallocIndex then begin
      logErrorF "typ still unknown for %s\n" (string_of_var pVar)
    end;
    newIn := addVarAttr pVar ( { vKind = kind; vType = typ; } ) !newIn
  in
  
  let assignPointer pVar targVal =
    try 
      let old = getBinding pVar !newIn in
      newIn := addBinding !newIn pVar (combineVals valBaseNone old targVal)
    with Not_found -> 
      newIn := addBinding !newIn pVar targVal
  in

  let assignRecord pVar off targVal =
    let old = 
      try getBinding pVar !newIn
      with Not_found -> emptyRecord
    in
    let rec doAssign old =
      match old with
        Records recs -> 
          let newStore = 
            strongUpdateRecordScalar !newIn.bindings pVar off recs targVal in
          newIn := { !newIn with bindings = newStore; } 
      | NCRecord (fm, m) ->
          let fm, m = strongUpdateRecord (fm, m) off targVal in
          let newStore = VarMap.add pVar (NCRecord (fm, m)) !newIn.bindings in
          newIn := { !newIn with bindings = newStore; }
            
      | FIRecs _ -> failwith "summary: doAssign TODO FIRecs"
      | _ -> 
          let recs = promoteToRecord old in
          doAssign (Records recs)
    in
    doAssign old
  in
  
  let rec getSetParent srcVar targVal  =
    (* Note that the srcVar had a deref stripped earlier, so it's 
       pretty much the parent already *)
    match srcVar with
      FInput ([AccVar vid], _) ->
        let pVar = FVar vid in
        assignPointer pVar targVal

    | FInput (AccDeref :: t, len) -> 
        let pVar, kind = getMappingKind mContext srcVar in
        assignInputAttrib kind pVar;
        assignPointer pVar targVal;
        let ptrToSelf = makeMayref (pVar, noOffSum) nullVar#getLoc in
        getSetParent (makeInVar (t, len - 1)) ptrToSelf

    | FInput (AccField o :: t, _) -> 
        let o, t = compressApOffs o t in
        let pVar = 
          (match t with 
             [AccVar vid] -> FVar vid
           | AccDeref :: t2 -> 
               let origVar = makeInVar (t, List.length t) in
               let pVar, kind = getMappingKind mContext origVar in
               assignInputAttrib kind pVar;
               let ptrToPar = 
                 makeMayref (pVar, noOffSum) nullVar#getLoc in
               getSetParent (makeInVar (t2, List.length t2)) ptrToPar;
               pVar
           | _ -> failOnVar "malformed invar " srcVar) in
        assignRecord pVar o targVal

    | _ -> failOnVar "malformed inVar" srcVar
  in

  let updateInitialFP var fpVal =
    initialFP := withOldValVarMap var fpVal !initialFP
  in
  
  let initializeGivenEqualities srcVar =
    (* srcVar has deref *)
    let parentVar = stripADeref srcVar in
    let targRepVar, targK = getMappingKind mContext srcVar in
    let newPtr = makeMayref (targRepVar, noOffSum) nullVar#getLoc in
    getSetParent parentVar newPtr;

    (* Add attributes for this targVar (others set by getSetParent) *)
    match targRepVar with
      FVar _ | FRet _ -> ()
    | FHeap _ | FInput _ -> 
        assignInputAttrib targK targRepVar
  in

  let initializeGivenGlobals srcVar gLocs =
    (* srcVar has deref *)
    let parentVar = stripADeref srcVar in
    let newPtr = Refs (addToPtrSet nullVar#getLoc gLocs) in
    getSetParent parentVar newPtr;
    
    (* Add attributes for gLocs *)
    FLocSet.iter 
      (fun (var, _) ->
         match var with
           FVar _ | FRet _ -> ()
         | FHeap _ | FInput _ -> 
             (* TODO: get type from global summary? *)
             let typ = unknownMallocIndex in 
             newIn := addVarAttr var 
               ( { vKind = HGlobal; vType = typ; } ) !newIn
      ) gLocs
  in

  let initializeGivenFpAccs srcVar fpVal =
    let targRepVar, targK = getMappingKind mContext srcVar in
    (* Don't actually set things to "EXT" *)
    (match filterExt fpVal with
       Some v -> getSetParent targRepVar v;
     | None -> ());
    updateInitialFP targRepVar fpVal;
    (* Add attributes for this srcVar (others set by getSetParent) *)
    match srcVar with
      FVar _ | FRet _ -> ()
    | FHeap _ | FInput _ -> 
        assignInputAttrib targK targRepVar
  in

  (* Close the equalities, then collapse the initial FPs according to
     this closed set of equalities *)
  let () = closeEqualities mContext in

  let mergedInitFP = mergeAliasedFPAccs mContext mContext.cFpAccs in
  
  (* Must do the FPs first so that assignments to non-fp parts happen
     to all record set parts *)
  VarMap.iter initializeGivenFpAccs mergedInitFP;
  VarH.iter (fun srcVar _ -> initializeGivenEqualities srcVar) mContext.cEqIns;
  VarH.iter initializeGivenGlobals mContext.cEqGlobals;

  (* Debug *)
  let printDebugStats mContext initialFP =
    logStatus "=============";
    logStatusF "SuggestContext for %s %d\n" mContext.cCalleeName calleeKey;
    if VarH.length mContext.cEqIns > 0 then begin
      logStatus "SuggestContext Equalities:";
      printUMap mContext.cEqIns;
    end else logStatus "SuggestContext No Equalities";

    if VarH.length mContext.cEqGlobals > 0 then begin
      logStatus "SuggestContext Global Aliases:";
      VarH.iter (fun v glocs ->
                   let header = dprintf "%s -> " (string_of_var v) in
                   logStatusD (defPrint#d_ptrSet header glocs ++ line)
                ) mContext.cEqGlobals      
    end else logStatus "SuggestContext No Global Aliases";

    if Stdutil.mapSize initialFP VarMap.fold > 0 then begin
      logStatus "SuggestContext InitialFPs:";
      printInitialFP initialFP;
    end else logStatus "SuggestContext No InitialFPs";
    logStatus "=============";
  in

  let initialFP = cleanupInitialFP !newIn !initialFP in
  printDebugStats mContext initialFP; (* Wait till clean up before printing *)
  
  {fpIn = !newIn; 
   fpSide = suggestSide initialFP mContext.cEqIns lubbedTypes;
   fpOut = bottomState; }


(* Is it possible to skip ahead or subsume or remove old things 
   if we know it doesn't match already? *)

exception ContextNotReady of 
  (contextMatchInfo * sumval * Summary_keys.sumKey)

let makeFinalKey calleeKey id =
  makeSumKey calleeKey id

let subsumeAliases = ref true

let doPushStub newStub sumList =
  let newID = List.length sumList in
  let newSumList = 
    if !subsumeAliases then
      (newStub, newID) :: sumList
    else
      sumList @ [(newStub, newID)] in
  newID, newSumList


let pushNewStub calleeKey key context sumList newStub =
  (* Double check for duplicates before firing off *)
  let rec findDuplicate rest =
    match rest with
      (oldStub, oldID) :: t -> 
        (* only check input *)
        if eqStModRep oldStub.fpIn newStub.fpIn then begin
          let sumKey = makeFinalKey calleeKey oldID in
          logErrorF "Duplicate sum w/ %s @ %s!\n" 
            (Summary_keys.string_of_sumKey sumKey) 
            (string_of_loc !currentLoc);
(*          addInspectLine !currentLoc.line; *)
          (oldStub, sumKey, context)
        end
        else 
          findDuplicate t

    | [] -> (* no dupe found! yay *)
        let newID, newSumList = doPushStub newStub sumList in
        sum#addReplace key newSumList;
        let sumKey = makeFinalKey calleeKey newID in
        logStatusF "Waiting for sumKey (1) %s @ %s\n" 
          (Summary_keys.string_of_sumKey sumKey) 
          (string_of_loc !currentLoc);
        raise (ContextNotReady (context, newStub, sumKey))
  in
  findDuplicate sumList

          
let findContext curFunc readerOpt startState calleeKey 
    calleeName formals actuals =

  let key = Summary_keys.inputFreeSumKey calleeKey in
  let sumList = sum#find key in
  let rec loopFold mContext rest =
    match rest with
      (sum, id) :: tail -> 
        if isWildcardSum sum then
          (assert (tail = []); 
           (sum, makeFinalKey calleeKey id, mContext))
        else
          (match Stat.time "matchCont" (matchAContext mContext id) sum with
             NoContextMatch counter -> loopFold counter tail
           | ContextMatch context -> (sum, makeFinalKey calleeKey id, context))
    | [] -> 
        let newStub = suggestANewContext calleeKey sumList mContext in
        pushNewStub calleeKey key mContext sumList newStub
  in

  if shouldInspect () then begin
    logStatusF "GOTCHA findContext %s %d" calleeName calleeKey;
    printState startState
  end;

  let mContext = 
    makeMatchContext curFunc readerOpt startState calleeName formals actuals in
  let sum, sumKey, mContext =
    loopFold mContext sumList in
  if isBottomSummary sum then begin
    logStatusF "Waiting for sumKey (2) %s @ %s\n" 
      (Summary_keys.string_of_sumKey sumKey) (string_of_loc !currentLoc);
    raise (ContextNotReady (mContext, sum, sumKey))
  end else begin
    logStatusF "Using sumKey %s @ %s\n" 
      (Summary_keys.string_of_sumKey sumKey) (string_of_loc !currentLoc);
    sum, sumKey, mContext
  end
    
