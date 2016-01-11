
(** Fancy function pointer analysis. Intraprocedural bits *)

open Cil
open Cildump
open Pretty
open Stdutil
open Fstructs
open Type_utils
open Fp_rci_types
open Fp_rci_unify_structs
open Fp_rci_flow_insens 
open Fp_rci_lattice_ops
open Fp_rci_malloc
open Fp_rci_slicer
open Fp_rci_store
open Fp_rci_utils
open Fp_rci_focus
open Fp_rci_summary
open Fp_rci_globals
open Fp_rci_unify_globals
open Summary_keys
open Logging
open Cildump
open Df_notify



(** Package the state operations / lattice *)
class fpLattice = object (self) 
  
  val mutable thesums : Fp_rci_summary.sumdb = Fp_rci_summary.sum

  method sums = thesums

  method setTheSums newSum = thesums <- newSum

  method eqStates st1 st2 = eqStModRep st1 st2
    
  method combineStates curFun st1 st2 = 
    Stat.time "FP combine" (combineStates curFun st1) st2

  method eqSums sum1 sum2 = 
    Stat.time "FP sum eq" (eqSummaries sum1) sum2
    
  method combineSums curFun sum1 sum2 =
    Stat.time "FP sum comb" (combineSummaries curFun sum1) sum2

  method isBottom st = isBottomState st

  method isBottomSummary sum = isBottomSummary sum

  method isWildcardSummary sum = isWildcardSum sum

  (***** Special values of the state *****)

  method bottom = bottomState

  (***** Debugging *****)
       
  method printState st = printState st
  
end

let lattice = new fpLattice

(*********************************************************
 * Intra-proc Dataflow Analysis
 *********************************************************)

(******** Debugging ********)

let debugFPCalls = Hashtbl.create 10

let markFPCall callerKey callStr loc =
  if Hashtbl.mem debugFPCalls (callerKey, callStr, loc) then ()
  else Hashtbl.replace debugFPCalls (callerKey, callStr, loc) []

let recordFPCall callerKey callStr loc target =
  let old = 
    try Hashtbl.find debugFPCalls (callerKey, callStr, loc) 
    with Not_found -> [] (* shouldn't happen but might as well be careful *)
  in
  let n = List_utils.addOnce old target in
  Hashtbl.replace debugFPCalls (callerKey, callStr, loc) n

let sortFPCalls () =
  let enum = Hashtbl.fold (fun k t cur -> (k, t) :: cur) debugFPCalls [] in
  List.sort (fun ((ck1, cs1, loc1), t1) ((ck2, cs2, loc2), t2) ->
               let c = Pervasives.compare ck1 ck2 in
               if c == 0 then
                 let c = Pervasives.compare loc1 loc2 in
                 if c == 0 then Pervasives.compare cs1 cs2
                 else c
               else c) enum

let printFPCalls () =
  logStatusF "=========================\nFunction pointer calls\n";
  let totalCalls = ref 0 in
  let totalTargets = ref 0 in
  List.iter 
    (fun ((callerKey, callStr, loc), targets) ->
       let numT = List.length targets in 
       incr totalCalls;
       totalTargets := !totalTargets + numT;
       logStatusF "%s / calls / %s: " 
         (string_of_sumKey callerKey)
         (string_of_loc loc);
       logStatusD (text "[" ++ 
                       seq_to_doc 
                       (text ", ") List.iter
                       (fun vname -> text vname) targets nil ++ 
                       text "]" ++ dprintf "(%d) : " numT ++ 
                       text callStr ++ line)
    ) (sortFPCalls ());
  let avg = (float_of_int !totalTargets /. float_of_int !totalCalls) in
  logStatusF "FP avg target size: %f\n" avg


let informFunkyCall str callExp =
  logErrorF "Potential %s fp call: %s @ %s\n" str 
    (string_of_exp callExp) (string_of_loc !currentLoc)

let noFilterFuncs = ref false

let matchesFuncType actualFun callSig =
  if !noFilterFuncs then begin
    if not (functionTypeMatches actualFun.vtype callSig) then 
      (* only warn *)
      logErrorF "notfilt: funtype mismatch %s:%s vs %s @ %s\n" 
        actualFun.vname (string_of_type actualFun.vtype) 
        (sprint 80 (d_funSig callSig))
        (string_of_loc !currentLoc)
    ;
    true
  end else
    functionTypeMatches actualFun.vtype callSig
      
      
(************** Actual Dataflow ***************)


(** Hack to reduce the number of actuals in case the function takes VarArgs *)
let trimActuals formals actuals fnode =
  let lenF = List.length formals in
  let lenA = List.length actuals in
  if lenF < lenA then begin
    logErrorF "Trimming actuals for %s (varargs?)\n" fnode.Callg.name;
    (formals, List_utils.listPrefix actuals lenF)
  end else if lenA < lenF then begin
    logErrorF "#Actuals < #Formals? %s @ %s\n" fnode.Callg.name 
      (string_of_loc !currentLoc);
    raise NullException
  end else (formals, actuals)
    

exception SummaryNotReady of sumKey * fpState

(** May raise ContextNotReady *)
let getUseCallContext funcs curFunc readerOpt state acts fkey 
    (withBody : sumval -> sumKey -> contextMatchInfo -> 'a) 
    (withNoBody : unit -> 'a) : 'a =
  match getFormals funcs fkey with
    Some (fnode, formals) ->
      if fnode.Callg.hasBody then 
        let sum, sumKey, mContext = 
          findContext curFunc readerOpt state fkey 
            fnode.Callg.name formals acts in
        withBody sum sumKey mContext
      else withNoBody () 
  | None -> withNoBody ()
      


let emptyNotify : sumKey toNotify =
  { notifyCaller = false;
    notifyPP = []; }


(** Transfer func based on requirements of module IDF, not module Intra *)
class fpTransFunc stLat =
object (self)

  (* current function's fKey and inputState *)
  val mutable funSumKey = (-1, "")
  val mutable funInSt = bottomState
  val mutable funSideCond = emptySide ()
  val mutable tempOldSide = None
  val mutable curFunc = dummyFunDec
  val mutable funcs : Callg.callG = Callg.emptyCG
    
  method setFuncs f =
    funcs <- f

  (** Set transfer func to handle given function *)
  method handleFunc cfg (sumK, inSt) =
    if funSumKey = sumK then ()
    else begin
      funSumKey <- sumK;
      (* Should assert that the input states are the same *)
      let oldS = getOldSummary stLat#sums funSumKey in
      funInSt <- inSt;
      funSideCond <- oldS.fpSide;
      !Fp_rci_unify.myMerger#setFunc sumK;
      curFunc <- cfg;
      logStatusF "=============\nAnalyzing %s : %s\n" cfg.svar.vname
        (string_of_sumKey funSumKey);

    end

  method private getCurrentStep () =
    (funSumKey, getCurrentPP ())

  method private getNfpSource () =
    makeNfpSource (curFunc.svar.vid)

  (************************************************************)

  method private updateSideCond side =
    (* Keep copy of old to compare and detect changes... ugh... *)
    (match tempOldSide with 
       None -> 
         tempOldSide <- Some funSideCond;
         funSideCond <- side
     | Some _ ->
         funSideCond <- side)

  method private checkSideChanges stOpt =
    let mergeSideAtts side =
      match stOpt with 
        None -> side 
      | Some st -> 
          if isBottomState st || isTopState st then side
          else 
            let side = 
              { side with svTypes = 
                  combineTypeAttributes side.svTypes st.vAttrs } in
            mergeAccPathCyclesAllSide side
    in
    let updateSideSummary newSide =
      let oldS = getOldSummary stLat#sums funSumKey in
      let newS = { oldS with fpSide = newSide; } in
      replaceSummary stLat#sums funSumKey newS;
    in

    let checkAttrChanges oldSide newerSide =
      (* Know that everything except the attribs are the same *)
      if not (eqTypeMap oldSide.svTypes newerSide.svTypes) then begin
(*        logStatusF "New side-conditions (only attrs) %s\n" 
          (string_of_sumKey funSumKey);
*)
        updateSideSummary newerSide;
      end
    in

    match tempOldSide with
      None -> 
        (* in this case, funSideCond is the same as the one in the summary *)
        let collapsed = mergeSideAtts funSideCond in
        if not (eqSideModAttrs funSideCond collapsed) then begin
          funSideCond <- collapsed;
          updateSideSummary collapsed;
          logStatusF "New side-conditions %s\n" (string_of_sumKey funSumKey);
          (*            printSideCondVerb funSideCond; *)
          true
        end else begin
          checkAttrChanges funSideCond collapsed;
          false
        end
    | Some old -> 
        (* in this case, old is the same as the one in the summary *)
        tempOldSide <- None;
        let side = mergeSideAtts funSideCond in
        funSideCond <- side;
        if not (eqSideModAttrs old funSideCond) then begin
          let old = mergeSideAtts old in
          (* Check again when they're on equal footing *)
          let changed = not (eqSideModAttrs old funSideCond) in
          if changed then begin
            updateSideSummary funSideCond;
            logStatusF "New side-conditions %s\n" (string_of_sumKey funSumKey);
(*            printSideCondVerb funSideCond;
*)
            changed
          end else begin
            checkAttrChanges old funSideCond;
            changed
          end
        end else begin
          checkAttrChanges old funSideCond;
          false
        end


  method private makeNotify stOpt notifyPP =
    let sidechanged = self#checkSideChanges stOpt in
    { notifyCaller = sidechanged;
      notifyPP = RS.elements notifyPP; }

  (** Wrapper to bring in known types to startup state *)
  method private startTF st = 
    if isBottomState st || isTopState st then st
    else 
      let newA = combineAttrTypes st.vAttrs funSideCond.svTypes in
      { st with vAttrs = newA; }

  (** Wrapper to update side conditions w/ known types at the end. *)
  method private endTF st notify =
    if isBottomState st || isTopState st then
      notify
    else 
      let alsoChanged = self#checkSideChanges (Some st) in
      { notify with notifyCaller = notify.notifyCaller || alsoChanged; }
        

  (*********** Actual transfer func stuff **************)

  (** Will handle return statements, if-statements, instr-blocks separately *)
  method handleStmt (stmt: stmt) (state:fpState) =
    state

  (************************************************************)

  (** Update summary. Return true if new *)
  method handleReturnStmt retOpt state =
    let state = self#startTF state in
    let state = Stat.time "mergeAPAll" mergeAccPathCyclesAll state in
    let newS, toNotify = 
      Stat.time "makeSum" (self#makeNewSummary retOpt) state in  
    self#tryUpdateSummary state newS toNotify

  (* Ugh... todo: always assume retvar is a root... use this
     method to assign retvar only... *)

  (** Update the output state by taking "return e" as "RETVAR := eval e..." *)
  method private updateRetVar state retOpt =
    match retOpt with
      Some (exp) ->
        let emisc = { (emptyMisc curFunc) with
                        readerOpt = Some (self#getCurrentStep ()); } in
        let rv, state = 
          (try eval state exp emisc
           with NullException ->
             logErrorF "NullDeref evaluating return value %s\n" 
               (string_of_exp exp);
             stLat#printState state;
             raise NullException) in        
        let typ = Cil_lvals.typeOfUnsafe exp in
        let retVar = FRet (addTyp typ) in
        assignRetValue state retVar rv
    | None -> state
      
  method private tryUpdateSummary state newS toNotify =
    let sumKey = funSumKey in
    let oldS = getOldSummary stLat#sums sumKey in
    if isTopSummary oldS 
    then self#makeNotify (Some state) toNotify
    else if isTopSummary newS then begin
      self#updateSummary sumKey newS;
      let notify = self#makeNotify (Some state) toNotify in
      { notify with notifyCaller = true; }
    end else begin
      (* after combining, may have converted some vars to globals 
         but they are still in the bindings / may have included unmodified
         guys, so do another pass to drop *)
      let reFilter comboS =
        if isBottomState comboS.fpOut || isTopState comboS.fpOut then
          let notify = self#makeNotify (Some comboS.fpOut) toNotify in
          comboS, notify
        else
          let comboSOut, gside = dropGlobalAssume curFunc comboS.fpOut in
          let comboSOut = gcState curFunc.sformals comboSOut in
          self#addGInputFPs comboSOut gside;
          let baseNotify = self#makeNotify 
            (Some comboSOut) (combineNotifyGR gside.gNotify toNotify) in
          let comboS = { comboS with fpOut = comboSOut; } in
          comboS, baseNotify
      in

      let comboS = stLat#combineSums curFunc newS oldS in

      let comboS, baseNotify = reFilter comboS in
      let oldS, _ = reFilter oldS in
      if stLat#eqSums oldS comboS 
      then baseNotify
      else begin
        self#updateSummary sumKey comboS;
        { baseNotify with notifyCaller = true; }
      end
    end

  (** Treat as similar to a return, but may set the "out-state" to BOTTOM *)
  method private doExitCall fvar state =
    let sum = { fpIn = funInSt; fpSide = funSideCond; fpOut = bottomState; } in
    logStatusF "Handling exit func %s @ %s\n" fvar.vname 
      (string_of_loc !currentLoc);
    let notify = self#tryUpdateSummary state sum emptyNotifyGR in
    (emptyFlows, { notify with notifyCaller = true; } , None)
      (* Ugh... just force it... *)
      

  method private updateSummary sumKey newS =
    logStatusF "New summary %s %s\n" (string_of_sumKey sumKey) 
      (string_of_loc !currentLoc);
    Stat.time "printSum" printInitialState (newS.fpOut, newS.fpSide);
    replaceSummary stLat#sums sumKey newS


  method private makeNewSummary retOpt state =
    let checkTopBot state els curNotify =
      if stLat#isBottom state || isTopState state then 
        ( { fpIn = funInSt; 
            fpSide = funSideCond; 
            fpOut = state; }, 
          curNotify)
      else els curNotify
    in
    
    let mainStuff curNotify =
      try
        let state = self#updateRetVar state retOpt in

        let sliceAndSuch curNotify =
          (* Check things reachable from OLD formals *)
          let state, gside = dropGlobalAssume curFunc state in
          self#addGInputFPs state gside;
          let sliced = Stat.time "weaken/GC"
            (weakenState curFunc.sformals (self#getNfpSource ())) state in
          let sliced = !Fp_rci_unify.myMerger#aggressiveMergeAll sliced in
          ( { fpIn = funInSt; fpSide = funSideCond; fpOut = sliced; }, 
            (combineNotifyGR curNotify gside.gNotify))
        in
        
        checkTopBot state sliceAndSuch curNotify
      with NullException ->
        (* wait a minute... this shouldn't happen? *)
        logError "NullDeref during sum slicing";
        ( { fpIn = funInSt; 
            fpSide = funSideCond;
            fpOut = stLat#bottom; }, 
          curNotify)
    in
    checkTopBot state mainStuff emptyNotifyGR
      
  method private addDelayedFPs curSt delayed =
    VarSet.iter 
      (fun inVar ->
         (* Don't add it if it's not extended from the current formals 
            (e.g., it might be a global) *)
         if not (extendsFormal curFunc.sformals inVar) then ()
         else begin
           match addFpAccs funSideCond inVar with
             None -> ()
           | Some side -> self#updateSideCond side
         end
      ) delayed
  
  method private addGInputFPs curSt gside =
    self#addDelayedFPs curSt gside.gInitialFP

  method private addContextInputFPs mContext =
    self#addDelayedFPs mContext.cState mContext.cDelayFpReqs


  (************************************************************)

  method private stringOptToString strOpt =
    match strOpt with Some a -> a | None -> ""

  method handleASM (atts:attributes) (templs:string list)
    (cos:(string option * string * lval) list)
    (cis: (string option * string * exp) list) 
    (clobs:string list) (loc:location) (state:fpState) =
    (* Attemp to pick up simple copies from cis into cos *)
    match (cos, cis) with
      [(out_cons_opt, out_cons, outLv)], 
      [(in_cons_opt, in_cons, inExp)] ->
        let os1 = self#stringOptToString out_cons_opt in
        let is1 = self#stringOptToString in_cons_opt in
        if templs = [""] then begin
          logStatusF "ASM assign? (%s, %s, %s) <- (%s, %s, %s) @ %s\n" 
            os1 out_cons (string_of_lval outLv)
            is1 in_cons (string_of_exp inExp)
            (string_of_loc !currentLoc);
(*          self#handleAssign outLv inExp loc state *)
          (state, emptyNotify)
        end else
          let instr = Asm (atts, templs, cos, cis, clobs, loc) in
          logErrorF "Skipping asm single in/out %s @ %s\n" 
            (string_of_instr instr) (string_of_loc !currentLoc);
          (state, emptyNotify)
    | _, _ ->
        (state, emptyNotify)


  (************************************************************)

  val alwaysUnchanged = Hashtbl.create 17
  val mutable numUnchChecks = 0


  (* only confident about slicing assignments for now... so let's see
     if that helps? (of course that will lead to slicing 
     if-statements too) *)
  method private debugUnchangedAssign startSt outSt loc =
    (* Keep set of assign instructions that ALWAYS look like no-ops *)
    let (fk, s) = funSumKey in
    if fk = 1111 then begin
      let key = (funSumKey, loc) in

      let updateCh unchanged =
        let oldUnchanged, countCh, countUnch = 
          try Hashtbl.find alwaysUnchanged key
          with Not_found -> true, 0, 0 in
        let newUnch, newCountCh, newCountUnch = 
          if unchanged 
          then oldUnchanged, countCh, countUnch + 1
          else unchanged, countCh + 1, countUnch in
        Hashtbl.replace alwaysUnchanged key 
          (newUnch, newCountCh, newCountUnch)
      in

      let printChData () =
        numUnchChecks <- numUnchChecks + 1;
        if numUnchChecks mod 5000 = 0 then begin
          let totUnch = ref 0 in
          logStatusD (map_to_doc line Hashtbl.iter
                        (fun (sumKey, loc) (unch, numCh, numUnch) ->
                           if unch then
                             totUnch := !totUnch + numUnch;
                           dprintf "%s : %s -> %b, %d, %d"
                             (string_of_sumKey sumKey)
                             (string_of_loc loc) unch numCh numUnch;
                        ) alwaysUnchanged nil);
          logStatusF "\nTotal (always) Unchanged vs Total Checks: %d / %d\n"
            !totUnch numUnchChecks
        end
      in
      
      let diff = diffState startSt outSt in
      updateCh (eqStates emptyState diff);
      printChData ()
    end

  (** Entry point for handling assignment instructions *)
  method handleAssign lhsLval rhs (loc:location) state =
    let state = self#startTF state in
    let state = Stat.time "mergeAPAll" mergeAccPathCyclesBindings state in
    let outSt, notify = 
      if dontFlowFuncState state 
      then (state, emptyNotify)
      else try
        let state = stUseReps curFunc.sformals state in
        (* Not using focus for the RHS vs LHS here... *)
        let emisc = { (emptyMisc curFunc) with 
                        readerOpt = Some (self#getCurrentStep ()); 
                    } in
        let rhs, state = eval state rhs emisc in
        let st, notifyPP = match assignedMalloc rhs with
            Some t -> self#doMalloc lhsLval t state
          | None -> self#doAssign lhsLval rhs state in
        let notify = self#makeNotify (Some st) notifyPP in
        (Fp_rci_hashcons.hc_state st, notify)
      with NullException -> 
        logError ("Assign Null Deref @ " ^ string_of_loc !currentLoc);
        let notify = self#makeNotify (Some state) emptyNotifyGR in
        stLat#bottom, notify in
    self#debugUnchangedAssign state outSt loc;
    outSt, (self#endTF outSt notify)


  method private doAssign lhsLval rhs state =
    let lhsT = Cil_lvals.typeOfLvalUnsafe lhsLval in
    let rhs = filterImpreciseWrite state.vAttrs rhs lhsT in
    let rhs = nfpOutType state.vAttrs (self#getNfpSource ()) lhsT rhs in
    match lhsLval with
      (* [COPY] (vi.off) = rhs *)
      (Var(vi), off) ->
        let var, off = getLocation false vi off in
        let state, toNotify = 
          assignVar curFunc lhsT state var off rhs (not (isSumOff off)) in
        (match addMods state.vAttrs funSideCond var off lhsT with
           None -> ()
         | Some newSide -> self#updateSideCond newSide);
        state, toNotify
          
    (* [STORE] *e.outerOff = rhs *)
    | (Mem(ptrExp), outerOff) ->
        let accessPath = ref [] in
        let locsHook = captureAccessPath accessPath in
        let emisc = { lookupHook = locsHook;
                      readerOpt = Some (self#getCurrentStep ()); 
                      assumedTyp = None; 
                      curFunc = curFunc; } in
        let ptrVal, newOff, state = 
          getPtrVal state ptrExp outerOff emisc in
        self#assignToPointerTarget ptrExp lhsT
          accessPath ptrVal newOff rhs state 


  method private assignToPointerTarget ptrExp lhsT accPath 
    ptrVal outerOff rhs state =
    match ptrVal with
      Refs ls ->
        (* Focus the state first, then update -- 
           or don't bother to focus *)
        let ls = stripSpecial ls in
        let strong = FLocSet.is_singleton ls in
        let targT = Cil_lvals.typeAfterDeref ptrExp in
        let finalSt, toNotify = 
          FLocSet.fold 
            (fun (addr, innerOff) (curSt, notify) ->
               (* update type of cell used in lhs if it's a noOffset loc ? *)
               let startSt = updateType state targT addr innerOff in
               let newOff = concatOffsetVar addr outerOff innerOff in
               try 
                 (match addMods startSt.vAttrs 
                    funSideCond addr newOff lhsT with 
                      None -> ()
                    | Some newSide -> self#updateSideCond newSide);
                 let newState, toNotify = 
                   assignVar curFunc lhsT startSt addr newOff rhs strong in
                 let newState = match curSt with
                     None -> Some (newState)
                   | Some x -> Some (stLat#combineStates curFunc x newState) in
                 let newNotify = combineNotifyGR notify toNotify in
                 (newState, newNotify)
               with NullException ->
                 (curSt, notify)
            ) ls (None, emptyNotifyGR) in
        (match finalSt with
           None -> 
             logErrorF "NULL: assignToPointer %s -> %s\n" 
               (string_of_exp ptrExp) (string_of_val ptrVal);
             state, toNotify
         | Some x -> x, toNotify)
          
    | FpRef var -> 
        logErrorF "doAssign on function %s @ %s\n" (string_of_var var)
          (string_of_loc !currentLoc);
        state, emptyNotifyGR

    | FNFP _ -> 
        state, emptyNotifyGR
    | FInt i64 -> 
        logErrorF "NULL: assignToPtr %s\n" (string_of_exp ptrExp);
(*        raise NullException *)
        state, emptyNotifyGR
    | Records _ | FIRecs _ | NCRecord _ ->
        logErrorF "doAssign: rec as ptr for store: %s -> %s\n" 
          (string_of_exp ptrExp) (string_of_val ptrVal);
        self#assignToPointerTarget ptrExp lhsT accPath 
          (demoteFromRecVal ptrVal) outerOff rhs state


  (** Process an assignment that is of the form "x = malloc" *)
  method private doMalloc lhs typ inSt =
    (* Check if there is already a binding and merge if there is. *)
    let pp = getCurrentPP () in
    let fkey = fkey_of_sumKey funSumKey in
    let malVar, malAtts = makeMalloc typ (pp, fkey) in
    let malVal = defaultValNonInputType typ in
    let st = mergeMallocs malVar malVal malAtts inSt in
    (* Assign to the lhs a pointer to the heap variable *)
    (* Allow null for now *)
    let ptr = makeMayref (malVar, noOffSum) (nullVar#getLoc) in
    self#doAssign lhs ptr st


  (************************************************************)

  (** Entry point for handling calls [retOpt = callExp(acts)] *)
  method handleCall retOpt callexp (acts:exp list) (loc:location) state 
    : (sumKey, fpState) callFlows * sumKey toNotify * fpState option =
    if stLat#isBottom state then 
      emptyFlows, emptyNotify, None
    else if isTopState state then
      self#handleTopCall callexp loc state
    else 
      let state = self#startTF state in 
      let state = Stat.time "mergeAPAll" mergeAccPathCyclesBindings state in
      let state = stUseReps curFunc.sformals state in
      let rec reallyHandleCall callexp () =
        match callexp with
          Lval(Var(vi),NoOffset) ->
            if Exit_funcs.isExitFun vi then
              (* maybe not the best place to check *)
              self#doExitCall vi state
            else begin
              let state, gside = dropGlobalAssume curFunc state in
              self#addGInputFPs state gside;
              (try 
                 let (calleeFlows, state, toNotify) = 
                   self#doCall retOpt acts loc state vi in
                 let notify = self#makeNotify
                   (Some state) (combineNotifyGR gside.gNotify toNotify) in
                 (calleeFlows, notify, Some (Fp_rci_hashcons.hc_state state))
               with 
                 SummaryNotReady (sumKey, inputs) ->
                   let notify = self#makeNotify (Some state) gside.gNotify in
                   (addCalleeFlow (sumKey, inputs) emptyFlows, 
                    notify, None)
               | NullException ->
                   informFunkyCall "null1" callexp;
                   let notify = self#makeNotify (Some state) gside.gNotify in
                   (emptyFlows, notify, Some (Fp_rci_hashcons.hc_state state))
              )
            end
              
        | Lval(Mem(callExp), NoOffset) ->
            (* DEBUG *)
            let callStr = string_of_exp callExp in
            markFPCall funSumKey callStr loc;
            (* /DEBUG *)
            let state, gside = dropGlobalAssume curFunc state in
            self#addGInputFPs state gside;

            let accessPath = ref [] in
            let emisc = { lookupHook = captureAccessPath accessPath;
                          readerOpt = Some (self#getCurrentStep ()); 
                          assumedTyp = None; 
                          curFunc = curFunc;
                        } in
            (try
               let ptrVal, state = eval state callExp emisc in
               self#handleFPCall retOpt callExp callStr gside 
                 ptrVal accessPath acts loc state
             with NullException ->
               informFunkyCall "null2" callExp;
               let notify = self#makeNotify (Some state) gside.gNotify in
               (emptyFlows, notify, Some (Fp_rci_hashcons.hc_state state))
            )

        | CastE (t, e') ->
            (* Don't need the assumedTyp ? *)
            reallyHandleCall e' ()
        | Lval _ | StartOf _ | AddrOf _ | BinOp _ | UnOp _ | Const _ 
        | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ | AlignOf _ ->
            failwith "unknown call expression"
      in
      let (calls, notify, stOpt) as res = 
        Fp_rci_unify.doWithoutUnify (reallyHandleCall callexp) in
      (match stOpt with 
         None -> res
       | Some st -> (calls, self#endTF st notify, stOpt)) 
        

  method private handleTopCall callExp loc st  =
    (* if topState then we DO want to flow to callees 
       (under a special TOP context) *)
    match callExp with
      Lval(Var(vi),NoOffset) ->
        let notify = self#makeNotify None emptyNotifyGR in
        if fHasBody funcs vi.vid then
          let sumKey = makeTopSumKey vi.vid in
          logError "TODO: update handleTopCall";
          (addCalleeFlow (sumKey, st) emptyFlows, 
           notify, Some (st))
        else (emptyFlows, notify, Some (st))

    | Lval(Mem(callExp), NoOffset) ->

        (* DEBUG *)
        let callStr = string_of_exp callExp in
        markFPCall funSumKey callStr loc;
        (* /DEBUG *)
        recordFPCall funSumKey callStr loc "top";

        let targets = Alias.deref_funptr callExp in
        let calleeQ = List.fold_left 
          (fun callees vid ->
             let vi = varinfoOfVid vid in 
             if matchesFuncType vi (getFunSigPtrExp callExp) then 
               begin
                 (* DEBUG *)
                 recordFPCall funSumKey callStr loc vi.vname;
                 if fHasBody funcs vi.vid then begin
                   let sumKey = makeTopSumKey vid in
                   logError "TODO: update handleTopCall";
                   addCalleeFlow (sumKey, st) callees
                 end else callees
               end else callees
          ) emptyFlows targets in
        let notify = self#makeNotify None emptyNotifyGR in
        calleeQ, notify, Some (st)

    | CastE (t, e') ->
        self#handleTopCall e' loc st

    | Lval _ | StartOf _ | AddrOf _ | BinOp _ | UnOp _ | Const _ 
    | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ | AlignOf _ ->
        failwith "unknown call expression"



  method private handleFPCall retOpt callExp callStr gside ptrVal 
    accessPath acts loc inSt =

    let callSig = getFunSigPtrExp callExp in

    (* Mark as relevant context if it comes from input heap... *)
    let markRelevantContexts inVar = 
      match addFpAccs funSideCond inVar with
        None -> ()
      | Some side -> self#updateSideCond side
    in
    
    let handleCallToExt only delay notify curSt =
      recordFPCall funSumKey callStr loc "EXT";
      (delay, notify, if only then Some (inSt) else curSt)
    in
    
    let directCallToVID only vid curFlows notify curSt =
      if nullVar#isVID vid then begin
        informFunkyCall "null3" callExp;
        (curFlows, notify, if only then Some (inSt) else curSt)
(*
        (curFlows, notify, if only then Some (stLat#bottom) else curSt)
*)
      end else if (nfpVar#isVID vid) then begin
        informFunkyCall "nfp" callExp;
        (curFlows, notify, if only then Some (inSt) else curSt)
      end else if extVar#isVID vid then 
        handleCallToExt only curFlows notify curSt
      else begin 
        let vi = varinfoOfVid vid in 

        let handleTypeMismatch () =
          informFunkyCall "typsig" callExp;
          logErrorF "funtype mismatch %s:%s vs %s @ %s\n" 
            vi.vname (string_of_type vi.vtype) 
            (sprint 80 (d_funSig callSig))
            (string_of_loc !currentLoc);
          curFlows, notify, curSt
        in

        if matchesFuncType vi callSig then begin
          (* DEBUG *)
          recordFPCall funSumKey callStr loc vi.vname;
          (* /DEBUG *)
          try 
            let inSt = 
              if not only 
              then focus !accessPath (FVar vid, noOffSum) inSt 
              else inSt in

            if isBottomState inSt then begin
              failwith ("handleFP focus on " ^ string_of_var (FVar vid)
                        ^ " leads to bottom");
            end else
              let (calleeFlows, newSt, toNotify) =
                self#doCall retOpt acts loc inSt vi in
              let newSt = 
                match curSt with
                  None -> Some (newSt)
                | Some x -> Some (stLat#combineStates curFunc x newSt) in
              (combineCalleeFlows calleeFlows curFlows, 
               combineNotifyGR notify toNotify, 
               newSt)
          with 
            SummaryNotReady (sumKey, inputs) ->
              addCalleeFlow (sumKey, inputs) curFlows, notify, curSt
          | NullException ->
              logErrorF "Null eval'ing args @ %s\n" (string_of_loc !currentLoc);
              curFlows, notify, curSt

          (* If we don't filter by type sig, we may not catch violations
             until we try to evaluate actuals *)
          | (Failure msg as f) ->
              if msg = "nth" then handleTypeMismatch ()
              else raise f
          | NoRetValue -> handleTypeMismatch ()
        end else handleTypeMismatch ()
      end
    in
    
    let doFPTarget only (addr, o) (curFlows, notify, curSt) =
      match addr with
        FVar vid ->
          directCallToVID only vid curFlows notify curSt

      | FInput ap ->
          (* Treat as NULL, but record *)
          let curFlows, notify, curSt =
            directCallToVID only initFPVar#getVID curFlows notify curSt in
          if extendsFormal curFunc.sformals addr 
          then markRelevantContexts addr
          else logErrorF "FpRef (FInput global) %s?\n" (string_of_var addr);
          curFlows, notify, curSt

      | FHeap _ | FRet _  -> 
          informFunkyCall "heap" callExp;
          logErrorF "var: %s\n" (string_of_var addr);
          curFlows, notify, curSt
    in

    let treatAsSkip () =
      let stOpt = Some inSt in
      let notify = self#makeNotify stOpt gside.gNotify in
      (emptyFlows, notify, stOpt)
    in

    match ptrVal with
      FpRef var ->
        let curFlows, notify, stateOpt = 
          doFPTarget true (var, noOffSum) (emptyFlows, gside.gNotify, None) in
        let notify = self#makeNotify stateOpt notify in
        curFlows, notify, stateOpt

    | Refs ls ->
        let ls = stripSpecial ls in
        let only = not (FLocSet.cardinal ls >= 2) in
        let calleeFlows, notify, stOpt =
          FLocSet.fold (doFPTarget only) ls (emptyFlows, gside.gNotify, None) in
        let stOpt = match stOpt with 
            None -> stOpt 
          | Some x -> Some (Fp_rci_hashcons.hc_state x) in
        let notify = self#makeNotify stOpt notify in
        calleeFlows, notify, stOpt

    | FInt _ -> 
        informFunkyCall "null4" callExp;
        treatAsSkip ()
          
    | FNFP ts ->
        logErrorF "FP is Nfp? %s -> %s @ %s\n" 
          (string_of_exp callExp)
          (sprint 80 (d_nfpsources ts))
          (string_of_loc !currentLoc);
        treatAsSkip ()

    | Records _ | FIRecs _ | NCRecord _ -> 
        informFunkyCall "record" callExp;
        treatAsSkip ()

  
  method private doCall retOpt acts loc state funVar 
    : (sumKey, fpState) callFlows *  fpState * notifyGR = 
    match self#handledSpecialCall retOpt acts loc state funVar with
      Some (flows, st, notify) -> flows, st, notify
    | None ->
        let fkey = funVar.vid in
        let readerOpt = Some (self#getCurrentStep ()) in
        let withBody sum sumKey mContext =
          self#addContextInputFPs mContext;
          let st, notify = self#applySummary retOpt sum fkey mContext in
          addCalleeFlow (sumKey, sum.fpIn) emptyFlows, st, notify
        in
        let withOutBody () =
          let st, notify = self#doCallNoBody retOpt acts loc state funVar in
          emptyFlows, st, notify
        in
        let rec useContextOrRefine state =
          try
            getUseCallContext funcs curFunc readerOpt state acts fkey 
              withBody withOutBody
          with SubstFPImprecise (revAccs, targs, cont) ->
            FLocSet.fold 
              (fun focusTarg (usedConts, comboSt, notify) ->
                 let startSt = focus revAccs focusTarg cont.cState in
                 if isBottomState startSt then begin
                   failwith ("SubstFPImprecise focus on " ^
                               (string_of_pointer focusTarg) ^ " leads to bottom")
                 end else
                   let calledC, postSt, newNotify = useContextOrRefine startSt in
                   (combineCalleeFlows calledC usedConts,
                    combineStates curFunc comboSt postSt,
                    combineNotifyGR notify newNotify)
              ) targs (emptyFlows, bottomState, emptyNotifyGR)
        in
        try useContextOrRefine state
        with ContextNotReady (mContext, sum, sumKey) ->
          (* Don't add in this case (let the context be ready first)
             self#addContextInputFPs mContext;
          *)
          if !fullFI then
            self#addContextInputFPs mContext; (* because of funky shit *)
          raise (SummaryNotReady (sumKey, sum.fpIn))

  method private handledSpecialCall retOpt acts loc state funVar =
    (* Maybe it's a malloc that we missed during translation -- 
       e.g., if it's an indirect call *)
    if isMallocFun funVar then 
      let st, notify = self#doMallocCall retOpt acts state in
      Some (emptyFlows, st, notify)
    else if isMemcpyFun funVar then
      let st, notify = self#doMemcpyCall retOpt acts state in
      Some (emptyFlows, st, notify)
    else None
          
          
  method private doMallocCall retOpt acts state =
    match retOpt with 
      Some (lhsLval) ->
        let t = Cil_lvals.unrollTypeNoAttrs (Cil_lvals.typeOfLvalUnsafe lhsLval) in
        let retTyp = match t with
            TPtr (retTyp, _) ->
              Trans_alloc.pickType retTyp acts
          | _ -> 
              let retT = Trans_alloc.pickType (TPtr ((TVoid []), [])) acts in
              logErrorF "MALLOC: Lval is not ptr %s : %s -- picked %s @ %s\n"
                (string_of_lval lhsLval) (string_of_type t)
                (string_of_type retT) (string_of_loc !currentLoc);
              retT
        in
        let st, toNotify = self#doMalloc lhsLval retTyp state in
        (Fp_rci_hashcons.hc_state st, toNotify)
    | None ->
        (state, emptyNotifyGR)

  method private doMemcpyCall retOpt acts state = 
    (* TODO *)
    (state, emptyNotifyGR)

  method private doCallNoBody retOpt acts loc state funVar =
    self#warnNoBody funVar.vname;
    let retType = 
      match retOpt with
        None -> TVoid []
      | Some lv -> Cil_lvals.typeOfLvalUnsafe lv in
    let sum : sumval = makeUnknownSummary retType in
    (* Hacky way of reusing applySummary *)
    let formals = [] in
    let readerOpt = Some (self#getCurrentStep ()) in
    let mContext = makeMatchContext curFunc readerOpt state 
      funVar.vname formals acts in
    self#applySummary retOpt sum funVar.vid mContext


  val warnedNoBody = Hashtbl.create 17    
  method private warnNoBody name =
    if Hashtbl.mem warnedNoBody name then ()
    else begin
      Hashtbl.add warnedNoBody name ();
      logStatusF "No body for func: %s\n" name
    end


  method private applySummary retOpt (sum: sumval) calleeK mContext =
    if stLat#isBottomSummary sum then
      (stLat#bottom, emptyNotifyGR)
    else if isTopSummary sum then
      (topState, emptyNotifyGR)
    else begin
      let sum = filterUnchanged sum in

      (* Fresh stuff *)
      let mContext, sum = self#extractMergeMallocs mContext sum calleeK in

      (* Okay, bring in attributes into a separate map... *)
      let mContext, newAtts, newGlobals, notifyGUnif = 
        Stat.time "applyAtts" (self#updateAttributes mContext) sum in

      let stWithNewAtts = 
        self#updateAttribTransitive mContext newAtts newGlobals in

      (* Stuff in terms of pre-call state *)
      let mContext = self#applyGlobMods mContext sum in
      let mContext = self#applyFpAccs mContext sum in
      let mContext = Stat.time "applyMod" (self#applyModIns mContext) sum in

      (** Now the rest of the vars are Ret, input-based or global 
          Hmm... need to apply the ret-val at the "same" time as the
          rest of the effects... *)
      let mContext, notifyRet, sum = 
        self#applyRetValue retOpt mContext sum in
      let mContext, notifyStore = 
        Stat.time "applyStore" (self#applyStoreUpdates mContext) sum in

      let finalNotify = 
        combineNotifyGR (combineNotifyGR notifyGUnif notifyRet) notifyStore in
      if stLat#isBottom mContext.cState then begin
        logErrorF "NullDeref: applySum? @ %s\n" (string_of_loc !currentLoc);
        (mContext.cState, finalNotify)
      end else 
        let finalSt = 
          { mContext.cState with
              vAttrs = combineAttributes 
              stWithNewAtts.vAttrs mContext.cState.vAttrs; } in
        (finalSt, finalNotify)
    end

  method private extractMergeMallocs mContext sum calleeK =
    (* Go through summary and collect any new "heap" cells *)
    let isNewGlobal sumAtts var =
      isGlobalDebug "extractMalloc" false sumAtts var
    in
    let collectExtendNewMallocs () = 
      let myKey = fkey_of_sumKey funSumKey in
      let newMappings = UnifLocs.makeMappings 7 in
      let newMallocs = VarMap.fold 
        (fun var v mals ->
           match var with
             FHeap hi ->
               (* Only extend non-globals allocated by callee...
                  Other heap vars should just be brought in as is... *)
               if funAllocedHID calleeK hi.hID && 
                 not (isNewGlobal sum.fpOut.vAttrs var) 
                 && not (VarMap.mem var mContext.cState.bindings)
               then begin
                 let newHID = extendHeapID (getCurrentPP (), myKey) hi.hID in
                 if newHID = hi.hID then begin
                   VarSet.add var mals
                 end else
                   (let newHVar = FHeap { hID = newHID; } in
                    UnifLocs.updateMap newMappings newHVar (var, noOff); 
                    VarSet.add newHVar mals) 
               end else begin
                 VarSet.add var mals
               end
           | FRet _ | FVar _ | FInput _ -> mals
        ) sum.fpOut.bindings VarSet.empty in
      let sumOut = updateStateMappings newMappings sum.fpOut in
      { sum with fpOut = sumOut; } , newMallocs
    in
    
    (** Partition summary for the new mallocs *)
    let extractValAttsForNew sum mContext newMallocs = 
      VarSet.fold
        (fun var (sum, mals, mContext) ->
           if not (VarMap.mem var sum.fpOut.bindings) then
             failwith ("NF: " ^ string_of_var var);
           let v = VarMap.find var sum.fpOut.bindings in
           let v, cont = substVal false sum v mContext in
           
           checkDiff "extractVAFN" mContext.cState cont.cState;

           let mals = 
             try 
               let att = VarMap.find var sum.fpOut.vAttrs in
               VarMap.add var (v, att) mals
             with Not_found ->            
               (* in case sliceAttr knocked out the vAttr from diff *)
               try 
                 let att = VarMap.find var cont.cState.vAttrs in
                 VarMap.add var (v, att) mals
               with Not_found ->
                 let pp = getCurrentPP () in
                 failwith (Printf.sprintf
                             "WTF: extractValAttFN no att %s @ %s (%d, %d)\n" 
                             (string_of_var var) (string_of_loc !currentLoc)
                             pp.pp_stmt pp.pp_instr);
           in
           ( { sum with fpOut =
                 { bindings = VarMap.remove var sum.fpOut.bindings;
                   vAttrs = VarMap.remove var sum.fpOut.vAttrs; }; }, 
             mals, cont )
        ) newMallocs (sum, VarMap.empty, mContext)
    in
    
    let sum, newMallocs = collectExtendNewMallocs () in
    let sum, newMallocs, mContext = 
      extractValAttsForNew sum mContext newMallocs in

    (* May also have fresh mallocs that were made global before returning
       and so have no binding... Those will be handled elsewhere  *)

    (** Now try merging these malloc guys w/ existing malloc vars or
        if they are new, just bring in their vals and attribs here *)
    let mContext = 
      { mContext with 
          cState = VarMap.fold 
          (fun var (v, att) cur -> 
             mergeMallocs var v att cur
          ) newMallocs mContext.cState; } in
    mContext, sum



  (** Update attributes after values have been updated -- 
      also unifies new globals from callee w/ vars known in caller *)
  method private updateAttributes mContext sum =
    let collectNewGlobal cont sumVar (callerVar, off) newGlobals notifyGR =
      let notifyGR = 
        if isNoOffsetSum off then begin
          Fp_rci_globals.doUnify notifyGR sumVar callerVar noOff
        end else begin
          (* pass an offset into doUnify *)
          if not (!nonUniformPtrArith) then begin
            logErrorF "NOT UNIFYING %s <--(%d)-- %s\n"
              (string_of_var sumVar) (int_of_off off) (string_of_var callerVar);
            notifyGR
          end else 
            Fp_rci_globals.doUnify notifyGR sumVar callerVar (int_of_off off)
        end;
      in
      (VarSet.add callerVar newGlobals, notifyGR)
    in
    
    let addAttToCurContext cont var att =
      let stWithAtt = { cont.cState with vAttrs = 
          VarMap.add var att cont.cState.vAttrs; } in
      { cont with cState =  stWithAtt }
    in

    let mergeCollectIfNewGlobal sumVar sumAtt (var, off) 
        (cont, atts, newGlobals, notifyGR) =
      let readerLoc = self#getCurrentStep () in
      match var with
        FInput _ | FHeap _ ->
          (try
             let oldAtt = VarMap.find var atts in
             let newGlobals, notifyGR  = 
               match sumAtt.vKind, oldAtt.vKind with
                 HGlobal, HGlobal -> 
                   (* May still need to unify the two...? *)
                   collectNewGlobal cont sumVar (var, off) newGlobals notifyGR
               | HGlobal, _ ->
                   collectNewGlobal cont sumVar (var, off) newGlobals notifyGR
               | _, _ -> (newGlobals, notifyGR)
             in
             let newAtt = 
               if isNoOffsetSum off then combineAttr sumAtt oldAtt 
               else (* else don't combine the type info *)
                 { oldAtt with 
                     vKind = combineVarKinds sumAtt.vKind oldAtt.vKind; }
             in

             (* if we don't have a binding -- add it to mContext ?
                Don't add if it is an input-based var though, as
                we need to keep the atts in the pre-call-state for 
                applying the mods... oh hmm, it might have gotten the attrib
                from the flow-insensitive type info *)
             let cont = 
               if not (isInputNode var) && 
                 not (VarMap.mem var cont.cState.bindings) then begin
                   addAttToCurContext cont var newAtt
                 end else cont
             in
             
             (cont, VarMap.add var newAtt atts, newGlobals, notifyGR)
           with Not_found ->
             (* new cell -- add it to mContext *)
             let cont = addAttToCurContext cont var sumAtt in
             let newGlobals, notifyGR = 
               match sumAtt.vKind with
                 HGlobal -> 
                   collectNewGlobal cont sumVar (var, off) newGlobals notifyGR
               | _ -> (newGlobals, notifyGR)
             in
             (cont, VarMap.add var sumAtt atts, newGlobals, notifyGR)
          )
      | FVar _ ->
          (match sumAtt.vKind with
             HGlobal ->
               (* May be that we want to unify a summary var *p w/ 
                  an existing global g2... However, just copy the values
                  from the existing global into the global sum var for now *)
               let notifyGR = if isGlobalFVar cont.cState.vAttrs var then
                 if isNoOffsetSum off then
                   (* TODO: make a link for reads, instead of copying *)
                   (* doUnify notifyGR var sumVar *)
                   doCopy readerLoc notifyGR sumVar var 
                 else begin
                   (* TODO: pass an offset into doCopy? *)
                   logErrorF "NOT COPYING %s <- %s @ %s\n"
                     (string_of_var sumVar) (string_of_pointer (var, off))
                     (string_of_loc !currentLoc);
                   notifyGR
                 end
               else begin
                 logErrorF "Local unified w/ global? %s\n" (string_of_var var);
                 notifyGR
               end
               in
               (cont, atts, newGlobals, notifyGR)
           | _ -> (cont, atts, newGlobals, notifyGR))
      | FRet _ ->
          logErrorF "Ret unified w/ global? %s\n" (string_of_var sumVar);
          (cont, atts, newGlobals, notifyGR)
    in
    VarMap.fold
      (fun sumVar sumAtt (mContext, newAtts, newGlobals, notifyGR) ->
         let callerLocs, cont = 
           Stat.time "substVar" (substVar sum sumVar) mContext in

         (* Ugh... if substVar brings in extra atts for mContext, 
            those should go in newAtts as well -- this may happen rarely?
            Is there a way to make this more efficient (e.g., have substVar
            record the extended vars?) *)
         let newAtts = 
           if cont.cState == mContext.cState then newAtts
           else 
             let moreAtts = makeDiffMapNF newAtts cont.cState.vAttrs in
             makeOverrideVarM newAtts moreAtts 
         in
         
         let callerLocs = stripSpecial callerLocs in
         FLocSet.fold (mergeCollectIfNewGlobal sumVar sumAtt) 
           callerLocs (cont, newAtts, newGlobals, notifyGR)
      ) sum.fpOut.vAttrs 
      (mContext, mContext.cState.vAttrs, VarSet.empty, emptyNotifyGR)


  method private updateAttribTransitive mContext newAtts newGlobals =
    let tempSt = { mContext.cState with vAttrs = 
        combineAttributes mContext.cState.vAttrs newAtts; } in
    VarSet.fold 
      (fun newg curSt ->
         try
           let v = getBinding newg curSt in
           checkEscape newg curSt v
         with Not_found -> curSt
      ) newGlobals tempSt

  method private applyRetValue retOpt mContext sum =
    match retOpt with
      Some lv ->
        let sumRetVal = getRetValue sum.fpOut in
        let sumRetVal, cont = substVal false sum sumRetVal mContext in

        let sum = { sum with fpOut = removeRetVal sum.fpOut } in
        (* Ok, now do the assignment *)
        let newSt, toNotify = self#doAssign lv sumRetVal cont.cState in

        { cont with cState = newSt; }, toNotify, sum
    | None ->
        let sum = { sum with fpOut = removeRetVal sum.fpOut } in
        mContext, emptyNotifyGR, sum

  method private applyStoreUpdates mContext sum =
   let applySumVarVal sumVar sumVal (mContext, postSt, notify) =
      let callerLocs, cont1 = substVar sum sumVar mContext in
      let callerLocs = stripSpecial callerLocs in
      let callerVal, cont2 = substVal false sum sumVal cont1 in
      (* What if the width is incomplete? *)
      let sinfo = structInfoVal callerVal in
      let sumVarT = 
        typeOfLocChoice sum.fpOut.vAttrs sinfo (sumVar, noOffSum) in
      let postSt, notify = FLocSet.fold
        (fun (var, off) (postSt, notify) ->
           (* also get the type from caller and combine that w/ sumVarT
              to see if it is more accurate ? *)
           let callerT = typeOfLocChoice postSt.vAttrs sinfo (var, off) in
           let expectedT = 
             findTyp (combineTypes (addTyp sumVarT) (addTyp callerT)) in
           let postSt, moreNotify = 
             self#doOverride callerVal expectedT (var, off) 
               postSt cont2.cState in
           postSt, combineNotifyGR moreNotify notify
        ) callerLocs (postSt, notify) in
      (cont2, postSt, notify)
    in
    let mContext, postSt, notify =
      VarMap.fold applySumVarVal sum.fpOut.bindings 
        (mContext, mContext.cState, emptyNotifyGR) in
    ({ mContext with cState = postSt; }, notify)
      

  (* Ugh... duplicating a lot of code from assignVar... but this is
     certainly handled differently in one specific case 

     Also, do we need the curSt vs startSt thing again? 
     checkEscape can change the globalness of vars, but does it
     matter that we use the update status?  *)
  method private doOverride diffVal expectedT (var, off) curSt startSt =
    if nullVar#isVar var then begin
      (* raise NullException *)
      logErrorF "write to null @ %s\n" (string_of_loc !currentLoc);
      (curSt, emptyNotifyGR)
    end else if nfpVar#isVar var || extVar#isVar var then
      (curSt, emptyNotifyGR)
    else if isFunctionFVar curSt.vAttrs var then
      (logErrorF "write to function: %s <- %s @ %s\n" 
         (string_of_var var) (string_of_val diffVal)
         (string_of_loc !currentLoc);
       (curSt, emptyNotifyGR))
    else begin
      let curSt, moreNotify =
        if isGlobalDebug "doOverride" true startSt.vAttrs var then
          (checkEscape var curSt diffVal, emptyNotifyGR)
        else (curSt, emptyNotifyGR)
      in      
      let curSt = match diffVal with
          FNFP _ | FInt _ | FpRef _ | Refs _ ->
            (* as usual *)
            let bits = int_of_off off in
            if canStrongUpdVar curSt var off
            then strongUpdate curFunc expectedT curSt var bits diffVal
            else weakUpdate curFunc expectedT curSt var bits diffVal
              
        | FIRecs _ ->
            (* It already is more of a weak update *)
            weakUpdate curFunc expectedT curSt var (int_of_off off) diffVal
              
        | Records recs ->
            (* Only different for records really *)
            let bits = int_of_off off in 
            let recs = 
              match alignRecordOff bits diffVal with
                Records recs -> 
(*
                  if not (isNoOffset bits) then begin
                    logStatusF "TODO: double-check alignOff override: %s %s\n"
                      (string_of_var var) (string_of_offsetval bits);
                    printVal (Records recs) None
                  end;
*)
                  recs 
              | _ -> failwith "alignRecordOff returned non record" in
            let newVal =
              try
                let oldval = getBinding var startSt in  
                let oldrecs = match oldval with
                    FNFP _ | FInt _ | FpRef _ | Refs _ ->
                      promoteToRecord oldval
                  | FIRecs _ -> failwith "TODO: doOverride w/ old FIRec"
                  | Records oldrecs -> oldrecs 
                  | NCRecord (fm, m) -> [(fm, m)]
                in
                let strongUp = canStrongUpdVar curSt var off in
                let newrecs = 
                  List_utils.mapCross
                    (fun (oldfm, oldm) ->
                       List.map 
                         (fun (newfm, newm) ->
                            (makeOverrideOM oldfm newfm, max oldm newm)
                         ) recs
                    ) oldrecs in

                if strongUp then
                  (* TODO: limit recs first? *)
                  if tooManyRecords newrecs
                  then NCRecord (recSetToNCRec newrecs)
                  else makeNormalizedRec newrecs
                else
                  let base = valBaseSome curSt var in
                  combineVals base (Records newrecs) (Records oldrecs)
                    
              with Not_found -> Records recs in
            addBinding curSt var newVal
              
        | NCRecord (fm, m) ->
            let bits = int_of_off off in
            let fm, m = 
              match alignRecordOff bits diffVal with
                NCRecord (fm, m) -> fm, m
              | _ -> failwith "alignRecordOff returned non record" in
            let newval =
              try
                let oldval = getBinding var startSt in  
                let oldrecs = match oldval with
                    FNFP _ | FInt _ | FpRef _ | Refs _ ->
                      promoteToRecord oldval
                  | FIRecs _ -> failwith "TODO: doOverride w/ old FIRec"
                  | Records oldrecs -> oldrecs 
                  | NCRecord (fm, m) -> [(fm, m)]
                in
                let strongUp = canStrongUpdVar curSt var off in
                let newrecs = 
                  List.map
                    (fun (oldfm, oldm) ->
                       (makeOverrideOM oldfm fm, max oldm m)
                    ) oldrecs in

                if strongUp then 
                  NCRecord (recSetToNCRec newrecs)
                else
                  let base = valBaseSome curSt var in
                  combineVals base (NCRecord (recSetToNCRec newrecs)) 
                    (Records oldrecs)
              with Not_found -> NCRecord (fm, m) in
            addBinding curSt var newval
      in
      curSt, moreNotify
    end

      
  method private applyGlobMods mContext sum =
    let side = VarSet.fold 
      (fun calleeVar side ->
         { side with nonAliasG = VarSet.add calleeVar side.nonAliasG }
      ) sum.fpSide.nonAliasG funSideCond in
    self#updateSideCond side;
    mContext

  (** Return true if the mods are only to offset [0] *)
  method private modNoOff offs =
    OffsetSet.is_singletonX noOff offs

  method private applyModIns mContext sum =
    let adjustOffs offs baseOff =
      if isNoOffset baseOff then offs
      else OffsetSet.fold 
        (fun o cur -> OffsetSet.add (concatOffset o baseOff) cur) 
        offs OffsetSet.empty
    in

    let doMod side var off offs =
      let offs = adjustOffs offs off in
      match doAddModIn side var offs with
        None -> side | Some x -> x
    in

    let applyModFor mContext offs (var, off) side =
      match var with
        FInput _ ->
          (* If it's a global now... ignore *)
          let bits = int_of_off off in 
          if isGlobalDebug "applyModIns" false mContext.cState.vAttrs var then
            side
          else
            if self#modNoOff offs 
            then doMod side var bits offs
            else 
              (try
                 let t = typeOfLoc mContext.cState.vAttrs (var, off) in
                 if not (isStructType t || isImpreciseMalloc t) then begin
                   logErrorF "applyModIn skipping scalar %s\n" 
                     (string_of_pointer (var, off));
                   side
                 end else doMod side var bits offs
               with UnknownType ->
                 logErrorF "applyModIn unknownType %s\n" 
                   (string_of_pointer (var, off));
                doMod side var bits offs)

      | FHeap _ | FVar _ | FRet _ -> side
    in
    
    let side, mContext = VarMap.fold
      (fun calleeVar offs (side, mContext) ->
         let callerLocs, mContext = substVar sum calleeVar mContext in
         let side = FLocSet.fold (applyModFor mContext offs) callerLocs side in
         side, mContext
      ) sum.fpSide.modIn (funSideCond, mContext) in
    self#updateSideCond side;
    mContext

  method private applyFpAccs mContext sum =
    (** Convert callerVal to a series of input-based vars *)
    let rec collectCallerFpVarsHelp baseV callerVal cur =
      (match callerVal with
         FInt _ | FNFP _ -> 
           logErrorF "applyFpAccs: skip %s -> %s\n" 
             (string_of_var baseV) (string_of_val callerVal);
           cur
       | FpRef var2 -> 
           (* Hmm... can't use the same symbol for NULL and input FP... *)
           if initFPVar#isVar var2 then VarSet.add (reattachADeref baseV) cur
           else cur

       | Refs locs -> 
           FLocSet.fold 
             (fun (var2, o2) cur ->
                if initFPVar#isVar var2 then
                  if isNoOffsetSum o2 then
                    VarSet.add (reattachADeref baseV) cur
                  else (logError "Non-triv off for ext";
                        VarSet.add (reattachADeref baseV) cur)
                else cur 
             ) locs cur
             
       | Records recs ->
           List.fold_left
             (fun cur (fm, _) ->
                OffsetMap.fold 
                  (fun o v cur ->
                     let newBase = reattachFieldAcc baseV o in
                     collectCallerFpVarsHelp newBase v cur
                  ) fm cur
             ) cur recs

       | NCRecord (fm, m) -> 
           OffsetMap.fold 
             (fun o v cur ->
                let newBase = reattachFieldAcc baseV o in
                collectCallerFpVarsHelp newBase v cur
             ) fm cur
             
       | FIRecs fir -> failwith "TODO: collectCallerFpVars")
    in

    let collectCallerFpVars cont baseV baseOff callerVal cur =
      (* Watch for "EXT"-valued funptrs stored in input-based baseV *)
      match baseV with
        FInput baseAP ->
          if isGlobalDebug "applyFpAccs" true mContext.cState.vAttrs baseV 
          then cur
          else 
            let bits = int_of_off baseOff in
            let callerVal = 
              if isNoOffset bits then callerVal
              else alignRecordOff bits callerVal in
            collectCallerFpVarsHelp baseV callerVal cur 
      | FHeap _ | FVar _ | FRet _ -> cur
    in

    let mContext, side = VarMap.fold 
      (fun calleeVar calleeVal (mContext, side) ->
         try 
           let callerLocs, cont = substVar sum calleeVar mContext in
           checkDiff "applyFP" mContext.cState cont.cState;
           let callerVal, cont = substVal false sum calleeVal mContext in
           let side = FLocSet.fold 
             (fun (var, off) side ->
                let callerFPVars = 
                  collectCallerFpVars cont var off callerVal VarSet.empty in
                let side = VarSet.fold 
                  (fun var side -> 
                     match addFpAccs side var with 
                       None -> side | Some s -> s) callerFPVars side in
                side
             ) callerLocs side in
           cont, side
         with SubstNonLval ->
           logErrorF "applyFpAccs: subst fail %s\n" (string_of_var calleeVar);
           mContext, side
      ) sum.fpSide.initialFP (mContext, funSideCond) in
    self#updateSideCond side;
    mContext


  (************************************************************)

  (** Entry point for handling ASSUMEs at each branch of if-statements *)
  method handleGuard (gexp: exp) state : fpState Dataflow.guardaction =
    if stLat#isBottom state then Dataflow.GUnreachable
    else if isTopState state then Dataflow.GDefault
    else Dataflow.GDefault 
      (* we stopped making record splits that contain null (it's implicit) 
         so it wouldn't be able to decide if a null branch is 
         true or not anyway... *)
(*
  begin
      let foldedExp = Cil.constFold true gexp in
      let castFree = Cil_lvals.omitCast foldedExp in
      let notifySkip () =
(*        logStatusF "Skipping test: %s\n" (string_of_exp foldedExp);
*)
        Dataflow.GDefault  
      in
      (** Only checking against pointers and null *)
      let focusNull eq maybePtrExp =
        (* don't use null info unless we know it's relevant to FPs *)
        if (Type_reach.hitsFunptrDeep (Cil_lvals.typeOfUnsafe maybePtrExp)) 
        then begin
          let accPath = ref [] in
          let emisc = { lookupHook = captureAccessPath accPath; 
                        readerOpt = Some (self#getCurrentStep ());
                        assumedTyp = None; 
                        curFunc = curFunc; } in
          try
            let v, state = eval state maybePtrExp emisc in
            match v with 
              FInt _ -> 
                if eq then Dataflow.GDefault else Dataflow.GUnreachable
            | FpRef _
            | Refs _ ->
                let newState = 
                  if eq 
                  then focus accPath (nullVar#getLoc) state
                  else focusNot accPath (nullVar#getLoc) state in
                if stLat#isBottom newState 
                then Dataflow.GUnreachable
                else begin 
                  Dataflow.GUse (Fp_rci_hashcons.hc_state newState)
                end
            | Records _ | FNFP _ | FIRecs _ ->
                Dataflow.GDefault
          with NullException ->
            logStatusF "NullDeref @ handleGuard: %s\n" 
              (string_of_exp maybePtrExp);
            Dataflow.GDefault
        end else Dataflow.GDefault
      in
      let rec checkExp exp =
        match exp with
          BinOp (Ne, e, (Const (ckind) as e2), _)
        | BinOp (Ne, (Const (ckind) as e2), e, _) ->
            if isZero e2 then focusNull false e 
            else notifySkip ()

        | BinOp (Eq, e, (Const (ckind) as e2), _)
        | BinOp (Eq, (Const (ckind) as e2), e, _) ->
            if isZero e2 then focusNull true e 
            else notifySkip ()
              
        | Lval (_, _) -> (* Checking if lval is not null *)
            focusNull false foldedExp
        | UnOp (LNot, (Lval _ as ptrExp), _) ->
            focusNull true ptrExp

        (* handle !'s in succession *)
        | UnOp (LNot, UnOp (LNot, exp, _), _) ->
            checkExp exp
              
        (* handle negations of Ne and Eq *)
        | UnOp (LNot, BinOp (Ne, e1, e2, typ), _) ->
            checkExp (BinOp (Eq, e1, e2, typ)) 
        | UnOp (LNot, BinOp (Eq, e1, e2, typ), _) ->
            checkExp (BinOp (Ne, e1, e2, typ)) 

        | _ -> notifySkip ()
      in
      checkExp castFree
    end
*)

end

let transF = new fpTransFunc lattice

  
