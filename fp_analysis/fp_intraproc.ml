
(** Fancy function pointer analysis. Intraprocedural bits *)

open Cil
open Cildump
open Type_utils
open Pretty
open Stdutil
open Fp_types
open Fp_lattice_ops
open Fp_malloc
open Fp_slice
open Fp_store
open Fp_utils
open Fp_context_lim
open Fp_summary
open Globals_ref
open Summary_keys
open Logging
open Cildump


(** Package the state operations / lattice *)
class fpLattice = object (self) 
  
  val mutable thesums = sum

  method sums = thesums

  method setTheSums newSum = thesums <- newSum

  method eqStates st1 st2 = eqStModRep st1 st2
    
  method combineStates st1 st2 = Stat.time "FP combine" (combineStates st1) st2

  method eqSums sum1 sum2 = 
    Stat.time "FP sum eq" (eqSummaries sum1) sum2
    
  method combineSums sum1 sum2 =
    Stat.time "FP sum comb" (combineSummaries sum1) sum2

  method isBottom st = isBottomState st

  method isBottomSummary sum = isBottomSummary sum

  method isWildcardSummary sum = isWildcardSummary sum
    
  (***** Special values of the state *****)

  method bottom = bottomState

  (***** Debugging *****)
 
  method printSummary sumkey = printSummary thesums sumkey
      
  method printState st = printState st
  
end

let lattice = new fpLattice
let conLimiter = ref (new noLimitContext)

(*********************************************************
 * Intra-proc Dataflow Analysis
 *********************************************************)

(******** Debugging ********)

let debugFPCalls = Hashtbl.create 10

let markFPCall callerKey callStr loc =
  if Hashtbl.mem debugFPCalls (callerKey, callStr, loc) then ()
  else Hashtbl.replace debugFPCalls (callerKey, callStr, loc) []

let recordFPCall callerKey callStr loc target =
  let old = Hashtbl.find debugFPCalls (callerKey, callStr, loc) in
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


(************** Actual Dataflow ***************)

(* DEBUG *)

let printAddrs caption targets =
  let len = List.length targets in
  if len == 0 then ()
  else let doc = 
    text caption ++ text "{" ++
      seq_to_doc (Pretty.text ", ") 
      List.iter
      (fun (addr, off) -> 
         defPrint#d_pointerTarg (addr, off))
      targets nil ++ dprintf "} (%d)\n\n" len in
  logStatusD doc

(* /DEBUG *)


exception SummaryNotReady of sumKey * fpState

(* TODO: make the notify readers a hook instead of a return value? *)

(** Transfer func based on requirements of module IDF, not module Intra *)
class fpTransFunc stLat =
object (self)

  (* current function's id and inputState *)
  val mutable funInSt = stLat#bottom
  val mutable funSumKey = Callg.dummyFID
  val mutable curFunc = dummyFunDec
  val mutable funcs = Callg.emptyCG (* context-insensitive one...
                                       just serves as a list of functions *)

  method setFuncs f =
    funcs <- f

  (** Set transfer func to handle given function *)
  method handleFunc cfg (sumK, st) =
    if funSumKey = sumK then ()
    else begin
      funSumKey <- sumK;
      !Fp_agg_merge.myMerger#setFunc sumK;
      funInSt <- st;
      curFunc <- cfg;
      logStatusF "=============\nAnalyzing %s : %s\n" cfg.svar.vname
        (string_of_sumKey funSumKey);

    (* Hmmm... should we weaken all the locals at the very beginning? I.e., 
       they don't start as 0, they start as NON-FP if they are ints and such.
       Really hope ptr arith doesn't involve intermediate vars then...

       Anything else we can do to speed up fixpointing high-branch /
       loop / lines-of-code functions like printf? 

    *)
    end

  method private getCurrentStep () =
    (funSumKey, getCurrentPP ())

  method private getNfpSource () =
    makeNfpSource (curFunc.svar.vid)

  method getCallContext acts loc inState fkey =
    match self#getInputsForCall acts inState fkey with
      Some (inputs, mappedInputs, inState, formals) ->
        let context = getSumContext formals mappedInputs in
        let context, sumKey = Stat.time "findEqCont" 
          (!conLimiter#matchContext fkey) context in
        let inState, mappedInputs = 
          if isTopState context then
            (topState, topState)
          else
            (inState, mappedInputs)
        in
        Some (sumKey, inState, inputs, mappedInputs, formals)
    | None -> None


  (*********** Actual transfer func stuff **************)

  (** Will handle return statements, if-statements, separately *)
  method handleStmt (stmt: stmt) (state:fpState) =
    state

  (** Update summary. Return true if new *)
  method handleReturnStmt stmt state =
    flushGlobals ();
    let sumKey = funSumKey in
    let oldS = stLat#sums#find sumKey in
    let newS, toNotify = self#makeNewSummary stmt state in
    let toNotify = RS.elements toNotify in
    if stLat#isBottomSummary oldS then
      if stLat#isBottomSummary newS then (false, toNotify)
      else 
        (self#updateSummary sumKey newS; 
         (true, toNotify))
    else if isTopSummary oldS then (false, toNotify)
    else if isTopSummary newS then
      (self#updateSummary sumKey newS;
       (true, toNotify))
    else
      let comboS = stLat#combineSums newS oldS in
      if stLat#eqSums oldS comboS then (false, toNotify)
      else 
        (self#updateSummary sumKey comboS;
         (true, toNotify))

  method private updateSummary sumKey newS =
    logStatusF "New summary %s %s\n" (string_of_sumKey sumKey) 
      (string_of_loc !currentLoc);
    stLat#printState newS.fpOut;
    stLat#sums#addReplace sumKey newS
  (*  stLat#sums#serializeAndFlush (* do on-demand *) *)


  (** return list of variables pointed-to by formals *)
  method private rootsOfFormals state formals curRoots =
    let addIfRealTarget (v,o) cur =
      if not (isSpecialVar v) then v :: cur else cur
    in
    let emisc = { emptyMisc with 
                    readerOpt = Some (self#getCurrentStep ()); } in
    List.fold_left 
      (fun (curRoots, curState) f -> 
         let fTarg, curState = 
           eval curState (Lval (Var (f), NoOffset)) emisc in
         match fTarg with
           Refs ls -> (FLocSet.fold addIfRealTarget ls curRoots, curState)
         | _ -> (curRoots, curState)
      ) (curRoots, state) formals


  method private makeNewSummary stmt state =
    let inputSt = funInSt in 
    if stLat#isBottom state then 
      ( { fpIn = inputSt; fpOut = state; }, RS.empty )
    else if isTopState state then 
      ( { fpIn = inputSt; fpOut = state; }, RS.empty )
    else
      try
        let state, roots =
          (match stmt.skind with
             Return (Some (exp), l) ->
               let emisc = { emptyMisc with
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
               let state = assignRetValue state retVar rv in
               (state, [retVar])
                 
           | Return (None, l) -> 
               (state, [])
                 
           | _ -> failwith "makeNewSummary: given non return statement" ) in
        (* May have died doing the assignment ? *)
        if stLat#isBottom state || isTopState state then begin
          logErrorF "WTF: state is bottom/top making sum? %s\n"
            (string_of_loc !currentLoc);
          ( { fpIn = inputSt; fpOut = state; }, RS.empty )
        end else begin
          (* Check things reachable from OLD formals *)
          let formals = curFunc.sformals in
          let roots, inputSt = self#rootsOfFormals inputSt formals roots in
          let state, toNotify = dropGlobalAssume state in
          (*        let sliced = sliceState state roots in *)
          let sliced = Stat.time "weaken/GC"
            (weakenState (Some roots) (self#getNfpSource ())) state in

          (* !Fp_agg_merge.myMerger#setVerbose "sum"; *)
          let sliced = !Fp_agg_merge.myMerger#aggressiveMergeAll sliced in
          (* !Fp_agg_merge.myMerger#setVerbose ""; *)

          let res = diffState inputSt sliced in
          ( { fpIn = inputSt; fpOut = res; }, toNotify)
        end
      with NullException ->
        ( { fpIn = inputSt; fpOut = stLat#bottom; }, RS.empty)




  method handleASM (atts:attributes) (templs:string list) 
    (cos:(string option * string * lval) list)
    (cis: (string option * string * exp) list) 
    (clobs:string list) (loc:location) (inState:fpState) =
    inState

  (** Entry point for handling assignment instructions *)
  method handleAssign lhsLval rhs (loc:location) inState : (fpState * greader list) =
    if dontFlowFuncState inState 
    then inState, []
    else try
      (* Not using focus for the RHS vs LHS here... *)
      let emisc = { emptyMisc with 
                      readerOpt = Some (self#getCurrentStep ()); } in
      let rhs, inState = eval inState rhs emisc in
      match assignedMalloc rhs with
        Some t -> 
          let st, toNotify = self#doMalloc lhsLval t inState in
          Fp_hashcons.hc_state st, RS.elements toNotify
      | None -> 
          let st, toNotify = self#doAssign lhsLval rhs inState in
          Fp_hashcons.hc_state st, RS.elements toNotify
    with NullException -> 
      logError ("Caught Null Deref @ " ^ string_of_loc !currentLoc);
      stLat#bottom, []


  method private doAssign lhsLval rhs state =
    let t = Cil_lvals.typeOfLvalUnsafe lhsLval in
    let sinfo = getStructInfo t in
    let rhs = filterImpreciseWrite rhs t in
    let rhs, state = checkAlpha rhs t state in
    let rhs = nfpOutType (self#getNfpSource ()) t rhs in
    match lhsLval with
      (* [COPY] (vi.off) = rhs *)
      (Var(vi), off) ->
        let var, off = getLocation vi off in
        let st, toNotify = assignVar state var off rhs true sinfo in
        st, toNotify
          
    (* [STORE] *e.outerOff = rhs *)
    | (Mem(ptrExp), outerOff) ->
        let accessPath = ref [] in
        let locsHook = captureAccessPath accessPath in
        let emisc = { lookupHook = locsHook;
                      readerOpt = Some (self#getCurrentStep ()); 
                      assumedTyp = None; } in
        let ptrVal, newOff, state = 
          getPtrVal state ptrExp outerOff emisc in
        self#assignToPointerTarget ptrExp sinfo
          accessPath ptrVal newOff rhs state 


  method private assignToPointerTarget ptrExp sinfo accPath 
    ptrVal outerOff rhs state =
    match ptrVal with
      Refs ls ->
        (* Focus the state first, then update -- or don't bother to focus *)
        let ls = stripSpecial ls in
        let strong = FLocSet.is_singleton ls in
        let finalSt, toNotify = 
          FLocSet.fold 
            (fun (addr, innerOff) (curSt, notify) ->
               let startState = state in
               let newOff = concatOffsetVar addr outerOff innerOff in
               try 
                 let newState, toNotify = 
                   assignVar startState addr newOff rhs strong sinfo in
                 let newState = match curSt with
                     None -> Some (newState)
                   | Some x -> Some (stLat#combineStates x newState) in
                 newState, RS.union notify toNotify
               with NullException ->
                 curSt, notify
            ) ls (None, RS.empty) in
        (match finalSt with
           None -> 
             logErrorF "NULL: assignToPointer %s -> %s\n" 
               (string_of_exp ptrExp) (string_of_val ptrVal);
             stLat#bottom, toNotify
         | Some x -> x, toNotify)
          
    | FpRef vid -> 
        let vi = varinfoOfVid vid in
        logErrorF "doAssign on function %s @ %s\n" vi.vname
          (string_of_loc !currentLoc);
        state, RS.empty

    | FNFP _ -> 
        (* Ok, if we have an invariant that FNfp is only for pointers that
           NEVER reach FPs in anyway, we still need to havoc the targets
           if we want to handle misc errors like nullptr derefs or
           if we prune branches based on these non-FP reaching pointers ? *)
        state, RS.empty
    | FInt i64 -> 
        logError ("NULL: assignToPtr " ^ (string_of_exp ptrExp));
        raise NullException
    | Records _ | FIRecs _ ->
        logError ("doAssign: rec as ptr for store: " ^ 
                      (string_of_exp ptrExp));
        state, RS.empty

  (** Process an assignment that is of the form "x = malloc" *)
  method private doMalloc lhs typ inSt =
    (* Check if there is already a binding and merge if there is. *)
    let pp = getCurrentPP () in
    let fkey = fkey_of_sumKey funSumKey in
    let malVar, hKind = makeMalloc typ (pp, fkey) in
    let st = mergeMallocs malVar (defaultVarVal malVar) hKind inSt in
    (* Assign to the lhs a pointer to the heap variable *)
    (* Allow null for now *)
    let ptr = makeMayref (malVar, noOff) (nullVar#getLoc) in
    self#doAssign lhs ptr  st


  (** Entry point for handling calls [retOpt = callExp(acts)] *)
  method handleCall retOpt callexp (acts:exp list) (loc:location) inState =
    if stLat#isBottom inState then 
      [], [], None
    else if isTopState inState then
      self#handleTopCall callexp loc inState
    else match callexp with
      Lval(Var(vi),NoOffset) ->
        let st, moreNotify = dropGlobalAssume inState in
        (try 
           let (st, toNotify), calleeQ = 
             self#doCall retOpt acts loc st vi [] in
           (calleeQ, RS.elements (RS.union moreNotify toNotify), 
            Some (Fp_hashcons.hc_state st))
         with SummaryNotReady (sumKey, inputs) ->
           [(sumKey, inputs)], [], None
         | NullException ->
             self#handleNullCall callexp)
          
    | Lval(Mem(callExp), NoOffset) ->
        (* DEBUG *)
        let callStr = string_of_exp callExp in
        markFPCall funSumKey callStr loc;
        (* /DEBUG *)

        let accessPath = ref [] in
        let emisc = { lookupHook = captureAccessPath accessPath;
                      readerOpt = Some (self#getCurrentStep ()); 
                      assumedTyp = None; } in
        (try
           let ptrVal, inState = eval inState callExp emisc in
           self#handleFPCall retOpt callExp callStr 
             ptrVal accessPath acts loc inState
         with NullException ->
           self#handleNullCall callExp)
    | CastE (t, e') ->
        (* Don't need the assumedTyp ? *)
        self#handleCall retOpt e' acts loc inState
    | Lval _ | StartOf _ | AddrOf _ | BinOp _ | UnOp _ | Const _ 
    | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ | AlignOf _ ->
        failwith "unknown call expression"


  method private handleTopCall callExp loc st  =
    (* if topState then we DO want to flow to callees (under a special 
       TOP context) but this means we need access to the FI data
       to get callees if it's an FP deref? *)
    match callExp with
      Lval(Var(vi),NoOffset) ->
        if fHasBody funcs vi.vid then
          let sumKey = makeTopSumKey vi.vid in
          [(sumKey, st)], [], Some (st)
        else 
          [], [], Some (st)

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
             if functionTypeMatches vi.vtype (getFunSigPtrExp callExp) then 
               begin
                 (* DEBUG *)
                 recordFPCall funSumKey callStr loc vi.vname;
                 if fHasBody funcs vi.vid then
                   let sumKey = makeTopSumKey vid in
                   (sumKey, st) :: callees
                 else callees
               end else callees
          ) [] targets in
        calleeQ, [], Some (st)

    | CastE (t, e') ->
        self#handleTopCall e' loc st
    | Lval _ | StartOf _ | AddrOf _ | BinOp _ | UnOp _ | Const _ 
    | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ | AlignOf _ ->
        failwith "unknown call expression"



  method private handleFPCall retOpt callExp callStr ptrVal accessPath 
    acts loc inSt =
    match ptrVal with
      FpRef vid ->
        if nullVar#isVID vid then 
          self#handleNullCall callExp
        else if (nfpVar#isVID vid) then begin
          self#informFunkyCall "nfp" callExp;
          ([], [], Some inSt)
        end else if extVar#isVID vid then begin
          recordFPCall funSumKey callStr loc "EXT";
          ([], [], Some inSt)
        end else begin 
          let vi = varinfoOfVid vid in 
          if functionTypeMatches vi.vtype (getFunSigPtrExp callExp) then begin
            (* DEBUG *)
            recordFPCall funSumKey callStr loc vi.vname;
            (* /DEBUG *)
            try 
              let st, moreNotify = dropGlobalAssume inSt in
              let (st, toNotify), calleeQ = 
                self#doCall retOpt acts loc st vi [] in
              (calleeQ, RS.elements (RS.union moreNotify toNotify), 
               Some (Fp_hashcons.hc_state st))
            with 
              SummaryNotReady (sumKey, inputs) ->
                [(sumKey, inputs)], [], None
            | NullException ->
                self#handleNullCall callExp 
          end else
            self#handleAuxCall callExp ptrVal
        end
    | Refs ls ->
        let callSig = getFunSigPtrExp callExp in
        let st, moreNotify = dropGlobalAssume inSt in
        (* Should we NOT focus and stuff if it's only a single target? *)
        let doFPTarget (addr, o) (delay, notify, curSt) =
          if nullVar#isVar addr then begin
            self#informFunkyCall "null" callExp;
            (delay, notify, curSt)
          end else if nfpVar#isVar addr then begin
            self#informFunkyCall "nfp" callExp;
            (delay, notify, curSt)
          end else if extVar#isVar addr then begin
            recordFPCall funSumKey callStr loc "EXT";
            (delay, notify, curSt)
          end else 
            match addr with
              FVar vid ->
                let vi = varinfoOfVid vid in
                if functionTypeMatches vi.vtype callSig then begin
                  let startSt = focus accessPath (addr, o) st in
                  if stLat#isBottom startSt 
                  then (delay, notify, curSt)
                  else try
                    (* DEBUG *)
                    recordFPCall funSumKey callStr loc vi.vname;
                    (* /DEBUG *)
                    
                    let (newSt, toNotify), calleeQ = 
                      self#doCall retOpt acts loc startSt vi delay in
                    let newSt = 
                      (match curSt with
                         None -> Some (newSt)
                       | Some x -> 
                           let combo = stLat#combineStates x newSt in
                           Some combo) in
                    (calleeQ, RS.union notify toNotify, newSt)
                  with 
                    SummaryNotReady (sumKey, inputs) ->
                      (sumKey, inputs) :: delay, notify, curSt
                  | NullException ->
                      delay, notify, curSt
                end else begin
                  self#informFunkyCall "aux" callExp;
                  logErrorF "var: %s\n" (string_of_var addr);
                  delay, notify, curSt
                end      
            | FHeap _ | FRet _ | FInput _ -> 
                self#informFunkyCall "aux" callExp;
                logErrorF "var: %s\n" (string_of_var addr);
                delay, notify, curSt
        in
        let calleeFlows, notify, st =
          FLocSet.fold doFPTarget ls ([], moreNotify, None) in
        let st = match st with None -> st 
          | Some x -> Some (Fp_hashcons.hc_state x) in
        calleeFlows, RS.elements notify, st
    | FInt _ ->
        self#handleNullCall callExp
    | FNFP ts ->
        logErrorF "FP is Nfp? %s @ %s\n" (sprint 80 (d_nfpsources ts))
          (string_of_loc !currentLoc);
        ([], [], Some inSt)
    | Records _ | FIRecs _ -> 
        self#handleAuxCall callExp ptrVal

  method private informFunkyCall str callExp =
    logErrorF "Potential %s fp call: %s @ %s\n" str 
      (string_of_exp callExp) (string_of_loc !currentLoc)
      
  method private handleNullCall callExp =
    self#informFunkyCall "null" callExp;
    ([], [], None)

  method private handleAuxCall callExp ptrVal =
    self#informFunkyCall "aux" callExp;
    logErrorF "ptrVal: %s\n" (string_of_val ptrVal);
    ([], [], None)
      
  method private doCall retOpt acts loc inState (fvar:varinfo) queue = 
    let fkey = fvar.vid in
    let useSummary inputs mappedInputs formals inState summary sumKey =
      (* Take the input (i - formals) and merge with inState so
         that the diff wrt to input makes sense *)
      let iB = List.fold_left (fun cur var -> VarMap.remove var cur) 
        summary.fpIn.bindings formals in
      let i = { summary.fpIn with bindings = iB; } in
      let diff = Stat.time "revMap" 
        (Fp_generalize.reverseMapping inputs formals sumKey) summary.fpOut in
      let i = Stat.time "revMap"
        (Fp_generalize.reverseMapping inputs formals sumKey) i in
      let inState = makeOverride inState i in 
      (* use this to "merge" instead because i has fewer bindings
         and we don't want those to come into play... *)
      self#applySummary retOpt acts loc inState fkey diff
    in
    (* Maybe it's a malloc that we missed during translation -- 
       e.g., if it's an indirect call *)
    if isMallocFun fvar then
      (match retOpt with 
         Some (lhsLval) ->
           let t = Cil_lvals.typeOfLvalUnsafe lhsLval in
           let retTyp = match Cil_lvals.unrollTypeNoAttrs t with
               TPtr (retTyp, _) ->
                 Trans_alloc.pickType retTyp acts
             | _ -> 
                 logErrorF "MALLOC: Lval is not ptr %s @ %s\n"
                   (string_of_lval lhsLval) (string_of_loc !currentLoc);
                 Trans_alloc.pickType (TPtr ((TVoid []), [])) acts
           in
           let st, toNotify = self#doMalloc lhsLval retTyp inState in
           ((Fp_hashcons.hc_state st, toNotify), queue)
       | None ->
           ((inState, RS.empty), queue))
    else try
      match self#getCallContext acts loc inState fkey with
        None ->
          self#warnNoBody fvar.vname;
          let retType = 
            match retOpt with
              None -> TVoid []
            | Some lv -> Cil_lvals.typeOfLvalUnsafe lv in
          let sum : sumval = makeUnknownSummary retType in
          (self#applySummary retOpt acts loc inState fkey sum.fpOut, 
           queue)
            
      | Some (sumKey, inState, inputs, mappedInputs, formals) ->
          if stLat#isBottom inState then begin
            logErrorF "NullDeref getting context? %s @ %s\n" 
              fvar.vname (string_of_loc !currentLoc);
            (bottomState, RS.empty), queue
          end 
          else if isTopState inState then 
            (inState, RS.empty), (sumKey, mappedInputs) :: queue 
          else begin
            let summary : sumval = stLat#sums#find sumKey in
            if stLat#isBottomSummary summary then
              raise (SummaryNotReady (sumKey, mappedInputs)) 
                (* let doReturn update the inputs *)
            else if isTopSummary summary then
              (topState, RS.empty), queue
            else if stLat#isWildcardSummary summary then
              (useSummary inputs mappedInputs formals inState summary sumKey, 
               queue) (* Don't need to queue call for wildcard funcs *)
            else begin
              let newInput = stLat#combineStates summary.fpIn mappedInputs in
              if stLat#eqStates summary.fpIn newInput then
                (useSummary inputs newInput formals inState summary sumKey, 
                 (sumKey, newInput) :: queue)
              else begin
                raise (SummaryNotReady (sumKey, newInput))
              end
            end
          end
    with NullException ->
      logErrorF "NullDeref eval'ing actuals %s @ %s\n" fvar.vname
        (string_of_loc !currentLoc);
      (bottomState, RS.empty), queue

  val warnedNoBody = Hashtbl.create 17

  method private warnNoBody name =
    if Hashtbl.mem warnedNoBody name then ()
    else begin
      Hashtbl.add warnedNoBody name ();
      logStatusF "No body for func: %s\n" name
    end
        
  (** Copy values of actuals to formals, then slice wrt to formals and 
      globals referenced by target. Rename heap nodes too? *)
  method private getInputsForCall acts inState fkey =
    let makeInitial (var, o) cur =
      match var with
        FHeap _ -> 
          logError ("no initial val for heap var? " ^ (string_of_var var));
          let v = defaultVarVal var in
          addBinding cur var v
      | FVar _ -> 
          let v = defaultVarVal var in
          addBinding cur var v
      | FRet _ -> failwith "makeInitial: retvar"
      | FInput _ -> failwith "makeInitial: invar"
    in
    let rec pullTransitiveVals curInst cur v =
      foldTargetsVal (pullBinding curInst) v cur 
    and pullBinding curInst (var, o) cur =
      if hasBinding var cur then cur
      else 
        if isGlobalDebug "pullB" false curInst var then 
          (match var with 
             FHeap _ | FInput _ -> addHeapAttr var HGlobal cur
           | FVar _ | FRet _ -> cur)
        else 
          try 
            let v = getBinding var curInst in
            let cur = addBinding cur var v in
            let cur = 
              match var with
                FHeap _ | FInput _ -> 
                  let att = getHeapAttr var curInst in
                  addHeapAttr var att cur
              | FVar _ | FRet _ -> cur in
            pullTransitiveVals curInst cur v
          with Not_found ->
            logStatus ("getInputs: not found " ^ string_of_var var ^
                         " " ^ string_of_loc !currentLoc);     
            makeInitial (var, o) cur
    in
    match getFormals funcs fkey with
      Some (fnode, formals) ->
        if fnode.Callg.hasBody then begin
          let formals, acts = self#trimActuals formals acts fnode in
          let formals = List.map 
            (fun f -> let fv, _ = getLocation f NoOffset in fv) formals in
          let emisc = { emptyMisc with 
                          readerOpt = Some (self#getCurrentStep ()); } in
          let inputs, inState = List.fold_left2
            (fun (cur, curInSt) form act -> 
               let v, curInSt = eval curInSt act emisc in
               let t = typeOfFVar form in
               let v, curInSt = checkAlpha v t curInSt in
               let v = nfpOutType (self#getNfpSource ()) t v in

               let cur = addBinding cur form v in
               (pullTransitiveVals curInSt cur v, curInSt)
            ) (emptyState, inState) formals acts in

          let inputs = gcState (Some formals) inputs in
          let mappedInputs = 
            Stat.time "generalize" 
              (Fp_generalize.genStoreMapping inputs formals) fkey in
          Some (inputs, mappedInputs, inState, formals)
        end else
          None
    | None ->
        None

  (** Hack to reduce the number of actuals in case the function takes VarArgs *)
  method private trimActuals formals actuals fnode =
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

  method private updateStoreSummary actuals inStore sumStore =
    makeOverrideMaps inStore sumStore

  (** Update attributes after values have been updated *)
  method private updateAttributes st summary =
    let newAtts = VarMap.fold
      (fun sumVar sumAtt newAtts ->
         (match sumAtt with
            HGlobal -> 
              (* What if it was already known to be global *)
              (try 
                 let v = VarMap.find sumVar st.bindings in
                 let readers, _ = updateGlobalBinding sumVar v in
                 (* Shouldn't have any readers yet... 
                    TODO merge this heap global w/ one in callee
                    as they are really aliases ... *)
                 if not (RS.is_empty readers) then
                   logError "TODO: notify glob reader" 
               with Not_found -> ())
          | _ -> ());
         VarMap.add sumVar sumAtt newAtts
      ) summary.hAttrs st.hAttrs in
    { st with hAttrs = newAtts; }


  method private applySummary retOpt acts loc inState calleeK summary =
    (* Go through summary and collect any new "heap" cells *)
    let myKey = fkey_of_sumKey funSumKey in
    let isNewGlobal var =
      try isGlobalFVar summary var with Not_found -> false
        (* Not found means it was already a global before *)
    in
    let collectExtendNewMallocs () = 
      let newMappings = makeMappings 7 in
      let newMallocs = VarMap.fold 
        (fun var v mals ->
           match var with
             FHeap hi ->
               (* Only extend non-globals allocated by callee *)
               if funAllocedHID calleeK hi.hID && not (isNewGlobal var) 
                 && not (VarMap.mem var inState.bindings)
               then
                 (let newHID = extendHeapID (getCurrentPP (), myKey) hi.hID in
                  if newHID == hi.hID then mals
                  else
                    (let newHVar = FHeap { hi with hID = newHID; } in
                     updateMap newMappings newHVar var; 
                     VarSet.add newHVar mals) )
               else mals
           | FRet _ | FVar _ | FInput _ -> mals
        ) summary.bindings VarSet.empty in
      let summary = updateStateMappings newMappings summary in
      summary, newMallocs
    in
    
    (** Partition summary for the new mallocs *)
    let extractValAttsForNew summary newMallocs = 
      VarSet.fold
        (fun var (sum, mals) ->
           let v = VarMap.find var summary.bindings in
           let mals = 
             try 
               let att = VarMap.find var summary.hAttrs in
               VarMap.add var (v, att) mals
             with Not_found ->            
               (* in case sliceAttr knocked out the hAttr from diff *)
               try 
                 let att = VarMap.find var inState.hAttrs in
                 VarMap.add var (v, att) mals
               with Not_found ->
                 let pp = getCurrentPP () in
                 logErrorF "WTF: collectValAttFN no att %s @ %s (%d, %d)\n" 
                   (string_of_var var) (string_of_loc !currentLoc)
                   pp.pp_stmt pp.pp_instr;
                 VarMap.add var (v, HSingle) mals
           in
           ( { bindings = VarMap.remove var sum.bindings;
               hAttrs = VarMap.remove var sum.hAttrs; },
             mals )
        ) newMallocs (summary, VarMap.empty)
    in
    
    let summary, newMallocs = collectExtendNewMallocs () in
    let summary, newMallocs = extractValAttsForNew summary newMallocs in

    (** Now try merging these guys w/ existing malloc vars. 
        If they are new, bring them in here also *)
    let st = VarMap.fold 
      (fun var (v, att) cur -> 
         mergeMallocs var v att cur) newMallocs inState in

    (** Now update values for the rest of the vars in the summary *)
    let st, toNotify = 
      match retOpt with
        Some lv ->
          let summaryRetVal = getRetValue summary in
          let summary = removeRetVal summary in
          (* Make sure to get all the vars from the other func before
             doing the assignment for the return value *)
          let st = 
            { st with
                bindings = 
                self#updateStoreSummary acts st.bindings summary.bindings;
            } in
          let st = self#updateAttributes st summary in
          (* Ok, now do the assignment *)
          self#doAssign lv summaryRetVal st
      | None ->
          let summary = removeRetVal summary in
          let st = 
            { st with
                bindings = 
                self#updateStoreSummary acts st.bindings summary.bindings;
            } in
          let st = self#updateAttributes st summary in
          st, RS.empty
    in
    if stLat#isBottom st then
      (logErrorF "NullDeref: handling ret? @ %s\n" (string_of_loc !currentLoc);
       st, toNotify)
    (* Now update attributes *)
    else 
      let st = self#updateAttributes st summary in
      (* Let extras be resolved later? *)
(*
      !Fp_agg_merge.myMerger#setVerbose "use";
      let st = !Fp_agg_merge.myMerger#aggressiveMergeAll st in
      !Fp_agg_merge.myMerger#setVerbose "";
*)
      (st, toNotify)
        


  (** Entry point for handling ASSUMEs at each branch of if-statements *)
  method handleGuard gexp inState : fpState Dataflow.guardaction =
    if stLat#isBottom inState then Dataflow.GUnreachable
    else if isTopState inState then Dataflow.GDefault
    else begin
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
                        assumedTyp = None; } in
          try
            let v, inState = eval inState maybePtrExp emisc in
            match v with 
              FInt _ -> 
                if eq then Dataflow.GDefault else Dataflow.GUnreachable
            | FpRef _
            | Refs _ ->
                let newState = 
                  if eq 
                  then focus accPath (nullVar#getLoc) inState
                  else focusNot accPath (nullVar#getLoc) inState in
                if stLat#isBottom newState 
                then Dataflow.GUnreachable
                else begin 
                  Dataflow.GUse (Fp_hashcons.hc_state newState)
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
          BinOp (Ne, e, (Const (ckind) as con), _)
        | BinOp (Ne, (Const (ckind) as con), e, _) ->
            if isZero con then focusNull false e 
            else notifySkip ()

        | BinOp (Eq, e, (Const (ckind) as con), _)
        | BinOp (Eq, (Const (ckind) as con), e, _) ->
            if isZero con then focusNull true e 
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

end

let transF = new fpTransFunc lattice

let initState (func:Cil.fundec) : unit = begin
  Fp_types.initState () 
    (* hmm... when to trigger this? When we flush the fun cache? *)

end

