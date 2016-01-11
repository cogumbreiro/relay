(*
  Copyright (c) 2006-2007, Regents of the University of California

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

(** The intra-procedural portion of the dataflow analysis for 
    finding data races *)

open Cil
open Pretty
open Callg
open Fstructs
open Manage_sums
open Lockset
open Guarded_access_base
open Logging

module DF = Dataflow
module Intra = IntraDataflow
module IDF = InterDataflow

module SPTA = Symstate2
(* module SPTA = Symex *)

module RS = Racesummary_split
(*module RS = Racesummary_combined*)

module BS = Backed_summary
module Stat = Mystats

module CLv = Cil_lvals
module Lv = Lvals

(* TODO: switch to IntraDataflow framework ! *)


(***************************************************)
(* Intra-proc Analysis                             *)

let debug = false
let inspect = ref false

let setInspect yesno =
  inspect := yesno

(* Current function *)
let curFunc = ref dummyFunDec
let curFunID = ref dummyFID

let curCG = ref emptyCG
let curSCCCG = ref Scc_cg.emptySCCCG


let getFunName fid : string =
  try
    let node = FMap.find fid !curCG in
    node.name
  with Not_found -> ""

(************************************************************)

(** Customization module for lockset intra-proc dataflow *)
module RaceDF = struct
  let name = "detect_data_races"

  let debug = ref debug 
      
  (** per program point state -- only need locks to be flow-sensitive *)
  type t = RS.lockState

  (** lockset before a program statement *)
  let stmtStartData: t Inthash.t = Inthash.create 17

  (** initializes DF facts -- ASSUMES curFunc is set! *)
  let initStmtStartData (input: t) =
    (* Set all dfFacts to $BOTTOM, except the entry stmt be INPUT  *)
    Inthash.clear stmtStartData;
    (* Assume first stmt in the list is the entry stmt *)
    match !curFunc.sallstmts with
      hd :: tl ->
        Inthash.add stmtStartData hd.sid input;
        List.iter (fun stmt ->
                     Inthash.add stmtStartData stmt.sid RS.bottomLS
                  ) tl
    | _ -> ()

  let ldiffCache = LSPH.create 7
  let ldiffHits = ref 0
  let ldiffMisses = ref 0

  let resetCache () =
    RS.GA.clearCache ();
    LSPH.clear ldiffCache;
    logStatusF "Prev ldiffCache hits: %d\tmisses: %d\n\n" 
      !ldiffHits !ldiffMisses;
    ldiffHits := 0;
    ldiffMisses := 0

  let printCacheStats () = begin
    RS.GA.printCacheStats ();
    logStatus (LS.string_of_hashstats "Golden LS");
  end

 

  (** Initialize the symbolic state before analzying the given func
      and initialize the dataflow facts *)
  let initState funID (func: Cil.fundec) (input: t) : unit = begin
    curFunID := funID;
    curFunc := func;
    initStmtStartData input;
    resetCache ()
  end

  let copy (d: t) = d 

  let pretty () (d: t) =
    (* TODO, use this instead of own print functions? *)
    Pretty.nil

  let computeFirstPredecessor (s: stmt) (newD: t) : t = 
    newD

  let combinePredecessors (s: stmt) ~(old: t) (newD: t) : t option =
    if Stat.time "LS subset" (RS.lStateSubset newD) old then
      None
    else begin
      if !inspect then begin
        logStatus "Inspecting LS: state before combining";
        logStatus (Cildump.string_of_stmt s);
        RS.printLockset old
      end;
      let comboState = Stat.time "LS union" (RS.combineLStates old) newD in
      if !inspect then begin
        logStatus "Inspecting LS: state after combining";
        RS.printLockset comboState
      end;
      Some (comboState)
    end


  (*** LOCK summary application ***)

  (** Apply a summary that says the lock represented by 
      formalLv has been acquired *)
  let applyLocked pp actuals lv lockInfo curLS =
    let mustAlias, subbedLvs = SPTA.substActForm2FI actuals lv in
    let subbedLvs = SPTA.NULL.filterNullsLvals subbedLvs in
    (* Only update if mustAlias *)
    if (mustAlias) then (
      (if (List.length subbedLvs > 1) 
       then 
         logErrorF "RS: summary lock maps to multiple @ %s:%s\n" 
           (!curFunc.svar.vname) (Cildump.string_of_pp pp)
      );
      List.fold_left
        (fun curLS subbedLv ->
           LS.doPlus curLS subbedLv lockInfo
        ) curLS subbedLvs )
    else
      curLS
        

  (** Apply a summary that says the lock represented by formalLv
      has been released *)
  let applyUnlocked pp actuals lv lockInfo curLS =
    let mustAlias, subbedLvs = SPTA.substActForm2FI actuals lv in
    let subbedLvs = SPTA.NULL.filterNullsLvals subbedLvs in
    (* Always update, even if not a must alias relationship *)
    List.fold_left 
      (fun curLS subbedLv ->
         LS.doMinusCancel curLS subbedLv lockInfo
      ) curLS subbedLvs



  (** Apply the lockDiff from another function *)
  let applyLDiff pp actuals (curLS:t) (lockDiff: t) =
    try
      let result = LSPH.find ldiffCache (curLS, lockDiff) in
      incr ldiffHits;
      result
    with Not_found ->
      let appLock = applyLocked pp actuals in
      let appUn   = applyUnlocked pp actuals in
      let didLocked = 
        LS.S.fold appLock (LS.getPlus lockDiff) curLS in
      let didUnlocked = 
        LS.unique (LS.S.fold appUn (LS.getMinus lockDiff) didLocked) in
      LSPH.add ldiffCache (curLS, lockDiff) didUnlocked;
      incr ldiffMisses;
      didUnlocked


  (** Transform input state according to function summary *)
  let findApplyLSumm pp actuals fkey inSt =
    let fsumm = RS.sum#findLockSum fkey in
    let sumState = (RS.summOutstate fsumm).RS.lState in
    if (RS.isBottomLS sumState || RS.isBottomLS inSt) then
      RS.bottomLS
    else begin
      let apply = applyLDiff pp actuals in
      let newLocks = Stat.time "LS ldiff" (apply inSt) sumState in
      newLocks
    end
      (* All known functions should have summary initialized *)
      

  (** Transform flow-sensitive state according to function summary.
      Assumes {!Cil.getCurrentPP} will return the current prog_point. *) 
  let postSens curInstr callSite targs
      (callexp:Cil.exp) (actuals:Cil.exp list) (inState:t) : t =
    (* Don't need this check? Already done in doInstr *)
    if (RS.isBottomLS inState) then
      RS.bottomLS
    else 
      let pp = getCurrentPP () in
      List.fold_left 
        (fun curState sumKey ->
           let newState = findApplyLSumm pp actuals sumKey inState in
           RS.combineLStates curState newState
        ) RS.bottomLS targs

  (** Update flow sensitive facts for the instruction *)
  let doInstr (i: instr) (inSt: t) =
    if !inspect then begin
      logStatus "Inspecting LS: state before instr";
      logStatus (Cildump.string_of_instr i);
      RS.printLockset inSt;
    end;
    
    (* If the input state is bottom, the next state should also
     * be bottom *)
    if (RS.isBottomLS inSt) then
      DF.Default
    else begin
      match i with 
        Set (lval, rhs, loc) -> begin
          DF.Default
        end
          
      | Call (retval_option, callexp, actuals, loc) ->
          let pp = getCurrentPP () in
          let targs = callTargsAtPP !curCG !curFunID pp in
          if targs = [] then begin
            Intra.warnNoCallees callexp;
            DF.Default
          end else
            let funned =  
              Stat.time "LS post" (postSens i loc targs callexp actuals) inSt in
            if !inspect then begin
              logStatus "Inspecting LS: state after instr";
              RS.printLockset funned;
            end;
            DF.Done (funned) (* post most likely changed state *)
      | Asm _ -> 
          (* Unsoundly handling ASM *)
          DF.Default
    end


  (** Update flow sensitive information for the stmt *)
  let doStmt (s: stmt) (d: t) = 
    DF.SDefault


  (** Find from flow-sensitive state preceding statement S, given the table.
      Returns BOTTOM if not found *)
  let getStmtData (data: t Inthash.t) (s: stmt) : t = 
    try Inthash.find data s.sid
    with Not_found -> 
      RS.bottomLS

  (* TODO use value of the guard *)  
  let doGuard (gexp: Cil.exp) (d: t) =
    DF.GDefault

  let filterStmt _ = true

    
  (***************************************************)
  (* Debugging Stuff                                 *)
    
  (* TODO: just use the pretty print functions? *)
    
  (** Print DF facts of given STMT *)
  let printData allData stmt =
    let data = getStmtData allData stmt in
    begin
      Printf.printf "*** DF info preceding statement: %s\n" 
        (sprint 80 (d_stmt () stmt));
      RS.printLockset data;
    end
      
      
  (** Print out per-statement state for current function *)
  let printCurFuncState () =
    List.iter (printData stmtStartData) !curFunc.sallstmts
      
end

module RaceForwardDF = DF.ForwardsDataFlow(RaceDF)

(** Inspect the lockset at the given instruction, assuming the 
    analysis has just been run (and not reset) *)
let getLocksBefore = RaceForwardDF.getDataBefore
let getLocksAfter = RaceForwardDF.getDataAfter


(************************************************************)

(** Guarded access pass *)

(********** Update read / write accesses flow insensitively *****)
  
class ['st] readWriteAnalyzer = object (self)

  val mutable curFunID = dummyFID

  method setFunID fid =
    curFunID <- fid
  
  (** Walk through the Cil expression and add any refs to the state,
      also returns true if the state changed 
      Eagerly filters lvals that should be considered *)
  method addRefs curLS (s: 'st) (loc:Cil.location) (exp:Cil.exp) : 'st =
    if (RS.isBottomLS curLS) then
      s
    else match exp with
      Lval(Var(vi) as host, off) -> (* No derefs; only count if global *)
        if Shared.varShareable vi then
          let newOff, _ = CLv.canonicizeOff off in
          RS.addReadCorr curLS s (Lv.abs_of_lval (host, newOff)) 
            loc curFunID
        else
          s
            
    | Lval((Mem(ptrExp), off) as lv) -> (* Has deref, count if glob or form *)
        let pp = getCurrentPP () in
        let mustPt, targets = SPTA.derefLvalAt pp lv in
        let targets = SPTA.NULL.filterNullsLvals targets in
        (* Add correlation regardless of must/may point to status *)
        List.fold_left 
          (fun curSt curLv ->
             match Shared.isShareableAbs !curFunc curLv with
               None -> curSt
             | Some(scope) ->
                 RS.addReadCorr curLS curSt curLv loc curFunID
          ) s targets
          
    | BinOp(_,lhs,rhs,_) ->
        let didLHS = self#addRefs curLS s loc lhs in
        let didRHS = self#addRefs curLS didLHS loc rhs in
        didRHS
    | UnOp(unop,e,_) ->
        self#addRefs curLS s loc e
    | CastE(_,e) -> 
        self#addRefs curLS s loc e
    | _ -> (* TODO: check is there's more missing *)
        s
          


  (** Add any shared writes to the state. Eagerly filters lvals 
      that should be considered *)
  method handleAssignLeft curLS (s: 'st) (loc:Cil.location) (lv:Cil.lval) : 'st =
    if (RS.isBottomLS curLS) then
      s (* unreachable *)
    else match lv with
      (* [COPY]  x = newVal *)
      (Var(vi) as host, off) ->
        if Shared.varShareable vi then
          let newOff, _ = CLv.canonicizeOff off in
          RS.addWriteCorr curLS s (Lv.abs_of_lval (host, newOff)) 
            loc curFunID
        else
          s
            
    (* [STORE] *e = newVal *)
    | (Mem(ptrExp),_) -> begin
        let pp = Cil.getCurrentPP () in
        let mustPt, targets = SPTA.derefLvalAt pp lv in
        let targets = SPTA.NULL.filterNullsLvals targets in
        (* Add correlation regardless of must/may point to status *)
        List.fold_left 
          (fun curSt curLv ->
             match Shared.isShareableAbs !curFunc curLv with
               None -> curSt
             | Some(scope) ->
                 RS.addWriteCorr curLS curSt curLv 
                   loc curFunID
          ) s targets
      end
        (* TODO, check refs done in determining target of write? *)
        
end (* TODO: make the summary application part of the class? *)


module FITransF = struct

  (** type of flow insensitive state *)
  type t = RS.corrState

  let fiState = ref RS.emptyCS


  (* Trial: Cache postInsens when:
     (curLocks, symState wrt actuals, funTargs, inState) 
     are exactly the same -- if we get the "diff" then we
     can ignore differences in inState also! *)
  module HashPostInsens = struct

    type t = LS.relSet * (Cil.exp list) * (funID list)
    let equal (ls1, args1, funs1) (ls2, args2, funs2) =
      LS.equal ls1 ls2 && 
        List_utils.compare Ciltools.compare_exp args1 args2 == 0 &&
        List_utils.compare compareFunID funs1 funs2 == 0

    let hash (ls1, args1, funs1) =
      LS.hash ls1 lxor 
        (List_utils.hash Ciltools.hash_exp args1 lsl 2) lxor
        (List_utils.hash Hashtbl.hash funs1 lsl 4)

  end
  module PostCache = Hashtbl.Make(HashPostInsens)
  let postGACache = PostCache.create 17
  let postGAHits = ref 0
  let postGAMisses = ref 0

  (** Initialize the symbolic state before analzying the given func
      and initialize the dataflow facts *)
  let initState funID (func: Cil.fundec) (input: t) : unit = begin
    curFunID := funID;
    curFunc := func;
    RS.GA.clearCache ();
    PostCache.clear postGACache;
    logStatusF "Prev GAPostCache hits: %d\tmisses: %d\n\n" 
      !postGAHits !postGAMisses;
    postGAMisses := 0;
    postGAHits := 0;

    (* Read in the globals GA for the SCC *)
    fiState := 
      (Stat.time "LS combineCS (i)" 
         (RS.combineCStates input) 
         (RS.summOutstate (RS.sum#find funID)).RS.cState) ;
    
    if func.svar.vname = "do_rewritelog" then begin
      logStatus "GOTCHA : sample";
      RS.printCorrState !fiState
    end
  end

    
  (*** Correlation / Access summary application ***)
    
  (** apply the access information on formalLv to this function's scope *)
  let applyCorrelation pp actuals updateLS formalLv corrInfo curCMap =
    (* Rename the formalLv *)
    let mustAlias, lvalNames = SPTA.substActForm2FI actuals formalLv in
    let lvalNames = SPTA.NULL.filterNullsLvals lvalNames in

    (* Bring correlation's locks up to date *)
    let newCorr = Stat.time "LS updateAcc" 
      (RS.GA.updateAcc corrInfo) updateLS in
    (* Next, merge this new constraint w/ any existing one *)
    Stat.time "LS updateCorr" 
      (List.fold_left
         (fun curConstMap curLval -> (* TODO, track callSite also? *)
            RS.GA.updateCorr curLval newCorr curConstMap
         ) curCMap) lvalNames


  (** Apply whole slew of accesses from corrDiffs *)
  let applyCDiff pp actuals curLocks corrDiffs curCMap =
    let updateLS = Stat.time "LS ldiff" 
      (RaceDF.applyLDiff pp actuals curLocks) in
    let apply = applyCorrelation pp actuals updateLS in
    let result = (CMap.fold apply corrDiffs curCMap) in
    result

  let applyACSumm pp actuals curLocks inSt sumState = 
    if !inspect then begin
      logStatus "findApplyCSumm sum:";
      RS.printCorrState sumState;
    end;
    let apply = applyCDiff pp actuals curLocks in
    let newWriteCorrs = Stat.time "LS cdiff" 
      (apply sumState.RS.writeCorrs) inSt.RS.writeCorrs in
    let newReadCorrs = Stat.time "LS cdiff" 
      (apply sumState.RS.readCorrs) inSt.RS.readCorrs in
    RS.makeCState newWriteCorrs newReadCorrs

  (** Augment flow insensitive access correlation state *)
  let findApplyCSumm pp actuals curLocks fkey inSt =
    (* All known functions should have summary initialized *)
    let fsumm = Stat.time "findCS" RS.sum#find fkey in
    let sumState = (RS.summOutstate fsumm).RS.cState in
    if (RS.isBottomCS sumState || RS.isBottomLS curLocks) then
      inSt
    else 
      applyACSumm pp actuals curLocks inSt sumState

  let aggregateCSums targs = 
    List.fold_left
      (fun (gSum, fSums) sumKey ->
         let local = RS.sum#findLocal sumKey in
         if RS.isBottomSummary local then (gSum, fSums)
         else 
           try
             let global = RS.sum#findGlobal sumKey in
             ((Stat.time "LS combineSum" (RS.combineSummary gSum) global),
              local :: fSums)
           with Not_found ->
             (gSum, local :: fSums)
      ) (RS.emptySummary, []) targs


  (** Should only use postCache if this is a function pointer call
      with many targets... Assumes summaries DO NOT CHANGE while
      this pass is being made!  *)
  let shouldUsePostCache targs = 
(*
    List.length targs > 10
*)
    false

  (* Uncached form *)
  let uncachedCall pp targs actuals curLocks startSt =
    let globSum, localSums = Stat.time "aggCSums" aggregateCSums targs in
    let globOut = (RS.summOutstate globSum).RS.cState in
    let postG = applyACSumm pp actuals curLocks startSt globOut in
    List.fold_left 
      (fun curState fSum ->
         let fOut = (RS.summOutstate fSum).RS.cState in
         applyACSumm pp actuals curLocks curState fOut
      ) postG localSums

  (** Transform flow-insensitive state according to function summary.
      Assumes {!Cil.getCurrentPP} will return the current prog_point. *) 
  let postInsens curInstr callSite targs 
      (callexp:Cil.exp) (actuals:Cil.exp list) curLocks (inState:t) : t =
    (* First check if instruction is reachable *)
    if (RS.isBottomLS curLocks) then
      inState
    else 
      let pp = getCurrentPP () in
      (* Try using post cache if possible *)
      if shouldUsePostCache targs then
        let cacheKey = (curLocks, actuals, targs) in
        let diff = 
          try
            let res = PostCache.find postGACache cacheKey in
            incr postGAHits;
            res
          with Not_found ->
            let res = uncachedCall pp targs actuals curLocks RS.emptyCS in
            let res = Stat.time "LS uniqCM" RS.hcGAState res in
            PostCache.add postGACache cacheKey res;
            incr postGAMisses;
            res 
        in
        Stat.time "LS combineCS (p)" (RS.combineCStates inState) diff
      else 
        uncachedCall pp targs actuals curLocks inState


  (** Visitor for recording all guarded accesses *)
  class guardedAccessSearcher rwChecker getLocks = object (self)
    inherit Pp_visitor.ppVisitor

    method handleAssign lval rhs loc curLocks inState =
      let didLHS = rwChecker#handleAssignLeft curLocks !fiState loc lval in
      let didRHS = rwChecker#addRefs curLocks didLHS loc rhs in
      { 
        RS.writeCorrs = didRHS.RS.writeCorrs;
        RS.readCorrs = didRHS.RS.readCorrs;
      }

    (* Visit instruction for flow insensitive facts *)
    method vinst (i:instr) : instr list visitAction =
      self#setInstrPP i;
      let pp = getCurrentPP () in
      let curLocks = getLocks pp in

      if !inspect then begin
        logStatus "Inspecting RS: state before instr";
        logStatus (Cildump.string_of_instr i);
        RS.printCorrState !fiState;
      end;

      (* Use lockset info to see if stmt is even reachable *)
      let result = 
        if (RS.isBottomLS curLocks) then
          DoChildren
        else begin 
          match i with 
            Set (lval, rhs, loc) -> begin
              let finalSt = 
                Stat.time "LS handleAssign" 
                  (self#handleAssign lval rhs loc curLocks) !fiState in
              fiState := finalSt;
              if !inspect then begin
                logStatus "Inspecting RS: state after instr";
                RS.printCorrState !fiState;
              end;
              DoChildren
            end
              
          | Call (retval_option, callexp, actuals, loc) ->
              let didLHS =
                match retval_option with
                  Some(lv) -> 
                    rwChecker#handleAssignLeft curLocks !fiState loc lv
                | None -> !fiState 
              in
              let reffedArgs = 
                List.fold_left
                  (fun curSt argExp -> 
                     let newSt = rwChecker#addRefs curLocks curSt loc argExp in
                     newSt)
                  didLHS actuals in
              let tempState = 
                {
                  RS.writeCorrs = reffedArgs.RS.writeCorrs;
                  RS.readCorrs = reffedArgs.RS.readCorrs;
                } in
              let targs = callTargsAtPP !curCG !curFunID (getCurrentPP ()) in
              let funned =  
                Stat.time "LS post" 
                  (postInsens i loc targs callexp actuals curLocks) tempState in
              let finalSt = 
                { 
                  RS.writeCorrs = funned.RS.writeCorrs;
                  RS.readCorrs = funned.RS.readCorrs;
                } in
              fiState := finalSt;
              if !inspect then begin
                logStatus "Inspecting RS: state after instr";
                RS.printCorrState !fiState;
              end;
              DoChildren
                
          | Asm _ -> 
              (* Unsoundly handling ASM *)
              DoChildren
        end
      in
      self#bumpInstr 1;
      result
      
    (** Visit the statement, updating flow insensitive state *)
    method vstmt (s: stmt) : stmt visitAction =
      self#setStmtPP s;
      let pp = getCurrentPP () in
      let curLocks = getLocks pp in
      if !inspect then begin
        logStatus "Inspecting RS: state before stmt";
        logStatus (Cildump.string_of_stmt s);
        RS.printCorrState !fiState;
      end;

      (* Check if stmt is even reachable (by checking LS) *)
      if (RS.isBottomLS curLocks) then
        DoChildren
      else
        match s.skind with
          If (e, _, _, loc) 
        | Switch (e, _, _, loc) 
        | Return (Some (e), loc) ->
            let reffed = rwChecker#addRefs curLocks !fiState loc e in
            let finalSt = 
              { reffed with
                  RS.readCorrs = reffed.RS.readCorrs;
              } in
            fiState := finalSt;

            if !inspect then begin
              logStatus "Inspecting RS: state after stmt";
              RS.printCorrState !fiState;
            end;

            DoChildren
        | Instr (il) -> DoChildren (* Let vinst handle the rest *)
        | _ -> DoChildren

  end (* end guardedAccessSearcher *)

end


(***************************************************)
(* Inter-proc Analysis                             *)

(** Combine flow-sensitive states at return statements *)
let combRetStates (curState:RaceDF.t) (s:stmt) : RaceDF.t =
  let combineS () =
    let newState = RaceDF.getStmtData RaceDF.stmtStartData s in
    RS.combineLStates curState newState
  in
  (* Consider Return statements *)
  match (s.skind, s.succs) with
    Return (_, _), _ ->
      combineS ()
  | _, _ -> 
      curState


(** Package up the RS and Lockset analysis *)  
class raceAnalysis rwChecker = object (self) 
  
  method setInspect yesno =
    inspect := yesno

  method isFinal key = 
    BS.isFinal key RS.sum#sumTyp
      
  method compute funID cfg =
    (* TODO: get rid of input *)
    let input = RS.emptyState in
    RaceDF.initState funID cfg input.RS.lState;
    RaceForwardDF.clearPPData ();
    FITransF.initState funID cfg input.RS.cState;
    rwChecker#setFunID funID;
    Stat.time "Race/Lockset DF: " 
      (fun () ->
         (* Compute locksets *)
         logStatus "doing lockset";
         flushStatus ();
         RaceForwardDF.compute cfg.sallstmts;
         
         (* Update read/write correlation info *)
         logStatus "doing guarded access";
         flushStatus ();
         let gaVisitor = 
           new FITransF.guardedAccessSearcher rwChecker getLocksBefore in
         ignore (visitCilFunction (gaVisitor :> cilVisitor) cfg);
      ) ()

  method private doSummarize fkey cfg =
    if self#isFinal fkey then false
    else begin
      (* Get the possibly-new output state for this function *)
      let outLocks = List.fold_left combRetStates RS.bottomLS cfg.sallstmts in
      let outCorrs = !FITransF.fiState in
      (* Assemble *)
      let outState = RS.makeState outLocks outCorrs in

      (* Ignore input for race summaries... any exposure of state 
         will screw up the ability to collect "analysis classes" in a list *)
      let input = RS.emptyState in
      let newSummary = RS.makeSumm input outState in
      let funname = getFunName fkey in
      let changed = Intra.checkupSummary fkey cfg newSummary 
        RS.sum#find
        RS.isBottomSummary
        RS.sumSubset
        RS.combineSummary
        RS.scopeSummary
        RS.sum#addReplace
        !inspect
        (RS.printSummary funname fkey) in
      changed
    end

  method summarize fkey cfg =
    Stat.time "LS summarize" (self#doSummarize fkey) cfg

  method flushSummaries () =
    RS.sum#serializeAndFlush

end

  
(** Info needed by the inter-proc driver *)
  
(**** List of analyses that should be run (listed in order) *****)
  
let ssAna = new SPTA.symexAnalysis
let rwCheck = new readWriteAnalyzer
let rsAna = new raceAnalysis rwCheck
  
let needsFixpoint = 
  [ ssAna; rsAna ]

module RaceBUTransfer = struct
  

  (**** Statistics kept ****)
  
  let sccsDone = ref 0

  let sccsTotal = ref 0

  let initStats cg sccCG : unit = 
    (curCG := cg;
     Intra.curCG := cg;
     curSCCCG := sccCG;
     Intra.curSCCCG := sccCG;

     (* Progress *)
     sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
     sccsDone := 0;
    )

  let updateStats lastSCC = 
    incr sccsDone;
    logStatusF ">>> PROGRESS %d/%d SCCs DONE!\n\n" !sccsDone !sccsTotal

  (**** State management / calculation ****)

  type state = Racesummary.state

  let flushSummaries () =
    RS.sum#serializeAndFlush;
    SPTA.SS.sum#serializeAndFlush

  let doFunc ?(input:state = RS.emptyState) fid callN : state IDF.interResult =
    let fn, defFile = callN.name, callN.defFile in
    logStatusF "Summarizing function: %s : %s\n" fn defFile;
    logStatus "-----";
    flushStatus ();
    match Cilinfos.getFunc (fid_to_fkey fid) defFile with
      Some cfg ->
        if Intra.runFixpoint needsFixpoint fid cfg then
          IDF.NewOutput (input, input) (* TODO: change return type *)
        else
          IDF.NoChange
          
    | None ->
        (* Don't have function definition *)
        logError ("doFunc can't get CFG for: " ^ 
                        (string_of_fNT (fn, defFile)));
        IDF.NoChange 


  (** TRUE if the function should be put on the worklist *)
  let filterFunc f : bool =
    true

  (* TODO: find out what sums are used instead of hardcoding? *)
  let hardCodedSumTypes ()  =
    BS.getDescriptors (RS.getSumTypes () @ [SPTA.SS.sum#sumTyp;])
    

  (** Prepare to start an scc, acquiring the required summaries *)
  let sccStart scc = begin
    (* Get all summaries for all callees *)
    logStatus "Acquiring needed summaries";
    flushStatus ();
    prepareSCCCalleeSums !curSCCCG scc (hardCodedSumTypes ());
  end

  let printSize (k, size) =
    logStatusF "SIZES: %s : %d\n" (fid_to_string k) size

  let printSizes fkSize =
    List.iter printSize fkSize

  (** Scc is summarized. Do the bookkeeping / cleanup *) 
  let sccDone scc (byThisGuy:bool) =
    let summPaths = if (byThisGuy) then
      let sccKeys = sumKeysOfScc scc [] in
      (* Debugging *)
      List.iter 
        (fun key ->
           let name = getFunName key in
           logStatusF "Summary for function: %s:%s\n" name (fid_to_string key);
           logStatus "=======\n";
           RS.findPrintSumm name key;
           SPTA.SS.printSummary SPTA.SS.sum key;
        ) sccKeys;
      
      (* Serialize and record where each fun was placed *)
      flushSummaries ();

      let sum_sizes = RS.sum#sizesOf sccKeys in
      printSizes sum_sizes;
      
      (* Find out where the summaries were stored *)
      (* TODO: force them to pick the same directory 
         (which they do unless on partition runs out of space) *)
      let tokenMap = RS.sum#locate sccKeys in
      
      (* Notify others that the functions are now summarized *)
      List.fold_left
        (fun paths (key, tok) -> 
           let path = BS.pathFromToken tok in
           (key, path) :: paths
        ) [] tokenMap
    else [] in
    updateStats scc; (* and possibly delete obsolete summaries *)
    summPaths
    (* RFC.clear !astFCache *)

end

module BUDataflow = IDF.BottomUpDataflow (RaceBUTransfer)
