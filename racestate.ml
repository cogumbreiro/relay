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

open Callg
open Cil
open Pretty
open Fstructs
open Scc
open Scope
open Messages
open Cilinfos
open Manage_sums
open Lockset

module IH = Inthash
module DF = Dataflow
module IDF = InterDataflow
module A = Alias
module Sh = Shared
module GA = Guarded_access
module RS = Racesummary
module SS = Symsummary
module SPTA = Symstate2
module Du = Cildump
module BS = Backed_summary
module Req = Request
module DC = Default_cache
module Stat = Mystats
module L = Logging

module CLv = Cil_lvals
module Lv = Lvals

module I = Inspect

(***************************************************)
(* State / Utilities :                             *)

(* Current function *)
let curFunc = ref dummyFunDec


(*******************************************************************)
(* Util functions                                                  *)

let substActFormN curStmt curInstr argExp lvalWithFormal =
  (* Expand aliases of actual *)
  let mustAlias, aliases = 
    SPTA.getAliasesAtInstr curStmt curInstr argExp in
  let results = 
    List.fold_left
      (fun curList actualE ->
         try
           let result = Lv.substActForm actualE lvalWithFormal in
           let (simpleH, simpleO), _ = Lv.simplifyLval result in
           (simpleH, simpleO) :: curList
         with CLv.SubstInvalidArg ->
           curList
      )  [] aliases in
  (mustAlias, results)


(* function for argument renaming, if necessary *)
let substActForm curStmt curInstr actuals lvalWithFormal namesScope =
  match namesScope with 
    SGlobal -> (* no renaming, just simplify offsets *)
      let simpleLVWF = Lv.canonicizeLval lvalWithFormal in
      (true, [simpleLVWF])
  | SFormal n -> begin
      let argExp = List.nth actuals n in
      substActFormN curStmt curInstr argExp lvalWithFormal
    end
  | _ ->
      failwith ("Racestate substActForm: didn't resolve scope: "
                ^ (Lv.string_of_lval lvalWithFormal) ^ "\n")


(***************************************************)
(* Intra-proc Analysis                             *)

let debug = false

(* caching of diff operations *)

module LSPH = GA.LSPH

module CorrDiffHash = 
struct
  type t = fullLS * GA.straintMap * GA.straintMap * Cil.exp list
  let equal (l1, f1, s1, acts1) (l2, f2, s2, acts2) =
    let actualsEQ = 
      try
        List.for_all2 
          (fun a1 a2 -> Ciltools.compare_exp a1 a2 == 0) acts1 acts2
      with Invalid_argument _ ->
        false
    in
    (* assume all given locksets / straintmaps are the main copy *)
    actualsEQ && (l1 == l2) &&
      ((f1 == f2 && s1 == s2) ||
         (f1 == s2 && s1 == f2))
  let hash ((l, f, s, acts) : t) =
    let actHash = 
      List.fold_left 
        (fun h a ->
           h lxor Ciltools.hash_exp a) 0 acts in
    actHash lxor (LS.hash l) lxor (GA.hashCM f) lxor (GA.hashCM s)
end

module LCMPH = Hashtbl.Make (CorrDiffHash)

(** Customization module for lockset intra-proc dataflow *)
module RaceDF = struct
  let name = "detect_data_races"

  let debug = ref debug 
      
  let curStmt = ref dummyStmt

  let curInstr = ref dummyInstr

  (* per program point state -- only need locks to be flow-sensitive *)
  type t = RS.lockState

  (* type of flow insensitive state *)
  type fiT = RS.corrState

  (* state before a program statement *)
  let stmtStartData: t IH.t = IH.create 17

  (* flow insensitive state *)
  let fiState = ref RS.emptyCS

  (** initializes DF facts -- ASSUMES curFunc is set! *)
  let initStmtStartData (input: t) =
    (* Set all dfFacts to $BOTTOM, except the entry stmt be INPUT  *)
    IH.clear stmtStartData;
    (* Assume first stmt in the list is the entry stmt *)
    match !curFunc.sallstmts with
      hd :: tl ->
        IH.add stmtStartData hd.sid input;
        List.iter (fun stmt ->
                     IH.add stmtStartData stmt.sid RS.bottomLS
                  ) tl
    | _ ->
        ()

  (** Cache of lock diff results *)  
  let (ldiffCache : t LSPH.t)= LSPH.create 17 

  (** Cache of correlation diff results *)
  let (cdiffCache : fiT LCMPH.t) = LCMPH.create 17

  (** initialize the cache of lock diff results *)
  let initDiffCache () = begin
    LSPH.clear ldiffCache;
    LCMPH.clear cdiffCache;
  end

  (** Initialize the symbolic state before analzying the given func
      and initialize the dataflow facts *)
  let initState (func: Cil.fundec) (input: t) : unit =
    (curFunc := func;
     fiState := RS.emptyCS;
     initStmtStartData input;
     initDiffCache ();
     GA.clearCache ();
    )

  let copy (d: t) = d 

  let pretty () (d: t) =
    (* TODO, use this instead of own print functions? *)
    Pretty.nil

  let computeFirstPredecessor (s: stmt) (newD: t) : t = 
    newD

  let combinePredecessors (s: stmt) ~(old: t) (newD: t) : t option =
    if (Stat.time "LS subset test" (RS.lStateSubset newD) old) then
      None
    else begin
      let inspect = I.inspector#mem !curFunc.svar.vname in
      if inspect then begin
        L.logStatus "Inspecting LS: state before combining";
        L.logStatus ((Du.string_of_stmt s));
        RS.printLockset old
      end;
      let comboState = Stat.time "LS combineStates" 
        (RS.combineLStates old) newD in
      if inspect then begin
        L.logStatus "Inspecting LS: state after combining";
        RS.printLockset comboState
      end;
      Some (comboState)
    end

  (********** Update read / write accesses flow insensitively *****)


  (* Walk through the Cil expression and add any refs to the state,
     also returns true if the state changed 
     Eagerly filters lvals that should be considered *)
  let rec addRefs curLS (s: fiT) 
      (loc:Cil.location) (exp:Cil.exp) : (fiT) =
    if (RS.isBottomLS (curLS)) then
      s
    else match exp with
      Lval(Var(vi) as host, off) -> (* No derefs; only count if global *)
        if Sh.varShareable vi then
          let newOff, _ = CLv.canonicizeOff off in
          RS.addReadCorr curLS s (Lv.abs_of_lval (host, newOff)) 
            loc !curFunc.svar.vid SGlobal
        else
          s
            
    | Lval((Mem(ptrExp), off) as lv) -> (* Has deref, count if glob or form *)
        let mustPt, targets = Stat.time "derefLvalAtInstr" 
          (SPTA.derefLvalAtInstr !curStmt !curInstr) lv in
        (* Add correlation regardless of must/may point to status *)
        List.fold_left 
          (fun curSt curLv ->
             match Sh.isShareableAbs !curFunc curLv with
               None -> curSt
             | Some(scope) ->
                 RS.addReadCorr curLS curSt curLv loc !curFunc.svar.vid scope
          ) s targets
          
    | BinOp(_,lhs,rhs,_) ->
        let didLHS = addRefs curLS s loc lhs in
        let didRHS = addRefs curLS didLHS loc rhs in
        didRHS
    | UnOp(unop,e,_) ->
        addRefs curLS s loc e
    | CastE(_,e) -> 
        addRefs curLS s loc e
    | _ -> (* TODO: check is there's more missing *)
        s
        


  (* Add any mods to the state. 
     Eagerly filters lvals that should be considered *)
  let handleAssignLeft curLS (s: fiT) 
      (loc:Cil.location) (lv:Cil.lval) : fiT =
    if(RS.isBottomLS (curLS)) then
      s (* unreachable *)
    else match lv with
      (* [COPY]  x = newVal *)
      (Var(vi) as host, off) ->
        if Sh.varShareable vi then
          let newOff, _ = CLv.canonicizeOff off in
          RS.addWriteCorr curLS s (Lv.abs_of_lval (host, newOff)) 
            loc !curFunc.svar.vid SGlobal
        else
          s

    (* [STORE] *e = newVal *)
    | (Mem(ptrExp),_) -> begin
        let mustPt, targets = Stat.time "derefLvalAtInstr" 
          (SPTA.derefLvalAtInstr !curStmt !curInstr) lv in
        (* Add correlation regardless of must/may point to status *)
        List.fold_left 
          (fun curSt curLv ->
             match Sh.isShareableAbs !curFunc curLv with
               None -> curSt
             | Some(scope) ->
                 RS.addWriteCorr curLS curSt curLv 
                   loc !curFunc.svar.vid scope
          ) s targets
      end
        (* TODO, check refs done in determining target of write? *)


  (*** LOCK summary application ***)

  (** Apply a summary that says the lock represented by 
      formalLv has been acquired *)
  let applyLocked curStmt curInstr callSite actuals lv lockInfo curLS =
    let scope = getScope lv in
    let mustAlias, subbedLvs = 
      substActForm curStmt curInstr actuals lv scope in
    (* Only update if mustAlias *)
    if (mustAlias) then (
      (if (List.length subbedLvs > 1) 
       then L.logError 
         ("RS: summary lock maps to multiple @ " ^ 
            (Du.string_of_loc callSite))
      );
      List.fold_left 
        (fun curLS subbedLv ->
           LS.doPlus curLS subbedLv lockInfo
        ) curLS subbedLvs )
    else
      curLS
        

  (** Apply a summary that says the lock represented by formalLv
      has been released *)
  let applyUnlocked curStmt curInstr callSite actuals lv lockInfo curLS =
    let scope = getScope lv in
    let mustAlias, subbedLvs = 
      substActForm curStmt curInstr actuals lv scope in
    (* Always update, even if not a must alias relationship *)
    List.fold_left 
      (fun curLS subbedLv ->
         LS.doMinus curLS subbedLv lockInfo
      ) curLS subbedLvs

(* HMM... cache is not exactly right because actuals can be 
   translated differently *)





  (** Apply the lockDiff from another function *)
  let applyLDiff curStmt curInstr callSite actuals 
      (lockDiff: t) (curLS:t) =
(*    try 
      LSPH.find ldiffCache (lockDiff, curLS)
    with Not_found ->
*)
      let appLock = applyLocked curStmt curInstr callSite actuals in
      let appUn   = applyUnlocked curStmt curInstr callSite actuals in
      let didLocked = 
        LS.S.fold appLock
          (LS.getPlus lockDiff) curLS in
      let didUnlocked = 
        LS.unique (LS.S.fold appUn
                     (LS.getMinus lockDiff) didLocked)
      in
      (* LSPH.add ldiffCache (lockDiff, curLS) didUnlocked; *)
      didUnlocked
    

  (*** Correlation / Access summary application ***)

 
  (** apply the access information on formalLv to this function's scope *)
  let applyCorrelation curStmt curInstr callSite actuals
      curLocks formalLv corrInfo curCMap =
    (* Rename the formalLv *)
    let mustAlias, lvalNames = 
      substActForm curStmt curInstr actuals
        formalLv corrInfo.GA.corrScope
    in
    (* Bring correlation's locks up to date *)
    let curLS = Stat.time "LS ldiff" 
      (applyLDiff curStmt curInstr callSite actuals 
         corrInfo.GA.corrLocks) curLocks in
    (* Next, merge this new constraint w/ any existing one *)
    List.fold_left 
      (fun curConstMap curLval -> (* TODO, track callSite also? *)
         GA.updateCorr corrInfo curLval curLS curConstMap
      ) curCMap lvalNames


  
  (** Apply whole slew of accesses from corrDiffs *)
  let applyCDiff curStmt curInstr callSite actuals
      curLocks corrDiffs curCMap =
(*    try 
      LCMPH.find cdiffCache (curLocks, corrDiffs, curCMap, actuals)
    with Not_found ->
*)
      let apply = 
        applyCorrelation curStmt curInstr callSite actuals curLocks in
      let result = GA.cacheCM (GA.CMap.fold
                                 apply corrDiffs curCMap) in
(*
      LCMPH.add cdiffCache (curLocks, corrDiffs, curCMap, actuals) result;
*)
      result
      


  (** Transform input state according to function summary *)
  let findApplyLSumm curStmt curInstr callSite actuals fkey inSt =
    let fsumm = RS.sum#find fkey in
    let sumState = (RS.summOutstate fsumm).RS.lState in
    if (RS.isBottomLS (sumState) || RS.isBottomLS (inSt)) then
      RS.bottomLS
    else begin
      let apply = applyLDiff curStmt curInstr callSite actuals in
      let newLocks = Stat.time "LS ldiff"
        (apply sumState) inSt in
      newLocks
    end
      (* All known functions should have summary initialized *)
      
        

  (** augment flow insensitive access correlation state *)
  let findApplyCSumm curStmt curInstr callSite actuals 
      curLocks fkey inSt =
    let fsumm = RS.sum#find fkey in
    let sumState = (RS.summOutstate fsumm).RS.cState in
    if (RS.isBottomCS (sumState) || RS.isBottomLS (curLocks)) then
      inSt
    else begin
      let apply = applyCDiff curStmt curInstr callSite actuals curLocks in
      let newWriteCorrs = Stat.time "LS cdiff" 
        (apply sumState.RS.writeCorrs) inSt.RS.writeCorrs in
      let newReadCorrs = Stat.time "LS cdiff" 
        (apply sumState.RS.readCorrs) inSt.RS.readCorrs in
      RS.makeCState newWriteCorrs newReadCorrs
    end
      (* All known functions should have summary initialized *)


  (** Transform flow-sensitive state according to function summary *) 
  let postSens (curInstr: instr) (callSite:Cil.location) (callexp:Cil.exp)
      (actuals:Cil.exp list) (inState:t) : t =
    (* Don't need this check? Already done in doInstr *)
    if (RS.isBottomLS (inState)) then
      RS.bottomLS
    else match callexp with
      (* Direct call *)
      Lval (Var (finfo), NoOffset ) ->
        let fkey = finfo.vid in
        findApplyLSumm !curStmt curInstr callSite actuals 
          fkey inState 

    (* Indirect call : Try to apply all summaries of aliased funcs *)
    | Lval(Mem(derefExp), NoOffset) -> (* Current AA is field insens. *)
        let aliasedFuns = A.deref_funptr derefExp in
        (* What to do if we don't know what the FP leads to? *)
        if (List.length aliasedFuns = 0) then begin
          L.logError ("post: funptr resolved to 0 fun(s): " ^
                          (Du.string_of_exp callexp));
          inState
        end
        else
          List.fold_left 
            (fun curState fkey ->
               let newState = 
                 findApplyLSumm !curStmt curInstr callSite actuals 
                   fkey inState in
               Stat.time "LS combineStates" 
                 (RS.combineLStates curState) newState
            ) RS.bottomLS aliasedFuns
            
    (* Other? *)
    | _ -> 
        L.logError "LS postSens: Unknown callexp structure";
        inState
     

  (** Transform flow-insensitive state according to function summary *) 
  let postInsens (curInstr: instr) (callSite:Cil.location) (callexp:Cil.exp)
      (actuals:Cil.exp list) curLocks (inState:fiT) : fiT =
    (* First check if instruction is reachable *)
    if (RS.isBottomLS (curLocks)) then
      inState
    else match callexp with
      (* Direct call *)
      Lval (Var (finfo), NoOffset ) ->
        let fkey = finfo.vid in
        findApplyCSumm !curStmt curInstr callSite actuals 
          curLocks fkey inState 
          (* Also do lHat *)

    (* Indirect call : Try to apply all summaries of aliased funcs *)
    | Lval(Mem(derefExp), NoOffset) -> (* Current AA is field insens. *)
        let aliasedFuns = A.deref_funptr derefExp in
        (* What to do if we don't know what the FP leads to? *)
        if (List.length aliasedFuns = 0) then begin
          L.logError ("post: funptr resolved to 0 fun(s): " ^
                          (Du.string_of_exp callexp));
          inState
        end
        else
          List.fold_left 
            (fun curState fkey ->
               findApplyCSumm !curStmt curInstr callSite actuals 
                 curLocks fkey curState 
            ) inState aliasedFuns
            
    (* Other? *)
    | _ ->
        L.logError "LS postInsens: Unknown callexp structure";
        inState



  (* Handle flow sensitive facts for the instruction *)
  let doInstr (i: instr) (inSt: t) =
    curInstr := i;
    let inspect = I.inspector#mem !curFunc.svar.vname in
    if inspect then begin
      L.logStatus "Inspecting LS: state before instr";
      L.logStatus ((Du.string_of_instr i));
      RS.printLockset inSt
    end;
    
    (* If the input state is bottom, the next state should also
     * be bottom *)
    if (RS.isBottomLS (inSt)) then
      DF.Default
    else begin
      match i with 
        Set (lval, rhs, loc) -> begin
          DF.Default
        end
          
      | Call (retval_option, callexp, actuals, loc) ->
          let funned =  
            Stat.time "LS post" (postSens i loc callexp actuals) 
              inSt in
          if inspect then begin
            L.logStatus "Inspecting LS: state after instr";
            RS.printLockset funned
          end;
          DF.Done (funned) (* post most likely changed state *)
      | Asm _ -> 
          (* Unsoundly handling ASM *)
          DF.Default
    end


  (* Visit instruction for flow insensitive facts *)
  let visitInstr (i:instr) (curLocks: t) =
    curInstr := i;
    Cil.currentLoc := Cil.get_instrLoc i;
    let inspect = I.inspector#mem !curFunc.svar.vname in
    if inspect then begin
      L.logStatus "Inspecting RS: state before instr";
      L.logStatus ((Du.string_of_instr i));
      RS.printCorrState !fiState
    end;
    
    (* If the input state is bottom, the next state should also
     * be bottom *)
    if (RS.isBottomLS (curLocks)) then
      ()
    else begin 
      match i with 
        Set (lval, rhs, loc) -> begin
          let didLHS = handleAssignLeft curLocks !fiState loc lval in
          let didRHS = addRefs curLocks didLHS loc rhs in
          let finalSt = {
            RS.writeCorrs = GA.cacheCM didRHS.RS.writeCorrs;
            RS.readCorrs = GA.cacheCM didRHS.RS.readCorrs;
          } in
          if inspect then begin
            L.logStatus "Inspecting RS: state after instr";
            RS.printCorrState finalSt;
          end;
          fiState := finalSt
        end
  
      | Call (retval_option, callexp, actuals, loc) ->
          let didLHS =
            match retval_option with
              Some(lv) ->
                handleAssignLeft curLocks !fiState loc lv
            | None ->
                !fiState 
          in
          let reffedArgs = 
            List.fold_left
              (fun curSt argExp -> 
                 let newSt = addRefs curLocks curSt loc argExp in
                 newSt)
              didLHS actuals in
          let tempState = 
            {
              RS.writeCorrs = GA.cacheCM reffedArgs.RS.writeCorrs;
              RS.readCorrs = GA.cacheCM reffedArgs.RS.readCorrs;
            } in
          let funned =  
            Stat.time "LS post" (postInsens i loc callexp actuals curLocks) 
              tempState in
          let finalSt = 
            { 
              RS.writeCorrs = GA.cacheCM funned.RS.writeCorrs;
              RS.readCorrs = GA.cacheCM funned.RS.readCorrs;
            } in
          if inspect then begin
            L.logStatus "Inspecting RS: state after instr";
            RS.printCorrState finalSt
          end;
          fiState := finalSt
      | Asm _ -> 
          (* Unsoundly handling ASM *)
          ()
    end

  (** Update flow sensitive information for the stmt *)
  let doStmt (s: stmt) (d: t) = 
    curStmt := s;
    DF.SDefault


  (** Find from flow-sensitive state preceding statement S, given the table.
      Returns BOTTOM if not found *)
  let getStmtData (data: t IH.t) (s: stmt) : t = 
    try IH.find data s.sid
    with Not_found -> 
      RS.bottomLS

    
  (** Visit the statement, updating flow insensitive state *)
  let visitStmt (s: stmt) =
    curStmt := s;
    Cil.currentLoc := Cil.get_stmtLoc s.skind;
    let initLocks = getStmtData stmtStartData s in
    if (RS.isBottomLS (initLocks)) then
      ()
    else
      match s.skind with
        If (e, _, _, loc) 
      | Switch (e, _, _, loc) 
      | Return (Some (e), loc) ->
          let reffed = addRefs initLocks !fiState loc e in
          let finalSt = { reffed with
                            RS.readCorrs = GA.cacheCM reffed.RS.readCorrs;
                        } in
          fiState := finalSt
      | Instr (il) -> begin
          let _ = List.fold_left 
            (fun curLocks curI ->
               visitInstr curI curLocks;
               let newLocks = 
                 match doInstr curI curLocks with
                   DF.Done st -> st
                 | DF.Default -> curLocks
                 | DF.Post f -> f curLocks
               in
               newLocks
            ) initLocks il in
          () (* flow insensitive state is already updated along the way *)
        end
      | _ ->
          ()

  (* TODO use value of the guard *)  
  let doGuard (gexp: Cil.exp) (d: t) =
    DF.GDefault

  let filterStmt _ = true

        
  (***************************************************)
  (* Debugging Stuff                                 *)
      
  (* Print DF facts of given STMT *)
  let printData allData stmt =
    let data = getStmtData allData stmt in
    begin
      Printf.printf "*** DF info preceding statement: %s\n" 
        (sprint 80 (d_stmt () stmt));
      RS.printLockset data;
    end
      
      
  (* Print out per-statement state for current function *)
  let printCurFuncState () =
    List.iter (printData stmtStartData) !curFunc.sallstmts
      
end




module RaceForwardDF = DF.ForwardsDataFlow(RaceDF)



(***************************************************)
(* Inter-proc Analysis                             *)
  
  
(** Info needed by the inter-proc driver 
    Note: user manages the summary datastructure 
*)
module RaceBUTransfer = 
struct
  

  (**** Statistics kept ****)
  
  let sccsDone = ref 0

  let sccsTotal = ref 0

  let pinCounts = ref (Hashtbl.create 1)

  let curCG = ref FMap.empty

  let curSCCCG = ref IntMap.empty

  let timerID = ref Timeout.nilTimer

  let initTimer () =
    if (!timerID == Timeout.nilTimer) then timerID := (Timeout.newTimerID ())

  let initStats (cg:simpleCallG) (sccCG: sccGraph) (finalFuncs:fKey list) : unit = 
    (curCG := cg;
     curSCCCG := sccCG;
     initTimer ();

     (* Progress *)
     sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
     sccsDone := 0;
     (* Dependencies of each function *)
     pinCounts := getDependencies cg finalFuncs;
    )

      
  let updateStats (lastSCC:scc) = 
    (* Progress *)
    incr sccsDone;
    L.logStatus (">>> PROGRESS " ^ (string_of_int !sccsDone) ^ "/" ^
                    (string_of_int !sccsTotal) ^ " SCCs DONE!\n");
    (* Function dependence update *)
    FSet.iter
      (fun fkey ->
         let fnode = FMap.find fkey !curCG in
         let callees = fnode.callees in
         List.iter 
           (fun calleeKey ->
              try
                let curPinCount = Hashtbl.find !pinCounts calleeKey in
                Hashtbl.replace !pinCounts calleeKey (curPinCount - 1);
                if (curPinCount <= 1) then
                  (* Ok to clear summary if not pinned *)
                  (* ACTUALLY, leave summaries for now...
                     fSummaries#removeSummary calleeKey
                  *) ()
              with Not_found ->
                ()
           ) callees
      ) lastSCC.scc_nodes

  (**** State management / calculation ****)

  type state = RS.state

  (** Combine flow-sensitive states at return statements *)
  let combRetStates (curState:RaceDF.t) (s:stmt) : RaceDF.t =
    let combineS () =
      let newState = RaceDF.getStmtData RaceDF.stmtStartData s in
      (Stat.time "LS combineStates" 
         (RS.combineLStates curState) newState)
    in
    (* Consider Return statements *)
    match (s.skind, s.succs) with
      Return (_, _), _ ->
        combineS ()
    | _, _ -> 
        curState

  let alarmHandler ret_ref () =
    ret_ref := IDF.NoChange (* skip *)
    
  let timeout_time = 600.0

  let registerTimeout ret_ref =
    Timeout.set !timerID timeout_time (alarmHandler ret_ref)

  let unregisterTimeout () =
    Timeout.cancel !timerID

  let noChangeInRS symSumChanged inspect input output =
    if (symSumChanged) then
      (if inspect then L.logStatus "Only symstate return changed";
       IDF.NewOutput (input, output))
    else
      (if inspect then L.logStatus "No change in summary";
       IDF.NoChange)

  (** Summarize a function w/ cfg, given a particular input *)
  let summarizeFun (cfg:fundec) (input:state) : state IDF.interResult =
    let (fn, ft) = (cfg.svar.vname,
                    Cildump.string_of_ftype cfg.svar.vtype) in
    let fkey = cfg.svar.vid in
    let ret = ref IDF.NoChange in
    let inspect = I.inspector#mem fn in
    (* Check if it's already summarized -- from the bootstrap, etc. 
       TODO: Improve API for accessing the summary type *)
    ret := if (not (BS.isFinal fkey RS.RaceSumType.id)) then begin
      registerTimeout ret;

      RaceDF.initState cfg input.RS.lState;
      
      (* Compute symstate / ptr info first *)
      L.logStatus "race_a: doing symstate analysis";
      L.flushStatus ();
      let symSumChanged = 
        Stat.time "Computing symstate DF: " SPTA.doSymState cfg in

      L.logStatus "race_a: doing lockset analysis";
      L.flushStatus ();
      Stat.time "Race/Lockset DF:" 
        (fun () ->
           (* Compute locksets *)
           RaceForwardDF.compute cfg.sallstmts;
           
           (* Update read/write correlation info *)
           List.iter 
             (fun stmt ->
                RaceDF.visitStmt stmt) cfg.sallstmts;
        ) ();
      unregisterTimeout (); (* the rest shouldn't take too long *)

      (* Check summary for this function *)
      let outLocks = List.fold_left combRetStates RS.bottomLS cfg.sallstmts in
      let outCorrs (*, outLHat *) = !RaceDF.fiState in
      let outState = RS.makeState outLocks outCorrs in


      (* Check if the new output state can be new *)
      if (not (RS.isBottomState(outState))) then
        (* If state is already bottom, combining won't do anything.
           Otherwise, proceed and check against existing summary   *)
        let curSummary = RS.sum#find fkey in
        if (RS.isBottomSummary (curSummary)) then
          (* if summary is already bottom, new state should overwrite *)
          (let scopedOut = RS.resolveScope outState cfg in
           RS.sum#addReplace fkey (RS.makeSumm input scopedOut);
           if inspect then
             (L.logStatus "Old RS summ is BOTTOM, replacing w/:";
              RS.printState scopedOut;
             );
           IDF.NewOutput (input, scopedOut)
          )
        else
          (* otherwise, try to combine the two *)
          let scopedOut = RS.resolveScope outState cfg in
          let curSumOut = RS.summOutstate curSummary in
          if (Stat.time "LS subset test" 
                (RS.statesSubset scopedOut) curSumOut) then
            noChangeInRS symSumChanged inspect input curSumOut
          else (
            if inspect then (
              L.logStatus "Trying to combine outState and curSummary";
              L.logStatus "scoped outState";
              RS.printState scopedOut;
              L.logStatus "\n\ncurSumOut";
              RS.printState curSumOut;
            );
            let combOut = Stat.time "LS combineStates" 
              (RS.combineStates curSumOut) scopedOut in
            if inspect then (
              L.logStatus "Finished combining outState and curSummary";
            );
            RS.sum#addReplace fkey (RS.makeSumm input combOut);
            IDF.NewOutput (input, combOut);
          )
      else
        (* New output is actually bottom... use old summary *)
        let curSummary = RS.sum#find fkey in
        let curOut = RS.summOutstate curSummary in
        noChangeInRS symSumChanged inspect input curOut
    end else begin
      L.logStatus ("func: " ^ (string_of_fNT (fn,ft)) ^ " already done");
      if inspect then
        (let curSummary = RS.sum#find fkey in
         let curSumOut = RS.summOutstate curSummary in
         L.logStatus "curSumOut:";
         RS.printState curSumOut;
        );
      IDF.NoChange
    end;
    !ret


  let flushSummaries () =
    RS.sum#serializeAndFlush;
    SS.sum#serializeAndFlush
      

        
  let doFunc ?(input:state = RS.emptyState) (fk: fKey) (f:simpleCallN) 
      : state IDF.interResult =
    let fn, ft = f.name, f.typ in
    L.logStatus ("Summarizing function: " ^ fn ^ " : " ^ ft);
    L.logStatus "-----";
    L.flushStatus ();
    let ast = !DC.astFCache#getFile f.defFile in
    Stat.time "AA setup" A.setCurrentFile ast;
    match getCFG fk ast with
      Some cfg ->
        let result = summarizeFun cfg input in
        result
          
    | None ->
        (* Don't have function definition *)
        L.logError ("doFunc can't get CFG for: " ^ 
                        (string_of_fNT (fn, ft)));
        IDF.NoChange 


  (** TRUE if the function should be put on the worklist *)
  let filterFunc f : bool =
    true

  (* TODO: make this not use all of the summary types, just the ones needed *)
  let hardCodedSumTypes ()  =
    !BS.allTypes

  (** Prepare to start an scc, acquiring the required summaries *)
  let sccStart (scc:scc)  = begin
    (* Get all summaries for all callees *)
    let callees = IntSet.fold
      (fun neighSCCID curList ->
         let neighSCC = IntMap.find neighSCCID !curSCCCG in
         FSet.fold
           (fun f curList ->
              f :: curList
           ) neighSCC.scc_nodes curList
      ) scc.scc_callees [] in
    L.logStatus "Acquiring needed summaries";
    L.flushStatus ();
    prepareSumms callees (hardCodedSumTypes ());
    (* Try to re-use summaries that are already on disk? 
       May only be partial summaries, but we can still use them. *)
(*    discoverSumms (FSet.elements scc.scc_nodes) fSummaries;
*)
  end


  (** Scc is summarized. Do the bookkeeping / cleanup *) 
  let sccDone (scc:scc) (byThisGuy:bool) =
    let summPaths = if (byThisGuy) then
      (try
         (* Debugging *)
         FSet.iter 
           (fun fkey ->
              L.logStatus ("Summary for function: " ^ 
                              (string_of_fkey fkey));
              L.logStatus "=======\n";
              RS.printSummary fkey;
              SS.printSummary fkey;
           ) scc.scc_nodes;
         
         (* Serialize and record where each fun was placed *)
         flushSummaries ();


         (* Find out where the summaries were stored *)
         (* TODO: force them to pick the same directory 
            (which they do unless on partition runs out of space) *)
         let tokenMap = RS.sum#locate (FSet.elements scc.scc_nodes) in
         
         (* Notify others that the functions are now summarized *)
         List.fold_left
           (fun paths (fkey, tok) ->
              let path = BS.pathFromToken tok in
(*              BS.setDone fkey; *)
              (fkey, path) :: paths
           ) [] tokenMap
       with e ->
         L.logError ("Caught exception in sccDone?" ^ 
                         (Printexc.to_string e));
         raise e
      ) else [] in
    updateStats scc; (* and possibly delete obsolete summaries *)
    summPaths
    (* RFC.clear !astFCache *)

end

module BUDataflow = IDF.BottomUpDataflow (RaceBUTransfer)


