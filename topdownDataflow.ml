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


(** Worklist, etc. for top-down inter-procedural data flow analysis *)

open Cil
open Pretty
open Fstructs
open Logging
open Cildump

(* Context-insensitive CG... more like a list of functions *)
open Callg

module Stat = Mystats
module PPH = Ciltools.PPHash


(** Parameters for lattice and context sensitivity *)
module type TopDownStateAndKeys  = sig
    
  type state (** The type of the data propagated from function to function. *)

  (** Give some flexibility for context sensitivity. 
      Functions w/ same funID will share the same dataflow tables *)
  type funID
  val compareFunID : funID -> funID -> int
  val fkeyOfID : funID -> fKey
  val funIDToSumKey : funID -> Summary_keys.sumKey

  val compareState : state -> state -> int
    (** comparison function to detect duplicate states *)

  val isBottom : state -> bool
    (** detect unreachable code *)

  val combinePredecessors : state -> state -> state option 
    (** return [None] if the merge of the states would be no different,
        or return [Some st] where [st] is the new state *)

  val diffState : state -> state -> state
    (* Temporarily for debugging *)

  val printState : state -> unit

  val sumType : string
    (** short string identifier to use as summary type *)

end

(** Info needed by the inter-proc driver *)
module type TopDownTransfer = sig
  include TopDownStateAndKeys

  type returnPoint = funID * prog_point
  type calleeFlow = funID * state

  val setFunc : fundec -> (funID * state) -> unit
    
  (****** Post functions ******)

  val doAssign : lval -> exp -> location -> state -> 
    (state * (returnPoint list))

  val doASM : attributes -> string list 
    -> (string option * string * lval) list 
    -> (string option * string * exp) list
    -> string list -> location -> state -> state

  val doCall : lval option -> exp -> exp list -> location -> state-> 
    (calleeFlow list * returnPoint list * state option)
      (** Return a flows into callee functions that are NEW,
          as well as a state flow to the next program point if ready *)

  val doStmt : stmt -> state -> state 
    (** Process a statement returning the new state and a list
        of follow-up program points to propagate the state *)

  val doReturn : stmt ->  state -> (bool * returnPoint list)
    (** Process a return stmt. Return true if there is a new summary 
        value to propagate to callers *)

  val doGuard : exp -> state -> state Dataflow.guardaction
   
  (***** Set up *****)

  val initWork: string -> FSet.t -> callG -> (funID * state) list
    (** Return an initial worklist w/ initial input states for functions,
        given the root directory and a collection of functions (callG) *)

  val moreWork: callG -> FSet.t -> (funID * state) list
    (** Ask for more roots given that the worklist is empty while
        only having analyzed the given functions *)

  (***** Limiter *****)
    
  val checkRestart: funID -> state -> (state * bool)
    
end

  
(** Output interface of dataflow driver *)
module type S = sig

  type state
  type funID

  (** Run the analysis on the entire program given:
      (1) the callgraph (more like a list of functions + def. file)
      (2) the root directory where ASTs are stored *)
  val compute : string -> callG -> unit

  (**** Operations to query state after computation *)

  val getInfoFunID : funID -> state PPH.t 

  val foldOnFKey : ((funID * state PPH.t) -> 'a -> 'a) -> 
    fKey -> 'a -> 'a

  val releaseInfo : funID * state PPH.t -> unit

end


(** Managing function dataflow information and worklist *)
module CachedWorklist (T: TopDownTransfer) = struct

 (** Reference to (approximate) call graph -- to identify the files
     that hold serialized CFGs *)
  let theCG = ref emptyCG

  (***** Function dataflow information ******)

  type caller = T.returnPoint

  let compareCaller (id1, pp1) (id2, pp2) =
    let c = Pervasives.compare pp1 pp2 in
    if c == 0 then T.compareFunID id1 id2
    else c
      
  module CSet = Set.Make (
    struct
      type t = caller
      let compare = compareCaller
    end )
          
  type stateKind = 
      FlowSens of T.state PPH.t
    | FlowInsens of T.state
        
  type funcData = {
    mutable fState : stateKind;
    fCallers : CSet.t;
    mutable fInput : T.state option; (* evolving function input state *)
  }

  module FM = Map.Make (
    struct 
      type t = T.funID
      let compare a b = T.compareFunID b a
    end )

  let fcSize = 80 (* Tune this based on amount of free memory? *)
  let funCache = ref FM.empty

  let cacheHasSpace () =
    Stdutil.mapSize !funCache FM.fold < fcSize

  (**** On-disk images of the function's dataflow information ****)

  let dummyState = FlowSens (PPH.create 0)

  let dummyFuncData = 
    { fState = dummyState;
      fCallers = CSet.empty; 
      fInput = None; }
      
  module FPDFInfo  = struct
    type t  = funcData
    type simpleSum  = t
    let simplify x = x
    let desimplify x = x
    let initVal = dummyFuncData
    let unknownSummary = dummyFuncData
  end
      
  module FPDFSums = Backed_summary.Make(FPDFInfo)

  let fpinfoSums = new FPDFSums.data 
    (Backed_summary.makeSumType (T.sumType ^ "_df"))
  let () = Backed_summary.registerType fpinfoSums

  let string_of_funID funID = 
    Summary_keys.string_of_sumKey (T.funIDToSumKey funID)

  let d_callers fpData =
    text "Callers: { " ++ 
      seq_to_doc (text ", ") CSet.iter 
      (fun (callerID, pp) -> 
         text (string_of_funID callerID) ++
           text (Printf.sprintf "(%d, %d)" pp.pp_stmt pp.pp_instr))
      fpData.fCallers nil ++ text "}" ++ line 

  let printFpinfoSums fpData =
    logStatus "Dataflow table:";
    (match fpData.fState with
       FlowSens stTables ->
         PPH.iter (fun pp st ->
                     logStatusF "State @ %s\n" (string_of_pp pp);
                     T.printState st) stTables;
     | FlowInsens st ->
         logStatus "FlowInsens state";
         T.printState st);
    logStatusD (d_callers fpData)
      
  (************************************************************)

  (** Evict the entries in the funCache, saving the dataflow info *)
  let evictFunctions () =
    FM.iter 
      (fun funID fpData ->
         let sumKey = T.funIDToSumKey funID in
         (* Forcibly mark summary dirty too *)
         fpinfoSums#addReplace sumKey fpData;
      ) !funCache;
    let () = fpinfoSums#serializeAndFlush in
    funCache := FM.empty


  (***** Worklist management ******)

  type workKind = 
      WCall of T.state * CSet.t (* queue call w/ input and additional callers *)
    | WIntra of prog_point  (* next pp *)
    | WReturn of caller     (* The particular callsite to return to *)
        (* Only use these if flow sensitive -- check when we first get
           the work list for a function *)

  type workEntry = {
    wFun : T.funID;    (* Function that should be loaded to perform work *)
    mutable wKind : workKind;
  }

  (* work within a function *)
  module FQueue = Set.Make (
    struct 
      type t = workEntry
      let compare a b = 
        (* Hmm... assume all the wFuns are the same anyway? *)
        match a.wKind, b.wKind with
          (* Sort by prog point, so calls come first, then internal, then ret *)
          WCall (st1, cs1), WCall (st2, cs2) ->
            let c = T.compareState st1 st2 in
            if c == 0 then CSet.compare cs1 cs2 
            else c
        | WCall _, WIntra _
        | WCall _, WReturn _ -> -1
        | WIntra a, WIntra b -> comparePP a b
        | WIntra _, WReturn _ -> -1
        | WIntra _, WCall _ -> 1
        | WReturn a, WReturn b -> compareCaller a b
        | WReturn _, WCall _
        | WReturn _, WIntra _ -> 1
    end)

  let worklist : FQueue.t FM.t ref = ref FM.empty
  let prevQueue = ref (FQueue.empty)

  let noWorkLeft () =
    FM.is_empty !worklist

  let workLeft () =
    FM.fold (fun fm q cur -> cur + FQueue.cardinal q) !worklist 0

  let initializeFunInfo cfg wCall =
    match wCall.wKind with
      WCall (inState, callers) ->
        let dfInfo = PPH.create (List.length cfg.sallstmts) in
        (match cfg.sallstmts with
           start :: _ -> PPH.add dfInfo (getStmtPP start) inState
         | [] -> failwith "cfg has no statements?");
        { fState = FlowSens dfInfo;
          fCallers = CSet.empty;  (* add callers when we start the call *)
          fInput = None;          (* add input when we start the call *)
        } 
    | _ -> failwith "initializeFunInfo not given a Call"

  let findCallWork queue =
    FQueue.fold 
      (fun we cur -> 
         match cur with 
           None -> (match we.wKind with WCall _ -> Some we | _ -> cur)
         | Some _ -> cur) queue None 

  let getCfg funID =
    let fkey = T.fkeyOfID funID in
    let fnode = FMap.find (fkey_to_fid fkey) !theCG in
    match Cilinfos.getFunc fkey fnode.defFile with
      None -> failwith ("getCfg: no CFG -- " ^ fnode.name)
    | Some cfg -> cfg

  let addFunctionToCache funID queue =
    let sumKey = T.funIDToSumKey funID in
    let fpInfo = fpinfoSums#find sumKey in
    if fpInfo = dummyFuncData then begin
      (* Initialize dataflow table for the first time -- 
         we should only need this the first time a function is called *)
      let cfg = getCfg funID in
      match findCallWork queue with
        Some we -> 
          let fpInfo = initializeFunInfo cfg we in
          let () = fpinfoSums#addReplace sumKey fpInfo in
          funCache := FM.add funID fpInfo !funCache;
      | _ -> failwith "addFunctionToCache: starting function non-call"

    end else
      funCache := FM.add funID fpInfo !funCache


  (** Find other calls to this same function and merge its info to 
      prevent repeatedly starting the function *)
  let mergeWork we oldQ =
    match we.wKind with
      WCall (i, cs) ->
        (match findCallWork oldQ with
           Some otherCall ->
             (match otherCall.wKind with
                WCall (i2, cs2) ->
                  if T.compareState i i2 == 0 && CSet.subset cs cs2 
                  then oldQ
                  else
                    let inSt = 
                      match T.combinePredecessors i i2 with
                        None -> i
                      | Some i -> i 
                    in
                    let newCall = 
                      { we with wKind = WCall (inSt, CSet.union cs cs2); } in
                    FQueue.add newCall (FQueue.remove otherCall oldQ)
              | _ -> failwith "findCallWork returned non-call" )
         | None ->
             FQueue.add we oldQ)
    | WIntra _ | WReturn _ -> 
        FQueue.add we oldQ


  let addWork we =
    try 
      let oldQ = FM.find we.wFun !worklist in
      let newQ = mergeWork we oldQ in
      if oldQ == !prevQueue then (* ugh... *)
        prevQueue := newQ;
      worklist := FM.add we.wFun newQ !worklist
    with Not_found -> 
      let newQ = FQueue.singleton we in
      worklist := FM.add we.wFun newQ !worklist
        
        
  (* TODO: sort functions (in FM) so that callees get run before callers.
     For now, just try to exhaust current function before doing others *)


  (** Re-populate the funcache w/ functions that are actually in the worklist
      -- return true worklist was non-empty *)
  let rebuildFunCache worklist =
    evictFunctions ();
    let cacheSpacey = cacheHasSpace () in
    assert cacheSpacey;
    let (_:bool) = 
      FM.fold (fun funID q hasSpace -> 
                 if hasSpace then (addFunctionToCache funID q; cacheHasSpace ())
                 else hasSpace ) 
        !worklist cacheSpacey in
    not (noWorkLeft ())

  (** Look for anything work entries that match functions in the funCache *)
  let findCheapWork () =
    FM.fold 
      (fun funID q cur -> 
         match cur with 
           Some x -> cur 
         | None ->
             if FM.mem funID !funCache then Some q
             else cur
      ) !worklist None

  exception AllDone

  let rec getNextWork () =
    let takeWork q =
      let we = FQueue.choose q in
      prevQueue := FQueue.remove we q;
      if FQueue.is_empty !prevQueue
      then worklist := FM.remove we.wFun !worklist
      else worklist := FM.add we.wFun !prevQueue !worklist;
      we
    in
    if not (FQueue.is_empty !prevQueue) then takeWork !prevQueue
    else 
      match findCheapWork () with
        None -> 
          if rebuildFunCache worklist then getNextWork ()
          else raise AllDone
      | Some q -> 
          prevQueue := q;
          takeWork q

  (* Ugh... need to expose work queue so that we can collapse work 
     when we switch to being flow insensitive? *)
  let dropWork funID = 
    (try
       let oldQ = FM.find funID !worklist in
       if oldQ == !prevQueue then
         prevQueue := FQueue.empty;
     with Not_found-> ());
    worklist := FM.remove funID !worklist


end


(** Main Top-down dataflow engine *)
module TopDownDataflow  (T: TopDownTransfer) = struct
  include CachedWorklist (T)

  type state = T.state
  type funID = T.funID
    
  (****** Misc statistics *******)

  let totalIter = ref 0
  let touchedFuncs = ref FSet.empty

  let inspecting = ref false

  (****** Misc utils / state management ******)

  let notifyNewState oldSt st pp =
    if !inspecting then 
      (match oldSt with 
         Some oldSt -> 
           logStatusF "New state (diff) at %s %s\n" 
             (string_of_loc !currentLoc) (string_of_pp pp);
           T.printState (T.diffState oldSt st);
       | None ->
           logStatusF "New state at %s %s\n" 
             (string_of_loc !currentLoc) (string_of_pp pp);
           T.printState st )
    else
      if T.isBottom st then
        (logStatusF "New state at %s %s\n" (string_of_loc !currentLoc) 
           (string_of_pp pp);
         T.printState st)


  let getState fState pp =
    match fState with 
      FlowSens stTables -> PPH.find stTables pp
    | FlowInsens _ -> failwith "grabbing state when flowInsens" 
        (* TODO: make it legal when we know we've collapsed all the 
           work entries in teh work queue *)

  let updateState fInfo pp oldD d =
    notifyNewState oldD d pp;
    match fInfo.fState with
      FlowSens stTables ->
        PPH.replace stTables pp d
    | FlowInsens old ->
        fInfo.fState <- FlowInsens (d)

  let setCurrentLocation cfg funInfo pp =
    let parentStmt = lookupStmtCFG cfg pp in
    match lookupStmtInstr parentStmt pp with
      PPStmt stmt ->
        Cil.setStmtLocation stmt
    | PPInstr instr ->
        Cil.setInstrLocation instr pp.pp_instr

  let getNextInstrPPs parentStmt pp =
    (* Check if it's the last instruction in the stmt list...
       if it IS, then propagate to the parent stmts successors *)
    match parentStmt.skind with
      Instr il -> 
        let nextI = pp.pp_instr + 1 in
        if nextI >= List.length il then List.map getStmtPP parentStmt.succs
        else [ { pp with pp_instr = nextI; } ]
    | _ -> failwith "getNextInstrPPs:  non-instr parent"

  let getFirstPP cfg =
    getStmtPP (List.hd cfg.sallstmts)

  let notifyStep pp =
    if !inspecting then
      logStatusF "step @ %s %s\n" (string_of_loc !currentLoc)
        (string_of_pp pp)

  let getStateFail pp fInfo =
    logStatus "Can't find state for when finfo is:\n";
    printFpinfoSums fInfo;
    failwith "Death"

  (********** Work constructors ************)

  let makeCallWork callerID callerPP (calleeID, calleeInSt) =
    { wFun = calleeID;
      wKind = WCall (calleeInSt, CSet.singleton (callerID, callerPP)); }

  let reachCallees callerID pp callees =
    List.map (makeCallWork callerID pp) callees

  (* TODO: check if the dude is flow insensitive first -- otherwise 
     we repeat work? Or we can check when we first get the queue... 
     for a particular function *)

  let reachCallers calleeID calleeInfo =
    CSet.fold (fun (callerID, callerPP) cur -> 
                 { wFun = callerID;
                   wKind = WReturn (callerID, callerPP); } :: cur ) 
      calleeInfo.fCallers []

  let reachGlobalReaders readers curWork =
    if readers <> [] then
      (logStatus "re-queueing global readers\n";
       List.fold_left (fun cur (readerID, readerPP) ->
                         { wFun = readerID;
                           wKind = WIntra readerPP; } :: cur) curWork readers)
    else curWork

  let makeIntraWork funID succPP =
    { wFun = funID;
      wKind = WIntra succPP; }

  let notifyChange descr calleeID calleeInfo =
    let calleeCfg = getCfg calleeID in
    logStatusF "=============\n%s for %s (%s)\n" descr calleeCfg.svar.vname 
      (string_of_funID  calleeID);
    let doc = d_callers calleeInfo in
    logStatusD doc

  let printGotMoreCallers calleeID calleeInfo =
    notifyChange "Got more callers" calleeID calleeInfo
      
  let notifyFlowInsens descr funID fInfo =
    notifyChange (descr ^ " Flow Insensitive") funID fInfo

  let makeFlowInsens fInfo funID newIn cfg =
    match fInfo.fState with
        FlowInsens oldSt -> 
          (match T.combinePredecessors oldSt newIn with
             None -> []
           | Some x -> 
               notifyFlowInsens "Redoing" funID fInfo;
               (* Just queue it up for now as "intraproc" work *)
               fInfo.fState <- FlowInsens x;
               [makeIntraWork funID (getFirstPP cfg)]
          )
            
      | FlowSens stTables ->
          notifyFlowInsens "Doing" funID fInfo;
          let combo = PPH.fold 
            (fun pp st cur ->
               (match T.combinePredecessors cur st with
                  None -> cur
                | Some x -> x) ) stTables newIn in
          fInfo.fState <- FlowInsens combo;
          (* Just queue it up for now as "intraproc" work *)
          [makeIntraWork funID (getFirstPP cfg)]


         
  (** Merge the [newState] w/ the successor's existing dataflow info.
      If new, add successor to worklist *)
  let reachSuccessor cfg funID funInfo newState curWork succPP =
    setCurrentLocation cfg funInfo succPP;
    try
      let old = getState funInfo.fState succPP in
      (match T.combinePredecessors old newState with
         None -> curWork
       | Some x -> 
           updateState funInfo succPP (Some old) x;
           makeIntraWork funID succPP :: curWork)
    with Not_found ->
      updateState funInfo succPP None newState;
      (makeIntraWork funID succPP) :: curWork
        
  let reachIfSuccs cfg funID fInfo state stmt ifExp =
    let not_e = UnOp(LNot, ifExp, intType) in
    let thenGuard = T.doGuard ifExp state in
    let elseGuard = T.doGuard not_e state in
    if thenGuard = Dataflow.GDefault && elseGuard = Dataflow.GDefault then
      (* this is the common case *)
      List.fold_left (reachSuccessor cfg funID fInfo state) [] 
        (List.map getStmtPP stmt.succs)
    else begin
      let doBranch succ curWork guard =
        let succPP = getStmtPP succ in
        match guard with
          Dataflow.GDefault -> 
            reachSuccessor cfg funID fInfo state curWork succPP
        | Dataflow.GUse d ->  
            reachSuccessor cfg funID fInfo d curWork succPP
        | Dataflow.GUnreachable -> 
            logStatusF "Not exploring branch to %s %s\n" 
              (string_of_loc (get_stmtLoc succ.skind)) (string_of_pp succPP);
            curWork
      in
      let thenSucc, elseSucc = Dataflow.ifSuccs stmt in
      let work = doBranch thenSucc [] thenGuard in
      doBranch elseSucc work elseGuard
    end


  (** Handle intra-procedural step and return successor work (if needed) *)
  let handleIntra funID fInfo pp : workEntry list =
    let cfg = getCfg funID in
    inspecting := Inspect.inspector#mem cfg.svar.vname;
    (match fInfo.fInput with 
       Some inSt -> T.setFunc cfg (funID, inSt)
     | None -> failwith "starting a func that had no input?") ;
    let parentStmt = lookupStmtCFG cfg pp in
    let stmtPP = getStmtPP parentStmt in
    Cil.setStmtLocation parentStmt; (* Must set this either way *)
    match lookupStmtInstr parentStmt pp with
      PPStmt stmt ->
        notifyStep pp;
        let startState = 
          try getState fInfo.fState pp 
          with Not_found -> getStateFail pp fInfo
        in
        let startState = T.doStmt stmt startState in
        (match stmt.skind with
           Instr il -> 
             let succPP = getNextInstrPPs stmt stmtPP in
             List.fold_left (reachSuccessor cfg funID fInfo startState) [] succPP
               
         | If (e, _, _, _) -> 
             reachIfSuccs cfg funID fInfo startState stmt e 

         | Goto _ | Break _ | Continue _
         | TryExcept _ | TryFinally _ | Switch _ | Loop _
         | Block _ ->
             List.fold_left (reachSuccessor cfg funID fInfo startState) [] 
               (List.map getStmtPP stmt.succs)

         | Return (exp, loc) ->
             let newSum, toNotify = T.doReturn parentStmt startState in
             let work = 
               if newSum then reachCallers funID fInfo else [] 
             in
             reachGlobalReaders toNotify work
        )

    | PPInstr instr ->
        Cil.setInstrLocation instr pp.pp_instr;
        notifyStep pp;
        let startState = 
          try getState fInfo.fState pp 
          with Not_found -> getStateFail pp fInfo
        in
        (match instr with
           (* TODO: make an instruction kind for the Block Assign? *)
           Set (lhs, rhs, loc) -> 
             let outSt, toNotify = T.doAssign lhs rhs loc startState in
             let nextPPs = getNextInstrPPs parentStmt pp in
             let work = 
               List.fold_left (reachSuccessor cfg funID fInfo outSt) 
                 [] nextPPs in
             reachGlobalReaders toNotify work
               
         | Asm (a,b,c,d,e,f) -> 
             let outSt = T.doASM a b c d e f startState in
             let nextPPs = getNextInstrPPs parentStmt pp in
             List.fold_left (reachSuccessor cfg funID fInfo outSt) 
               [] nextPPs

         | Call (lvopt, ce, args, loc) -> 
             let calleeFlows, toNotify, outSt = 
               T.doCall lvopt ce args loc startState in
             match outSt with
               Some outSt -> 
                 let nextPPs = getNextInstrPPs parentStmt pp in
                 let calleeWork = reachCallees funID pp calleeFlows in
                 let work = 
                   List.fold_left (reachSuccessor cfg funID fInfo outSt) 
                     calleeWork nextPPs in
                 reachGlobalReaders toNotify work
             | None ->
                 let work = reachCallees funID pp calleeFlows in
                 reachGlobalReaders toNotify work
                   (* doCall should return the calleeFlow only if the summary 
                      doesn't already exist... this assumes sumKey is 
                      tied to input state... *)
        )


  (** Handle flow-insensitive intra-procedural steps *)
  let handleFlowInsIntra funID fInfo : workEntry list =
    let cfg = getCfg funID in
    inspecting := Inspect.inspector#mem cfg.svar.vname;
    (match fInfo.fInput with 
       Some inSt -> T.setFunc cfg (funID, inSt)
     | None -> failwith "starting a func that had no input?");

    let startState = match fInfo.fState with 
        FlowInsens x -> x 
      | FlowSens _ -> failwith "handleFlowInsIntra given flow sens" in

    let comboState curSt nextSt =
      match T.combinePredecessors curSt nextSt with
        None -> curSt
      | Some x -> x 
    in

    let handleInstrs (curSt, globReads, calls, instrIndex) instr =
      Cil.setInstrLocation instr instrIndex;
      let pp = getCurrentPP () in
      notifyStep pp;
      let nextSt, globReads, calls = 
        (match instr with
           Set (lhs, rhs, loc) -> 
             let outSt, toNotify = T.doAssign lhs rhs loc curSt in
             outSt, List.rev_append toNotify globReads, calls
               
         | Asm (a,b,c,d,e,f) -> 
             let outSt = T.doASM a b c d e f curSt in
             outSt, globReads, calls
               
         | Call (lvopt, ce, args, loc) -> 
             let calleeFlows, toNotify, outSt = 
               T.doCall lvopt ce args loc curSt in
             let moreCalls = reachCallees funID pp calleeFlows in
             match outSt with
               Some outSt -> 
                 (outSt, List.rev_append toNotify globReads, 
                  List.rev_append moreCalls calls )
             | None ->
                 (curSt, List.rev_append toNotify globReads, 
                  List.rev_append moreCalls calls)) in
      (comboState curSt nextSt, globReads, calls, instrIndex + 1)             
    in
    
    let finalSt, shouldReturn, globReads, calls =
      List.fold_left
        (fun (curSt, shouldReturn, globReads, calls) stmt ->
           Cil.setStmtLocation stmt; 
           (match stmt.skind with
              Instr il ->
                let nextSt, globReads, calls, _ = 
                  List.fold_left handleInstrs (curSt, globReads, calls, 0) il in
                (nextSt, shouldReturn, globReads, calls)
                  
            | Return (exp, loc) ->
                let pp = getCurrentPP () in
                notifyStep pp;
                let newSum, toNotify = T.doReturn stmt curSt in
                (curSt, shouldReturn || newSum, 
                 List.rev_append toNotify globReads, calls)

            | If _ | Goto _ | Break _ | Continue _ | Block _ 
            | TryExcept _ | TryFinally _ | Switch _ | Loop _ ->
                (curSt, shouldReturn, globReads, calls)
           )
        ) (startState, false, [], []) cfg.sallstmts in
    let maybeOtherFuncWork = reachGlobalReaders globReads calls in
    match T.combinePredecessors startState finalSt with
      None -> 
        (* Just the inter-proc flows *)
        maybeOtherFuncWork 
    | Some newSt -> 
        (* Inter-proc flows plus one more round on self *)
        logStatusF "Iter flowInsensitive again: %s\n" (string_of_funID funID);
        fInfo.fState <- FlowInsens newSt;
        makeIntraWork funID (getFirstPP cfg) :: maybeOtherFuncWork

 
  let printCallers descr calleeID inState calleeInfo =
    let calleeCfg = getCfg calleeID in
    logStatusF "=============\n%s call for %s (%s)\n" 
      descr calleeCfg.svar.vname (string_of_funID calleeID);
    let doc = d_callers calleeInfo in
    logStatusD doc;
    logStatus "Input state:";
    T.printState inState

  let queueCall descr funID newCallers fInfo input cfg =
    (* More like start call... since we do handle intra right away! *)
    let fInfo = { fInfo with 
                    fCallers = CSet.union fInfo.fCallers newCallers; 
                    fInput = Some input;
                } in
    funCache := FM.add funID fInfo !funCache;
    printCallers descr funID input fInfo;
    match fInfo.fState with
      FlowSens stTables ->
        let firstPP = getFirstPP cfg in
        PPH.add stTables firstPP input;
        handleIntra funID fInfo firstPP

    | FlowInsens _ -> 
        makeFlowInsens fInfo funID input cfg

  let queueReturns funID fInfo cfg =
    List.iter 
      (fun stmt ->
         match stmt.skind with 
           Return _ -> 
             let pp = getStmtPP stmt in
             (try (* Only queue if we've reached it before *)
                let _ = getState fInfo.fState pp in               
                addWork { wFun = funID; wKind = WIntra pp; }
              with Not_found -> ())
         | _ -> () ) cfg.sallstmts
      
      
  let startCall funID fInfo inState newCallers =
    let cfg = getCfg funID in
    match fInfo.fInput with
      None -> (* first call *)        
        queueCall "Starting" funID newCallers fInfo inState cfg
    | Some oldIn ->
        (match T.combinePredecessors oldIn inState with
           None -> 
             if CSet.subset newCallers fInfo.fCallers then []
             else begin
               let fInfo = 
                 { fInfo with 
                     fCallers = CSet.union fInfo.fCallers newCallers; } in
               printGotMoreCallers funID fInfo;
               funCache := FM.add funID fInfo !funCache;
               []
             end
         | Some newIn ->
             let newIn, switch = T.checkRestart funID newIn in
             if not switch then begin
               queueReturns funID fInfo cfg;
               queueCall "Restarting" funID newCallers fInfo newIn cfg;
             end else begin
               let fInfo = 
                 { fInfo with 
                     fCallers = CSet.union fInfo.fCallers newCallers; 
                     fInput = Some newIn;
                 } in
               funCache := FM.add funID fInfo !funCache;
               makeFlowInsens fInfo funID newIn cfg
             end
        )


  let stepFlowSensitive fInfo wEntry =
    match wEntry.wKind with
      WCall (inState, newCallers) ->
        startCall wEntry.wFun fInfo inState newCallers
    | WIntra pp
    | WReturn (_, pp) -> 
        (* assume it's returning to the call instruction... *)
        handleIntra wEntry.wFun fInfo pp


  let stepFlowInsens fInfo wEntry =
    match wEntry.wKind with
      WCall (inState, newCallers) ->
        startCall wEntry.wFun fInfo inState newCallers
    | WIntra _
    | WReturn (_, _) ->
        (* Step through the whole damn thing *)
        handleFlowInsIntra wEntry.wFun fInfo

        
  (************* Queue management / Misc ***************)


  let updateSteps () =
    incr totalIter;
    if !totalIter mod 100 == 0 then
      logStatusF "Step #%d\n" !totalIter;
    if !totalIter mod 10000 == 0 then
      Stat.print stdout "FPA: "

  let addTouchedFunc wEntry =
    let contextInsensFunID = fkey_to_fid (T.fkeyOfID wEntry.wFun) in
    touchedFuncs := FSet.add contextInsensFunID !touchedFuncs

  let printCompletedFuncs () =
    let doneFuncs = d_funcset !theCG !touchedFuncs in
    logStatus "====================================";
    logStatus "Completed functions:";
    logStatusD doneFuncs;
    logStatusF "Total: %d\n" (FSet.cardinal !touchedFuncs);
    logStatus "===================================="

  let enqueueInitial stateKeys =
    List.iter 
      (fun (funID, st) -> 
         let work = { wFun = funID;
                      wKind = WCall (st, CSet.empty); } in
         addWork work) stateKeys

  let mergeFlowInsWork fInfo wEntry =
    dropWork wEntry.wFun

      
  (** Return true if we really can get more work -- 
      only call this when we are out of work! *)
  let canGetMoreWork () =
    assert (noWorkLeft ());
    let moreWork = Stat.time "moreWork" (T.moreWork !theCG) !touchedFuncs in
    if moreWork = [] then false
    else 
      (logStatusF "Got more work: %d\n" (List.length moreWork);
       enqueueInitial moreWork; 
       true)

  (** Process funWork until a fixed-point is reached *) 
  let fixedPoint () =
    (* Relying on the short-circuitry to only get more work when out *)
    while not (noWorkLeft ()) || canGetMoreWork () do
      updateSteps ();
      let wEntry = getNextWork () in
      addTouchedFunc wEntry;
      let fInfo = FM.find wEntry.wFun !funCache in
      let toEnqueue = 
        match fInfo.fState with
          FlowSens _ -> stepFlowSensitive fInfo wEntry 
        | FlowInsens _ -> 
            mergeFlowInsWork fInfo wEntry;
            stepFlowInsens fInfo wEntry
      in
      List.iter addWork toEnqueue
    done;
    printCompletedFuncs ()

  (************************************************************)

  (** Iterate to a fixed-point on all the functions/asts stored in the 
      given root directory. *)
  let compute rootDir addrTk cg =
    theCG := cg;
    let initialWork = Stat.time "initWork" (T.initWork rootDir addrTk) cg in
    enqueueInitial initialWork;
    logStatusF "InterDF initial worklist: %d\n" (workLeft ());
    fixedPoint ();
    logStatusF "InterDF DONE: %d iters\n" !totalIter

  (**** Operations to query state after computation *)

  let getInfoFunID funID =
    let sumKey = T.funIDToSumKey funID in
    let fpInfo = fpinfoSums#find sumKey in
    fpInfo.fState

  let releaseInfo (funID, state) =
    fpinfoSums#evictSummaries

  let foldOnFKey foo fKey acc =
    fpinfoSums#foldOnKey 
      (fun sumKey info acc -> foo sumKey info.fState acc) fKey acc


end
  
