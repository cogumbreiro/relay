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
open Df_notify

(* Context-insensitive CG... more like a list of functions *)
open Callg

module Stat = Mystats
module PPH = Ciltools.PPHash


(** Parameters for lattice and context sensitivity *)
module type RciDfInfoAndKeys  = sig
    
  type dfInfo (** The type of the data propagated from function to function. *)

  (** Give some flexibility for context sensitivity. 
      Functions w/ same funID will share the same dataflow tables *)
  type funID
  val compareFunID : funID -> funID -> int
  val fkeyOfID : funID -> fKey
  val funIDToSumKey : funID -> Summary_keys.sumKey

  val compareDfInfo : dfInfo -> dfInfo -> int
    (** comparison function to detect duplicate dfInfos *)

  val isBottom : dfInfo -> bool
    (** detect unreachable code *)

  val combinePredecessors : fundec -> dfInfo -> dfInfo -> dfInfo option 
    (** return [None] if the merge of the dfInfos would be no different,
        or return [Some st] where [st] is the new dfInfo *)

  val diffDfInfo : dfInfo -> dfInfo -> dfInfo
    (* Temporarily for debugging *)

  val printDfInfo : dfInfo -> unit

  val sumType : string  (** short identifier to use as summary type *)

  val cacheSize : int

end

(** Info needed by the inter-proc driver *)
module type RciTransfer = sig
  include RciDfInfoAndKeys

  val setFunc : fundec -> (funID * dfInfo) -> unit
    
  (****** Post functions ******)

  val doAssign : lval -> exp -> location -> dfInfo -> 
    (dfInfo * funID toNotify)

  val doASM : attributes -> string list 
    -> (string option * string * lval) list 
    -> (string option * string * exp) list
    -> string list -> location -> dfInfo -> (dfInfo * funID toNotify)

  val doCall : lval option -> exp -> exp list -> location -> dfInfo-> 
    ((funID, dfInfo) callFlows * funID toNotify * dfInfo option)
      (** Return a flows into callee functions, as well as a flow
          to the next program point (if ready) *)

  val doStmt : stmt -> dfInfo -> dfInfo 
    (** Process a dfInfoment returning the new dfInfo and a list
        of follow-up program points to propagate the dfInfo *)

  val doReturn : exp option ->  dfInfo -> funID toNotify
    (** Process a return stmt. Return true if there is a new summary 
        value to propagate to callers *)

  val doGuard : exp -> dfInfo -> dfInfo Dataflow.guardaction
   
  (***** Set up *****)

  val initWork: string -> FSet.t -> callG -> (funID * dfInfo) list
    (** Return an initial worklist w/ initial input dfInfos for functions,
        given the root directory and a collection of functions (callG) *)

  val moreWork: callG -> FSet.t -> (funID * dfInfo) list
    (** Ask for more roots given that the worklist has emptied out
        after having analyzed the given functions *)

  (***** Limiter *****)
    
  val checkRestart: funID -> dfInfo -> (dfInfo * bool)
    
end

  
(** Output interface of dataflow driver *)
module type S = sig

  type dfInfo
  type funID

  (** Run the analysis on the entire program given:
      (1) the callgraph (more like a list of functions + def. file)
      (2) the root directory where ASTs are stored *)
  val compute : string -> callG -> unit

  (**** Operations to query dfInfo after computation *)

  val getInfoFunID : funID -> dfInfo PPH.t 

  val foldOnFKey : ((funID * dfInfo PPH.t) -> 'a -> 'a) -> 
    fKey -> 'a -> 'a

end

(************************************************************)

(** Managing function dataflow information and worklist *)
module CachedWorklist (T: RciTransfer) = struct

  (** Reference to (approximate) call graph -- to identify the files
      that hold serialized CFGs and approximate (pre,post)-visit nums *)
  let theCG = ref FMap.empty
  let funRanks = ref FMap.empty

  (***** Function dataflow information ******)

  type caller = T.funID * prog_point

  let compareCaller (id1, pp1) (id2, pp2) =
    let c = Pervasives.compare pp1 pp2 in
    if c == 0 then T.compareFunID id1 id2
    else c
      
  module CSet = Set.Make (
    struct
      type t = caller
      let compare = compareCaller
    end )

  module CMap = Map.Make (
    struct
      type t = caller
      let compare = compareCaller
    end )

  type dfInfoKind = 
      FlowSens of T.dfInfo PPH.t
    | FlowInsens of T.dfInfo
    | Abandoned of dfInfoKind 
        (* Decide not to analyze any more, but keep old kind around! *)
      
  let makeAbandonedSt oldKind =
    match oldKind with
      Abandoned _ -> oldKind
    | _ -> Abandoned oldKind
  
  type funcData = {
    mutable fDfInfo : dfInfoKind;
    fCallers : CSet.t;
    mutable fInput : T.dfInfo option; (* evolving function input dfInfo *)
  }

  module CompFuns = 
    struct 
      type t = T.funID
      let compare a b = T.compareFunID b a
    end

  module FM = Map.Make (CompFuns)
  module FS = Set.Make (CompFuns)

  let fcSize = T.cacheSize (* Tune this based on amount of free memory? *)
  let funCache = ref FM.empty

  let cacheHasSpace () =
    Stdutil.mapSize !funCache FM.fold < fcSize

  (**** On-disk images of the function's dataflow information ****)

  let dummyDfInfo = FlowSens (PPH.create 0)

  let dummyFuncData = 
    { fDfInfo = dummyDfInfo;
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

  let d_pp pp =
    dprintf "(%d, %d)" pp.pp_stmt pp.pp_instr

  let d_funID funID = 
    text (string_of_funID funID)

  let d_funPP (funID, pp) =
    d_funID funID ++ d_pp pp


  let limitPrintCallers = 32
  let d_callers fInfo =
    text "Callers: { " ++ 
      seq_to_doc_limit 
      (text ", ") CSet.iter 
      d_funPP
      fInfo.fCallers nil limitPrintCallers ++ text "}" ++ line 
      
  let printFpinfoSums fInfo =
    logStatus "Dataflow table:";
    let rec printKind kind =
      match kind with
        FlowSens stTables ->
          PPH.iter (fun pp st ->
                      logStatusF "DfInfo @ %s\n" (string_of_pp pp);
                      T.printDfInfo st) stTables;
      | FlowInsens st ->
          logStatus "FlowInsens dfInfo";
          T.printDfInfo st
      | Abandoned oldStKind ->
          logStatus "Abandoned!!! Pre-abandonment dfInfo is:";
          printKind oldStKind
    in
    printKind fInfo.fDfInfo;
    logStatusD (d_callers fInfo)
      

  (************************************************************)

  let evictFID funID fInfo =
    let sumKey = T.funIDToSumKey funID in
    (* Forcibly mark summary dirty too *)
    fpinfoSums#addReplace sumKey fInfo

  (** Evict the entries in the funCache, saving the dataflow info *)
  let evictFunctions () =
    FM.iter evictFID !funCache;
    let () = fpinfoSums#serializeAndFlush in
    funCache := FM.empty


  (***** Worklist management ******)

  type workKind = 
      WCall of T.dfInfo * CSet.t 
        (* queue call w/ input and additional callers *)
    | WIntra of prog_point  (* next pp *)
    | WReturnTo of caller     (* The particular callsite to return to *)
    | WReturnStmt of prog_point (* like WIntra, but known to be a ret stmt *)

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
          WCall (ist1, cs1), WCall (ist2, cs2) ->
            let c = T.compareDfInfo ist1 ist2 in
            if c == 0 then CSet.compare cs1 cs2 
            else c

        | WIntra a, WIntra b -> comparePP a b
        | WReturnTo a, WReturnTo b -> compareCaller a b
        | WReturnStmt a, WReturnStmt b -> comparePP a b
        | WCall _, WReturnStmt _
        | WIntra _, WReturnStmt _
        | WReturnTo _, WReturnStmt _ -> -1
        | WReturnStmt _, WCall _ 
        | WReturnStmt _, WIntra _
        | WReturnStmt _, WReturnTo _ -> 1

        | WCall _, WIntra _
        | WCall _, WReturnTo _ -> -1

        | WIntra _, WCall _ 
        | WReturnTo _, WCall _ -> 1

        | WReturnTo _, WIntra _ -> 1
        | WIntra _, WReturnTo _ -> -1
    end)

  (* worklist of functions themselves -- sorted by pre-post-visit nums *)
  module InterSort = struct
    type t = T.funID 

    let compare fid1 fid2 =
      if fid1 == fid2 then 0
      else 
        (* We only have the "ranks" for the context-insensitive callgraph,
           so we need to convert the fids to use tha info *)
        let fk1 = T.fkeyOfID fid1 in
        let fk2 = T.fkeyOfID fid2 in
        if fk1 = fk2 then T.compareFunID fid1 fid2
        else 
          try
            let fk1 = fkey_to_fid fk1 in
            let fk2 = fkey_to_fid fk2 in
            let pre1, post1 = FMap.find fk1 !funRanks in
            let pre2, post2 = FMap.find fk2 !funRanks in
            if pre1 < pre2 && post2 < post1 then 1 (* fid1 is ancestor *)
            else if pre2 < pre1 && post1 < post2 then -1 (* fid2 is ancestor *)
            else begin
              let c = post1 - post2 in (* fid1 is more "left-most" *)
              if c == 0 then  (* they are the same fkey??? *)
                failwith "InterSort different fkeys with same post num?"
              else c
            end
          with Not_found -> 
            failwith ("missing pre-post nums " ^ (string_of_funID fid1)
                      ^ " or " ^ (string_of_funID fid2) ^ "\n")
              
  end

  module InterQ = Prioqueuemap.Make (InterSort)

  let worklist : FQueue.t InterQ.t ref = ref (InterQ.create ())
  let prevQueue = ref (None, FQueue.empty)

  let d_workKind work =
    match work.wKind with
      WIntra pp -> text "I-TRA: " ++ d_pp pp
    | WCall (_,_) -> text "CALL: " ++ d_funID work.wFun
    | WReturnTo caller -> text "RET_TO: " ++ d_funPP caller
    | WReturnStmt pp -> text "RET_STMT: " ++ d_pp pp

  let d_localWork worklist =
    text "[" ++ seq_to_doc (text ", ") List.iter d_workKind worklist nil ++ 
      text "]\n"

  let printGlobalWorklist descr =
    logStatusD (dprintf "Global work %s:\n" descr ++ 
                  InterQ.fold
                  (fun id wl cur -> cur ++ text (string_of_funID id) ++
                     d_localWork (FQueue.elements wl))
                  !worklist nil ++ line)
      
  let setPrevQueue (fidOpt, q) =
    prevQueue := (fidOpt, q)

  let setIfPrevQueue newFID newQ =
    match !prevQueue with
      (Some id, _) -> 
        if T.compareFunID id newFID == 0 then
          prevQueue := (Some newFID, newQ)
    | (None, _) -> ()

  let updateWorklist fid newQ = begin
    setIfPrevQueue fid newQ;
    InterQ.addOnce !worklist fid newQ;
(*    printGlobalWorklist "add"  *)
  end

  let noWorkLeft () =
    InterQ.is_empty !worklist

  let workLeft () =
    InterQ.length !worklist (* doesn't include the innards *)

  let initializeFunInfo cfg wCall =
    match wCall.wKind with
      WCall (inDfInfo, callers) ->
        let dfInfo = PPH.create (List.length cfg.sallstmts) in
        (match cfg.sallstmts with
           start :: _ -> PPH.add dfInfo (getStmtPP start) inDfInfo
         | [] -> failwith "cfg has no statements?");
        { fDfInfo = FlowSens dfInfo;
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
    let ciFID = fkey_to_fid fkey in
    let fnode = FMap.find ciFID !theCG in
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


  (** Mark funID as abandoned (through funCache, or in the summaries) *)
  let abandonFunction funID =
    logStatusF "Abandoning %s\n" (string_of_funID funID);
    try 
      let info = FM.find funID !funCache in
      let info = { info with fDfInfo = makeAbandonedSt info.fDfInfo; } in
      funCache := FM.add funID info !funCache;
    with Not_found ->
      let sumKey = T.funIDToSumKey funID in
      let info = fpinfoSums#find sumKey in
      let info = { info with fDfInfo = makeAbandonedSt info.fDfInfo; } in
      fpinfoSums#addReplace sumKey info


  (** Find other calls to this same function and merge its info to 
      prevent repeatedly starting the function *)
  let mergeWork we oldQ =
    let updateQueue inSt newCS otherCall oldQ =
      let newCall = 
        { otherCall with wKind = WCall (inSt, newCS); } in
      FQueue.add newCall (FQueue.remove otherCall oldQ)
    in
    match we.wKind with
      WCall (i, cs) ->
        (match findCallWork oldQ with
           Some otherCall ->
             (match otherCall.wKind with
                WCall (i2, cs2) ->
                  (* Require them to be the same for now... *)
                  if T.compareDfInfo i i2 == 0 then
                    if CSet.subset cs cs2 then oldQ 
                    else updateQueue i (CSet.union cs cs2) otherCall oldQ
                  else failwith "mergeWork given different inputs on Calls"
             
              | _ -> failwith "findCallWork returned non-call" )
         | None ->
             FQueue.add we oldQ)
    | WIntra _ | WReturnTo _ | WReturnStmt _ -> 
        FQueue.add we oldQ


  let addWork we =
    try 
      let oldQ = InterQ.find we.wFun !worklist in
      let newQ = mergeWork we oldQ in
      updateWorklist we.wFun newQ
    with Not_found -> 
      let newQ = FQueue.singleton we in
      updateWorklist we.wFun newQ

  (** raised when the worklist is completely empty *)
  exception AllDone

  (** Re-populate the funcache w/ functions that are actually in the worklist
      @return true if worklist was non-empty *)
  let rebuildFunCache worklist =
    evictFunctions ();
    let cacheSpacey = cacheHasSpace () in
    assert cacheSpacey;
    let (_:bool) = 
      InterQ.fold 
        (fun funID q hasSpace -> 
           if hasSpace then begin
             addFunctionToCache funID q; 
             cacheHasSpace ()
           end
           else hasSpace ) 
        !worklist cacheSpacey in
    not (noWorkLeft ())


  let reanalyzeCounts = Hashtbl.create 10
  let reanalyzeLimit = 100

  let reanalyzedTooMuch funID =
    let old = try Hashtbl.find reanalyzeCounts funID 
    with Not_found -> 0 in
    let newCount = old + 1 in
    Hashtbl.replace reanalyzeCounts funID newCount;
    if old > reanalyzeLimit then begin
      logErrorF "Reanalyzed: %s --> %d\n" (string_of_funID funID) newCount;
      true
    end 
    else false

  let nontrivialWorkEnt we =
    match we.wKind with WCall _ -> false | _ -> true


  let nonTrivialWork q =
    FQueue.exists nontrivialWorkEnt q

  (** Debug why a particular context is pulled off the global queue > 1 *)
  let debugReanalyze funID q =
    (* Inspect what's on the queue.

       - If it's WIntra for some assignment, then it must have been
         a read of a global.
       - if it's WIntra for calls to F2 then diff cur summary of F2 w/ old.
         This means we need to somehow keep around old summaries for F2.
         It could also have been a read of a global, but that's unlikely? 

         Is there a point to inspecting more than one of the elements of the Q?
    *)
    if (nonTrivialWork q) && reanalyzedTooMuch funID then begin
      logStatus "REQUEUED w/ work:";
      let cfg = getCfg funID in
      (* Also start inspecting that foo or not? *)
(*      Inspect.inspector#addInspect cfg.svar.vname; *)
      logStatusD 
        (seq_to_doc line FQueue.iter
           (fun we ->
              match we.wKind with
                WIntra pp 
              | WReturnStmt pp ->
                  let d = dprintf "Intra: %s\n" (string_of_pp pp) in
                  let instr = Ciltools.getInstrFromPP cfg pp in
                  d ++ (d_instr () instr)
              | WReturnTo (id, pp) ->
                  let d = dprintf "Call retry: %s %s" 
                    (string_of_funID id) (string_of_pp pp) in
                  let instr = Ciltools.getInstrFromPP cfg pp in
                  d ++ (d_instr () instr)
              | WCall _ -> text "Start"
           ) q nil ++ line)
    end
      
  (** Return the next item on the queue, if it is in the funCache *)
  let findCheapWork () =
    try
      let funID, q = InterQ.peek !worklist in
      if FM.mem funID !funCache then begin
        let funID, q = InterQ.pop !worklist in 
(*        printGlobalWorklist "pop"; *)
        debugReanalyze funID q;
        Some q
      end else None
    with Queue.Empty -> raise AllDone

  let takeWork q =
    let we = FQueue.choose q in
    let newQ = FQueue.remove we q in
    (* Manually update here... *)
    prevQueue := (Some we.wFun, newQ);
    InterQ.addOnce !worklist we.wFun newQ;
(*    printGlobalWorklist "add 2"; *)
    we

  let rec getNextWork () =
    if not (FQueue.is_empty (snd !prevQueue))
    then takeWork (snd !prevQueue)
    else 
      match findCheapWork () with
        None -> 
          if rebuildFunCache worklist 
          then getNextWork ()
          else raise AllDone
      | Some q -> 
          setPrevQueue (None, q);
          getNextWork ()

  (* Ugh... need to expose work queue so that we can collapse work 
     when we switch to being flow insensitive? *)
  let dropWork funID = 
    if InterQ.mem !worklist funID then
      let newQ = FQueue.empty in
      updateWorklist funID newQ
    else 
      setIfPrevQueue funID FQueue.empty

end


(** Profile # of iterations and total time spent per function *)
module IterStats (T:RciDfInfoAndKeys) = struct

  let totalIter = ref 0
  let perFuncIter = Distributions.makeDistro ()

  module THisto = Distributions.Make 
    (struct
       type key = Summary_keys.sumKey
       type value = float

       let string_of_key x = Summary_keys.string_of_sumKey x
       let string_of_value x = string_of_float x
       let incr_value x y = x +. y
     end)

  let perFuncTime = THisto.makeDistro ()
    
  let printPerFuncStats () =
    let div = "===================" in
    logStatus div;
    Distributions.printDistroSortFreq 
      perFuncIter Summary_keys.string_of_sumKey "Func ITERS";
    logStatus div;
    THisto.printDistroSortFreq perFuncTime "Func TIME";
    logStatus div


  let recordStep (funID:T.funID) (time:float) =
    incr totalIter;
    let k = T.funIDToSumKey funID in
    Distributions.updateDistro perFuncIter k;
    THisto.updateDistro perFuncTime k time;
    if !totalIter mod 100 == 0 then
      logStatusF "Step #%d\n" !totalIter;
    if !totalIter mod 10000 == 0 then begin
      Stat.print stdout "FPA: ";
      printPerFuncStats ()
    end

end


(** Main Top-down/RCI dataflow engine *)
module RciDataflow  (T: RciTransfer) = struct
  include CachedWorklist (T)

  module IStats = IterStats(T)

  type dfInfo = T.dfInfo
  type funID = T.funID
    
  (****** Misc statistics *******)

  let touchedFuncs = ref FSet.empty

  let inspecting = ref false

  (****** Misc utils / dfInfo management ******)

  let notifyNewDfInfo oldSt st pp =
    if !inspecting then 
      (match oldSt with 
         Some oldSt -> 
           logStatusF "New dfInfo (diff) at %s %s\n" 
             (string_of_loc !currentLoc) (string_of_pp pp);
           T.printDfInfo (T.diffDfInfo oldSt st);
       | None ->
           logStatusF "New dfInfo at %s %s\n" 
             (string_of_loc !currentLoc) (string_of_pp pp);
           T.printDfInfo st )
    else
      if T.isBottom st then
        (logStatusF "New dfInfo at %s %s\n" (string_of_loc !currentLoc) 
           (string_of_pp pp);
         T.printDfInfo st)


  let rec getDfInfo fDfInfo pp =
    match fDfInfo with 
      FlowSens stTables -> PPH.find stTables pp
    | FlowInsens st -> 
        logError "grabbing dfInfo from flowInsens";
        st
    | Abandoned oldSt -> 
        logError "grabbing dfInfo from abandoned func";
        getDfInfo oldSt pp

  let updateDfInfo fInfo pp oldD d =
    notifyNewDfInfo oldD d pp;
    match fInfo.fDfInfo with
      FlowSens stTables ->
        PPH.replace stTables pp d
    | FlowInsens old ->
        fInfo.fDfInfo <- FlowInsens (d)
    | Abandoned _ -> failwith "updating dfInfo for abandoned func"

  let setCurrentLocation cfg funInfo pp =
    let parentStmt = lookupStmtCFG cfg pp in
    match lookupStmtInstr parentStmt pp with
      PPStmt stmt ->
        Cil.setStmtLocation stmt
    | PPInstr instr ->
        Cil.setInstrLocation instr pp.pp_instr

  let getStmtAndPP stmt =
    (stmt, getStmtPP stmt)

  let getNextInstrPPs parentStmt pp =
    (* Check if it's the last instruction in the stmt list...
       if it IS, then propagate to the parent stmts successors *)
    match parentStmt.skind with
      Instr il -> 
        let nextI = pp.pp_instr + 1 in
        if nextI >= List.length il 
        then List.map getStmtAndPP parentStmt.succs
        else [ (parentStmt, { pp with pp_instr = nextI; }) ]
    | _ -> failwith "getNextInstrPPs:  non-instr parent"

  let getFirstPP cfg =
    getStmtPP (List.hd cfg.sallstmts)

  let notifyStep dfInfo pp funID =
    if !inspecting then begin
      logStatusF "step @ %s %s %s\n" (string_of_loc !currentLoc)
        (string_of_pp pp) (string_of_funID funID);
      T.printDfInfo dfInfo
    end

  let getDfInfoFail pp fInfo =
    logStatus "Can't find dfInfo for when finfo is:\n";
    printFpinfoSums fInfo;
    failwith "Death"

  (********** Work constructors ************)

  (* Blindly assume function is in flow-sensitive mode here, but check
     when finally pulling out of the queue *)

  let makeCallWork callerID callerPP (calleeID, calleeInSt) =
    { wFun = calleeID;
      wKind = WCall (calleeInSt, CSet.singleton (callerID, callerPP)); }

  let reachCallees callerID pp callees =
    List.map (makeCallWork callerID pp) callees

  let reachCallers calleeID calleeInfo curWork =
    CSet.fold (fun (callerID, callerPP) cur -> 
                 { wFun = callerID;
                   wKind = WReturnTo (callerID, callerPP); } :: cur ) 
      calleeInfo.fCallers curWork

  let reachGlobalReaders readers curWork =
    if readers <> [] then
      (let work, doc, len = 
         List.fold_left 
           (fun (curWork, curDoc, len) (readerID, readerPP) ->
              ({ wFun = readerID; wKind = WIntra readerPP; } :: curWork,
               curDoc ++ text ", " ++ d_funPP (readerID, readerPP), len + 1
              )) (curWork, nil, 0) readers in
       logStatusF "re-queueing global readers : %s (%d)\n"
         (sprint 80 doc) len;
       work)
    else curWork

  let makeIntraWork funID succPP =
    { wFun = funID;
      wKind = WIntra succPP; }

  let makeReturnStmtWork funID succPP =
    { wFun = funID;
      wKind = WReturnStmt succPP; }
    

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
    match fInfo.fDfInfo with
        FlowInsens oldSt -> 
          (match T.combinePredecessors cfg oldSt newIn with
             None -> []
           | Some x -> 
               notifyFlowInsens "Redoing" funID fInfo;
               (* Just queue it up for now as "intraproc" work *)
               fInfo.fDfInfo <- FlowInsens x;
               [makeIntraWork funID (getFirstPP cfg)]
          )
            
      | FlowSens stTables ->
          notifyFlowInsens "Doing" funID fInfo;
          let combo = PPH.fold 
            (fun pp st cur ->
               (match T.combinePredecessors cfg cur st with
                  None -> cur
                | Some x -> x) ) stTables newIn in
          fInfo.fDfInfo <- FlowInsens combo;
          (* Just queue it up for now as "intraproc" work *)
          [makeIntraWork funID (getFirstPP cfg)]
      | Abandoned _ ->
          failwith "makeFlowInsens on abandoned func"

  let isReturnStmt stmt =
    match stmt.skind with
      Return _ -> true | _ -> false
         
  (** Merge the [newDfInfo] w/ the successor's existing dataflow info.
      If new, add successor to worklist *)
  let reachSuccessor cfg funID funInfo newDfInfo curWork (succ, succPP) =
    let addWork funID succ succPP curWork =
      if isReturnStmt succ 
      then makeReturnStmtWork funID succPP :: curWork
      else makeIntraWork funID succPP :: curWork
    in
    setCurrentLocation cfg funInfo succPP;
    try
      let old = getDfInfo funInfo.fDfInfo succPP in
      (match T.combinePredecessors cfg old newDfInfo with
         None -> curWork
       | Some x ->
           updateDfInfo funInfo succPP (Some old) x;
           addWork funID succ succPP curWork)
    with Not_found ->
      updateDfInfo funInfo succPP None newDfInfo;
      addWork funID succ succPP curWork

  (** Version of reachSuccessor that undoubtedly uses newDfInfo rather 
      than try to combine newDfInfo w/ old. Must be sure that this is okay *)
  let mustReach cfg funID funInfo newDfInfo curWork (succ, succPP) =
    let addWork funID succ succPP curWork =
      if isReturnStmt succ 
      then makeReturnStmtWork funID succPP :: curWork
      else makeIntraWork funID succPP :: curWork
    in
    setCurrentLocation cfg funInfo succPP;
    updateDfInfo funInfo succPP (Some newDfInfo) newDfInfo;
    addWork funID succ succPP curWork

  
  let reachIfSuccs cfg funID fInfo dfInfo stmt ifExp =
    let not_e = UnOp(LNot, ifExp, intType) in
    let thenGuard = T.doGuard ifExp dfInfo in
    let elseGuard = T.doGuard not_e dfInfo in
    if thenGuard = Dataflow.GDefault && elseGuard = Dataflow.GDefault then
      (* this is the common case *)
      List.fold_left (reachSuccessor cfg funID fInfo dfInfo) [] 
        (List.map getStmtAndPP stmt.succs)
    else begin
      let doBranch succ curWork guard =
        let succPP = getStmtPP succ in
        match guard with
          Dataflow.GDefault -> 
            reachSuccessor cfg funID fInfo dfInfo curWork (succ, succPP)
        | Dataflow.GUse d ->  
            reachSuccessor cfg funID fInfo d curWork (succ, succPP)
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
        let startDfInfo = 
          try getDfInfo fInfo.fDfInfo pp 
          with Not_found -> getDfInfoFail pp fInfo
        in
        let startDfInfo = T.doStmt stmt startDfInfo in
        notifyStep startDfInfo pp funID;
        (match stmt.skind with
           Instr il -> 
             let succPP = getNextInstrPPs stmt stmtPP in
             assert (List.length succPP <= 1);
             List.fold_left (reachSuccessor cfg funID fInfo startDfInfo) [] succPP

         | If (e, _, _, _) -> 
             reachIfSuccs cfg funID fInfo startDfInfo stmt e 

         | Goto _ | Break _ | Continue _
         | TryExcept _ | TryFinally _ | Switch _ | Loop _ | Block _ ->
             List.fold_left (reachSuccessor cfg funID fInfo startDfInfo) [] 
               (List.map getStmtAndPP stmt.succs)

         | Return (exp, loc) ->
             let toNotify = T.doReturn exp startDfInfo in
             let work = 
               if toNotify.notifyCaller 
               then reachCallers funID fInfo [] else [] in
             reachGlobalReaders toNotify.notifyPP work

        )

    | PPInstr instr ->
        Cil.setInstrLocation instr pp.pp_instr;
        let startDfInfo = 
          try getDfInfo fInfo.fDfInfo pp 
          with Not_found -> getDfInfoFail pp fInfo
        in
        notifyStep startDfInfo pp funID;
        (match instr with
           (* TODO: make an instruction kind for the Block Assign? *)
           Set (lhs, rhs, loc) -> 
             let outSt, toNotify = T.doAssign lhs rhs loc startDfInfo in
             let work = 
               if toNotify.notifyCaller 
               then reachCallers funID fInfo [] else [] in
             let nextPPs = getNextInstrPPs parentStmt pp in
             let work = 
               List.fold_left (reachSuccessor cfg funID fInfo outSt) 
                 work nextPPs in
             reachGlobalReaders toNotify.notifyPP work
               
         | Asm (a,b,c,d,e,f) -> 
             let outSt, toNotify = T.doASM a b c d e f startDfInfo in
             let nextPPs = getNextInstrPPs parentStmt pp in
             let work = 
               if toNotify.notifyCaller 
               then reachCallers funID fInfo [] else [] in
             List.fold_left (reachSuccessor cfg funID fInfo outSt) 
               work nextPPs

         | Call (lvopt, ce, args, loc) -> 
             let calleeFlows, toNotify, outSt = 
               T.doCall lvopt ce args loc startDfInfo in

             let work = reachCallees funID pp calleeFlows.addCallees in
             List.iter abandonFunction calleeFlows.removeCallees;

             let work = 
               if toNotify.notifyCaller 
               then reachCallers funID fInfo work else work in
             let work = reachGlobalReaders toNotify.notifyPP work in

             let work = match outSt with
                 Some outSt -> 
                   let nextPPs = getNextInstrPPs parentStmt pp in
                   List.fold_left (reachSuccessor cfg funID fInfo outSt) 
                     work nextPPs
               | None -> work
             in
             work
        )


  (** Handle flow-insensitive intra-procedural steps *)
  let handleFlowInsIntra funID fInfo : workEntry list =
    let cfg = getCfg funID in
    inspecting := Inspect.inspector#mem cfg.svar.vname;
    (match fInfo.fInput with 
       Some inSt -> T.setFunc cfg (funID, inSt)
     | None -> failwith "starting a func that had no input?");

    let startDfInfo = match fInfo.fDfInfo with 
        FlowInsens x -> x 
      | FlowSens _ | Abandoned _ -> 
          failwith "handleFlowInsIntra given non flow-insens" in
    
    let comboDfInfo curSt nextSt =
      match T.combinePredecessors cfg curSt nextSt with
        None -> curSt
      | Some x -> x 
    in

    let handleInstrs (curSt, shouldRet, globReads, calls, instrIndex) instr =
      Cil.setInstrLocation instr instrIndex;
      let pp = getCurrentPP () in
      notifyStep curSt pp funID;
      let nextSt, shouldRet, globReads, calls = 
        (match instr with
           Set (lhs, rhs, loc) -> 
             let outSt, toNotify = T.doAssign lhs rhs loc curSt in
             (outSt, shouldRet || toNotify.notifyCaller,
              List.rev_append toNotify.notifyPP globReads, calls)
               
         | Asm (a,b,c,d,e,f) -> 
             let outSt, toNotify = T.doASM a b c d e f curSt in
             (outSt, shouldRet || toNotify.notifyCaller,
              List.rev_append toNotify.notifyPP globReads, calls)
               
         | Call (lvopt, ce, args, loc) -> 
             let calleeFlows, toNotify, outSt = 
               T.doCall lvopt ce args loc curSt in

             let moreCalls = reachCallees funID pp calleeFlows.addCallees in
             List.iter abandonFunction calleeFlows.removeCallees;

             match outSt with
               Some outSt -> 
                 (outSt, shouldRet || toNotify.notifyCaller,
                  List.rev_append toNotify.notifyPP globReads, 
                  List.rev_append moreCalls calls )
             | None ->
                 (curSt, shouldRet || toNotify.notifyCaller,
                  List.rev_append toNotify.notifyPP globReads, 
                  List.rev_append moreCalls calls)) in
      (comboDfInfo curSt nextSt, shouldRet, globReads, calls, instrIndex + 1)
    in
    
    let finalSt, shouldReturn, globReads, calls =
      List.fold_left
        (fun (curSt, shouldReturn, globReads, calls) stmt ->
           Cil.setStmtLocation stmt; 
           (match stmt.skind with
              Instr il ->
                let nextSt, shouldRet, globReads, calls, _ = 
                  List.fold_left handleInstrs 
                    (curSt, shouldReturn, globReads, calls, 0) il in
                (nextSt, shouldRet, globReads, calls)
                  
            | Return (exp, loc) ->
                let pp = getCurrentPP () in
                notifyStep curSt pp funID;
                let toNotify = T.doReturn exp curSt in
                (curSt, shouldReturn || toNotify.notifyCaller, 
                 List.rev_append toNotify.notifyPP globReads, calls)
                  
            | If _ | Goto _ | Break _ | Continue _ | Block _ 
            | TryExcept _ | TryFinally _ | Switch _ | Loop _ ->
                (curSt, shouldReturn, globReads, calls)
           )
        ) (startDfInfo, false, [], []) cfg.sallstmts in
    
    let work = reachCallers funID fInfo [] in
    let work = List.rev_append calls work in
    let maybeOtherFuncWork = reachGlobalReaders globReads work in
    match T.combinePredecessors cfg startDfInfo finalSt with
      None -> 
        (* Just the inter-proc flows *)
        maybeOtherFuncWork 
    | Some newSt -> 
        (* Inter-proc flows plus one more round on self *)
        logStatusF "Iter flowInsensitive again: %s\n" (string_of_funID funID);
        fInfo.fDfInfo <- FlowInsens newSt;
        makeIntraWork funID (getFirstPP cfg) :: maybeOtherFuncWork

 
  let printCallers descr calleeID calleeCfg inDfInfo calleeInfo =
    logStatusF "=============\n%s call for %s (%s)\n" 
      descr calleeCfg.svar.vname (string_of_funID calleeID);
    let doc = d_callers calleeInfo in
    logStatusD doc;
    logStatus "Input dfInfo:";
    T.printDfInfo inDfInfo


  (* Would this bookkeeping take too much space? 
     O(callsites x contexts)... *)
  let reverseCallMapping = ref CMap.empty

  (** If a context has no callers, then pause it *)
  let checkToggleStopFunction funID fInfo =
    if CSet.is_empty fInfo.fCallers then begin
      logStatusF "Pausing context (no callers): %s\n" 
        (string_of_funID funID);
      { fInfo with fDfInfo = makeAbandonedSt fInfo.fDfInfo; }
    end else fInfo

  (** If a context has more callers, and was paused, unpause it *)
  let checkToggleStartFunction funID fInfo =
    if not (CSet.is_empty fInfo.fCallers) then
      (match fInfo.fDfInfo with
         Abandoned oldSt -> 
           logStatusF "Unpausing context (more callers): %s\n" 
             (string_of_funID funID);
           { fInfo with fDfInfo = oldSt; }
       | _ -> fInfo)
    else fInfo

  let removeACaller caller oldID =
    logStatusF "Removing obsolete caller: %s %s\n" 
      (string_of_funID oldID) (sprint 80 (d_funPP caller));
    let doRemove caller oldID fInfo =
      let fInfo = 
        { fInfo with fCallers = CSet.remove caller fInfo.fCallers; } in
      checkToggleStopFunction oldID fInfo
    in
    try
      let fInfo = FM.find oldID !funCache in
      let fInfo = doRemove caller oldID fInfo in
      funCache := FM.add oldID fInfo !funCache
    with Not_found ->
      (* Ugh... need to update from disk *)
      let sumKey = T.funIDToSumKey oldID in
      let fInfo = fpinfoSums#find sumKey in
      let fInfo = doRemove caller oldID fInfo in
      fpinfoSums#addReplace sumKey fInfo


  let updateACaller newCalleeID caller =
    let oldCallees = 
      try CMap.find caller !reverseCallMapping 
      with Not_found -> FS.empty
    in
    let calleeKey = T.fkeyOfID newCalleeID in
    (* Remove obsolete targets -- this callsite can only call one
       of the "foo" contexts for each "foo" *)
    let newCallees =
      FS.fold (fun oldID callees ->
                 if (T.compareFunID newCalleeID oldID) <> 0 then
                   let oldKey = T.fkeyOfID oldID in
                   if oldKey = calleeKey then begin
                     removeACaller caller oldID;
                     FS.remove oldID callees
                   end else callees
                 else callees
              ) oldCallees oldCallees in
    let newCallees = FS.add newCalleeID newCallees in
    reverseCallMapping := CMap.add caller newCallees !reverseCallMapping


  let updateCallers funID fInfo newCallers =
    let fInfo = { fInfo with 
                    fCallers = CSet.union fInfo.fCallers newCallers; } in
    let fInfo = checkToggleStartFunction funID fInfo in
    funCache := FM.add funID fInfo !funCache;
    (* For each newCaller, see if it makes old function call lists obsolete *)
    CSet.iter (updateACaller funID) newCallers;
    fInfo



  let queueCall descr funID newCallers fInfo input cfg =
    (* More like start call... since we do handle intra right away! *)
    let fInfo = { fInfo with fInput = Some input; } in
    let fInfo = updateCallers funID fInfo newCallers in
    printCallers descr funID cfg input fInfo;
    match fInfo.fDfInfo with
      FlowSens stTables ->
        let firstPP = getFirstPP cfg in
        PPH.add stTables firstPP input;
        handleIntra funID fInfo firstPP
    | FlowInsens _ -> 
        makeFlowInsens fInfo funID input cfg
    | Abandoned _ ->
        logErrorF "Not queuing call to abandoned: %s\n" (string_of_funID funID);
        []
          

  let queueReturns funID fInfo cfg =
    List.iter 
      (fun stmt ->
         if isReturnStmt stmt then
           let pp = getStmtPP stmt in
           (try (* Only queue if we've reached it before *)
              let _ = getDfInfo fInfo.fDfInfo pp in               
              addWork { wFun = funID; wKind = WReturnStmt pp; }
            with Not_found -> ())
         else ()
      ) cfg.sallstmts
      
      
  let startCall funID fInfo inDfInfo newCallers =
    let cfg = getCfg funID in
    match fInfo.fInput with
      None -> (* first call *)        
        queueCall "Starting" funID newCallers fInfo inDfInfo cfg
    | Some oldIn ->
        (match T.combinePredecessors cfg oldIn inDfInfo with
           None -> 
             if CSet.subset newCallers fInfo.fCallers then []
             else begin
               let fInfo = updateCallers funID fInfo newCallers in
               printGotMoreCallers funID fInfo;
               []
             end
         | Some newIn ->
             (* For now... keep the old guy, don't allow combining input
                dfInfos, otherwise we can't detect duplicate contexts *)
(*
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
*)
             logStatusF "Didn't restart: %s %s\n" cfg.svar.vname 
               (string_of_funID funID);
             logStatus "Diff in input dfInfo:";
             T.printDfInfo (T.diffDfInfo newIn oldIn);
             if CSet.subset newCallers fInfo.fCallers then []
             else begin
               let fInfo = updateCallers funID fInfo newCallers in
               printGotMoreCallers funID fInfo;
               []
             end
        )


  let stepFlowSensitive fInfo wEntry =
    match wEntry.wKind with
      WCall (inDfInfo, newCallers) ->
        startCall wEntry.wFun fInfo inDfInfo newCallers
    | WIntra pp
    | WReturnTo (_, pp) (* it's returning to the call instruction... *)
    | WReturnStmt pp ->
        handleIntra wEntry.wFun fInfo pp


  let stepFlowInsens fInfo wEntry =
    match wEntry.wKind with
      WCall (inDfInfo, newCallers) ->
        startCall wEntry.wFun fInfo inDfInfo newCallers
    | WIntra _
    | WReturnTo (_, _) 
    | WReturnStmt _ ->
        (* Step through the whole damn thing *)
        handleFlowInsIntra wEntry.wFun fInfo

        
  (************* Queue management / Misc ***************)


  let updateSteps funID time =
    IStats.recordStep funID time

  let addTouchedFunc wEntry =
    let ciFID = fkey_to_fid (T.fkeyOfID wEntry.wFun) in
    touchedFuncs := FSet.add ciFID !touchedFuncs

  let printCompletedFuncs () =
    let doneFuncs = d_funcset !theCG !touchedFuncs in
    logStatus "====================================";
    logStatus "Completed functions:";
    logStatusD doneFuncs;
    logStatusF "Total: %d\n" (FSet.cardinal !touchedFuncs);
    logStatus "===================================="

  let enqueueInitial dfInfoKeys =
    List.iter 
      (fun (funID, st) -> 
         let work = { wFun = funID;
                      wKind = WCall (st, CSet.empty); } in
         addWork work) dfInfoKeys

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
      try
        let wEntry = getNextWork () in
        addTouchedFunc wEntry;
        let fInfo = FM.find wEntry.wFun !funCache in

        let rec doWEntry (wEntry, fInfo, fDfInfo) =
          match fDfInfo with
            FlowSens _ -> stepFlowSensitive fInfo wEntry 
          | FlowInsens _ -> 
              mergeFlowInsWork fInfo wEntry;
              stepFlowInsens fInfo wEntry
          | Abandoned oldSt -> 
              (* Skip this work unless this is a WCall (which may unpause)!!! *)
              (match wEntry.wKind with
                 WCall _ -> doWEntry (wEntry, fInfo, oldSt)
               | _ -> 
                   (* TODO: unsound if this function is later unabandoned,
                      because this drops work on the floor.
                      Should actually keep it queued up until
                      the next unpause... *)
                   [])
        in

        let toEnqueue, time = 
          Stat.timethis doWEntry (wEntry, fInfo, fInfo.fDfInfo) in
        updateSteps wEntry.wFun time;
        List.iter addWork toEnqueue
      with AllDone -> ()
    done;
    printCompletedFuncs ()

  (************************************************************)

  (** Iterate to a fixed-point on all the functions/asts stored in the 
      given root directory. *)
  let compute rootDir addrTk cg = begin
    theCG := cg;
    funRanks := prePostNums cg;

    logStatus "Pre/Post nums";
    logStatusD (map_to_doc line FMap.iter
                  (fun fk (pre, post) ->
                     let fnode = FMap.find fk cg in
                     dprintf "%s (%s) -> (%d, %d)" 
                       fnode.name (fid_to_string fk) pre post) 
                  !funRanks nil);
    logStatus "\n";

    let initialWork = Stat.time "initWork" (T.initWork rootDir addrTk) cg in
    enqueueInitial initialWork;
    logStatusF "InterDF initial worklist: %d\n" (workLeft ());
    fixedPoint ();
    logStatusF "InterDF DONE: %d iters\n" !IStats.totalIter;
(*    printGlobalWorklist "final" *)
  end

  (**** Operations to query dfInfo after computation *)

  let getInfoFunID funID =
    let sumKey = T.funIDToSumKey funID in
    let fpInfo = fpinfoSums#find sumKey in
    fpInfo.fDfInfo

  let foldOnFKey foo fKey acc =
    fpinfoSums#foldOnKey 
      (fun sumKey info acc -> foo sumKey info.fDfInfo acc) fKey acc


end
  
