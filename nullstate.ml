(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Ravi Chugh
  
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



(* TEMPORARY CLONE OF RACESTATE for null pointer df analysis 
   Sorry about the mess

*)

open Callg
open Cil
open Pretty
open Fstructs
open Scc_cg
open Messages
open Cilinfos
open Manage_sums
open Lockset
open Cildump
open Logging

module RS = Racestate.RS
module SPTA = Racestate.SPTA
module BS = Backed_summary
module Stat = Mystats

module LP = Lockset_partitioner
module Par = Pseudo_access
module AU = All_unlocks
module NW = Null_warnings
module PPHash = Ciltools.PPHash
module LSHash = Par.LSHash
module LvalHash = Par.LvalHash

(***************************************************)
(* Intra-proc Analysis                             *)

let debug = false
let inspect = ref false

let setInspect yesno =
  inspect := yesno

let stLat = new Rns.NullState.rc

(** Customization module for lockset intra-proc dataflow *)
module NullDF = struct
  let name = "detect_null_pointers"

  (* Current function *)
  let curFunc = ref dummyFunDec
  let curFID = ref dummyFID

  let debug = ref debug 

  (** per program point state -- only need locks to be flow-sensitive *)
  type t = Rns.RS.st 

  let transferFunc = Rns.NullTransfer.sequentialTransF

  (* state before a program statement *)
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
                     Inthash.add stmtStartData stmt.sid stLat#bottom
                  ) tl
    | _ ->
        ()
      
  (** Initialize the symbolic state before analzying the given func
      and initialize the dataflow facts *)
  let initState fid (func: Cil.fundec) (input: t) : unit = begin
    curFID := fid;
    curFunc := func;
    initStmtStartData input;
    (* Ughh... call handleFunc on each of the damn transfer funcs *)
    transferFunc#handleFunc fid func;
  end

  let copy (d: t) = d 

  let pretty () (d: t) =
    (* TODO, use this instead of own print functions? *)
    Pretty.nil

  let computeFirstPredecessor (s: stmt) (newD: t) : t = 
    newD

  let combinePredecessors (s: stmt) ~(old: t) (newD: t) : t option =
    if (Stat.time "RNS subset test" (stLat#stateSubset newD) old) then
      None
    else begin
      let comboState = Stat.time "RNS combineStates" 
        (stLat#combineStates old) newD in
      Some (comboState)
    end


  (* Handle flow sensitive facts for the instruction *)
  let doInstr (i: instr) (inSt: t) =

    if (stLat#isBottom inSt) then
      Dataflow.Default
    else
      (if !inspect then begin
         logStatus "Inspecting RNS: state before instr";
         logStatus ((string_of_instr i));
         stLat#printState inSt;
         flushStatus ();
       end;
       let result = transferFunc#handleInstr i inSt in
       if !inspect then begin
         logStatus "Inspecting RNS: state after instr";
         (match result with
            Dataflow.Default -> logStatus "No change"
          | Dataflow.Done outSt ->
              stLat#printState outSt
          | Dataflow.Post _ ->
              logStatus "Inspect instr: No idea");
         flushStatus ();
       end;
       result)

  (** Update flow sensitive information for the stmt *)
  let doStmt (s: stmt) (d: t) = 
    transferFunc#handleStmt s d


  (** Find from flow-sensitive state preceding statement S, given the table.
      Returns BOTTOM if not found *)
  let getStmtData (data: t Inthash.t) (s: stmt) : t = 
    try Inthash.find data s.sid
    with Not_found -> 
      stLat#bottom 


  (* TODO use value of the guard *)  
  let doGuard (gexp: Cil.exp) (d: t) =
    transferFunc#handleGuard gexp d

  let filterStmt _ = true

        
  (***************************************************)
  (* Debugging Stuff                                 *)
      
  (* Print DF facts of given STMT *)
  let printData allData stmt =
    let data = getStmtData allData stmt in
    begin
      Printf.printf "\n********** %s\n********** "
        (sprint 80 (d_stmt () stmt));
      stLat#printState data;
    end
      
 
  (* Print out per-statement state for current function *)
  let printCurFuncState () =
    List.iter (printData stmtStartData) !curFunc.sallstmts

end
(* TODO: use IntraDataflow.FlowSensitive functor instead *)


module NullForwardDF = Dataflow.ForwardsDataFlow(NullDF)
let getRns = NullForwardDF.getDataBefore


class nullSummarizer stLat sums = object(self)
  inherit [Rns.RS.st, Rns.RS.st] IntraDataflow.summaryIsOutput stLat sums

  method scopeIt fdec rns =
    match rns with 
      Some s -> Some (Rns.scopeRns2 fdec s)
    | None -> None

end


(** Package up the Null analysis *)
class nullAnalysis = object (self) 

  val summarizer = new nullSummarizer 
    (stLat :> (Rns.state, Rns.sumval) IntraDataflow.stateLattice) Rns.sums  
  
  method setInspect yesno =
    inspect := yesno

  method isFinal key = (* don't need to skip *)
    false
      
  method compute (funID:funID) cfg =
    logStatus "doing null analysis";
    flushStatus ();
    (* TODO: get rid of input *)
    let input = stLat#initialState in
    NullDF.initState funID cfg input;
    NullForwardDF.clearPPData ();
    Stat.time "Null DF: " 
      (fun () ->
         NullForwardDF.compute cfg.sallstmts;         
      ) ()

  method summarize key cfg : bool =
    if self#isFinal key then false
    else (* Get the possibly-new output state for this function *)
      summarizer#summarize key cfg NullForwardDF.getDataBefore

  method flushSummaries () =
    summarizer#flushSummaries ()

end

(********* Pseudo access stuff ****************)

let pseudoAccessCount = ref 0
  
class pseudoAccessVisitor = object(self)
  inherit Pp_visitor.ppVisitor
    
  val mutable fid = dummyFID
  val mutable curCG = emptyCG
  method setCG cg = curCG <- cg

  method setKey key =
    fid <- key
    
  val mutable paCount = -1
    
  val mutable pp2int : LP.pp2intTable =
    PPHash.create 0
      
  val mutable int2ls : LP.int2lsTable =
    Inthash.create 0
      
  val mutable par : Par.paRegions =
    LSHash.create 0
      
  method getPar =
    par
      
  method initState fk =
    self#setKey fk;
    (* clear state from last run *)
    par <- LSHash.create 0;
    paCount <- -1;
    (* load LSP tables *)
    let p2i, i2l = LP.sums#find fid in
    pp2int <- PPHash.copy p2i;
    int2ls <- Inthash.copy i2l;
    LP.sums#evictOne fid
      
  method vinst i = 
    self#setInstrPP i;
    let pp = getCurrentPP () in
    let ls = 
      try LP.getLocksetAtPP pp (pp2int, int2ls)
      with LP.LsAtPP ->
        logError ~prior:1 ("paVisitor#vinst: LsAtPP " ^ string_of_pp pp);
        RS.emptyLS in
    let rns = getRns pp in
    (match i with
       Set (_, _, _)
     | Asm (_, _, _, _, _, _) ->
         self#processRns rns ls

     | Call (_,  _, _, _) ->
         let weakerls = AU.getWeakerLS curCG fid ls i in
         self#processRns rns weakerls
    );
    self#bumpInstr 1;
    DoChildren

  method vstmt s = 
    self#setStmtPP s;
    let pp = getCurrentPP () in
    let ls = 
      try RS.hcLockstate (LP.getLocksetAtPP pp (pp2int, int2ls))
      with LP.LsAtPP ->
        logError ~prior:1 ("paVisitor#vstmt: LsAtPP " ^ string_of_pp pp);
        RS.emptyLS in
    let rns = getRns pp in
    self#processRns rns ls;
    DoChildren

  method processRns rnsState ls =
    match rnsState with
      None -> ()
    | Some rns ->
        let plus = Rns.RNS.getPlus rns in
        Rns.RNS.S.iter (fun lv _ -> self#processLval lv ls) plus

  method processLval lv ls = 
    (*logStatus ("processLval " ^ Lvals.string_of_lval lv);*)
    let regionLoc, paBindings =
      if LSHash.mem par ls then
        LSHash.find par ls
      else
        (* this is the first location accessed in this lockset region,
           so we'll arbitrarily use it for the entire region *)
        let loc = !Cil.currentLoc in
        let emptyPaBindings = LvalHash.create 0 in
        LSHash.add par ls (loc, emptyPaBindings);
        LSHash.find par ls in

    let pakeyhead, targetStatus =
      if LvalHash.mem paBindings lv then
        LvalHash.find paBindings lv
      else
        let newpakeyhead = self#generatePaKeyHead in
        let table = LvalHash.create 0 in
        newpakeyhead, table in
    
    let targetLvs =
      match lv with
      | (Lvals.CVar vi), _ ->
          [lv]
      | (Lvals.CMem ptrExp), _ ->
          let pp = getCurrentPP () in
          let _, mayPt = SPTA.derefALvalAt pp lv in
          mayPt
      | _ ->
          logError ~prior:1 "processLval: lv has unknown structure";
          [] in
    let targetLvs = List.map Lvals.mergeLv targetLvs in

    let pakeytail = ref 0 in
    List.iter
      (fun t ->
         (* if a target was already listed for this lv, replacing it
            with the None doesn't have any effect.
            if this target is new for this lv, we add an entry for
            it initializing its status to None. *)
         LvalHash.replace targetStatus t (!pakeytail, None);
         pakeytail := !pakeytail + 1
      ) targetLvs;

    LvalHash.replace paBindings lv (pakeyhead, targetStatus);
    LSHash.replace par ls (regionLoc, paBindings)


  method generatePaKeyHead =
    paCount <- paCount + 1;
    fid, paCount

  method printPaRegions =
    Par.printPaRegions par fid

  method createAllGAs fdec =
    (* save PAR summary *)
    Par.sums#addReplace fid (Par.wrapSummary par);

    (* create guarded accesses for everything *)
    let sum = ref (RS.sum#find fid) in
    let corrs = ref (!sum.RS.sum_out.RS.cState) in
    LSHash.iter 
      (fun ls (loc, paBindings) -> LvalHash.iter 
         (fun origLv (pakeyhead, targetStatus) -> LvalHash.iter 
            (fun target (pakeytail, _) ->
               
               match Shared.isShareableAbs fdec target with
                 None -> ()
               | Some _ ->
                   pseudoAccessCount := !pseudoAccessCount + 1;
                   let pakey = Par.smashPaKey pakeyhead pakeytail in
                   corrs :=
                     RS.addPseudoCorr ls !corrs target loc fid pakey;
                   sum := 
                     { !sum with 
                         RS.sum_out = {!sum.RS.sum_out with
                                          RS.cState = !corrs }};
            ) targetStatus         
         ) paBindings
      ) par;
    sum := RS.scopeSummary fdec !sum;
    sum := RS.hcSummary !sum;
    RS.sum#addReplace fid !sum;
    RS.sum#flushOne fid;
    AU.sums#evictSummaries


end 


(** Package up the RNS-based pseudo access analysis *)
class parAnalysis (rnsSkipped : Summary_keys.sumKey -> bool) = object (self)

  val paVisitor = new pseudoAccessVisitor

  (* UGH... ugly and error-prone! *)
  method setCG cg =
    paVisitor#setCG cg

  method setInspect yesno =
    inspect := yesno

  method isFinal fkey =
    rnsSkipped fkey

  method compute fid cfg =
    paVisitor#initState fid;
    ignore (visitCilFunction (paVisitor :> cilVisitor) cfg)

  method summarize key cfg =
    (* Create guarded accesses for pseudo accesses *)
    if self#isFinal key then
      (* ? *)
      logStatusF "parAnalysis: summary for %s is finalized" 
        (Summary_keys.string_of_sumKey key)
    else paVisitor#createAllGAs cfg;
    false

  method flushSummaries () =
    Par.sums#serializeAndFlush

end


(***************************************************)
(* Inter-proc Analysis                             *)
  
  
(** Info needed by the inter-proc driver *)
module NullBUTransfer = 
struct
  

  (**** Statistics kept ****)
  
  let sccsDone = ref 0

  let sccsTotal = ref 0

  let curCG = ref emptyCG

  let curSCCCG = ref emptySCCCG

  let numDerefsSafe = ref 0

  let numDerefsWarn = ref 0
      
  let updateStats lastSCC = 
    (* Progress *)
    incr sccsDone;
    logStatus (">>> PROGRESS " ^ (string_of_int !sccsDone) ^ "/" ^
                    (string_of_int !sccsTotal) ^ " SCCs DONE!\n")


  (**** State management / calculation ****)

  type state = Rns.RS.st

  (* RAVI: adding empty summaries for all functions without a body 
     JAN: this should be handle by the summary module now... *)
  let initializeFuncsWithoutBody cg =
(*
    FMap.iter
      (fun k n ->
         if not n.hasBody then begin
           logStatus ("Setting RNS and PAR summaries of "
                        ^ n.name ^ "(" ^ fid_to_string k
                        ^ ") to empty since no body");
           (* empty .rns *)
           stLat#sums#addReplace k (Some Rns.RNS.empty);
           (* empty .par *)
           Par.sums#addReplace k (Par.emptySumm());
           (* empty .nwarn *)
           !NW.sums#addReplace k (NW.emptySumm())
         end
      ) cg
*)
    ()

  (**** List of analyses that should be run (listed in order) *****)

  class symexAnalysis = object 
    inherit SPTA.symexAnalysis
    (** Override to not write out summaries (already done!) *)
    method summarize key (cfg:fundec) =
      false
  end

  (* TODO: may need to set the callgraph for the parAnalysis later *)

  let ssAna = new symexAnalysis
  let rnsAna = new nullAnalysis
  let parAna = new parAnalysis rnsAna#isFinal
  let nwAna = new NW.nwAnalysis rnsAna#isFinal stLat getRns
                 numDerefsSafe numDerefsWarn
  let kVis = (new Rns.rnsKnowledgeVisitor "sequential" getRns stLat) 
  let knowledgeAna = new Knowledge_pass.knowledgeAnalysis rnsAna#isFinal kVis
                     

  let (needsFixpoint : IntraDataflow.analysis list) = [ ssAna; rnsAna ]
  let (nonFixpoint : IntraDataflow.analysis list) = 
    [ (nwAna :> IntraDataflow.analysis); 
      (parAna :> IntraDataflow.analysis); 
      knowledgeAna ]


  let initStats cg sccCG : unit = 
    (curCG := cg;
     curSCCCG := sccCG;
     (* TODO: clean up *)
     IntraDataflow.curCG := cg;
     IntraDataflow.curSCCCG := sccCG;

     (* Ugh... there must be a cleaner way to initialize these *)
     NullDF.transferFunc#setCG cg;
     parAna#setCG cg;
     nwAna#setCG cg;

     (* Progress *)
     sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
     sccsDone := 0;
    )

  let flushSummaries () =
    RS.sum#serializeAndFlush;
    SPTA.SS.sum#evictSummaries;
    AU.sums#evictSummaries;
    LP.sums#evictSummaries;
    stLat#sums#serializeAndFlush;
    Par.sums#serializeAndFlush;
    !NW.sums#serializeAndFlush


  let doFunc ?(input:state = stLat#initialState) fid fnode 
      : state InterDataflow.interResult =
    let fn, defFile = fnode.name, fnode.defFile in
    logStatusF "Summarizing function: %s(%s):%s\n" 
      fn (fid_to_string fid) defFile;
    logStatus "-----";
    flushStatus ();
    match Cilinfos.getFunc (fid_to_fkey fid) defFile with
      Some cfg ->
        if IntraDataflow.runFixpoint needsFixpoint fid cfg then
          InterDataflow.NewOutput (input, input) (* TODO: change return type *)
        else
          InterDataflow.NoChange
    | None ->
        (* Don't have function definition *)
        logErrorF "doFunc can't get CFG for: %s:%s\n" fn defFile;
        InterDataflow.NoChange 
          

  (** TRUE if the function should be put on the worklist *)
  let filterFunc f : bool =
    true

  let hardCodedSumTypes () =
    BS.getDescriptors [RS.sum#sumTyp;
                       SPTA.SS.sum#sumTyp;
                       AU.sums#sumTyp;
                       LP.sums#sumTyp;
                      ]
	(* RNS summary? *)

  (** Prepare to start an scc *)
  let sccStart scc = begin
    (* Get all summaries for all callees *)
    logStatus "Acquiring callee summaries";
    flushStatus ();
    prepareSCCCalleeSums !curSCCCG scc (hardCodedSumTypes ());

    (* Get the RS summaries for functions in SCC 
       (as we are not recomputing them) *)
    logStatus "Acquiring SS/RS summaries for current SCC";
    prepareSCCSums scc (BS.getDescriptors [SPTA.SS.sum#sumTyp;
                                           RS.sum#sumTyp;
                                           AU.sums#sumTyp;
                                           LP.sums#sumTyp;
                                          ]);
  end

  (** Scc is summarized. Do the bookkeeping / cleanup *) 
  let sccDone scc (byThisGuy:bool) =
    let summPaths = if (byThisGuy) then
      let sccKeys = sumKeysOfScc scc [] in
      (* Now that all null info is computed do remaining passes *)
      let prevFKey = !NullDF.curFID in
      IntraDataflow.runNonFixpoint nonFixpoint needsFixpoint prevFKey scc;
      
      (* Debugging *)
(*
         FSet.iter 
           (fun fkey ->
              logStatus ("Summary for function: " ^ 
                              (string_of_fkey fkey));
              logStatus "=======\n";
              stLat#printSummary fkey;
           ) scc.scc_nodes;
*)
         
      (* Serialize and record where each fun was placed *)
      flushSummaries ();

      (* Find out where the summaries were stored *)
      (* TODO: force them to pick the same directory (which they do unless 
         the chosen disk partition runs out of space) *)
      let tokenMap = Rns.sums#locate sccKeys in
      
      (* Notify others that the functions are now summarized *)
      List.fold_left
        (fun paths (key, tok) ->
           (* Err... why wouldn't it be? *)
           if (List.mem key sccKeys) then
             let path = BS.pathFromToken tok in
             (key, path) :: paths
           else paths
        ) [] tokenMap
    else [] in
    updateStats scc; (* and possibly delete obsolete summaries *)
    summPaths

end

module BUDataflow = InterDataflow.BottomUpDataflow (NullBUTransfer)


