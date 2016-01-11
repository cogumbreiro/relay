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
open Messages
open Cilinfos
open Manage_sums
open Lockset

module DF = Dataflow
module IDF = InterDataflow
module Intra = IntraDataflow
module A = Alias
module RS = Racesummary
module SPTA = Symstate2
module Du = Cildump
module BS = Backed_summary
module Stat = Mystats
module L = Logging

module CLv = Cil_lvals
module Lv = Lvals

module I = Inspect
module LP = Lockset_partitioner
module Par = Pseudo_access
module AU = All_unlocks
module NW = Null_warnings

(***************************************************)
(* State / Utilities :                             *)

(* Current function *)
let curFunc = ref dummyFunDec


(***************************************************)
(* Intra-proc Analysis                             *)

let debug = false
let inspect = ref false

let setInspect yesno =
  inspect := yesno

let stMan = new Rns.NullState.rc

(* trying to test with dummy modules from IntraDataflow... *)
(*let stMan = new IntraDataflow.RaceState.rc*)



(** Customization module for lockset intra-proc dataflow *)
module NullDF = struct
  let name = "detect_null_pointers"

  let debug = ref debug 

  (** per program point state -- only need locks to be flow-sensitive *)
  type t = Rns.RS.st 

  (* making this mutable, so we can run the sequential first (to reach
     a fixed-point), and then run the adjusted version. *)
  let transferFunc = ref (Rns.NullTransfer.transferWithNoAdjust)

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
                     Inthash.add stmtStartData stmt.sid stMan#bottom
                  ) tl
    | _ ->
        ()
      
  (** Initialize the symbolic state before analzying the given func
      and initialize the dataflow facts *)
  let initState (func: Cil.fundec) (input: t) (firstPhase: bool) : unit =
    (curFunc := func;
     initStmtStartData input;
     (* set transfer function to sequential version *)
     transferFunc := (match firstPhase with
         true ->  Rns.NullTransfer.transferWithNoAdjust
       | false -> Rns.NullTransfer.pessimisticWithNoAdjust)
    )

  let initStateForSecondPass (func: Cil.fundec) (firstPhase: bool) =
    (* set transfer function to composed/adjusted version *)
    transferFunc := (match firstPhase with
        true ->  Rns.NullTransfer.transferWithAdjust
      | false -> Rns.NullTransfer.pessimisticWithAdjust);
    !transferFunc#loadPseudoAccessInfo (funToKey !curFunc)

  let copy (d: t) = d 

  let pretty () (d: t) =
    (* TODO, use this instead of own print functions? *)
    Pretty.nil

  let computeFirstPredecessor (s: stmt) (newD: t) : t = 
    newD

  let combinePredecessors (s: stmt) ~(old: t) (newD: t) : t option =
    if (Stat.time "RNS subset test" (stMan#stateSubset newD) old) then
      None
    else begin
      let comboState = Stat.time "RNS combineStates" 
        (stMan#combineStates old) newD in
      Some (comboState)
    end


  (* Handle flow sensitive facts for the instruction *)
  let doInstr (i: instr) (inSt: t) =

    if (stMan#isBottom inSt) then
      DF.Default
    else
      (if !inspect then begin
         L.logStatus "Inspecting RNS: state before instr";
         L.logStatus ((Du.string_of_instr i));
         stMan#printState inSt;
         L.flushStatus ();
       end;
       let result = !transferFunc#handleInstr i inSt in
       if !inspect then begin
         L.logStatus "Inspecting RNS: state after instr";
         (match result with
            DF.Default -> L.logStatus "No change"
          | DF.Done outSt ->
              stMan#printState outSt
          | DF.Post _ ->
              L.logStatus "Inspect instr: No idea"
         );
         L.flushStatus ();
       end;
       result)

  (** Update flow sensitive information for the stmt *)
  let doStmt (s: stmt) (d: t) = 
    !transferFunc#handleStmt s d


  (** Find from flow-sensitive state preceding statement S, given the table.
      Returns BOTTOM if not found *)
  let getStmtData (data: t Inthash.t) (s: stmt) : t = 
    try Inthash.find data s.sid
    with Not_found -> 
      stMan#bottom 


  (* TODO use value of the guard *)  
  let doGuard (gexp: Cil.exp) (d: t) =
    !transferFunc#handleGuard gexp d

  let filterStmt _ = true

        
  (***************************************************)
  (* Debugging Stuff                                 *)
      
  (* Print DF facts of given STMT *)
  let printData allData stmt =
    let data = getStmtData allData stmt in
    begin
      Printf.printf "\n********** %s\n\
                       ********** "
        (sprint 80 (d_stmt () stmt));
      stMan#printState data;
    end
      
 
  (* Print out per-statement state for current function *)
  let printCurFuncState () =
    List.iter (printData stmtStartData) !curFunc.sallstmts

end
(* TODO: use Intra.FlowSensitive functor instead *)


module NullForwardDF = DF.ForwardsDataFlow(NullDF)
let getRns = NullForwardDF.getDataBefore

class nullSummarizer stMan sums = object(self)
  inherit [Rns.RS.st, Rns.RS.st] Intra.summaryIsOutput stMan sums

  method scopeIt fdec rns =
    match rns with 
      Some s -> Some (Rns.scopeRns2 fdec s)
    | None -> None

end

(** Package up the Null analysis *)
(* firstPhase should be true for race-sensitive adjust,
   false for pessimistic adjust *)
class nullAnalysis firstPhase = object (self) 

  val summarizer = new nullSummarizer 
    (stMan :> (Rns.state, Rns.sumval) Intra.stateLattice) Rns.sums
  
  method setInspect yesno =
    inspect := yesno

  method isFinal fk = (* don't need to skip *)
    false

  method compute cfg =
    L.logStatus "doing null analysis";
    L.flushStatus ();
    (* TODO: get rid of input *)
    let input = stMan#initialState in
    NullDF.initState cfg input firstPhase;
    NullForwardDF.clearPPData ();
    L.logStatus "Pass 4 Phase 1";
    Stat.time "Null DF: " 
      (fun () ->
         NullForwardDF.compute cfg.sallstmts;         
      ) ();

    NullDF.initStateForSecondPass cfg firstPhase;
    (!NullDF.transferFunc)#setSetRnsAtPP (NullForwardDF.setDataBefore);
    L.logStatus "Pass 4 Phase 2";
    Stat.time "Null DF part 2: "
      (fun () ->
         NullForwardDF.compute cfg.sallstmts;
      ) ()

      (* Only difference from nullstate.ml is call to different compute *)

  method summarize fkey cfg : bool =
    if self#isFinal fkey then false
    else begin
      (* Get the possibly-new output state for this function *)
      summarizer#summarize fkey cfg NullForwardDF.getDataBefore
      (*
      let newSummary = summarizer#makeSummary cfg NullForwardDF.getDataBefore in
      Intra.checkupSummary fkey cfg newSummary 
        Rns.sums#find
        summarizer#isBottomSummary
        summarizer#sumSubset
        summarizer#combineSummaries
        (fun fdec rns -> match rns with 
           Some s -> Some (Rns.scopeRns2 fdec s)
         | None -> None)
        (fun fk newsum -> 
           Rns.sums#addReplace fk newsum;
           (* also flush *)
           Rns.sums#flushOne fk;
           AU.sums#evictSummaries;
        )
        !inspect
        stMan#printState
        *)
    end

  method flushSummaries () = summarizer#flushSummaries ()
  
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


  let initStats cg sccCG (finalFuncs:fKey list) : unit = 
    (curCG := cg;
     curSCCCG := sccCG;
     (* TODO: clean up *)
     Intra.curCG := cg;
     Intra.curSCCCG := sccCG;

     (* Progress *)
     sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
     sccsDone := 0;

     numDerefsSafe := 0;
     numDerefsWarn := 0;
    )

      
  let updateStats lastSCC = 
    (* Progress *)
    incr sccsDone;
    L.logStatus (">>> PROGRESS " ^ (string_of_int !sccsDone) ^ "/" ^
                    (string_of_int !sccsTotal) ^ " SCCs DONE!\n")


  (**** State management / calculation ****)

  type state = Rns.RS.st


  (* RAVI: adding empty summaries for all functions without a body *)
  let initializeFuncsWithoutBody cg =
    Ci_cg.CG.FMap.iter
      (fun k n ->
         let sumKey = BS.inputFreeSumKey k in
         if n.hasBody then ()
         else
          (L.logStatus ("Setting summary of " ^ n.name ^ " to empty,\
                         since no body for it");
           L.flushStatus();
           
           (* empty .rns *)
           stMan#sums#addReplace sumKey (Some Rns.RNS.empty);
           (* empty .nwarn *)
           !NW.sums#addReplace sumKey (NW.emptySumm ());
          )
      ) cg

  (**** List of analyses that should be run (listed in order) *****)

  class symexAnalysis = object 
    inherit SPTA.symexAnalysis
    (** Override to not write out summaries (already done!) *)
    method summarize key (cfg:fundec) =
      false
  end

  let ssAna = new symexAnalysis
  let rnsAna = new nullAnalysis true
  let nwAna = new NW.nwAnalysis rnsAna#isFinal stMan getRns
    numDerefsSafe numDerefsWarn
  let kVis = (new Rns.rnsKnowledgeVisitor "race-sensitive-adjust" 
                getRns stMan)
  let knowledgeAna = new Knowledge_pass.knowledgeAnalysis rnsAna#isFinal kVis

  let needsFixpoint = [ ssAna; rnsAna; ]
  let nonFixpoint = [ nwAna; knowledgeAna ]


  let flushSummaries () =
  	RS.sum#evictSummaries;
    SPTA.SS.sum#evictSummaries;
    Par.sums#evictSummaries;
    AU.sums#evictSummaries;
    LP.sums#evictSummaries;
    stMan#sums#serializeAndFlush;
    !NW.sums#serializeAndFlush

  (* TODO: have one place where it can call all the 
     serializeAndFlushes? register the actual created objects, not just
     the module IDs? *)



  let doFunc ?(input:state = stMan#initialState) fk node : state IDF.interResult =
    let fn, defFile = node.name, node.defFile in
    L.logStatus ("Summarizing function: " ^ fn ^ " : " ^ defFile);
    L.logStatus "-----";
    L.flushStatus ();
    match Cilinfos.getFunc fk defFile with
      Some cfg ->
        (* TODO... make context-sensitive? *)
        let sumKey = Backed_summary.inputFreeSumKey fk in
        if Intra.runFixpoint needsFixpoint sumKey cfg then
          IDF.NewOutput (input, input) (* TODO: change return type *)
        else IDF.NoChange
    | None ->
        (* Don't have function definition *)
        L.logError ("doFunc can't get CFG for: " ^ 
                        (string_of_fNT (fn, defFile)));
        IDF.NoChange 


  (** TRUE if the function should be put on the worklist *)
  let filterFunc f : bool =
    true

  (* TODO: make this not hardCoded to all types, just what's needed *)
  let hardCodedSumTypes () =
    BS.getDescriptors [RS.sum#sumTyp;
                       SPTA.SS.sum#sumTyp;
                       LP.sums#sumTyp;
                       Par.sums#sumTyp;
                       (*NW.sums#sumTyp;*)
                      ]
	(* RNS summary? *)

  (** Prepare to start an scc *)
  let sccStart scc = begin
    L.logStatus "Acquiring callee summaries";
    L.flushStatus ();
    prepareSCCCalleeSums !curSCCCG scc (hardCodedSumTypes ());

    (* Get the RS summaries for functions in SCC 
       (as we are not recomputing them) *)
    L.logStatus "Acquiring SS/RS/LSP/PAR summaries for current SCC";
    prepareSCCSums scc (BS.getDescriptors [RS.sum#sumTyp;
                                           SPTA.SS.sum#sumTyp;
                                           LP.sums#sumTyp;
                                           Par.sums#sumTyp;
                                           AU.sums#sumTyp;
                                          ]);
    
  end

  (** Scc is summarized. Do the bookkeeping / cleanup *) 
  let sccDone scc (byThisGuy:bool) =
    let summPaths = if (byThisGuy) then
      let sumKeys = sumKeysOfScc scc [] in
      (* Now that all locksets have been computed, do the remaining passes *)
      let prevFunc = !curFunc in
      let prevFKey = funToKey prevFunc in
      Intra.runNonFixpoint nonFixpoint needsFixpoint prevFKey scc;

      (* Debugging *)
(*
      List.iter 
        (fun key ->
           L.logStatus ("Summary for function: " ^ BS.string_of_sumKey fkey));
           L.logStatus "=======\n";
           stMan#printSummary key;
        ) sumKeys;
*)
         
      (* Serialize and record where each fun was placed *)
      flushSummaries ();
      
      (* Find out where the summaries were stored *)
      (* TODO: force them to pick the same directory 
         (which they do unless on partition runs out of space) *)
      let tokenMap = Rns.sums#locate sumKeys in
      
      (* Notify others that the functions are now summarized *)
      List.fold_left
        (fun paths (key, tok) ->
           if (List.mem key sumKeys) then
             let path = BS.pathFromToken tok in
             (key, path) :: paths
           else paths
        ) [] tokenMap
    else [] in
    updateStats scc; (* and possibly delete obsolete summaries *)
    summPaths
    (* RFC.clear !astFCache *)

end

module BUDataflow = IDF.BottomUpDataflow (NullBUTransfer)


