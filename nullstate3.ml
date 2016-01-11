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

module IH = Inthash
module DF = Dataflow
module IDF = InterDataflow
module Intra = IntraDataflow
module A = Alias
module RS = Racesummary
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
module LP = Lockset_partitioner
module Par = Pseudo_access
module AU = All_unlocks
module NW = Null_warnings

(***************************************************)
(* State / Utilities :                             *)

(* Current function *)
let curFunc = Nullstate2.curFunc


(***************************************************)
(* Intra-proc Analysis                             *)

let debug = false
let inspect = ref false

let setInspect yesno =
  inspect := yesno


let stMan = Nullstate2.stMan
let getRns = Nullstate2.getRns


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
  let rnsAnaPess = new Nullstate2.nullAnalysis false
  let nwAnaPess = new NW.nwAnalysis rnsAnaPess#isFinal stMan getRns
    numDerefsSafe numDerefsWarn
  let kVis = (new Rns.rnsKnowledgeVisitor "pessimistic-adjust" 
                getRns stMan)
  let knowledgeAna = new Knowledge_pass.knowledgeAnalysis rnsAnaPess#isFinal kVis

  let needsFixpoint = [ ssAna; rnsAnaPess ]
  let nonFixpoint = [ nwAnaPess; knowledgeAna ]


  let flushSummaries () =
  	RS.sum#evictSummaries;
    SPTA.SS.sum#evictSummaries;
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
        else
          IDF.NoChange
          
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
    (* Get all summaries for all callees *)
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
      List.iter 
        (fun key ->
           L.logStatus ("Summary for function: " ^ 
                          (BS.string_of_sumKey key));
           L.logStatus "=======\n";
           stMan#printSummary key;
        ) sumKeys;
      
      (* Serialize and record where each fun was placed *)
      flushSummaries ();

      (* Find out where the summaries were stored *)
      (* TODO: force them to pick the same directory 
         (which they do unless on partition runs out of space) *)
      let tokenMap = Rns.sums#locate sumKeys in
      
      (* Notify others that the functions are now summarized *)
      List.fold_left
        (fun paths (fkey, tok) ->
           if (List.mem fkey sumKeys) then
             let path = BS.pathFromToken tok in
             (fkey, path) :: paths
           else paths
        ) [] tokenMap
    else [] in
    updateStats scc; (* and possibly delete obsolete summaries *)
    summPaths
    (* RFC.clear !astFCache *)

end

module BUDataflow = IDF.BottomUpDataflow (NullBUTransfer)


