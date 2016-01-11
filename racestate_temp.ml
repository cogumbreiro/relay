(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung, Ravi Chugh
  
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

(** Radar pre-pass to generate race equivalence regions and 
    all-unlocks summary *)

open Cil
open Callg
open Pretty
open Fstructs
open Manage_sums
open Lockset
open Logging

module LP = Lockset_partitioner
module AU = All_unlocks

module IDF = InterDataflow
module Intra = IntraDataflow
module BS = Backed_summary
module Stat = Mystats
module SPTA = Racestate.SPTA
module RS = Racestate.RS


(***************************************************)
(* Intra-proc Analysis                             *)

let debug = false
let inspect = ref false

let setInspect yesno =
  Racestate.setInspect yesno;
  inspect := yesno


module RaceDF = Racestate.RaceDF
module FITransF = Racestate.FITransF
module RaceForwardDF = Racestate.RaceForwardDF
let getLocksBefore = Racestate.getLocksBefore


(***************************************************)
(* Inter-proc Analysis                             *)
  
  
(** Info needed by the inter-proc driver *)
module RaceBUTransfer = struct
  

  (**** Statistics kept ****)
  
  let sccsDone = ref 0

  let sccsTotal = ref 0

  let curCG = ref emptyCG

  let curSCCCG = ref Scc_cg.emptySCCCG

  let initStats cg sccCG : unit = 
    (curCG := cg;
     curSCCCG := sccCG;
     (* TODO: clean this up... *)
     Intra.curCG := cg;
     Intra.curSCCCG := sccCG;
     Racestate.curCG := cg;
     Racestate.curSCCCG := sccCG;

     (* Progress *)
     sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
     sccsDone := 0;
    )


  let updateStats lastSCC = 
    (* Progress *)
    incr sccsDone;
    logStatusF ">>> PROGRESS %d/%d SCCs DONE!\n\n" !sccsDone !sccsTotal;
    flushStatus ()


  (**** State management / calculation ****)

  type state = RS.state

  (**** List of analyses that should be run (listed in order) *****)

  class ['st] writeOnlyAnalyzer = object (self)
    inherit ['st] Racestate.readWriteAnalyzer

    method addRefs curLS st loc exp =
      st
      
  end

  let ssAna = new SPTA.symexAnalysis
  let rsAna = new Racestate.raceAnalysis (new writeOnlyAnalyzer)
  let lpAna = new LP.lspAnalysis rsAna#isFinal getLocksBefore
  let auAna = new AU.auAnalysis rsAna#isFinal getLocksBefore

  let needsFixpoint = [ ssAna; rsAna ]

  let nonFixpoint = [ lpAna; auAna ]


  let flushSummaries () = begin
    BS.printSizeOfAll "Summarize (pre-flush)";
    RS.sum#serializeAndFlush;
    SPTA.SS.sum#serializeAndFlush;
    LP.sums#serializeAndFlush;
    AU.sums#serializeAndFlush;
    BS.printSizeOfAll "Summarize (post-flush)";
  end

        
  let doFunc ?(input:state = RS.emptyState) fid node : state IDF.interResult =
    let fn, defFile = node.name, node.defFile in
    logStatusF "Summarizing function: %s(%s):%s\n" 
      fn (fid_to_string fid) defFile;
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
        logErrorF "doFunc can't get CFG for: %s:%s\n" fn defFile;
        IDF.NoChange 


  (** TRUE if the function should be put on the worklist *)
  let filterFunc f : bool =
    true

  (* TODO: make this not use all of the summary types, just the ones needed *)
  let hardCodedSumTypes ()  =
    BS.getDescriptors [RS.sum#sumTyp;
                       SPTA.SS.sum#sumTyp;]
      (* LP and AU don't depend on callees, so leave out here *)

  (** Prepare to start an scc, acquiring the required summaries *)
  let sccStart scc  = begin
    logStatus "Acquiring needed summaries";
    flushStatus ();
    prepareSCCCalleeSums !curSCCCG scc (hardCodedSumTypes ());
  end

      
  (** Scc is summarized. Do the rest *) 
  let sccDone scc (byThisGuy:bool) =
    let summPaths = if (byThisGuy) then
      let sumKeys = sumKeysOfScc scc [] in
      (* Now that all locksets have been computed, do the remaining passes *)
      let prevFID = !Racestate.curFunID in
      Intra.runNonFixpoint nonFixpoint needsFixpoint prevFID scc;
      
      (* Debugging *)
      List.iter 
        (fun key ->
           let name = Racestate.getFunName key in
           logStatus ("Summary for function: " ^ fid_to_string key);
           logStatus "=======\n";
           RS.findPrintSumm name key;
           SPTA.SS.printSummary SPTA.SS.sum key;
           (* LP summaries, etc.? *)
        ) sumKeys;
      
      (* Serialize and record where each fun was placed *)
      flushSummaries ();

      (* Find out where the summaries were stored *)
      (* TODO: force them to pick the same directory 
         (which they do unless one partition runs out of space) *)
      let tokenMap = RS.sum#locate sumKeys in
      
      (* Notify others that the functions are now summarized *)
      List.fold_left
        (fun paths (key, tok) ->
           let path = BS.pathFromToken tok in
           (key, path) :: paths
        ) [] tokenMap
    else [] in
    updateStats scc;
    summPaths

end

module BUDataflow = IDF.BottomUpDataflow (RaceBUTransfer)
