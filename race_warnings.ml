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


(** Worker-logic for using data race analysis summaries to 
    generate warnings. Assumes server process is live. *)

open Cil
open Callg
open Manage_sums
open Warn_reports
open Logging

module RP = Race_reports
module LS = Lockset
module RS = Racestate.RS
module SPTA = Racestate.SPTA
module Stat = Mystats
module Lv = Lvals


(*************** State **************)

let warnRepLock = ref false
let localRaces = new RP.raceReports ()

(*
let localRaces2 = new RP.RaceWarnOut.report 
  ~maxWarningsPerKey:5 ~limitWarnings:true ()
*)

let makeLockList ls =
  (* let ls = LS.getLocked ls in
  LS.LMap.fold 
    (fun lv lock curList ->
       (LS.string_of_lock lv lock) :: curList) ls []
  *)
  LS.LS.ditchMinus ls



(**************** CHECK *****************)

(* TODO: make this not hardcoded *)
let hardCodedSumTypes () =
  Backed_summary.getDescriptors [RS.sum#sumTyp;
                                 SPTA.SS.sum#sumTyp; ]
    


class warningChecker cg cgDir = object (self)
  inherit [Roots.state] Roots.unordRootChecker 
  inherit Roots.ordAccessChecker cg cgDir as superAcc
  inherit Roots.netChecker
    
  val mutable curTR1 = (locUnknown, "", dummyFID)
    
  val mutable curTR2 = (locUnknown, "", dummyFID)

    
  method notifyDone =
    logStatus "completed all thread pairs";
    flushStatus ();
    try
      Request.notifyRace localRaces#data
    with e ->
      logError ("Warnings notifyDone: " ^ (Printexc.to_string e));
      raise e
      
        
  (** Override accessChecker *)
  method startState (loc1, fn1, fk1) (loc2, fn2, fk2) =
    superAcc#startState (loc1, fn1, fk2) (loc2, fn2, fk2);
    curTR1 <- (loc1, fn1, fk1);
    curTR2 <- (loc2, fn2, fk2)


  (** Update race reports w/ a new race *)
  method addRace imp lv1 lv2 
    (access1, locks1, locEmpty1, esc1) 
    (access2, locks2, locEmpty2, esc2) =
    let raceData = ( { RP.access = access1;
                       RP.threadRoot = curTR1;
                       RP.imprec = imp;
                       RP.emptied = locEmpty1;
                       RP.lval = lv1;
                       RP.locks = makeLockList locks1;
                       RP.threadEsc = esc1;
                     },
                     { RP.access = access2;
                       RP.threadRoot = curTR2;
                       RP.imprec = imp;
                       RP.emptied = locEmpty2;
                       RP.lval = lv2;
                       RP.locks = makeLockList locks2;
                       RP.threadEsc = esc2;
                     } ) in
    
    let _ = localRaces#addWarning raceData in
(*    let _ = localRaces2#addWarning (access1, access2) raceData in *)
    ()

  (* HACK for main for now... we should actually have a config file
     or something for functions that are only spawned once or
     pairs of functions known not to happen in parallel *)
  method private funsRunInParallel () =
    let _, fn1, _ = curTR1 in
    let _, fn2, _ = curTR2 in
    fn1 = "main" && fn2 = "main"


  (** Check if a pair of accesses is racey *)
  method checkPair accessTypes (lv1, corr1) (lv2, corr2) =
    if self#funsRunInParallel () then ()
    else 
      let lvCheck = Stat.time "sameLval" (Lv.sameLval lv1) lv2 in
      match lvCheck with
        None -> ()
      | Some (imprec) ->
          RS.GA.iterGuardedAccs 
            (fun access1 locks1 locEmpty1 _ ->
               RS.GA.iterGuardedAccs
                 (fun access2 locks2 locEmpty2 _ ->
                    let noCommonLS = LS.inter_isEmpty locks1 locks2 in
                    let sumLocks1 = LS.hasSummaryLock locks1 in
                    let sumLocks2 = LS.hasSummaryLock locks2 in
                    let esc1 = Shared.escapeableAbs lv1 in
                    let esc2 = Shared.escapeableAbs lv2 in
                    if esc1 && esc2 then
                      if noCommonLS then
                        self#addRace imprec lv1 lv2 
                          (access1, locks1, locEmpty1, esc1) 
                          (access2, locks2, locEmpty2, esc2)
                      else if !warnRepLock && (sumLocks1 || sumLocks2) then
                        (* TODO: if it's REPX.field + REPX.lock on both
                           accesses then that is okay *)
                        self#addRace imprec lv1 lv2
                          (access1, locks1, locEmpty1, esc1)
                          (access2, locks2, locEmpty2, esc2)
                      else ()
                    else ()
                 ) corr2
            ) corr1

  (** Helper function to clear some memory between each checked pair *)
  method evictSumms =
    List.iter (fun s -> s#evictSummaries) (hardCodedSumTypes ())


end

(** Check for candidate pairs of accesses that may result in a data race 
    Assumes: function summaries computed *)
let flagRacesFromSumms cg cgDir = begin
  let checker = new warningChecker cg cgDir in 
  checker#run;
  (* For now, have the client write out the warning data (separate file).
   * TODO: give the server the race2pakey data so that the server
   * writes out the warnings w/ IDs that match *)
  localRaces#saveToXML (Filename.concat cgDir "warnings2.xml");
(*  localRaces2#saveToXML (Filename.concat cgDir "warnings3.xml");
  localRaces2#serialize (Filename.concat cgDir "warnings3.dat");
  localRaces2#clear (); (* don't need it after... *)
*)
  localRaces#printWarnings;  (* for local debugging *)
end




let printAliasUses () =
  localRaces#printAliasAssumptions ()
    (* TODO: make it happen at server side so that we have the
       aggregated warnings instead of "localRaces"? *)
    
