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
open Fstructs
open Messages
open Cilinfos
open Manage_sums
open Warn_reports

module RP = Race_reports

module LS = Lockset
module Th = Threads
module BS = Backed_summary
module RS = Racesummary
module Race = Racestate
module SPTA = Race.SPTA
module Req = Request
module L = Logging

module Stat = Mystats

module Lv = Lvals
module CLv = Cil_lvals

(*************** State **************)

let localRaces = new RP.raceReports ()


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
  BS.getDescriptors [RS.sum#sumTyp;
                     SPTA.SS.sum#sumTyp;
                    ]
    


class warningChecker cg cgDir = object (self)
  inherit [Roots.state] Roots.unordRootChecker 
  inherit Roots.ordAccessChecker cg cgDir
  inherit Roots.netChecker
    
  val mutable curTR1 = ({line = 0;
                         file = "";
                         byte = 0;}, "", 0)
    
  val mutable curTR2 = ({line = 0;
                         file = "";
                         byte = 0;}, "", 0 )

    
  method notifyDone =
    L.logStatus "completed all thread pairs";
    L.flushStatus ();
    localRaces#printWarnings;  (* for local debugging *)
    try
      Req.notifyRace localRaces#data
    with e ->
      L.logError ("Warnings notifyDone: " ^ (Printexc.to_string e));
      raise e
      
        
  (** Override accessChecker *)
  method startState (loc1, fn1, fk1) (loc2, fn2, fk2) =
    curTR1 <- (loc1, fn1, fk1);
    curTR2 <- (loc2, fn2, fk2);
    L.logStatus ("now checking thread roots: " ^ fn1 ^ ", " ^ fn2)


  (** Update race reports w/ a new race *)
  method addRace imp lv1 lv2 (access1, locks1) (access2, locks2) esc1 esc2 =
    let _ = localRaces#addRace
      ( { RP.access = access1;
          RP.threadRoot = curTR1;
          RP.imprec = imp;
          RP.emptied = Cil.locUnknown; (* corr1.RS.GA.corrLEmpty; *)
          RP.lval = lv1;
          RP.locks = makeLockList locks1;
          RP.threadEsc = esc1;
        },
        { RP.access = access2;
          RP.threadRoot = curTR2;
          RP.imprec = imp;
          RP.emptied = Cil.locUnknown; (* corr2.RS.GA.corrLEmpty; *)
          RP.lval = lv2;
          RP.locks = makeLockList locks2;
          RP.threadEsc = esc2;
        } ) in
    ()

  (** Check if a pair of accesses is racey *)
  method checkPair accessTypes (lv1, corr1) (lv2, corr2) =
    let lvCheck = Stat.time "sameLval" (Lv.sameLval lv1) lv2 in
    match lvCheck with
      None -> ()
    | Some (imprec) ->
        RS.GA.iterGuardedAccs 
          (fun access1 locks1 _ ->
             RS.GA.iterGuardedAccs
               (fun access2 locks2 _ ->
                  let noCommonLS = LS.inter_isEmpty locks1 locks2 in
                  let sumLocks1 = LS.hasSummaryLock locks1 in
                  let sumLocks2 = LS.hasSummaryLock locks2 in
                  let esc1 = Shared.escapeableAbs lv1 in
                  let esc2 = Shared.escapeableAbs lv2 in
                  if noCommonLS then
                    if esc1 && esc2 then
                      self#addRace imprec lv1 lv2 
                        (access1, locks1) (access2, locks2) esc1 esc2
                    else begin
                      (* Just have it there to see how many we prune 
                         from esc analysis *)
                      self#addRace imprec lv1 lv2 
                        (access1, locks1) (access2, locks2) esc1 esc2
                    end          
                  else if (sumLocks1 || sumLocks2) then
                    (* Record, lockset itself has info on whether on it 
                       uses rep. nodes *)
                    self#addRace imprec lv1 lv2 
                      (access1, locks1) (access2, locks2) esc1 esc2
               ) corr2
          ) corr1       

  (** Helper function to clear some memory between each checked pair *)
  method evictSumms =
    List.iter (fun s -> s#evictSummaries) (hardCodedSumTypes ())


end

(** Check for candidate pairs of accesses that may result in a data race 
    Assumes: function summaries computed *)
let flagRacesFromSumms cg cgDir =
  let checker = new warningChecker cg cgDir in
  checker#run;
  (* For now, have the client write out the warning data (separate file).
   *      TODO: give the server the race2pakey data so that the server
   *           writes out the warnings w/ IDs that match *)
  localRaces#saveToXML (Filename.concat cgDir "warnings2.xml")




let printAliasUses () =
  localRaces#printAliasAssumptions ()
    (* TODO: make it happen at server side so that we have the
       aggregated warnings instead of "localRaces"? *)
    
