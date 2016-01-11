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
module GA = Guarded_access
module RS = Racesummary
module Race = Racestate
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

(** Return true if the lockset uses a summary lock to ensure race-freedom *)
let hasSummaryLock ls =
  LS.LS.S.exists
    (fun lv _ -> match lv with 
       Lv.AbsHost _, _ -> true
     | _ -> false) (LS.LS.getPlus ls)
    (** should check the size of the rep node too... *)



(**************** CHECK *****************)

let inter_isEmpty ls1 ls2 =
  let ls1' = LS.LS.ditchMinus ls1 in
  let ls2' = LS.LS.ditchMinus ls2 in
  LS.LS.emptyPlus (LS.LS.inter ls1' ls2')

class warningChecker cg cgDir = object (self)
  inherit [Roots.state] Roots.unordRootChecker 
  inherit Roots.unordAccessChecker cg cgDir
  inherit [Roots.state] Roots.netChecker
    
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
  method addRace imp lv1 lv2 corr1 corr2 =
    let access1, access2 = corr1.GA.corrAccess, corr2.GA.corrAccess in
    localRaces#addRace
      ( { RP.access = access1;
          RP.threadRoot = curTR1;
          RP.imprec = imp;
          RP.emptied = corr1.GA.corrLEmpty;
          RP.lval = lv1;
          RP.locks = makeLockList corr1.GA.corrLocks; },
        { RP.access = access2;
          RP.threadRoot = curTR2;
          RP.imprec = imp;
          RP.emptied = corr2.GA.corrLEmpty;
          RP.lval = lv2;
          RP.locks = makeLockList corr2.GA.corrLocks; })

  method checkPair (lv1, corr1) (lv2, corr2) =
    let lvCheck = Stat.time "sameLval" (Lv.sameLval lv1) lv2 in
    match lvCheck with
      None -> ()
      | Some (imprec) ->
          if (inter_isEmpty corr1.GA.corrLocks corr2.GA.corrLocks) then
            self#addRace imprec lv1 lv2 corr1 corr2
              (*        else if (hasSummaryLock corr1.GA.corrLocks 
                        || hasSummaryLock corr2.GA.corrLocks ) 
                        then self#addRace imprec lv1 lv2 corr1 corr2
              *)
              
end

(** Check for candidate pairs of accesses that may result in a data race 
    Assumes: function summaries computed *)
let flagRacesFromSumms cg cgDir =
  let checker = new warningChecker cg cgDir in
  checker#run 


    
