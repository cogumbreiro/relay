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


(** Calculate the set of locks responsible for protecting an lvalue.
    Lvals have been translated in terms of names at a thread root. 
    Remember those lval-lockset correlations.
*)


open Cil

module L = Logging
module Th = Threads 
module Race = Racestate

let evictSumms () =
  Racesummary.sum#evictSummaries;
  Symsummary.sum#evictSummaries

class protLockSumm cg = object (self)

  val mutable curTR1 = ( {line = 0;
                          file = "";
                          byte = 0;}, "", 0 )
    
  val mutable  curTR2 = ( {line = 0;
                           file = "";
                           byte = 0;}, "", 0)

  method private compareSummLists stateList1 stateList2 =
    ()
    

  method private compareTCLists l1 l2 l2start =
    match l1, l2, l2start with
      [], _, _ -> 
        (* all done! *)
        L.logStatus "completed all thread pairs";
        L.flushStatus ();
        (* printProts *)
        (* Req.notifyWarn (Marshal.to_string localRaces [Marshal.Closures]) *)
        
    | _ :: tl1, [], _ ->
        self#compareTCLists tl1 l2start l2start

    | ((fk1, _) as r1) :: _ , ((fk2, _) as r2) :: tl2, _ ->
(*        match Req.lockWarn fk1 fk2 with
          MSuccess ->
*)
        L.logStatus "checking a thread-creation pair";
        L.flushStatus ();
(* TODO: use Roots.ml framework
        let stateList1 = Roots.getStates cg r1 in
        let stateList2 = Roots.getStates cg r2 in
        self#compareSummLists stateList1 stateList2;
*)
        (* clear up some memory *)
        evictSumms ();
(*        let _ = Req.unlockWarn fk1 fk2 in 
        Stat.print stdout "";
*)
        self#compareTCLists l1 tl2 l2start

(*          
        | MDone 
        | MLocked ->
            (* Locked, or someone else completed... for now assume
               that locker won't crash, so don't need to retry if locked *)
            compareTCLists l1 tl2 l2start
        | _ ->
            L.logError "unexpected reply for lockWarn";
            compareTCLists l1 tl2 l2start
        self#compareTCLists l1 tl2 l2start
*) 

  (** For each access in a thread, calculate the actual protecting locks...
      the locks common between all accesses to the same mem in all threads *)
  method calcProtectingLocks =
    (* Find which functions actually fork new threads *)
    let threadCreatorCallers = Th.findTCCallers cg in
    let threadCreatorCallers = List.map (fun (k,n) -> (k, Roots.Thread n)) 
      threadCreatorCallers in
    let tcList = threadCreatorCallers in
    let len = List.length tcList in
    let numPairs = (len * len) in (* all ordered pairs *)
    L.logStatus ("Prot Locks: Expected # thread-creation pairs: " ^ 
                   (string_of_int numPairs));
    L.flushStatus ();
    (*  Req.reqWarnBarrier numPairs;  *)
    self#compareTCLists tcList tcList tcList
      
end






(* Should track number of times the overall intersection is smaller than
   the intersection between any one pair *)

(* How to index the "same" lvalue? Keep separate per thread? *)



(* 
   HMM... for each thread, intersect w/ all other threads, 
   and see the survivors? 
   But... that means we need to trace the accessed lvals also?!
   Problem is you don't know what the lval will alias with in other
   threads, so you need to trace everything up...

   Even if we didn't do this tracing of Locks... 
   to know that a local lval is not racy... we need to trace it to the top

   Well, try flushing summaries, flushing parts of the graph, etc... 
   maybe the graph itself is large but not intractably large?
*)
