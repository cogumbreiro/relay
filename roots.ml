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

(** General framework for checking state at thread roots / entry points.
    This is not entirely general, in that it only does pairwise comparisons. *)


open Cil
open Pretty
open Cildump
open Callg
open Fstructs
open Manage_sums
open Messages
open Entry_points
open Logging


module A = Alias
module Lv = Lvals
module Th = Threads
module Du = Cildump
module Race = Racestate
module RS = Race.RS
module SPTA = Race.SPTA
module Req = Request
module FC = Filecache
module DC = Default_cache


type iterCorrTypes = WW | WR | RW


(** Basic root iterator/checker.

    {ol
    {- Synchronize w/ others (through a barrier) }
    {- Uses [#getRoots] to find the root set (client can override). }
    {- Iterates through pairs (ordered/unordered depending on 
    user override) of roots ([#iterRoots]) }
    {- Gets the state at each root, and }
    {- does some operation on each pair of states ([#checkPair]) }
    {- Notify others (server) that we are done here }
    }

    @param  's    type of state acquired at each root and checked
*)
class virtual ['s] rootChecker = object(self)

  (** Checks a pair of states. Assumes outcome achieved through mutation *)
  method virtual checkStates : 's -> 's -> unit

  (** @return a list of functions that should be considered a root *)
  method virtual getRoots : root list

  (** @return a list of relevant state info for the given root *)
  method virtual getStates : root -> 's list

  (** Ask server if it's okay to compare these 2 roots 
      TODO: have server push instead of client request?
      @return [true] if the pair is available for processing  *)
  method virtual requestWork : root -> root -> bool

  (** Notify server that requested work is now done *)
  method virtual notifyWork : root -> root -> unit

  (** Synchronize the start-time with server (i.e., a barrier). *)
  method virtual requestStart : root list -> unit

  (** A barrier for the end of all checks *)
  method virtual notifyDone : unit

  (***** Handle basic iteration *****)

  (** Each root may actually have a list of states (not just one state). 
      Pick pairs from the lists of states for two roots 
      and iterate (n^{2}). The operation performed 
      for each pair is [#checkStates] *)
  method iterStates sl1 sl2 =
    List.iter 
      (fun s1 ->
         List.iter 
           (fun s2 ->
              self#checkStates s1 s2
           ) sl2
      ) sl1


  (** Check the all possible states for two given roots.  *)
  method checkRoot r1 r2 =
    if (self#requestWork r1 r2) then
      (logStatus "checking a thread-creation pair";
       flushStatus ();
       let stateList1 = self#getStates r1 in
       let stateList2 = self#getStates r2 in
       self#iterStates stateList1 stateList2;
       logStatus "done with pair, sending notification";
       flushStatus ();
       self#notifyWork r1 r2;
      )
    else
      () (* Hmm... can't try if work is locked but not done *)


  (** Method for iterating through all roots. May choose ordered pairs or
      unordered pairs. The operation performed for each pair is [#checkRoot] *)
  method virtual iterRoots : root list -> unit

  (** Entry point for checking. The body of the analysis is in 
      the [#checkStates] method. *)
  method run =
    let roots = self#getRoots in
    self#requestStart roots;
    self#iterRoots roots

end



(** Class that iterates through pairs of roots as unordered pairs *)
class virtual ['s] unordRootChecker = object (self)
  inherit ['s] rootChecker
  
  (** Iterate through each pair of roots (n choose 2) *)
  method iterRoots roots =
    let rec loop l1 l2 l2start =
      match l1, l2, l2start with
        [], _, _
      | _, _, [] -> 
          (* all done! *)
          self#notifyDone
            
      | _ :: tl1 , [], _ :: tl2 -> 
          loop tl1 tl2 tl2
            
      | r1 :: tl1 , r2 :: tl2, _ ->
          self#checkRoot r1 r2;
          loop l1 tl2 l2start
    in
    loop roots roots roots


  method numPairs (roots : root list) =
    let len = List.length roots in
    (len * (len + 1)) / 2  (* (len + 1) choose 2 *)
    

end


(* DEPRECATED
     for the case where all roots have the same summary type,
     ordered pairs of roots are a superset of unordered pairs of roots,
     so this should never be used in this case.
     it may make sense to use this if root1 and root2 take
     different summary types.  but right now, checking pairs
     of different summary types is not possible. *)

(** Class that iterates through pairs of roots as ordered pairs *)
class virtual ['s] ordRootChecker = object (self)
  inherit ['s] rootChecker
  
  (** Iterate through each pair of roots (n{^2}) *)
  method iterRoots roots =
    List.iter 
      (fun r1 ->
         List.iter (fun r2 -> self#checkRoot r1 r2) roots
      ) roots;
    self#notifyDone

  method numPairs (roots : root list) = 
    let len = List.length roots in
    len * len

end


(** Part of the checker that handles basic communication with the server *)
class virtual netChecker = object (self)

  (** Do this silly translation to break module circularity *)
  method translateRoot r =
    match r with 
      Entry (fk, _) -> MEntry fk
    | Thread (fk, _) -> MThread fk
                                   
  (** TODO: At requestStart, tell the server what kind of work? *)
  method requestWork r1 r2 =
    match Req.lockWarn 
            (self#translateRoot r1) (self#translateRoot r2) with
      MSuccess ->
        true
    | MDone
    | MLocked -> false
    | _ ->             
        logError "unexpected reply for lockWarn";
        false

  (** Helper to release summaries from memory *)
  method virtual evictSumms : unit
      
  (** Identify number of root-pairs to check before all checking is done *)
  method virtual numPairs : root list -> int

  (** Notify server that requested work is now done *)
  method notifyWork r1 r2 =
    Req.unlockWarn (self#translateRoot r1) (self#translateRoot r2);
    self#evictSumms


  (** Start barrier. Also sends the expected number of roots. 
      TODO check if sending number of roots needed *)
  method requestStart roots =
    let numPairs = self#numPairs roots in
    logStatus ("Expected # thread-creation site pairs: " ^ 
                   (string_of_int numPairs));
    flushStatus ();
    Req.reqWarnBarrier numPairs

(* Expect user to implement method #notifyDone *)

end


(*************************************************************
         Extensions that get assume state is read/write/lock 
         guarded accesses (@see guarded_access.ml). 

         Checks roots which are thread creation sites and
         user-specified entry points.
**************************************************************)

type state = 
    Cil.location * string * funID * RS.corrState

type sumDB = BS.dbManagement

(* debugging *)
let printFuns flist =
  let doc = seq_to_doc (text ", ") List.iter 
    (fun fid -> text (fid_to_string fid)) flist nil ++ line in
  logStatusD doc


(** A visitor that searches for function calls to thread creation 
    functions, and maps the summary of the "forked" functions to
    the parent thread's scope *)
class virtual threadRootStateCollector cg = object (self)
  inherit Th.threadCreateVisitor cg

  (** a list of states for (one state for each thread-creation site 
      within the analyzed function) *)
  val mutable stateList = []

  method virtual hardCodedSumTypes : unit -> sumDB list
  
  (** Prepare to load (possibly download) summaries needed by the
      thread creator [tc] *)
  method private prepareTCSumms tc = 
    let callees = calleeKeys tc in 
    prepareSumms callees (self#hardCodedSumTypes ())


  (** Handle a given call (at instruction [i], file position [loc],
      and within function [f]) to a thread creation function. 
      The target thread functions are believed to be [funs] and
      are supplied the argument list [args] *)
  method handleThreadRoots i loc f funs args =
    (try prepareSumms funs (self#hardCodedSumTypes ())
     with Req.SummariesNotFound ->
       (* Maybe the function isn't even in the callgraph (no function body).
          Can still continue w/ a dummy summary. *)
       let doc = seq_to_doc (text ", ") List.iter 
         (fun fid -> text (fid_to_string fid)) funs nil ++ line in
       logErrorD doc
    );
    try
      let pp = getCurrentPP () in
      let locks = RS.emptyLS in
      let appCSum = Race.FITransF.findApplyCSumm pp args locks in
      List.iter 
        (fun fkey -> (* Try to apply mapping *)
           try
             let fnode = FMap.find fkey cg in
             (* Record creation site info *)
             stateList <-
               (* Map the state of the new thread to parent's scope... 
                  only map the read / writes. The new thread starts with
                  an empty lockset. *)
               let corrs = appCSum fkey RS.emptyCS in
               (loc, fnode.name, fkey, corrs) :: stateList
           with Not_found ->
             logError ("handleThreadRoots: no callgraph node for " ^ 
                         (fid_to_string fkey))
               (* No fnode? *)
        ) funs
    with 
      Not_found -> logError "handleThreadRoots: Not_found?!"

  (** Return a list of states for each thread root spawned by the TC site *)
  method collectTRStates (tk, tc) : state list = 
    try 
      match Cilinfos.getFunc (fid_to_fkey tk) tc.defFile with
        Some func ->
          stateList <- [];
          self#prepareTCSumms tc;
          Race.RaceDF.initState tk func RS.emptyLS;
          (* Re-run the SymState so that function summaries can be applied *)
          let _ = Race.ssAna#compute tk func in
          let _ = Cil.visitCilFunction (self :> cilVisitor) func in
          stateList
      | None -> []
    with
      FC.File_not_found fname ->
        logError ("collectTRStates: can't find " ^ fname);
        []
    | Not_found ->
        logError "collectTRStates: Not_found?!";
        []


end


class virtual entrypointStateCollector = object (self)

  method virtual hardCodedSumTypes : unit -> sumDB list

  (** Unique "creation" location for all user-specified entry points *)
  val entryLoc = {line = 0; file = "#entry_point"; byte = 0}

  (** Add the state of a given entry point function 
      ([entK, entN] -- key and node) to the list of curStates  *)
  method collectEntryStates ((entK, entN) : (funID * callN)) : state list =
    let () = prepareSumms [entK] (self#hardCodedSumTypes ()) in
    let summ = RS.sum#find entK in
    [(entryLoc, entN.name, entK, (RS.summOutstate summ).RS.cState)]


end

(********* Complete guarded access state collector **********)

let hardCodedSumTypes () =
  BS.getDescriptors [RS.sum#sumTyp;
                     SPTA.SS.sum#sumTyp;]
    
(* Helper function for clearing some memory between each checked pair *)
let evictSumms () =
  RS.sum#evictSummaries; (* TODO: move this part elsewhere? *)
  SPTA.SS.sum#evictSummaries

class stateCollector cg = object (self)
  inherit threadRootStateCollector cg
  inherit entrypointStateCollector    

  method hardCodedSumTypes () =
    hardCodedSumTypes ()

end



(*********************************************************)


(** Class for checking guarded access summaries at roots *)
class virtual accessChecker cg cgDir = object (self)
  inherit stateCollector cg

  (** Function to check a pair of guarded accesses *)
  method virtual checkPair : iterCorrTypes -> (Lv.aLval * RS.GA.correlation) -> 
    (Lv.aLval * RS.GA.correlation) -> unit 

  (** Return the list of tagged roots that are relevant to the analysis. *)
  method getRoots : root list =
    let rooter = new Entry_points.rootGetter cg cgDir in
    rooter#getRoots ()

  (** Get the states for a given root *)
  method getStates r : state list =
    match r with
      Entry rootInfo -> self#collectEntryStates rootInfo
    | Thread rootInfo -> self#collectTRStates rootInfo
  
end




(** Class for (ordered) pair comparison of access summaries *)
class virtual ordAccessChecker cg cgDir = object (self)
  inherit accessChecker cg cgDir

  (** Iterate through all pairs of guarded accesses formed by
      the two input lists and apply [#checkPair] to each pair *)
  method iterCorrs accessTypes (i1, c1) (i2, c2) =
    i1 (fun lv1 c1 -> 
          i2 (fun lv2 c2 ->
                self#checkPair accessTypes (lv1, c1) (lv2, c2)
             ) c2
       ) c1

  (** Indicate that we are beginning to check a pair of root states *)
  method startState (loc1, fn1, fk1) (loc2, fn2, fk2) =
    logStatusF "now checking thread roots: %s(%s), %s(%s)\n"
      fn1 (fid_to_string fk1) fn2 (fid_to_string fk2)
      
  (** Check a pair of root states *)
  method checkStates 
    ((loc1, fn1, fk1, st1) : state) 
    ((loc2, fn2, fk2, st2) : state) =
    self#startState (loc1, fn1, fk1) (loc2, fn2, fk2);
    let wIter = RS.iterWrites in
    let rIter = RS.iterReads in
    self#iterCorrs WW (wIter, st1) (wIter, st2);
    self#iterCorrs WR (wIter, st1) (rIter, st2);
    self#iterCorrs RW (rIter, st1) (wIter, st2);


end

(***************** DEBUG *****************)

(** Print out summaries of thread roots *)
let  printTRootSumms cg tcList = 
  let sc = new stateCollector cg in
  let rec loop tcList = 
    match tcList with
      [] -> ()
        
  | (tcKey, tc) :: tl ->
      (* Walk through those functions, processing the actual calls.
         Get race state for each of the thread-start-functions *)
      let stateList = sc#collectTRStates (tcKey, tc) in
      List.iter 
        (fun (loc, fn, _, state) ->
           logStatus ("Found thread creation at: " ^ (string_of_loc loc));
           logStatus ("Initial function is: " ^ fn);
           logStatus ("and state:");
           if(RS.isBottomCS (state)) then
             logStatus "state is $BOTTOM"
           else
             () (* RS.printState state; *)
        ) stateList;
      (* Free some more memory before printing more *)
      evictSumms ();
      loop tl
  in
  loop tcList
