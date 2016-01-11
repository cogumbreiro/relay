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
open Callg
open Fstructs
open Manage_sums
open Cilinfos
open Messages

module A = Alias
module Lv = Lvals
module Th = Threads
module SPTA = Symstate2
module Du = Cildump
module GA = Guarded_access
module RS = Racesummary
module Race = Racestate
module Req = Request
module FC = Filecache
module DC = Default_cache

module L = Logging


type root = 
    Entry of simpleCallN
  | Thread of simpleCallN


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
  method virtual getRoots : (fKey * root) list

  (** @return a list of relevant states for the given root *)
  method virtual getStates : (fKey * root) -> 's list

  (** Ask server if it's okay to compare these 2 roots 
      TODO: have server push instead of client request?
      @return [true] if the pair is available for processing  *)
  method virtual requestWork : fKey -> fKey -> bool

  (** Notify server that requested work is now done *)
  method virtual notifyWork : fKey -> fKey -> unit

  (** Synchronize the start-time with server (i.e., a barrier). 
      TODO: check if int is what we want to send *)
  method virtual requestStart : int -> unit

  (** Another barrier for the end of the root checking *)
  method virtual notifyDone : unit

  (***** Handle basic iteration *****)

  (** Each root may actually have a list of states (not just one state). 
      Pick pairs from the lists of states for two roots 
      and iterate (n choose 2). The operation performed 
      for each pair is [#checkStates] *)
  method iterStates sl1 sl2 =
    let rec loop sl1 sl2 sl2start = 
      match sl1, sl2, sl2start with
        [], _, _
      | _, _, [] -> ()
          
      | _ :: tl1 , [], _ :: tl2 -> 
          loop tl1 tl2 tl2
            
      | s1 :: tl1 , s2 :: tl2, _ ->
          self#checkStates s1 s2;
          loop sl1 tl2 sl2start
    in
    loop sl1 sl2 sl2

  (** Check the all possible states for two given roots.  *)
  method checkRoot fk1 r1 fk2 r2 =
    if (self#requestWork fk1 fk2) then
      (L.logStatus "checking a thread-creation pair";
       L.flushStatus ();
       let stateList1 = self#getStates r1 in
       let stateList2 = self#getStates r2 in
       self#iterStates stateList1 stateList2;
       L.logStatus "done with pair, sending notification";
       L.flushStatus ();
       self#notifyWork fk1 fk2;
      )
    else
      () (* Hmm... can't try if work is locked but not done *)


  (** Method for iterating through all roots. May choose ordered pairs or
      unordered pairs. The operation performed for each pair is [#checkRoot] *)
  method virtual iterRoots : (fKey * root) list -> unit


  (** Entry point for checking. The body of the analysis is in 
      the [#checkStates] method. *)
  method run =
    let roots = self#getRoots in
    let len = List.length roots in
    let numPairs = (len * (len + 1)) / 2 in (* (len + 1) choose 2 *)
    L.logStatus ("Expected # thread-creation site pairs: " ^ 
                   (string_of_int numPairs));
    L.flushStatus ();
    self#requestStart numPairs;
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
            
      | ((fk1, _) as r1) :: tl1 , ((fk2, _) as r2) :: tl2, _ ->
          self#checkRoot fk1 r1 fk2 r2;
          loop l1 tl2 l2start
    in
    loop roots roots roots

end




(** Class that iterates through pairs of roots as ordered pairs *)
class virtual ['s] ordRootChecker = object (self)
  inherit ['s] rootChecker
  
  (** Iterate through each pair of roots (n{^2}) *)
  method iterRoots roots =
    List.iter 
      (fun ((fk1, _) as r1) ->
         List.iter
           (fun ((fk2, _) as r2) ->
              self#checkRoot fk1 r1 fk2 r2
           ) roots
      ) roots;
    self#notifyDone

end

(* Helper function for clearing some memory between each checked pair *)
let evictSumms () =
  RS.sum#evictSummaries; (* TODO: move this part elsewhere? *)
  SS.sum#evictSummaries



(** A rootChecker that handles basic communication with the server *)
class virtual ['s] netChecker = object (self)
  inherit ['s] rootChecker

  (* TODO: make it not warning-generation specific...
     At requestStart, tell the server what kind of work? *)
  method requestWork fk1 fk2 =
    match Req.lockWarn fk1 fk2 with
      MSuccess ->
        true
    | MDone
    | MLocked -> false
    | _ ->             
        L.logError "unexpected reply for lockWarn";
        false

  (** Notify server that requested work is now done *)
  method notifyWork fk1 fk2 =
    let _ = Req.unlockWarn fk1 fk2 in
    evictSumms () (* TODO: move this elsewhere? *)


  (** Start barrier. Also sends the expected number of roots. 
      TODO check if sending number of roots needed *)
  method requestStart numPairs =
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
    Cil.location * string * fKey * RS.corrState

(* debugging *)
let printFuns flist =
  let buff = Buffer.create 12 in
  List.iter (fun fkey -> Buffer.add_string buff 
               ((string_of_fkey fkey) ^ "; ")) flist;
  L.logStatus (Buffer.contents buff)
    
(* TODO: make this not hardCoded to allTypes, just use what's needed *)
let hardCodedSumTypes () =
  !BS.allTypes
    

(** Prepare to load (possibly download) summaries needed by the
    thread creator [tc] *)
let prepareTCSumms tc = 
  prepareSumms tc.callees (hardCodedSumTypes ())


(** A visitor that searches for function calls to thread creation 
    functions, and maps the summary of the "forked" functions to
    the parent thread's scope *)
class threadRootStateCollector cg startList = object (self)
  inherit Th.threadCreateVisitor cg

  (** a list of states for (one state for each thread-creation site 
      within the analyzed function) *)
  val mutable stateList = startList
  
  method getStates () =
    stateList


  (** Handle a given call (at instruction [i], file position [loc],
      and within function [f]) to a thread creation function. 
      The target thread functions are believed to be [funs] and
      will begin with argument [arg] *)
  method handleThreadRoots i loc f funs arg =
    (try prepareSumms funs (hardCodedSumTypes ())
     with Req.SummariesNotFound ->
       (* Maybe the function isn't even in the callgraph (no function body) *)
       let buff = Buffer.create 80 in
       Buffer.add_string buff ("roots: no summary for: " ^ (Du.string_of_exp f));
       List.iter 
         (fun fkey -> Buffer.add_string buff ((string_of_fkey fkey) ^ ", ")) 
         funs;
       L.logError (Buffer.contents buff)
    );
    try
      let appCSum = Race.RaceDF.findApplyCSumm curStmt i loc [arg] in
      List.iter 
        (fun fkey ->
           (* Try to apply mapping *)
           try
             let fnode = FMap.find fkey cg in
             (* Record creation site info *)
             stateList <-
               (* Map the state of the new thread to parent's scope... 
                  only map the read / writes. The new thread starts with
                  an empty lockset. *)
               let locks = RS.emptyLS in
               let corrs = appCSum locks fkey RS.emptyCS in
               (loc, fnode.name, fkey, corrs) :: stateList
           with Not_found ->
             L.logError ("handleThreadRoots: no callgraph node for " ^ 
                           (string_of_fkey fkey))
               (* No fnode? *)
        ) funs
    with 
      Not_found ->
        L.logError "handleThreadRoots: Not_found?!"

end


(** Return a list of states for each thread root spawned by the TC site *)
let collectTRStates cg curStates (tk, tc) = 
  try 
    let ast = !DC.astFCache#getFile tc.defFile in
    A.setCurrentFile ast;
    match getCFG tk ast with
      Some func ->
        prepareTCSumms tc;
        Race.RaceDF.initState func RS.emptyLS;
        let vis = new threadRootStateCollector cg curStates in
        (* Re-run the SymState so that function summaries can be applied *)
        let _ = SPTA.doSymState func in
        let _ = Cil.visitCilFunction (vis :> cilVisitor) func in
        vis#getStates ()
    | None -> 
        curStates
  with
    FC.File_not_found fname ->
      L.logError ("collectTRStates: can't find " ^ fname);
      curStates
  | Not_found ->
      L.logError "collectTRStates: Not_found?!";
      curStates


(** Unique "creation" location for all user-specified entry points *)
let entryLoc = {line = 0; file = "#entry_point"; byte = 0}


(** Add the state of a given entry point function 
    ([entK, entN] -- key and node) to the list of curStates  *)
let collectEntryStates cg curStates (entK, entN) : state list =
  (* incomplete to assume LS = {} for these entry points, 
     but it's sound and that's all we can do... *)
  try
    prepareSumms [entK] (hardCodedSumTypes ());
    let summ = RS.sum#find entK in
    (entryLoc, entN.name, entK, (RS.summOutstate summ).RS.cState) :: curStates
  with Not_found ->
    curStates



(*********************************************************)


(** Class for checking guarded access summaries at roots *)
class virtual accessChecker cg cgDir = object (self)

  (** Function to check a pair of guarded accesses *)
  method virtual checkPair : (Lv.aLval * GA.correlation) -> 
    (Lv.aLval * GA.correlation) -> unit 


  (** Return the list of tagged roots that are relevant to the analysis. *)
  method getRoots : (fKey * root) list =
    (* Find which functions actually fork new threads *)
    let threadCreatorCallers = Th.findTCCallers cg in
    (* Find possible / user-specified entry-points. *)
(*
    (* [a] Get user-specified entry points (in case they aren't roots)
       [b] Find call graph roots that reach spawn sites. *)
    let entryRoots = Entry_points.getEntries cgDir cg in

    let tcc = List.fold_left
      (fun cur (fkey, _) -> FSet.add fkey cur) FSet.empty threadCreatorCallers in
    let roots = FSet.union entryRoots (rootsThatReach cg tcc) in
    let roots = rootsThatReach cg tcc in

    (* Thread roots may be "entry-points" as well, but they're already
       handled by the "Thread n" tagging above, so filter *)
    let threadRoots = Th.getThreadRoots cg threadCreatorCallers in
    let roots = FSet.filter
      (fun f -> not (FSet.mem f threadRoots)) roots in
*)
    (* finally, tag the thread creators as "Thread n" and tag
       other entry-points as "Entry n" *)
    let threadCreatorCallers = List.map 
      (fun (k,n) -> (k, Thread n)) threadCreatorCallers in
(*
    let roots = FSet.fold 
      (fun k cur -> 
         try let n = FMap.find k cg in
         (k, Entry n) :: cur
         with Not_found -> 
           L.logError ("getRoots: no node for " ^ (string_of_fkey k));
           cur
      ) roots threadCreatorCallers in
    roots
*) threadCreatorCallers
   
  (** Get the states for a given root ([fk, r]) *)
  method getStates (fk, r) : state list =
    match r with
      Entry e -> collectEntryStates cg [] (fk, e)
    | Thread tc -> collectTRStates cg [] (fk, tc)
  
end




(** Class for (unordered) pair comparison of access summaries *)
class virtual unordAccessChecker cg cgDir = object (self)
  inherit accessChecker cg cgDir


  (** Iterate through all pairs of guarded accesses formed by
      the two input lists and apply [#checkPair] to each pair *)
  method iterCorrs cl1 cl2 =
    let rec loop l1 l2 l2start =
      match l1, l2, l2start with
        [], _, _
      | _, _, [] -> ()
          
      | _ :: tl1 , [], _ :: tl2 -> 
          loop tl1 tl2 tl2
            
      | (lv1, corr1) :: tl1 , (lv2, corr2) :: tl2, _ ->
          self#checkPair (lv1, corr1) (lv2, corr2);
          loop l1 tl2 l2start  
    in
    loop cl1 cl2 cl2

  (** Indicate that we are beginning to check a pair of root states *)
  method startState (loc1, fn1, fk1) (loc2, fn2, fk2) =
    L.logStatus ("now checking thread roots: " ^ fn1 ^ ", " ^ fn2)

  (** Check a pair of root states *)
  method checkStates 
    ((loc1, fn1, fk1, st1) : state) 
    ((loc2, fn2, fk2, st2) : state) =
    self#startState (loc1, fn1, fk1) (loc2, fn2, fk2);
    let wCorrList1 = RS.listWriteCorr st1 in
    let wCorrList2 = RS.listWriteCorr st2 in
    let rCorrList1 = RS.listReadCorr st1 in
    let rCorrList2 = RS.listReadCorr st2 in
    L.logStatus ("w/ wr1, wr2, rd1, rd2 sets: " ^
                   (string_of_int (List.length wCorrList1)) ^ ", " ^
                   (string_of_int (List.length wCorrList2)) ^ ", " ^ 
                   (string_of_int (List.length rCorrList1)) ^ ", " ^
                   (string_of_int (List.length rCorrList2)));
    L.flushStatus ();
    self#iterCorrs wCorrList1 wCorrList2;
    self#iterCorrs wCorrList1 rCorrList2;
    self#iterCorrs rCorrList1 wCorrList2
  
end



(** Class for (ordered) pair comparison of access summaries *)
class virtual ordAccessChecker cg cgDir = object (self)
  inherit accessChecker cg cgDir

  (** Iterate through all pairs of guarded accesses formed by
      the two input lists and apply [#checkPair] to each pair *)
  method iterCorrs (i1, c1) (i2, c2) =
    i1 (fun lv1 c1 -> 
          i2 (fun lv2 c2 ->
                self#checkPair (lv1, c1) (lv2, c2)
             ) c2
       ) c1

  (** Indicate that we are beginning to check a pair of root states *)
  method startState (loc1, fn1, fk1) (loc2, fn2, fk2) =
    L.logStatus ("now checking thread roots: " ^ fn1 ^ ", " ^ fn2)
      
  (** Check a pair of root states *)
  method checkStates 
    ((loc1, fn1, fk1, st1) : state) 
    ((loc2, fn2, fk2, st2) : state) =
    self#startState (loc1, fn1, fk1) (loc2, fn2, fk2);
    let wIter = RS.iterWrites in
    let rIter = RS.iterReads in
    self#iterCorrs (wIter, st1) (wIter, st2);
    self#iterCorrs (wIter, st1) (rIter, st2);
    self#iterCorrs (rIter, st1) (wIter, st2);


end

(***************** DEBUG *****************)

(** Print out summaries of thread roots *)
let rec printTRootSumms cg tcList = 
  match tcList with
    [] -> ()

  | (tcKey, tc) :: tl ->
      (* Walk through those functions, processing the actual calls.
         Get race state for each of the thread-start-functions *)
      let stateList = collectTRStates cg [] (tcKey, tc) in
      List.iter 
        (fun (loc, fn, _, state) ->
           L.logStatus ("Found thread creation at: " ^ 
                           (Du.string_of_loc loc));
           L.logStatus ("Initial function is: " ^ fn);
           L.logStatus ("and state:");
           if(RS.isBottomCS (state)) then
              L.logStatus "state is $BOTTOM"
           else
              () (* RS.printState state; *)
        ) stateList;
      (* Free some more memory before printing more *)
      evictSumms ();
      printTRootSumms cg tl


