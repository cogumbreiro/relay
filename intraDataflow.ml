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

(** Module defining the interface for extending + glue between 
    the CIL intra-procedural  dataflow framework 
    and all the inter-procedural components. Steps to use:

    1) Define a State Manager [S]
    2) Define a Transfer function [T]
    3) Package S and T as a DFTransfer [D]
    4) Make a flow-sensitive or flow-insensitive analysis by
       calling the appropriate functor : [D] -> [A]
    5) Inter-proceduralize the analysis [A] -> [InterProcA] 
       w/ the appropriate functor

    This module also provides basic transfer functions for analyses
    involving relative sets (of [aLvals])
*)




open Cil
open Cilinfos
open Callg
open Scc
open Fstructs
open Sym_types
open Scope

module IH = Inthash
module SPTA = Symstate2
module A = Alias
module Lv = Lvals
module CLv = Cil_lvals
module Stat = Mystats
module L = Logging
module Du = Cildump
module FC = Filecache
module DC = Default_cache
module BS = Backed_summary

module DF = Dataflow
module IDF= InterDataflow



(*******************************************************************)
(* Util functions                                                  *)

(** Do function call lval substitution, given the occurance of the function
    call ([curStmt], [curInstr]), the exact actual argument ([argExp])
    and the summary lvalue ([lvalWithFormal]) *)
let substActFormN curStmt curInstr argExp lvalWithFormal =
  (* Expand aliases of actual *)
  let mustAlias, aliases = 
    SPTA.getAliasesAtInstr curStmt curInstr argExp in
  let results = 
    List.fold_left
      (fun curList actualE ->
         try
           let result = Lv.substActForm actualE lvalWithFormal in
           let (simpleH, simpleO), _ = Lv.simplifyLval result in
           (simpleH, simpleO) :: curList
         with CLv.SubstInvalidArg ->
           curList
      )  [] aliases in
  (mustAlias, results)


(** Do function call argument renaming, if necessary (depending on 
    the [scope] of [lvalWithFormal] -- which is possibly just a global) *)
let substActForm curStmt curInstr actuals lvalWithFormal scope =
  match scope with 
    SGlobal -> (* no renaming, just simplify offsets *)
      let simpleLVWF = Lv.canonicizeLval lvalWithFormal in
      (true, [simpleLVWF])
  | SFormal n -> begin
      let argExp = List.nth actuals n in
      substActFormN curStmt curInstr argExp lvalWithFormal
    end
  | _ ->
      failwith ("IntraDataflow substActForm: didn't resolve scope: "
                ^ (Lv.string_of_lval lvalWithFormal) ^ "\n")

        
(** Get the AST + CFG for the given function *)
let getFunc fKey fNode =
  try 
    let ast = !DC.astFCache#getFile fNode.defFile in
    A.setCurrentFile ast;
    getCFG fKey ast
  with FC.File_not_found fname ->
    L.logError ("getFunc/CFG: can't find " ^ fname);
    None


(************************************************************
           Relative DF Framework: The interfaces
 ************************************************************)


(** Expected interface for operations on the dataflow state *)
class type ['st] stateLattice = object

  method  stateSubset : 'st -> 'st -> bool 
    
  method  combineStates : 'st -> 'st -> 'st
    
  method  isBottom : 'st -> bool
    
  (***** Special values of the state *****)

  method  bottom : 'st
    
  method  initialState : 'st
    
  (***** Operations on summaries *****)
    
  (** Look up the summary for the given function ID. This is an input/output
      pair. Additionally, given the input, return the output. *) 
  method  summaryOutput : fKey ->  'st -> 'st

  (** Given that summary states are new (input, output), update *)
  method  updateSummary : fKey -> 'st -> 'st -> unit

  (** Hint to summary manager that summaries don't need to be 
      in memory anymore. Also, ensures that the most current summary is 
      available on disk (in case we need to reboot) *)
  method serializeAndFlush : unit

  (***** Debugging *****)
 
  (** Print the summary for the given function to the log. 
      TODO: Move this somewhere else? Hmmm, didn't supply input either? *)
  method printSummary : fKey -> unit   

  method sumTyp : BS.sumType

end



(** An extended state interface (@see stateLattice) 
    w/ additional relative state operations *)
class type ['st, 'relSt, 'part, 'key, 'info] relativeState = object
  inherit ['st] stateLattice

  (** Return the portion of the state that is relative *)
  method projRel : 'st -> 'relSt

  (** Update the given state w/ a new relative state portion *)
  method injRel : 'st -> 'relSt -> 'st

  (** Given a relative state, return the "plus" portion (facts that 
      MUST have been added since the beginning of the analyzed function) *)
  method getPlus : 'relSt -> 'part

  (** Given a relative state, return the "minus" portion (facts that 
      MAY no longer be true since the beginning of the analyzed function) *)
  method getMinus : 'relSt -> 'part 

  (** Update the given relative state w/ a new MUST fact based on the 
      ('key, 'info) pair, moving it out of the "minus" set if needed *)
  method doPlus : 'relSt ->  'key -> 'info -> 'relSt
    
  (** Update the given relative state w/ a new MAY fact based on the 
      ('key, 'info) pair, moving it out of the "plus" set if needed *)      
  method doMinus : 'relSt -> 'key -> 'info -> 'relSt

  (** The "fold" function that can act on one of the relative parts *)
  method fold_rel : 'a . ('key -> 'info -> 'a -> 'a) ->  'part -> 'a -> 'a 

  (** Hash-cons the relative state *)
  method uniq_rel : 'relSt -> 'relSt

end



(** Expected interface for the transfer function *)
class type ['st] transFunc = object

  (** Analyze assignment instructions of the form [lv := exp] *)
  method handleAssign : lval -> exp -> location -> 'st -> 'st DF.action 
    
  (** Analyze function calls of the form [lv := callexp(acts)] *)
  method handleCall : lval option -> exp -> exp list -> location -> 'st ->
    'st DF.action

  (** Analyze the return value of a function call [lv := callexp(acts)] *)
  method handleCallExp : exp -> exp list -> location -> 'st -> 'st 

  (** Analyze the call itself in a func call of form [lv := callexp(acts)] *)
  method handleCallRet : lval -> exp -> exp list -> location -> 'st ->
    'st DF.action
    
  (** Analyze inline assembly code *)
  method handleASM : attributes * string list  
    * (string * lval) list * (string * exp) list
    * string list * location -> 
    'st -> 'st DF.action
    
  (** Begin analyzing an instruction (could delegate to handleAssign, etc.) *)
  method handleInstr : instr -> 'st -> 'st DF.action 
  
  (** Analyze the guard of an if-statement *)
  method handleGuard : exp -> 'st -> 'st DF.guardaction
    
  (** Begin analyzing a statement (could delegate to handleInstr, etc.) *)
  method handleStmt : stmt -> 'st -> 'st DF.stmtaction

end


(******* The full expected input interface of state/transfer funcs *******)


(** Expected dataflow analysis interface *)
module type DFTransfer = sig

  (** Debug settings for the CIL dataflow framework *)
  val debug : bool

  (** Name of the analysis given to CIL *)
  val name : string

  (* Expose the summary manager, or somehow link the analysis name w/
     the summary type? *)

  (** The type of state tracked at each program point (possibly bottom) *)
  type st

  (** an instance of the class/interface for operations on state *)
  val stMan : st stateLattice

  (** an instance of the class/interface for transfer function operations *)
  val transF : st transFunc

(* st must not be mutable (otherwise we need to add a "copy" routine here) *)

end



(************************************************************
     Formulate analysis as flow-sensitive or insensitive 
 ************************************************************)

(** Generic intra-procedural analysis (flow-sensitive or insensitive) *)
module type IntraProcAnalysis = sig
 
  module S : DFTransfer

  (** Compute fixed-point on a control-flow graph *)
  val compute : fundec -> unit

  (** initialize everything *)
  val initialize : fundec -> S.st ->  unit

  (** return the "output" state *)
  val getOutState : unit -> S.st

end

(**************************************************
       Flow sensitive intra-proc analysis
**************************************************)


(** Functor to make a intra-procedural analysis that is flow-sensitive *)
module FlowSensitive (S:DFTransfer) : IntraProcAnalysis = struct

  module S  = S

  type state = S.st

  (** The function being analyzed *)
  let curFunc = ref dummyFunDec

  (******************************
       Logging / debugging?
  ******************************)

  (* TODO: Add inspection operations here (@see inspect.ml)? *)

  (** Given a message [m], create a message specific to this analysis *)
  let message (m:string) : string = (S.name ^ ": " ^ m)


  (**************************************************
    Interface with CIL for flow-sensitive analysis.
    (@see dataflow.ml)
  **************************************************)
  module CilTrans = struct
    
    let debug = ref S.debug

    let name = S.name

    type t = S.st

    let stmtStartData : t IH.t = IH.create 37

    let pretty () (d: t) =
      (* TODO, use this instead of own print functions for inspect? *)
      Pretty.nil

    (** initializes DF facts -- ASSUMES curFunc is set! *)
    let initStmtStartData (input: t) =
      (* Set all state to $BOTTOM, except set entry stmt state to INPUT  *)
      IH.clear stmtStartData;
      (* Assume first stmt in the list is the entry stmt *)
      match !curFunc.sallstmts with
        hd :: tl ->
          IH.add stmtStartData hd.sid input;
          List.iter (fun stmt -> 
                       IH.add stmtStartData stmt.sid S.stMan#bottom) tl
      | _ ->
          ()

    (** @return the dataflow fact at statement [s]. If it is not set,
        return $BOTTOM *)
    let getStmtData (data: t IH.t) (s: stmt) : t = 
      try IH.find data s.sid
      with Not_found -> S.stMan#bottom
    

    let computeFirstPredecessor (s: stmt) (newD: t) : t = 
      newD
        
    let combinePredecessors (s: stmt) ~(old: t) (newD: t) : t option =
      if (S.stMan#stateSubset newD old) then
        None
      else
        Some (S.stMan#combineStates old newD)

    let doInstr (i: instr) (inSt: t) =
      (* If the input state is bottom, the next state should also be bottom *)
      if (S.stMan#isBottom inSt) then
        DF.Default
      else 
        S.transF#handleInstr i inSt
          
    let filterStmt _ = 
      true

    let doGuard (gexp: Cil.exp) (d: t) =
      S.transF#handleGuard gexp d

    let doStmt (s: stmt) (d: t) = 
      S.transF#handleStmt s d

    let copy (d:t) = d

  end (* End CilTrans *)

  module CilDF = Dataflow.ForwardsDataFlow(CilTrans) 

  (******** Output functor signature *******)

  (** Prepare analysis *)
  let initialize (func:fundec) (input:state) : unit =
    curFunc := func;
    CilTrans.initStmtStartData input

  (** Run the fixed-point computation on the function [cfg] *)
  let compute (cfg:fundec) : unit =
    CilDF.compute cfg.sallstmts

  (** Return the output state for summarization *)
  let getOutState () : state  =
    List.fold_left 
      (fun curState s ->
         (* Consider Return statements *)
         match (s.skind, s.succs) with
           Return _, _ ->
             let n = CilTrans.getStmtData CilTrans.stmtStartData s in
               S.stMan#combineStates curState n
         | _, _ -> 
             curState
      ) S.stMan#bottom !curFunc.sallstmts


end


(**************************************************
   One-pass flow insensitive intra-proc analysis
**************************************************)


(** Functor to make a intra-procedural analysis that is 
    one-pass and flow-insensitive *)
module FlowInsensitive (S:DFTransfer) : IntraProcAnalysis = struct

  module S  = S
  
  type state = S.st

  let curFunc = ref dummyFunDec

  let fiState = ref S.stMan#bottom

  (** Initialize the analysis for function [func] *)
  let initialize func input =
    curFunc := func;
    fiState := input


  (** Module to manage flow-insensitive updates, delegating some tasks 
      to the DFTransfer module [S] *)
  module FIVisitor = struct

    (** Visit instruction for flow insensitive facts *)
    let visitInstr (i:instr) =
      match S.transF#handleInstr i !fiState with
        DF.Done newSt -> fiState := newSt
      | _ -> ()


    (** Visit the statement (ignore instruction blocks... those are
        handled by [visitInstr]), updating flow insensitive state *)
    let visitStmt (s: stmt) =
      match S.transF#handleStmt s !fiState with
        DF.SUse newSt -> fiState := newSt
      | _ -> ()

    (* Ignore if-statement guards *)


  end

  (** Analyze the function [cfg] *)
  let compute (cfg:fundec) : unit =
    (* Do one pass on all the statements, updating FI state *)
    List.iter (fun stmt -> FIVisitor.visitStmt stmt) cfg.sallstmts

  (** Return the output summary state *)
  let getOutState () : state =
    !fiState

end



(************************************************************
   Inter-proceduralize analysis (@see interDataflow.ml)
************************************************************)

(** Functor to glue intra-procedural analysis together w/ inter-procedural *)
module RelayDF  (Ana:IntraProcAnalysis) = struct

  module S = Ana.S

  (** Given a message [m], create a message specific to this analysis *)
  let message (m:string) : string = (S.name ^ ": " ^ m)


  (**************************************************
       Interface with Inter-procedural framework
  **************************************************)
  module RelayBUTransfer = 
  struct

    type state = S.st

    let curCG = ref FMap.empty
      
    let curSCCCG = ref IntMap.empty

    (**** Timer state ****)

    (** Timer for canceling analysis after timeout. TODO: make configurable *)
    let timerID = ref Timeout.nilTimer
      
    let initTimer () : unit =
      if (!timerID == Timeout.nilTimer) then 
        timerID := (Timeout.newTimerID ())

    exception AnalysisTimeout

    (** Time limit for analyzing one function *)
    let timeout_time = 1200.0

    let alarmHandler () =
      L.logError (message "timed out!"); 
      raise AnalysisTimeout

    let registerTimeout () =
      Timeout.set !timerID timeout_time alarmHandler

    let unregisterTimeout () =
      Timeout.cancel !timerID


    (**** Statistics kept ****)
      
    let sccsDone = ref 0
      
    let sccsTotal = ref 0

    let init (cg:simpleCallG) (sccCG: sccGraph) : unit = 
      (curCG := cg;
       curSCCCG := sccCG;
       initTimer ();
       
       (* Progress *)
       sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
       sccsDone := 0;
      )

    let updateStats (lastSCC:scc) : unit = 
      (* Update local progress. We may think we've done 25/100 SCCs but
         with other workers, it could mean 100/100 SCCs overall *)
      incr sccsDone;
      L.logStatus (message (">>> PROGRESS " ^ (string_of_int !sccsDone) ^ "/" ^
                              (string_of_int !sccsTotal) ^ " SCCs DONE!\n"))


    (**** Analyze a function ****)



    (** Compare (input, outState) w/ existing summary *)
    let checkupSummary fkey input outState : state IDF.interResult  =
      (* If the new output is bottom, it can't be newer *)
      if (not (S.stMan#isBottom outState)) then
        (* Otherwise, proceed and check against existing summary *)
        let curOut = S.stMan#summaryOutput fkey input in
        if (S.stMan#isBottom curOut) then (
          (* if summary is already bottom, new state should overwrite *)
(*          let scopedOut = RS.resolveScope outState cfg in *)
          L.logError "TODO: decide on interface for adding scope attribs";
          let scopedOut = outState in
          S.stMan#updateSummary fkey input scopedOut;
          IDF.NewOutput (input, scopedOut)
        )

        else (
          (* otherwise, see if we should try to combine the two *)
 (*          let scopedOut = RS.resolveScope outState cfg in *)
          L.logError "TODO: decide on interface for adding scope attribs";
          let scopedOut = outState in
          
          if (S.stMan#stateSubset scopedOut curOut) then
            IDF.NoChange
          else (
            let combOut = S.stMan#combineStates curOut scopedOut in
            S.stMan#updateSummary fkey input combOut;
            IDF.NewOutput (input, combOut);
          )
        )
      else
        (* New output is actually bottom... use old summary *)
        IDF.NoChange
          
          
    (** Summarize a function for ONE analysis. If this analysis depends on
        others, assume the others have run. If there is a circular 
        dependence, inter-proc. framework should run them together
        to a fixed point. *)
    let summarizeFun (cfg:fundec) (input:state) : state IDF.interResult =
      let (fn, fts) = (cfg.svar.vname,
                       Cildump.string_of_ftype cfg.svar.vtype) in
      let fkey = cfg.svar.vid in
      let ret = ref IDF.NoChange in
      (* Proceed if it's not hand-summarized *)
      ret := if (not (BS.isFinal fkey S.stMan#sumTyp)) then begin
        (* Register a time-out to skip this function if it takes too long *)
        try 
          registerTimeout ();
          Ana.initialize cfg input;

          L.logStatus (message "doing analysis");
          L.flushStatus ();
          Stat.time (message "compute:") 
            (fun () ->
               Ana.compute cfg;               
            ) ();
          unregisterTimeout (); 
          
          (* Create summary for this function *)
          let outState = Ana.getOutState () in          
          checkupSummary fkey input outState

        with AnalysisTimeout ->
          unregisterTimeout ();
          IDF.NoChange

      end else begin
        L.logStatus ("func: " ^ (string_of_fNT (fn,fts)) ^ " already done");
        IDF.NoChange
      end;
      !ret




    (** Entry-point of worklist processing (do one function) *)
    let doFunc ?(input:state = S.stMan#initialState) (fk: fKey) 
        (f:simpleCallN) : state IDF.interResult = 
      let fn, ft = f.name, f.typ in
      L.logStatus ("Summarizing function: " ^ fn ^ " : " ^ ft);
      L.logStatus "-----";
      L.flushStatus ();
      match getFunc fk f with
        Some cfg ->
          summarizeFun cfg input
      | None ->
          (* Don't have function definition *)
          L.logError ("doFunc no CFG for: " ^ (string_of_fNT (fn, ft)));
          IDF.NoChange 



    (** TRUE if the function should be put on the worklist *)
    let filterFunc f : bool =
      true

        
    (** Prepare to start an scc *)
    let sccStart (scc:scc) =
      (* First get all summaries for all callees *)
      let callees = IntSet.fold
        (fun neighSCCID curList ->
           let neighSCC = IntMap.find neighSCCID !curSCCCG in
           FSet.fold
             (fun f curList ->
                f :: curList
             ) neighSCC.scc_nodes curList
        ) scc.scc_callees [] in
      (* If funcs in own scc are already done, ignore. It's unlikely. 
         Do all of scc to prevent useless checking for non-existent files. *)
      L.logStatus "Acquiring needed summaries";
      L.flushStatus ();
(*
      Manage_sums.prepareSumms callees fSummaries
*) ()

    (* 
       TODO: Only need to DL summaries of this pass and previous passes.
       Don't want to try dl'ing too many unneccessary files (increases
       net traffic if each summary is a separate file?). 
       Encode dependencies between passes or just merge summary files? 
       Seems like the interDataflow.ml dude should do this step...
    *)


    (** Notification that an SCC is now fix-pointed. 
        @return a list of locations of summaries written to disk *) 
    let sccDone (scc:scc) byThisGuy =
      let summPaths = 
        (try (* Debugging *)
           FSet.iter 
             (fun fkey ->
                L.logStatus ("Summary for function: " ^ (string_of_fkey fkey));
                L.logStatus ("This is from IntraDataflow.  REMOVE THIS MSG");
                L.logStatus "=======\n";
                S.stMan#printSummary fkey
             ) scc.scc_nodes;
           
           (* Serialize and return record of where each fun was placed. *)
           S.stMan#serializeAndFlush;
           let tokenMap = [] in
             
           (* Notify others that the functions are now summarized *)
           List.fold_left
             (fun paths (fkey, tok) ->
                if (FSet.mem fkey scc.scc_nodes) then
                  let path = BS.pathFromToken tok in
                  (* BS.setDone fkey;  * TODO: only done for this pass *)
                  (fkey, path) :: paths
                else paths
             ) [] tokenMap
         with e ->
           L.logError ("Caught exception in sccDone?" ^ (Printexc.to_string e));
           []
        ) 
      in
      updateStats scc;
      summPaths

  end  (* End RelayBUTransfer *)
  module BUDF = IDF.BottomUpDataflow (RelayBUTransfer)

end



(* TODO: Add some method to initialize/finalize with the server?
   Reset work queue... etc... Do that in interDataflow.ml *)



(************************************************************
          Simple implementations of the interface
************************************************************)



(********** Simple, partially-defined, relative state managers **********)
  

(** Input module for extending a stateLattic / manager with relative
    state operations based on [Relative_set] *)
module type ComposedRel = sig

  type st

  type relSt (* How to enforce that relSt is a relSet? *)

  val projRel : st -> relSt

  val injRel : st -> relSt -> st

  (** The class [c] has basic operations on the state. [c] be extended 
      with relative state operations by the next module 
      (@see MakeRelStateManager). *)
  class c : [st] stateLattice

end

(** Functor for creating an extended version of stateLattice with
    the additional operations that make it a relativeState *)
module MakeRelStateManager (R:Relative_set.S) (C:ComposedRel) = struct

  type st = C.st

  type relSt = R.relSet

  type info = R.value

  class rc = object
    inherit C.c

    (***** Operations for relative state *****)
    method projRel = C.projRel
     
    method injRel = C.injRel

    method getPlus = R.getPlus

    method getMinus = R.getMinus

    method doPlus = R.doPlus
      
    method doMinus = R.doMinus

    method fold_rel : 'a . (R.key -> info -> 'a -> 'a) ->  info R.S.t -> 'a -> 'a = 
      R.S.fold

    method uniq_rel = R.unique

  end

end




(*************** Simple transfer functions ***************)


(** Dataflow functions where each transfer func is the identity func *)
class ['state] idTransFunc stMan = object (self) 

  (** the statement currently analyzed *)
  val mutable curStmt = Cil.dummyStmt
    
  (** the instruction currently analyzed *)
  val mutable curInstr = Cil.dummyInstr

  method handleAssign lv exp loc inState =
    DF.Default
    
  method handleCallExp callexp acts loc (inState:'state) =
    inState

  method handleCallRet lv callexp acts loc (inState:'state) : 'state DF.action =
    DF.Default

      
  method handleCall retOpt callexp acts loc inState =
    let newstate = self#handleCallExp callexp acts loc inState in
    if (stMan#isBottom newstate) then
      DF.Done newstate
    else match retOpt with
      Some lv -> self#handleCallRet lv callexp acts loc newstate
    | None    -> DF.Done newstate
        
  method handleASM (atts, templs, cos, cis, clobs, loc) inState =
    DF.Default

  method handleInstr (i:Cil.instr) (inState:'state) : 'state DF.action =
    curInstr <- i;
    match i with 
      Set (lval, rhs, loc) ->
        self#handleAssign lval rhs loc inState
    | Call (retval_option, callexp, actuals, loc) ->
        self#handleCall retval_option callexp actuals loc inState
    | Asm  (attrs,  templates, con_out_list, con_in_list, clobbered, loc) -> 
        self#handleASM (attrs, templates, con_out_list, 
                        con_in_list, clobbered, loc) inState


  (** Given a guard and an input state, return the new state for 
      this branch or None if no change is needed *)
  method handleGuard (gexp: Cil.exp) (inState:'state) : 'state DF.guardaction =
    DF.GDefault

  method handleStmt (stmt: Cil.stmt) (inState:'state) : 'state DF.stmtaction = 
    curStmt <- stmt;
    DF.SDefault
    
end
  
  
(** Basic class providing a transfer function for handling function calls
    with relative state. Assumes the elements (keys) of the 
    relative sets are {!Lv.aLval} (it is restricted by the interaction
    with Symstate2 for substitution) *)
class ['state, 'relSt, 'part, 'info] relTransFunc 
  (stMan : ('state, 'relSt, 'part, Lv.aLval, 'info) relativeState) = object (self) 
    inherit ['state] idTransFunc stMan
    inherit Scope.scoper
      
    (* TODO: see if we can hide relState info types? *)
      
      
    (** Apply one update on [curState] based on a "plus" element of the 
        function summary for the call at the given [callSite]. The summary
        says that [lv] and [info] are in the final "plus" set *)
    method doPlus actuals callSite 
      lv (info: 'info) (curState : 'relSt) : 'relSt =
      let scope = self#getScope lv in
      let mustAlias, subbedLvs = 
        substActForm curStmt curInstr actuals lv scope in
      (* Only update if mustAlias... how to generalize soundness requirement? *)
      if (mustAlias) then (
        (if (List.length subbedLvs > 1) 
         then L.logError ("RS: summary lock maps to multiple @ " ^ 
                            (Du.string_of_loc callSite))
        );
        List.fold_left 
          (fun curState subbedLv ->
             stMan#doPlus curState subbedLv info
          ) curState subbedLvs )
      else
        curState
          
    (** Apply one update on [curState] based on a "minus" element from
        the function summary for the call at the given [callSite]. *)
    method doMinus actuals callSite 
      lv (info:'info) (curState : 'relSt) : 'relSt =
      let scope = self#getScope lv in
      let mustAlias, subbedLvs = 
        substActForm curStmt curInstr actuals lv scope in
      (* Always update, even if not a must alias relationship *)
      List.fold_left 
        (fun curState subbedLv ->
           stMan#doMinus curState subbedLv info
        ) curState subbedLvs
        
    (** Apply all updates to the current relative state [curState] based
        one a function summary for the call at the given [callSite] *)
    method applyRelSum actuals callSite diffSum (curState : 'relSt) : 'relSt =
      let applyPlus = self#doPlus actuals callSite in
      let applyMinus = self#doMinus actuals callSite in
      let p = 
        stMan#fold_rel applyPlus (stMan#getPlus diffSum) curState in
      let pm = 
        stMan#fold_rel applyMinus (stMan#getMinus diffSum) p in
      (* Finally, hash-cons the result *)
      let finalSt = stMan#uniq_rel pm in
      finalSt
        
        
    method applySum actuals loc (sumState : 'st) (inState : 'st) : 'st =
      (* Apply the summary only to the relative part of the state *)
      let relSum = stMan#projRel sumState in
      let relState = stMan#projRel inState in
      let newRel = self#applyRelSum actuals loc relSum relState in
      stMan#injRel inState newRel
        
    (** Partially handle function calls by only updating the relative state *)
    method handleCallExp callexp actuals loc (inState : 'st) =
      let funs = A.funsForCall callexp in
      if (List.length funs = 0) then
        (L.logError ("handleCall: funptr resolved to 0 fun(s): " ^
                                (Du.string_of_exp callexp));
         inState)    
      else
        (* Handle each of the called functions (may be >1 if based
           on a function pointer) by merging the outputs of each call *)
        (List.fold_left 
           (fun curState fkey ->
              (* Find the output state for this call by assuming 
                 each funptr call started with inState *)
              let newState =
                let sumState = stMan#summaryOutput fkey inState in
                if (stMan#isBottom sumState) then
                  stMan#bottom
                else
                  self#applySum actuals loc sumState inState
              in (* Summary should not be Not_found *)

              (* Finally, merge outcome with the rest of the results *)
              Stat.time "LS combineStates"
                (stMan#combineStates curState) newState
           ) stMan#bottom funs)

  end
  
