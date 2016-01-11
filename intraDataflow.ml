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
*)



open Cil
open Callg
open Scc_cg
open Fstructs

module IH = Inthash
module A = Alias
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


(** Get the AST + CFG for the given function *)
let getFunc fKey fNode : fundec option =
  try 
    let ast = !DC.astFCache#getFile fNode.defFile in
    A.setCurrentFile ast;
    Cilinfos.getCFG fKey ast
  with FC.File_not_found fname ->
    L.logError ("getFunc/CFG: can't find " ^ fname);
    None


(************************************************************
           Relative DF Framework: The interfaces
 ************************************************************)


(** Expected interface for operations on the dataflow state *)
class type ['st, 'sum] stateLattice = object

  method  stateSubset : 'st -> 'st -> bool 
    
  method  combineStates : 'st -> 'st -> 'st
    
  method  isBottom : 'st -> bool
    
  (***** Special values of the state *****)

  method  bottom : 'st
    
  method  initialState : 'st
    
  (***** Debugging *****)
 
  method printState : 'st -> unit

  (***** Operations on summaries *****)
 
  (** Print the summary for the given function to the log. 
      TODO: Move this somewhere else? Hmmm, didn't supply input either? *)
  method printSummary : fKey -> unit   

  (* rkc: hack alert, to allow Radar to plug in seq or adj summary database *)
  (* val mutable sumtyp : BS.sumType *)
  (* method setSumTyp : BS.sumType -> unit *)
  val mutable thesums : 'sum BS.base
  method setTheSums : 'sum BS.base -> unit
  method sums : 'sum BS.base

end


(** A general relative state interface *)
class type ['st, 'relSt, 'part, 'sum] genRelState = object
  inherit ['st, 'sum] stateLattice

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
  
  (** Hash-cons the relative state *)
  method uniq_rel : 'relSt -> 'relSt

end


(** An extended state interface (@see stateLattice) 
    w/ additional relative state operations *)
class type ['st, 'relSt, 'part, 'key, 'info, 'sum] relativeState = object
  inherit ['st, 'relSt, 'part, 'sum] genRelState

  (** Update the given relative state w/ a new MUST fact based on the 
      ('key, 'info) pair, moving it out of the "minus" set if needed *)
  method doPlus : 'relSt ->  'key -> 'info -> 'relSt
    
  (** Update the given relative state w/ a new MAY fact based on the 
      ('key, 'info) pair, moving it out of the "plus" set if needed *)      
  method doMinus : 'relSt -> 'key -> 'info -> 'relSt

  (** The "fold" function that can act on one of the relative parts *)
  method fold_rel : 'a . ('key -> 'info -> 'a -> 'a) ->  'part -> 'a -> 'a 

  (* rkc: Exposing some more operations from MapSet *)
  (*method mem : 'a . ('key -> 'part -> bool)*)
  method mem : 'key -> 'part -> bool
  (*method cardinal : 'a . ('part -> int)*)
  method cardinal : 'part -> int
  (*method find : 'a . ('key -> 'part -> 'a)*)
  method find : 'key -> 'part -> 'info
  (*method iter : 'a . (('key -> 'a -> unit) -> 'part -> unit)*)
  method iter : ('key -> 'info -> unit) -> 'part -> unit

end



(** Expected interface for the transfer function *)
class type ['st] transFunc = object

  method curFunc : fundec

  (** Set up transfer func to analyze the given function *)
  method handleFunc : fundec -> unit

  (** Analyze assignment instructions of the form [lv := exp] *)
  method handleAssign : lval -> exp -> location -> 'st -> 'st DF.action 
    
  (** Analyze function calls of the form [lv := callexp(acts)] *)
  method handleCall : lval option -> exp -> exp list -> location -> 'st ->
    'st DF.action

  (** Analyze the call itself in a func call of form [lv := callexp(acts)] *)
  method handleCallExp : exp -> exp list -> location -> 'st -> 'st 

  (** Analyze the return value of a function call [lv := callexp(acts)] *)
  method handleCallRet : lval -> exp -> exp list -> location -> 'st -> 'st 
    
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
  val debug : bool (* TODO: make this a ref so that it can be toggled? *)

  (** Name of the analysis given to CIL *)
  val name : string

  (** The type of state tracked at each program point (possibly bottom) *)
  type st

  type sum

  (** an instance of the class/interface for operations on state *)
  val stMan : (st, sum) stateLattice

  (* TODO have the summary manager here instead of part of the state latt *)

  (** an instance of the class/interface for transfer function operations *)
  val transF : st transFunc ref
  (* Ravi: changed to reference to allow two flow functions for ADJ phase *)

(* st must not be mutable (otherwise we need to add a "copy" routine here) *)

end



(************************************************************
     Formulate analysis as flow-sensitive or insensitive 
 ************************************************************)


(** Generic intra-procedural analysis (flow-sensitive or insensitive) *)
module type IntraProcAnalysis = sig
 
  module T : DFTransfer

  (** Compute fixed-point on a control-flow graph *)
  val compute : fundec -> unit

  (** initialize everything *)
  val initialize : fundec -> T.st ->  unit

  (** return the "output" state *)
  val getOutState : unit -> T.st

  (* Ravi: exposing this for when initialize needs to get extended *)
  val curFunc : fundec ref

  (* TODO determine if all of these need to be exposed *)
  val getDataBefore : prog_point -> T.st
  val getDataAfter : prog_point -> T.st
  val setDataBefore : prog_point -> T.st -> unit
  val setDataAfter : prog_point -> T.st -> unit

  (* FOR DEBUGGING *)
  val sizeOfState : unit -> int

end
  (* TODO: make this functor return a class in the form of
     Analysis_dep.analysis instead *)


(**************************************************
       Flow sensitive intra-proc analysis
**************************************************)


(** Functor to make a intra-procedural analysis that is flow-sensitive *)
module FlowSensitive (S:DFTransfer) : IntraProcAnalysis with type T.st  = S.st = struct

  module T = S

  type state = T.st

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
        !S.transF#handleInstr i inSt
          
    let filterStmt _ = 
      true

    let doGuard (gexp: Cil.exp) (d: t) =
      !S.transF#handleGuard gexp d

    let doStmt (s: stmt) (d: t) = 
      !S.transF#handleStmt s d

    let copy (d:t) = d

  end (* End CilTrans *)

  module CilDF = Dataflow.ForwardsDataFlow(CilTrans) 

  (******** Output functor signature *******)

  (** Prepare analysis *)
  let initialize (func:fundec) (input:state) : unit =
    curFunc := func;
    CilTrans.initStmtStartData input;
    !S.transF#handleFunc func


  (** @return the dataflow fact at the given prog_point. 
      If it is not set, return $BOTTOM *)
  let getDataBefore (pp:prog_point) : state = 
    CilDF.getDataBefore pp

  let getDataAfter pp =
    CilDF.getDataAfter pp

  (** Replace the state at the given prog_point *)
  let setDataBefore (pp:prog_point) (s:state) : unit =
    CilDF.setDataBefore pp s

  let setDataAfter pp s =
    CilDF.setDataAfter pp s

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

  let sizeOfState () =
    Osize.size_kb CilTrans.stmtStartData

end


(**************************************************
   One-pass flow insensitive intra-proc analysis
**************************************************)


(** Functor to make a intra-procedural analysis that is 
    one-pass and flow-insensitive *)
module FlowInsensitive (S:DFTransfer) : IntraProcAnalysis with type T.st=S.st = struct

  module T = S
  
  type state = T.st

  let curFunc = ref dummyFunDec

  let fiState = ref S.stMan#bottom

  (** Initialize the analysis for function [func] *)
  let initialize func input =
    curFunc := func;
    fiState := input;
    (* Initialize transfer func *)
    !S.transF#handleFunc func


  (** Class to manage flow-insensitive updates, delegating some tasks 
      to the DFTransfer module [S] *)
  class fiVisitor = object (self)
    inherit Pp_visitor.ppVisitor

    method vinst (i:instr) =
      self#setInstrPP i;

      (match !S.transF#handleInstr i !fiState with
         DF.Done newSt -> fiState := newSt
       | _ -> ());

      self#bumpInstr 1;
      DoChildren

    method vstmt (s: stmt) =
      self#setStmtPP s;
      (* First pass on stmt *)
      (match !S.transF#handleStmt s !fiState with
         DF.SUse newSt -> fiState := newSt
       | _ -> ()
      );
      (* Let visitor continue w/ instructions (if stmt is Instr block) *)
      DoChildren

  end

  (** Inspect data that's valid at a program point. 
      For the whole function, in this case *)
  let getDataBefore _ = !fiState
  let getDataAfter _ = !fiState

  (** Replace the data at a program point. For the whole function,
      in this case. *)
  let setDataBefore _ s = 
    fiState := s
  let setDataAfter _ s =
    fiState := s

  (* TODO: make FIVisitor start from the entry statement and 
     only visit reachable statements (according to the CFG)! *)

  let visitor = new fiVisitor

  (** Analyze the function [cfg] *)
  let compute (cfg:fundec) : unit =
    (* Do one pass on all the statements, updating FI state *)
    ignore (Cil.visitCilFunction (visitor :> cilVisitor) cfg)

  (** Return the output summary state *)
  let getOutState () : state =
    !fiState

  let sizeOfState () =
    Osize.size_kb !fiState

end


(************************************************************
   Inter-proceduralize analysis (@see interDataflow.ml)
************************************************************)

(**** Must initialize the Call graph references first... ****)

let curCG = ref FMap.empty

let (curSCCCG : sccGraph ref) = ref IntMap.empty

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
  raise AnalysisTimeout
    
let registerTimeout () =
  Timeout.set !timerID timeout_time alarmHandler
    
let unregisterTimeout () =
  Timeout.cancel !timerID

(************************************************************
 Useful bits for running different analyses in sequence
************************************************************)

(** Get an old summary and compare it w/ a new one. Combine,
    annotate scopes, and replace old summary if it's newer *)
let checkupSummary fkey cfg newSummary getOld isBottom subset combine 
    scopeIt replace inspect printSum : bool =
  (* Check if the new output state offers new information *)
  if not (isBottom newSummary) then
    (* If state is already bottom, combining won't do anything.
       Otherwise, proceed and check against existing summary   *)
    let curSummary = getOld fkey in
    if isBottom curSummary then begin
      (* if summary is already bottom, new state should overwrite *)
      let scopedOut = scopeIt cfg newSummary in
      replace fkey scopedOut;
      if inspect then
        (L.logStatus "Old summ is BOTTOM, replacing w/:";
         printSum scopedOut;
        );
        true
    end
    else
      (* otherwise, try to combine the two *)
      let scopedOut = scopeIt cfg newSummary in
      if subset scopedOut curSummary then
        false
      else begin
        if inspect then (
          L.logStatus "Trying to combine outState and curSummary";
          L.logStatus "scoped outState";
          printSum scopedOut;
          L.logStatus "\n\ncurSumOut";
          printSum curSummary;
        );
        let combOut = combine curSummary scopedOut in
        if inspect then (
          L.logStatus "Finished combining (not printing result)";
        );
        replace fkey combOut;
        true
      end
  else
    let _ = getOld fkey in (* just to make sure it's on disk *)
    false

module AD = Analysis_dep

(** Do the intra-procedural analyses that need fixpointing (on one func).
    @return true if one of the summaries is new. *)
let runFixpoint (needsFixpoint:AD.analysis list) (cfg:fundec) : bool = 
  let (fn, ft) = (cfg.svar.vname,
                  Cildump.string_of_ftype cfg.svar.vtype) in
  let fkey = cfg.svar.vid in
  let inspect = Inspect.inspector#mem fn in
  List.fold_left
    (fun changed analysis ->
       try
         registerTimeout ();
         
         analysis#setInspect inspect; 
         if not (analysis#isFinal fkey) then
           analysis#compute cfg
         ;
         (* NOTE: assume summarize checks isFinal also... but why bother? *)
         let result = analysis#summarize fkey cfg in
         
         unregisterTimeout ();
         changed || result
       with AnalysisTimeout ->
         unregisterTimeout ();
         L.logError "timed out!";  
         (* TODO: identify the analysis that timed out *)
         changed
    ) false needsFixpoint



(** Do the intra-procedural analyses that don't need fixpointing (on scc) *)
let runNonFixpoint (nonFixpoint:AD.analysis list) (pre_req:AD.analysis list) 
    (prevFkey:fKey) (scc:scc) : unit =
  
  let runOn fkey cfg =
    List.iter 
      (fun analysis ->
         if not (analysis#isFinal fkey) then
           analysis#compute cfg
         ;
         let changed = analysis#summarize fkey cfg in
         if changed then L.logError "runNonFixpoint: Why did summary change?"
      ) nonFixpoint
  in
  
  let fillDependencies fkey cfg =
    List.iter 
      (fun analysis ->
         if not (analysis#isFinal fkey) then begin
           analysis#compute cfg;
           analysis#flushSummaries ();
         end
      ) pre_req
  in
  
  let getCFGAndDo fkey foo =
    (try 
       let fnode = FMap.find fkey !curCG in
       (match getFunc fkey fnode with
          Some cfg -> foo fkey cfg
        | None -> ()
       )
     with Not_found -> 
       L.logError ("intraDataflow: runNonFixpoint can't find CFG for " ^ 
                     (string_of_fkey prevFkey));
    )
  in
    

  (* First run the dude that already had its dataflow tables filled *)
  getCFGAndDo prevFkey runOn;
  
  (* Then run the rest *)
  let theRest = (FSet.remove prevFkey scc.scc_nodes) in
  FSet.iter (fun fk -> 
               getCFGAndDo fk 
                 (fun fk cfg -> 
                    fillDependencies fk cfg;
                    runOn fk cfg)
            ) theRest
    


(* TODO: Add some method to initialize/finalize with the server?
   Reset work queue... etc... Do that in interDataflow.ml *)



(************************************************************
          Simple implementations of the interface
************************************************************)




(*************** Simple transfer functions ***************)


(** Dataflow functions where each transfer func is the identity func *)
class ['state] idTransFunc (stMan : ('state, 'b) stateLattice) = object (self) 


  (** the function currently analyzed *)
  val mutable curFunc = Cil.dummyFunDec

  method curFunc = curFunc

  method handleFunc func =
    curFunc <- func

  method handleAssign lv exp loc inState =
    DF.Default
    
  method handleCallExp callexp acts loc (inState:'state) : 'state =
    inState

  method handleCallRet lv callexp acts loc (inState:'state) : 'state =
    inState

      
  method handleCall retOpt callexp acts loc inState =
    let newstate = self#handleCallExp callexp acts loc inState in
    if (stMan#isBottom newstate) then
      DF.Done newstate
    else match retOpt with
      Some lv -> DF.Done (self#handleCallRet lv callexp acts loc newstate)
    | None    -> DF.Done newstate


  method handleASM (atts, templs, cos, cis, clobs, loc) inState =
    DF.Default

  method handleInstr (i:Cil.instr) (inState:'state) : 'state DF.action =
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
    DF.SDefault
    
end


(** Gadget with inspection capabilities for mixin in *)
class ['st] inspectorGadget 
  (stMan : ('st, 'sum) stateLattice) analysisName = object (self)
  
  val mutable inspect = false
  val name = analysisName
    
  val mutable prevState = stMan#bottom

  (* TODO: just provide a full wrapper around doInstr and doStmt
     that calls the xxxBefore and xxxAfter routines at the right time *)

  method setInspect yesno = 
    if yesno then
      L.logStatusF "inspectorGadget inspecting!\n"
    ;
    inspect <- yesno

  method inspect = inspect

  method inspectInstBefore i st =
    if inspect then 
      (L.logStatus (name ^ " before instr: ");
       L.logStatus ((Du.string_of_instr i));
       stMan#printState st;
       prevState <- st
      )
   
  (* TODO: actually compare state to see if it changed instead of
   * trusting that the return values indicate change / no change *)
  method inspectInstAfter (i:instr) (result : 'st DF.action) =
    if inspect then
      (match result with
         DF.Default -> 
           L.logStatus (name ^ " after instr: No change\n")
       | DF.Done newSt -> 
           if stMan#stateSubset newSt prevState then
             L.logStatus (name ^ " after instr: No change\n")
           else begin
             L.logStatus (name ^ " after instr: \n");
             stMan#printState newSt
           end
       | DF.Post f -> 
           L.logStatus (name ^ " after instr: Post (not eval'ed)\n");
      )
    

  method inspectStmtBefore s st =
    if inspect then 
      (L.logStatus (name ^ " before stmt: ");
       L.logStatus ((Du.string_of_stmt s));
       stMan#printState st;
       prevState <- st
      )

  method inspectStmtAfter (s:stmt) (result: 'st DF.stmtaction) =
    if inspect then
      (match result with
         DF.SDefault -> 
           L.logStatus (name ^ " after stmt: No change\n")
       | DF.SDone -> 
           L.logStatus (name ^ " after stmt: DONE/STOP\n");
       | DF.SUse newSt ->
           if stMan#stateSubset newSt prevState then
             L.logStatus (name ^ " after stmt: No change\n")
           else begin
             L.logStatus (name ^ " after stmt: \n");
             stMan#printState newSt
           end
      )

end


(**************** Simple summary manager **************)

class type ['sum, 'st] summarizer = object

  (** Make the summary, and possibly update the database. 
      Assume implementation only needs to be able query state.
      Return true if the new summary has new information *)
  method summarize : fKey -> fundec -> (prog_point -> 'st) -> bool

  method scopeIt : fundec -> 'sum -> 'sum

  method setInspect : bool -> unit

  method flushSummaries : unit -> unit

end

class ['st, 'sum] summaryIsState (stMan : ('st, 'sum) stateLattice) = object
  constraint 'st = 'sum

  method private isBottomSummary = stMan#isBottom

  method private sumSubset = stMan#stateSubset

  method private combineSummaries = stMan#combineStates

  method private printSummary = stMan#printState 

end

(** Partial summary manager where the summary is just the state 
    at the return statements *)
class virtual ['st, 'sum] summaryIsOutput stMan (sums : 'sum BS.base) = 
object (self)
  constraint 'st = 'sum
  inherit ['st, 'sum] summaryIsState stMan

  val mutable inspect = false

  method setInspect yes =
    inspect <- yes

  method virtual scopeIt : fundec -> 'sum -> 'sum 

  (** Return as summary, the merged state from each return stmt,
      given the cfg and the mapping from statement to state. *)
  method private makeSummary (cfg:fundec) (getState : prog_point -> 'st) : 'sum =
    List.fold_left 
      (fun curState s ->
         (* Consider Return statements *)
         match (s.skind, s.succs) with
           Return _, _ ->
             let pp = getStmtPP s in
             let n = getState pp in
             stMan#combineStates curState n
         | _, _ -> 
             curState
      ) stMan#bottom cfg.sallstmts


  (** Get the possibly-new output and update the summary *)
  method summarize (fk : fKey) cfg (getState : prog_point -> 'st) =
    let newSummary = self#makeSummary cfg getState in
    let replace fk newSum =
      sums#addReplace fk newSum;
      sums#flushOne fk;
(*    AU.sums#evictSummaries  *) (* TODO have other summaries evicted elsewhere? *)
    in
    checkupSummary
      fk
      cfg
      newSummary
      sums#find
      self#isBottomSummary
      self#sumSubset
      self#combineSummaries
      self#scopeIt
      replace
      inspect
      self#printSummary

  method flushSummaries () : unit =
    sums#serializeAndFlush  
      
end


(** Partial summary manager where the summary is just the 
    flow-insensitive state *)
class ['st, 'sum] summaryIsFIS stMan = object
  constraint 'st = 'sum
  inherit ['st, 'sum] summaryIsState stMan

  (** Return as summary the flow insensitive state, given the cfg
      and the mapping from any statement to that FI state *)
  method makeSummary (_:fundec) (getState : stmt -> instr -> 'st) : 'sum =
    getState dummyStmt dummyInstr

 
end

