
(** Interprocedural bits for function pointer analysis *)

open Cil
open Callg
open Fstructs
open Fp_types
open Fp_store
open Fp_intraproc
open Fp_summary
open Summary_keys

(************************************************************
    Packing up for Interprocedural dataflow
************************************************************)

module FPTopDown = struct
  
  type state = fpState
  let compareState a b = compareStates a b
  let isBottom x = isBottomState x
  let printState x = printState x

  type funID = sumKey
  let funIDToSumKey sumk = sumk
  let compareFunID sumk1 sumk2 =
    Pervasives.compare sumk1 sumk2
  let fkeyOfID sumk = fkey_of_sumKey sumk


  type returnPoint = funID * prog_point
  type calleeFlow = funID * state
      
  let combinePredecessors oldSt newSt =
    let combo = Fp_hashcons.hc_state (lattice#combineStates oldSt newSt) in
    if lattice#eqStates oldSt combo then None
    else Some combo

  let diffState oldSt newSt =
    Fp_store.diffState oldSt newSt

  let sumType = "fp"
  let transFunc = transF

  let setFunc cfg (fk, inSt) = 
    transFunc#handleFunc cfg (fk, inSt)

  let doAssign lv rhs loc st =
    Stat.time "doAssign" (transFunc#handleAssign lv rhs loc) st

  let doASM atts templs cos cis clobs loc st =
    transFunc#handleASM atts templs cos cis clobs loc st
      
  let doReturn stmt st =
    Stat.time "doReturn" (transFunc#handleReturnStmt stmt) st

  let doGuard exp st =
    Stat.time "doGuard" (transFunc#handleGuard exp) st

  let doStmt stmt st =
    transFunc#handleStmt stmt st

  let doCall lvOpt ce args loc st =
    Stat.time "doCall" (transFunc#handleCall lvOpt ce args loc) st

  let initWork rootDir addrTk cg =
    initNfpOutData rootDir;
    Init_visit.initVoidMap rootDir;
    transFunc#setFuncs cg;
    Init_visit.getInitialStates rootDir addrTk cg

  let moreWork cg touchedFuncs =
    Init_visit.getMoreStates cg touchedFuncs

  let checkRestart funID st =
    Fp_context_lim.limitRestarts funID st

end

module InterDF = TopdownDataflow.TopDownDataflow(FPTopDown)

let initSettings settings cg =
  ()

