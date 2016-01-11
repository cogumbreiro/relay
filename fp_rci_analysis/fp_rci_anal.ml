
(** Interprocedural bits for function pointer analysis *)

open Cil
open Callg
open Fstructs
open Fp_rci_types
open Fp_rci_store
open Fp_rci_intraproc
open Fp_rci_summary
open Summary_keys

(************************************************************
    Packing up for Interprocedural dataflow
************************************************************)

module FPTopDown = struct
  
  type dfInfo = fpState
  let compareDfInfo a b = compareStates a b
  let isBottom x = isBottomState x
  let printDfInfo x = printState x

  type funID = sumKey
  let funIDToSumKey sumk = sumk
  let compareFunID sumk1 sumk2 = compareSumKey sumk1 sumk2
  let fkeyOfID sumk = fkey_of_sumKey sumk


  type returnPoint = funID * prog_point
  type calleeFlow = funID * dfInfo
      
  let combinePredecessors curFun oldSt newSt =
    let combo = Fp_rci_hashcons.hc_state 
      (lattice#combineStates curFun oldSt newSt) in
    if lattice#eqStates oldSt combo then None
    else Some combo

  let diffDfInfo oldSt newSt =
    Fp_rci_store.diffState oldSt newSt

  let sumType = "fp"

  let setFunc cfg (funID, initialDfInfo) = 
    transF#handleFunc cfg (funID, initialDfInfo)

  let doAssign lv rhs loc st =
    Stat.time "doAssign" (transF#handleAssign lv rhs loc) st

  let doASM atts templs cos cis clobs loc st =
    transF#handleASM atts templs cos cis clobs loc st
      
  let doReturn retOpt st =
    Stat.time "doReturn" (transF#handleReturnStmt retOpt) st

  let doGuard exp st =
    Stat.time "doGuard" (transF#handleGuard exp) st

  let doStmt stmt st =
    transF#handleStmt stmt st

  let doCall lvOpt ce args loc st =
    Stat.time "doCall" (transF#handleCall lvOpt ce args loc) st

  let initWork rootDir addrTk cg =
    initNfpOutData rootDir;
    initGAddrTaken rootDir;
    transF#setFuncs cg;
    Fp_rci_initial.getInitialStates rootDir addrTk cg

  let moreWork cg touchedFuncs =
    Fp_rci_initial.getMoreStates cg touchedFuncs

  let checkRestart funID st =
    (st, false)

  let cacheSize = Fp_rci_summary.cacheSize

end

module InterDF = RciDataflow.RciDataflow(FPTopDown)

let initSettings settings cg =
  ()

