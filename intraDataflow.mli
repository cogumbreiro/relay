


val getFunc : int -> Callg.simpleCallN -> Cil.fundec option

class type ['st, 'sum] stateLattice =
  object
    method bottom : 'st
    method combineStates : 'st -> 'st -> 'st
    method initialState : 'st
    method isBottom : 'st -> bool
    method printState : 'st -> unit
    method printSummary : Fstructs.fKey -> unit
    method setTheSums : 'sum Backed_summary.base -> unit
    method stateSubset : 'st -> 'st -> bool
    method sums : 'sum Backed_summary.base
  end

class type ['st, 'relSt, 'part, 'sum] genRelState =
object inherit ['st, 'sum] stateLattice
    method getMinus : 'relSt -> 'part
    method getPlus : 'relSt -> 'part
    method injRel : 'st -> 'relSt -> 'st
    method projRel : 'st -> 'relSt
    method uniq_rel : 'relSt -> 'relSt
  end

class type ['st, 'relSt, 'part, 'key, 'info, 'sum] relativeState =
object inherit ['st, 'relSt, 'part, 'sum] genRelState
    method cardinal : 'part -> int
    method combineStates : 'st -> 'st -> 'st
    method doMinus : 'relSt -> 'key -> 'info -> 'relSt
    method doPlus : 'relSt -> 'key -> 'info -> 'relSt
    method find : 'key -> 'part -> 'info
    method fold_rel : ('key -> 'info -> 'g -> 'g) -> 'part -> 'g -> 'g
    method iter : ('key -> 'info -> unit) -> 'part -> unit
    method mem : 'key -> 'part -> bool
  end

class type ['a] transFunc =
  object

    method curFunc : Cil.fundec

    method handleASM :
      Cil.attributes * string list * (string * Cil.lval) list *
      (string * Cil.exp) list * string list * Cil.location ->
      'a -> 'a Dataflow.action
    method handleAssign :
      Cil.lval -> Cil.exp -> Cil.location -> 'a -> 'a Dataflow.action
    method handleCall :
      Cil.lval option ->
      Cil.exp -> Cil.exp list -> Cil.location -> 'a -> 'a Dataflow.action
    method handleCallExp :
      Cil.exp -> Cil.exp list -> Cil.location -> 'a -> 'a
    method handleCallRet :
      Cil.lval -> Cil.exp -> Cil.exp list -> Cil.location -> 'a -> 'a
    method handleFunc : Cil.fundec -> unit
    method handleGuard : Cil.exp -> 'a -> 'a Dataflow.guardaction
    method handleInstr : Cil.instr -> 'a -> 'a Dataflow.action
    method handleStmt : Cil.stmt -> 'a -> 'a Dataflow.stmtaction
  end

module type DFTransfer =
  sig
    val debug : bool
    val name : string
    type st
    type sum
    val stMan : (st, sum) stateLattice
    val transF : st transFunc ref
  end

module type IntraProcAnalysis =
  sig
    module T : DFTransfer
    val compute : Cil.fundec -> unit
    val initialize : Cil.fundec -> T.st -> unit
    val getOutState : unit -> T.st
    val curFunc : Cil.fundec ref
    val getDataBefore : Cil.prog_point -> T.st
    val getDataAfter : Cil.prog_point -> T.st
    val setDataBefore : Cil.prog_point -> T.st -> unit
    val setDataAfter : Cil.prog_point -> T.st -> unit
    val sizeOfState : unit -> int
  end

module FlowSensitive :
  functor (S : DFTransfer) -> IntraProcAnalysis with type T.st = S.st

module FlowInsensitive :
  functor (S : DFTransfer) -> IntraProcAnalysis with type T.st = S.st

val curCG : Callg.simpleCallN Fstructs.FMap.t ref
val curSCCCG : Scc_cg.sccGraph ref

val checkupSummary :
  'a ->
  'b ->
  'c ->
  ('a -> 'c) ->
  ('c -> bool) ->
  ('c -> 'c -> bool) ->
  ('c -> 'c -> 'c) ->
  ('b -> 'c -> 'c) -> ('a -> 'c -> 'd) -> bool -> ('c -> unit) -> bool


val runFixpoint : Analysis_dep.analysis list -> Cil.fundec -> bool

val runNonFixpoint :
  Analysis_dep.analysis list -> Analysis_dep.analysis list -> 
  Fstructs.fKey -> Scc_cg.scc -> unit


class ['a] idTransFunc : ('a, 'sum) stateLattice -> ['a] transFunc

class ['a] inspectorGadget : ('a, 'sum) stateLattice -> string ->
object
  method inspect : bool
  method inspectInstAfter : Cil.instr -> 'a Dataflow.action -> unit
  method inspectInstBefore : Cil.instr -> 'a -> unit
  method inspectStmtAfter : Cil.stmt -> 'a Dataflow.stmtaction -> unit
  method inspectStmtBefore : Cil.stmt -> 'a -> unit
  method setInspect : bool -> unit
end


class type ['sum, 'st] summarizer =
  object
    method flushSummaries : unit -> unit
    method setInspect : bool -> unit
    method scopeIt : Cil.fundec -> 'sum -> 'sum
      (* TODO: do we really need more than just summarize? *)
    method summarize :
      Fstructs.fKey -> Cil.fundec -> (Cil.prog_point -> 'st) -> bool
  end

class ['a, 'b] summaryIsState :
  ('a, 'a) stateLattice ->
  object
    constraint 'b = 'a
    method private combineSummaries : 'a -> 'a -> 'a
    method private isBottomSummary : 'a -> bool
    method private printSummary : 'a -> unit
    method private sumSubset : 'a -> 'a -> bool
  end

class virtual ['a, 'b] summaryIsOutput :
  ('a, 'a) stateLattice -> 'a Backed_summary.base ->
object 
    constraint 'b = 'a
    method virtual scopeIt : Cil.fundec -> 'a -> 'a
    method flushSummaries : unit -> unit
    method setInspect : bool -> unit
    method summarize :
      Fstructs.fKey -> Cil.fundec -> (Cil.prog_point -> 'b) -> bool
  end

(* TODO: make this like the summaryIsOutput -- i.e., make it a summarizer *)
class ['a, 'b] summaryIsFIS :
  ('a, 'a) stateLattice ->
  object
    constraint 'b = 'a
    method makeSummary : Cil.fundec -> (Cil.stmt -> Cil.instr -> 'a) -> 'a
  end
