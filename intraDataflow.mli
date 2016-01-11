

class type ['st, 'sum] stateLattice =
  object
    method bottom : 'st
    method combineStates : 'st -> 'st -> 'st
    method initialState : 'st
    method setInitialState : 'st -> unit
    method isBottom : 'st -> bool
    method printState : 'st -> unit
    method printSummary : Summary_keys.sumKey -> unit
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
    method curFunID : Callg.funID

    method setCG : Callg.callG -> unit
    method curCG : Callg.callG

    method handleASM : Cil.attributes * string list
      * (string option * string * Cil.lval) list 
      * (string option * string * Cil.exp) list
      * string list * Cil.location -> 
      'a -> 'a Dataflow.action
    method handleAssign :
      Cil.lval -> Cil.exp -> Cil.location -> 'a -> 'a Dataflow.action
    method handleCall :
      Cil.lval option -> Callg.funID list ->
      Cil.exp -> Cil.exp list -> Cil.location -> 'a -> 'a Dataflow.action
    method handleCallExp :
      Callg.funID list -> Cil.exp -> Cil.exp list -> Cil.location -> 'a -> 'a
    method handleCallRet : Cil.lval -> Callg.funID list -> 
      Cil.exp -> Cil.exp list -> Cil.location -> 'a -> 'a
    method handleFunc : Callg.funID -> Cil.fundec -> unit
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
    val initialize : Callg.funID -> Cil.fundec -> T.st -> unit
    val compute : Cil.fundec -> unit
    val getOutState : unit -> T.st
    val curFunc : Cil.fundec ref
    val curFunID : Callg.funID ref
    val getDataBefore : Cil.prog_point -> T.st
    val getDataAfter : Cil.prog_point -> T.st
    val setDataBefore : Cil.prog_point -> T.st -> unit
    val setDataAfter : Cil.prog_point -> T.st -> unit
    val sizeOfState : unit -> int
  end

module FlowSensForward :
  functor (S : DFTransfer) -> IntraProcAnalysis with type T.st = S.st

module FlowInsensitive :
  functor (S : DFTransfer) -> IntraProcAnalysis with type T.st = S.st

module FlowSensBackward :
  functor (S : DFTransfer) -> IntraProcAnalysis with type T.st = S.st

val curCG : Callg.callG ref
val curSCCCG : Scc_cg.sccGraph ref

val checkupSummary :
  Callg.funID ->
  Cil.fundec ->
  'c ->
  (Callg.funID -> 'c) ->
  ('c -> bool) ->
  ('c -> 'c -> bool) ->
  ('c -> 'c -> 'c) ->
  (Cil.fundec -> 'c -> 'c) -> (Callg.funID -> 'c -> 'd) -> bool -> ('c -> unit) -> bool

class type analysis = 
object
  method setInspect : bool -> unit
  method isFinal : Summary_keys.sumKey -> bool 
  method compute : Callg.funID -> Cil.fundec -> unit
  method summarize : Summary_keys.sumKey -> Cil.fundec -> bool
  method flushSummaries : unit -> unit
end

val runFixpoint : analysis list -> Summary_keys.sumKey -> Cil.fundec -> bool

val runNonFixpoint :
  analysis list -> analysis list -> Callg.funID -> Scc_cg.scc -> unit

val warnNoCallees : Cil.exp -> unit

class ['a] idTransFunc : ('a, 'sum) stateLattice -> ['a] transFunc

class ['a] inspectorGadget : ('a, 'sum) stateLattice -> string ->
object
  method private inspect : bool
  method private setInspect : bool -> unit
  method private inspectInstAfter : Cil.instr -> 'a Dataflow.action -> unit
  method private inspectInstBefore : Cil.instr -> 'a -> unit
  method private inspectStmtAfter : Cil.stmt -> 'a Dataflow.stmtaction -> unit
  method private inspectStmtBefore : Cil.stmt -> 'a -> unit
end

class ['a] inspectingTransF : ('a, 'sum) stateLattice -> string -> ['a] transFunc

class type ['st, 'sum] summarizer =
  object
    method flushSummaries : unit -> unit
    method setInspect : bool -> unit
    method scopeIt : Cil.fundec -> 'sum -> 'sum
      (* TODO: do we really need more than just summarize? *)
    method summarize :
      Summary_keys.sumKey -> Cil.fundec -> (Cil.prog_point -> 'st) -> bool
  end

class ['a, 'b] noneSummarizer : 
  'b Backed_summary.base -> ['a, 'b] summarizer

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
  ('a, 'a) stateLattice -> 'b Backed_summary.base -> 
object 
  constraint 'b = 'a
  method virtual scopeIt : Cil.fundec -> 'b -> 'b
  method flushSummaries : unit -> unit
  method setInspect : bool -> unit
  method summarize :
    Summary_keys.sumKey -> Cil.fundec -> (Cil.prog_point -> 'b) -> bool
end

(* TODO: make this like the summaryIsOutput -- i.e., make it a summarizer *)
class ['a, 'b] summaryIsFIS :
  ('a, 'a) stateLattice ->
object
  constraint 'b = 'a
  method makeSummary : Cil.fundec -> (Cil.stmt -> Cil.instr -> 'a) -> 'a
end
