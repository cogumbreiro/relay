open Symex_base
open Symex_types
open IntraDataflow

include SYMEX

type symLatt = (symState, SS.sumval) stateLattice

val fullLattice : symLatt

val sumIsFinal : Summary_keys.sumKey -> bool 
  (* shouldn't this depend on the sumType? *)

class symSummarizer : symLatt -> (SS.data) -> [symState, SS.sumval] summarizer

class symTransFunc : (symState, SS.sumval) stateLattice -> 
  object
  inherit [symState] transFunc
  inherit [symState] inspectorGadget

  method private isFirstStmt : Cil.prog_point -> bool

  method private havocTarget : symState -> (symAddr * Cil.offset) -> symState

end

