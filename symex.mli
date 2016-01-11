open Symex_base
open Symex_types
open IntraDataflow
open Fstructs

include SYMEX

type symLatt = (symState, SS.sumval) stateLattice

val fullLattice : symLatt

val sumIsFinal : fKey -> bool (* shouldn't this depend on the sumType? *)

class symSummarizer : symLatt -> (SS.data) -> [SS.sumval, symState] summarizer

class symTransFunc : symLatt -> 
  object
  inherit [symState] transFunc
  inherit [symState] inspectorGadget

  method isFirstStmt : Cil.prog_point -> bool

  method havocTarget : symState -> (symAddr * Cil.offset) -> symState 

end

