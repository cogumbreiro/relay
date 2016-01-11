
val is_volatile_tp : Cil.typ -> bool

val is_volatile_vi : Cil.varinfo -> bool

val compare_offset : Cil.offset -> Cil.offset -> int

val compare_var : Cil.varinfo -> Cil.varinfo -> int

val compare_var_attr : Cil.varinfo -> Cil.varinfo -> int

val compare_type : Cil.typ -> Cil.typ -> int

val compare_lval : Cil.lval -> Cil.lval -> int

val compare_lhost : Cil.lhost -> Cil.lhost -> int

val compare_exp : Cil.exp -> Cil.exp -> int

val compare_fundec : Cil.fundec -> Cil.fundec -> int

val compare_compi : Cil.compinfo -> Cil.compinfo -> int

val equal_type : Cil.typ -> Cil.typ -> bool

val hash_exp : Cil.exp -> int

val hash_lval : Cil.lval -> int

val hash_lhost : Cil.lhost -> int

val hash_type : Cil.typ -> int

val hash_offset : Cil.offset -> int

val hash_var : Cil.varinfo -> int

val hash_var_attr : Cil.varinfo -> int

val hash_fundec : Cil.fundec -> int

val isSubchainOffset : Cil.offset -> Cil.offset -> int option

val compareCurOffset : Cil.offset -> Cil.offset -> int

val cutEQSuffixOffsets : Cil.offset -> Cil.offset -> (Cil.offset * Cil.offset)

val typeAfterDeref : Cil.typ -> Cil.typ

val eq_offset_tail : Cil.offset -> Cil.offset -> bool

val predPP : Cil.stmt -> Cil.prog_point list

val getStmtFromPP : Cil.fundec -> Cil.prog_point -> Cil.stmt

val getInstrFromPP : Cil.fundec -> Cil.prog_point -> Cil.instr

module PPHash : Hashtbl.S with type key = Cil.prog_point

module PPSet : Set.S with type elt = Cil.prog_point

val getIndex : Cil.varinfo list -> Cil.varinfo -> int option
val isFormal : Cil.fundec -> Cil.varinfo -> bool

(************** Deprecated ***************)

module InstrHash : Hashtbl.S with type key = Cil.instr

