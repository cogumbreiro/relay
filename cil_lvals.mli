

(***************************************************
 * Cil AST op
 ***************************************************)

val cZero : Cil.exp

val cOne : Cil.exp

val canonicizeOff : Cil.offset -> Cil.offset * bool

val canonicizeExp : Cil.exp -> Cil.exp

val canonicizeLval : Cil.lval -> Cil.lval

val simplifyOff : Cil.offset -> Cil.offset

val simplifyLval : Cil.lval -> Cil.lval * bool


exception BaseVINotFound


val getIndex : Cil.varinfo list -> Cil.varinfo -> int option

val findBaseVarinfoExp : Cil.exp -> Cil.varinfo

val findBaseVarinfoLval : Cil.lval -> Cil.varinfo

val countOpsInExp : Cil.exp -> int

val countOpsWithCount : Cil.exp -> int -> int



(***************************************************
 * Actual / Formal substitution on ASTs
 ***************************************************)

exception SubstInvalidArg

val substActForm : Cil.exp -> Cil.lval -> Cil.lval

val substActFormDeref : Cil.lval -> Cil.lval -> Cil.lval


(*****************************************************
 * Unsafe versions of Cil type calculation functions
 * Also ditches attributes
 * Still flags errors involving Arrays
 *****************************************************)

exception TypeNotFound

val unrollTypeNoAttrs : Cil.typ -> Cil.typ

val typeOfUnsafe : Cil.exp -> Cil.typ 

val typeOfInitUnsafe : Cil.init -> Cil.typ

val typeOfLvalUnsafe : Cil. lval -> Cil.typ

val typeOffsetUnsafe : Cil.typ -> Cil.offset -> Cil.typ

val typeAfterDeref : Cil.exp -> Cil.typ


exception OffsetMismatch

val attachOffset : Cil.lhost -> Cil.offset -> Cil.lval

val mkMemChecked : Cil.exp -> Cil.offset -> Cil.lval

val addOffsetChecked : Cil.offset -> Cil.offset -> Cil.offset

val locIsUnknown : Cil.location -> bool


(*****************************************************
 * Distillation and hash-consing
 *****************************************************)

val distillOff : Cil.offset -> Cil.offset

val distillLoc : Cil.location -> Cil.location

val distillEnuminfo : Cil.enuminfo -> unit

val distillVar : Cil.varinfo -> unit

val distillExp : Cil.exp -> Cil.exp

val mergeLv : Cil.lval -> Cil.lval

val mergeType : Cil.typ -> Cil.typ
