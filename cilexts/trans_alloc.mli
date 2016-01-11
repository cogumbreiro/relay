

val isAllocVar : string -> bool
val findMalloc : Cil.exp -> Cil.varinfo option
val feature : Cil.featureDescr
val unknownMallocType : Cil.typ
val pickType : Cil.typ -> Cil.exp list -> Cil.typ
