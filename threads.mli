open Fstructs
open Callg
open Cil

type threadCreatorAttribs

val initSettings : Config.settings -> unit

val find : fNT -> threadCreatorAttribs

type tccs = (fKey * simpleCallN) list

val findTCCallers : simpleCallG ->  tccs

val getThreadRoots : simpleCallG -> tccs -> FSet.t


class virtual threadCreateVisitor : simpleCallG -> object 
  inherit Pp_visitor.ppVisitor

  method handleThreadCreate : instr -> location -> 
    threadCreatorAttribs -> exp list -> unit

  method virtual handleThreadRoots : instr -> location -> exp -> 
    fKey list -> exp list -> unit

end

val getThreadActuals : simpleCallG -> tccs -> exp list * varinfo list
