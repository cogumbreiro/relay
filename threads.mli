open Cil
open Fstructs
open Callg

type threadCreatorAttribs

val initSettings : Config.settings -> unit

val find : fNT -> threadCreatorAttribs

(** Thread creator callers (functions that call pthread_create, etc.) *)
type tcc = {
  tccID : funID;
  tccName : string;
  tccDefFile : string;
}


val findTCCallers : callG ->  tcc list

val getThreadRoots : callG -> tcc list -> FSet.t

class virtual threadCreateVisitor : callG -> object 
  inherit Pp_visitor.ppVisitor

  method handleThreadCreate : instr -> location -> 
    threadCreatorAttribs -> exp list -> unit

  method virtual handleThreadRoots : instr -> location -> exp -> 
    funID list -> exp list -> unit

end

val getThreadActuals : callG -> tcc list -> exp list * varinfo list
