open Fstructs
open Callg
open Cil

type threadCreatorAttribs = {
  tcStarts : bool;         (* true if the function starts the thread also *)
  tcStartFunName : string; (* if fun doesn't start thread, which fun does? *)
  tcStartFunType : string;
  tcFPIndex : int;         (* index of arg list for function pointer *)
  tcArgIndex : int;        (* index of arg list for the args of new thread *)
}

val initSettings : Config.settings -> unit

val find : fNT -> threadCreatorAttribs

type tccs = (fKey * simpleCallN) list

val findTCCallers : simpleCallG ->  tccs

val getThreadRoots : simpleCallG -> tccs -> FSet.t


class virtual threadCreateVisitor : simpleCallG -> object 
  inherit nopCilVisitor

  val mutable curStmt : stmt

  method handleThreadCreate : instr -> location -> 
    threadCreatorAttribs -> exp list -> unit

  method virtual handleThreadRoots : instr -> location -> exp -> fKey list -> exp -> unit

end
