
(** Filters to debug imprecision in call graph *)
class type cgFilter = object
  method filter : Fstructs.fKey list -> Cil.fundec -> 
    Fstructs.fKey -> Cil.exp  -> Fstructs.fKey list
end

class noFilter : cgFilter

val curFilter : cgFilter ref

val dumpCalls : Cil.file -> string -> unit

val setDumpTo : string -> unit

val writeCallgraph : string -> unit 

val getCallsFile : string -> string

val isIndirect : Cil.exp -> bool
