class type fpTestable =
  object
    method init : Cil.file -> unit
    method name : string
    method resolve_fp : Cil.exp -> int list
  end

class cilFPTest : fpTestable

module StringSet : Set.S with type elt = string

class fpTestDriver : fpTestable -> fpTestable ->
object inherit Cil.nopCilVisitor

  method testFunPtrs : string -> unit

end
