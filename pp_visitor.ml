
open Cil

class ppTracker = object (self)

  (** Track the index of the current instruction. Child classes must 
      remember to call the appropriate increment function once
      an instruction has been visited! *)
  val mutable instrIndex = -1    
    
  (** Bump (add to) instruction index up [n]. Child class is responsible 
      for invoking this before next instruction! If the child class
      transforms the instruction into multiple, just bump by the 
      appropriate amount. *)
  method private bumpInstr n =
    instrIndex <- instrIndex + n

  (** Start visiting a block of instructions. Child classes must 
      remember to call this version to start the instruction index *)
  method private startVinst () =
    instrIndex <- 0

  (* Not clearing the instrIndex with vinstEnd? *)

  (** Set {!Cil.currentLoc} and {!Cil.curProgPoint} for an instruction
      (assuming pp was already set for the parent statement).
      Child class is responsible for calling this. *)
  method private setInstrPP i =
    setInstrLocation i instrIndex

  (** Set {!Cil.currentLoc} and {!Cil.curProgPoint}for this statement.
      Child class is responsible for calling this. *)
  method private setStmtPP s =
    setStmtLocation s

end

(** Extension of {!Cil.nopCilVisitor} that tracks the prog_point of
    each visited {stmt} and {instr} for the visited function.
    A bit messy right now because of all the child-class obligations. *)
class ppVisitor = object(self)
  inherit ppTracker as ppT
  inherit nopCilVisitor as super

  (***** Demonstration of how to use API, for a nopVisitor 
         that still tracks prog_points *****)

  method vinst i =
    self#setInstrPP i;
    let nopResult = super#vinst i in
    self#bumpInstr 1;
    nopResult

  method vinstStart il =
    ppT#startVinst ();
    super#vinstStart il

  method vstmt s =
    self#setStmtPP s;
    (match s.skind with
       Instr il 
     | TryExcept (_, (il, _), _, _) -> 
         self#vinstStart il;
     | _ -> ());
    super#vstmt s
      
end
