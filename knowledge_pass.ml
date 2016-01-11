
open Fstructs
open Cil

module L = Logging

class type knowVisitor = object
  inherit Pp_visitor.ppVisitor
  
  method initState : fKey -> unit

  method incrementKnowledge : unit -> unit

  method printStats : unit -> unit

end



class virtual knowledgeVisitor caption = object(self)
  inherit Pp_visitor.ppVisitor

  val mutable fkey = -1

  val mutable knowledge = 0

  method initState fk =
    fkey <- fk;
    knowledge <- 0

  method virtual incrementKnowledge : unit -> unit

  method vinst i =
    self#setInstrPP i;
    self#incrementKnowledge ();
    self#bumpInstr 1;
    DoChildren

  method vstmt s =
    self#setStmtPP s;
    self#incrementKnowledge ();
    DoChildren

  method printStats () =
    L.logStatus (caption ^ " knowledge counter for fkey "
                 ^ string_of_int fkey ^ ": " ^ string_of_int knowledge)

end

class knowledgeAnalysis (skipped : fKey -> bool) 
  (kVisitor:knowVisitor) = object (self)

  method setInspect (yesno:bool) =
    ()

  method isFinal fkey =
    skipped fkey

  method compute cfg =
    let fkey = funToKey cfg in
    kVisitor#initState fkey;
    ignore (visitCilFunction (kVisitor :> cilVisitor) cfg)

  method summarize (fkey:fKey) (cfg:fundec) =
    (if self#isFinal fkey then ()
     else
       kVisitor#printStats ());
    false

  method flushSummaries () = ()

end


