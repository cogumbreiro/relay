
(** Tracks the sum of the amount of dataflow information per function.
    TODO: Get something more like an average per line of the function?
    At least average per context of a particular function to make
    comparing context-sensitive against context-insensitive easier *)

open Cil
open Fstructs
open Callg
open Logging

class type knowVisitor = object
  inherit Pp_visitor.ppVisitor
  
  method initState : funID -> unit

  method incrementKnowledge : unit -> unit

  method printStats : unit -> unit

end



class virtual knowledgeVisitor caption = object(self)
  inherit Pp_visitor.ppVisitor

  val mutable fid = dummyFID

  val mutable knowledge = 0

  method initState fk =
    fid <- fk;
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
    logStatusF "%s knowledge counter for fkey %s : %d\n"
      caption (fid_to_string fid) knowledge

end

class knowledgeAnalysis (skipped : Summary_keys.sumKey -> bool) 
  (kVisitor:knowVisitor) : IntraDataflow.analysis= object (self)

  method setInspect (yesno:bool) =
    ()

  method isFinal key =
    skipped key 

  method compute funID cfg =
    kVisitor#initState funID;
    ignore (visitCilFunction (kVisitor :> cilVisitor) cfg)

  method summarize key (cfg:fundec) =
    (if self#isFinal key then ()
     else kVisitor#printStats ());
    false
      
  method flushSummaries () = ()

end


