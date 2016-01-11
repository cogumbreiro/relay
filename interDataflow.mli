open Fstructs
open Summary_keys
open Callg
open Scc_cg
open Backed_summary

(** A framework for inter-procedural data flow analysis built on
 *  the CIL intra-procedural framework *)

open Fstructs

type 't interResult = 
    NoChange (* No change *)
  | NewOutput of ('t * 't) (* New output summary for pair (in,out) *)

(* TODO: make this work w/ context-sensitive callgraphs *)

(** Info needed by the inter-proc driver *)
module type ProcTransfer = sig
  
  type state (** The type of the data propagated from function to function. 
              *  May be imperative. *)

  val doFunc: ?input:state -> funID -> callN -> state interResult
  (** Analyze the function, given an input state, possibly getting 
   *  a new output state *)

  val filterFunc: callN -> bool
  (** TRUE if the function should NOT be put on the worklist,
   *  when it normally should be put on the worklist *)

  val sccDone: scc -> bool -> (sumKey * string) list
  (** Callback function to inform user that an SCC is now fix-pointed *)

  val sccStart: scc -> unit
  (** Callback hint that an SCC is about to be fix-pointed *) 

    
end


(** Interface to Dataflow driver *)
module type S = sig
  
  val compute : callG -> sccGraph -> unit

end


(******************************************************************
 
 BOTTOM UP                                                                  
 
 ******************************************************************)


module BottomUpDataflow (T : ProcTransfer) : S 
