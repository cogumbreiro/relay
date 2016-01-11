(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following 
  conditions are met:
  
  1. Redistributions of source code must retain the above copyright 
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above 
  copyright notice, this list of conditions and the following disclaimer 
  in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the University of California, San Diego, nor 
  the names of its contributors may be used to endorse or promote 
  products derived from this software without specific prior 
  written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
*)


(** 
    Glue/framework for inter-procedural data flow analysis built on
    the CIL intra-procedural framework.  Run by each worker process 
    to communicate w/ the server/directory.
*)

open Fstructs
open Summary_keys
open Callg
open Scc_cg
open Messages
open Logging

module Stat = Mystats

(** Type of value returned by intra-procedural analysis and used
    by the inter-procedural engine to propagate info and do fixed-point 
    check. Parameterized by the type of state *)
type 't interResult = 
    NoChange (* No change *)
  | NewOutput of ('t * 't) (* New summary for pair of states (in,out) *)


(** Input module: Info needed by the inter-proc driver 
    Note: user manages the summary datastructure *)
module type ProcTransfer = sig
    
  type state (** The type of the data propagated from function to function. *)

  val doFunc: ?input:state -> funID -> callN -> state interResult
  (** Analyze the function, given an input state, possibly 
      getting a new output state *)

  val filterFunc: callN -> bool
  (** TRUE if the function should be put on the worklist *)

  val sccDone: scc -> bool -> (sumKey * string) list
  (** Callback function to inform user that an SCC is now fix-pointed *)

  val sccStart: scc -> unit
  (** Callback hint that an SCC is about to be analyzed *) 
    
end


(******************************************************************
                        BOTTOM UP
 ******************************************************************)

(** Interface to Dataflow driver *)
module type S = sig

  (** Run the analysis on the entire program (given the callgraph) *)
  val compute : callG -> sccGraph -> unit

end

(* Time individual functions and the scc *)
module FunIndex = struct
  
  type t = string
      
  let hash (s:t) = Hashtbl.hash s
  let equal (s1:t) (s2:t) = s1 = s2
  let to_string (s:t) = s
  let getTime () = Stat.getWCTime ()
  let prefix = "FUN TIMES : "

end


module BottomUpDataflow = functor (T : ProcTransfer) -> struct

  
  (* Module for worklist of functions *)
  module FQ = Queueset.Make(CompKey)

  module FunTime = Stat.IndexedTimer(FunIndex)

  (** Worklist of functions to process (from current SCC) *)
  let funWork = FQ.create ()
  
  (** Reference to the call graph *)
  let theCG = ref emptyCG
    
  (** Propagate new data to the target function, where the new
      data is meant as input to the target function *)
  let propagateIn (newData:T.state) (targetFunc:funID) : unit =
    logError "progateIn not implemented"
      
  (** Propagate new data to the target function, where the new data is
      the output of a source function *) 
  let propagateOut (newData:T.state) (targetFunc:funID) : unit =
    try 
      let node = FMap.find targetFunc !theCG in
      if (T.filterFunc node) then begin
          FQ.addOnce targetFunc funWork;
      end
    with Not_found ->
      logError ("Function not in callgraph (1): " ^ (fid_to_string targetFunc))
        
  exception NoFuncInCallg

  let funsDone = ref 0 

  (** Process funWork until a fixed-point is reached *) 
  let fixedPoint curSCC =
    while (not (FQ.is_empty funWork)) do
      (* Do one iteration *)
      let curKey = FQ.pop funWork in 
      begin
        let curNode =
          try FMap.find curKey !theCG
          with Not_found ->
            (logErrorF "Function not in callgraph (2): %s\n" 
               (fid_to_string curKey);
             raise NoFuncInCallg)
        in
        let funLabel = "FUN:" ^ fid_to_string curKey in
        (match (FunTime.time funLabel (T.doFunc curKey) curNode) with
           NoChange -> ()
         | NewOutput (i,o) -> 
             let mayNotify = curNode.ccallers in
             let toNotify = List.filter 
               (fun nk -> FSet.mem nk curSCC.scc_nodes)
               mayNotify in
             List.iter (propagateOut o) toNotify);

        incr funsDone;
        if !funsDone mod 50 == 0 then begin
          FunTime.printTimes ();
          Stat.print stdout "STATS:\n";
        end;
      end;
    done


  (** Complete the work related to an SCC *)
  let doSCC curSCC =
    Checkpoint.record curSCC "Race";
    (* Add functions to work queue *)
    FSet.iter (fun k -> FQ.addOnce k funWork) curSCC.scc_nodes;

    Stat.time "sccStart" T.sccStart curSCC;

    let sccSize = (string_of_int (FQ.length funWork)) in
    logStatus ("=================================");
    logStatus ("Starting an SCC (" ^ sccSize ^ ") #" ^ 
                   (string_of_int curSCC.scc_num));
    logStatus ("=================================");          
    flushStatus ();

    (try
       fixedPoint curSCC;
     with e ->
       logError "InterDF: fixed-pointing died?";
       raise e
    );

    logStatus ("=================================");
    logStatus ("Finished an SCC (" ^ sccSize ^ ")");
    logStatus ("=================================");
    
    (* Inform client, then server that an SCC is now summarized *)
    let summPaths = T.sccDone curSCC true in
    Request.sccDone curSCC summPaths;
    logStatus ("=================================");
    Checkpoint.complete curSCC "Race"


  (** Check if there was previously unfinished business *)
  let checkRecoverCrash sccCG =
    match Checkpoint.whatsLeft () with
      None -> ()
    | Some (scc_num, analysisType) -> (* ignore analysisType for now *)
        try
          let scc = IntMap.find scc_num sccCG in
          logStatus ("Recovering scc: " ^ (string_of_int scc_num));
          doSCC scc
        with Not_found as e ->
          logError "Failed to recover scc (Not_found?)";
          raise e

      
  (** Iterate through the call graph in bottom-up order and
      compute the fixedpoint of summaries *)
  let compute cg sccCG =
    FunTime.reset ();
    let isDone = ref false in
    theCG := cg;
    checkRecoverCrash sccCG;
    while (not (!isDone)) do
      logStatus "getting next SCC from server";
      flushStatus ();
      match Request.getSCCWork () with
        MSCCReady curSCC ->
          doSCC curSCC

      | MDone ->
          isDone := true

      | _ -> 
          (* retry *)
          ()
    done

end
