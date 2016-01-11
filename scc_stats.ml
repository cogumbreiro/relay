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


(** Print scc and callgraph statistics. *)

open Scc
open Callg
open Readcalls
open Fstructs
open Stdutil

module Dis = Distributed
module L = Logging


let cgFile = ref ""

let setCGFile (fname:string) = 
  cgFile := fname

let findCycles = ref false

let pruneDone = ref false

let printRoots = ref false

let printIDs = ref false

(* Command-line argument parsing *)
 
let argSpecs = 
  [("-cg", Arg.String setCGFile, "the call graph file");
   ("-pr", Arg.Set printRoots, "print names of functions that are cg roots");
   ("-cy", Arg.Set findCycles, "test if there are cycles (debug)");
   ("-pd", Arg.Set pruneDone, "prune away sccs that have been analyzed");
   ("-id", Arg.Set printIDs, "print the scc ids + the fkeys in the scc")
   ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg filename [options]\n"



(************************ Frequency check for SCC sizes *************)

(* Returns a map from scc size -> frequency 
 *)
let accumFreqs (s:scc) (freqMap:int IntMap.t ) : int IntMap.t =
  let updateMap (curSize:int) = 
    let newCount = 
      try 
        (IntMap.find curSize freqMap) + 1
      with
        Not_found -> 1
    in
    IntMap.add curSize newCount freqMap
  in
  updateMap (FSet.cardinal s.scc_nodes)
        
        
(* prints the distribution of SCC sizes *)
let printFreqs (sccCG : sccGraph) : unit =
  let freqs = IntMap.fold (fun k d freq -> 
                           accumFreqs d freq) sccCG IntMap.empty in
  L.logStatus "size,\tfreq";
  L.logStatus "----------------";
  IntMap.iter (fun size count -> Printf.printf "%d,\t%d\n" size count) freqs 



(************************ Print roots of the callgraph *************)


let printRootFuns cg : unit =
  L.logStatus ("Root functions:\n====================================");
  let roots = getRoots cg in
  List.iter 
    (fun (fk, n) ->
       L.logStatus ((string_of_fNT (n.name, n.typ)))
    ) roots
    
  

(******************* DEBUG ******************)

module OrderedSCC =
  struct
    type t = scc
    let compare a b =
      a.scc_num - b.scc_num
  end

module SS = Stackset.Make(OrderedSCC)


let printCycle (path:SS.t) =
  L.logStatus "FOUND A CYCLE!!1\n---------------------------";
  let buff = Buffer.create 32 in
  SS.iter 
    (fun scc ->
       Buffer.add_string buff ((string_of_int scc.scc_num) ^ " w/ fkeys: ");
       FSet.iter 
         (fun fkey ->
            Buffer.add_string buff ((string_of_fkey fkey) ^ ", ")
         ) scc.scc_nodes;
       Buffer.add_string buff "\n\n";
    ) path;
  L.logStatus (Buffer.contents buff ^ "\n")

let checkCycles (sccG:sccGraph) =
  let visited = ref IntSet.empty in
  let curPath = SS.create () in
  let rec visit scc = 
    if (IntSet.mem scc.scc_num !visited) then
      if (SS.mem scc curPath) then
        printCycle curPath
      else
        ()
    else begin
      visited := IntSet.add scc.scc_num !visited;
      SS.pushOnce scc curPath;
      IntSet.iter 
        (fun neighK ->
           try
             let neighSCC = IntMap.find neighK sccG in
             visit neighSCC;
           with Not_found ->
             ()
        ) scc.scc_callees;
      let _ = SS.pop curPath in
      ()
    end
  in
  IntMap.iter 
    (fun k scc ->
       visit scc;
    ) sccG



(* TODO: Make it read the config file before this will work! *)
let checkCyclesPruned (sccG:sccGraph) filter =
  let prunedSCC = IntMap.fold 
    (fun k scc curG ->
       if (filter scc) then
         IntMap.add k scc curG
       else  
         curG
    ) sccG IntMap.empty in
  checkCycles prunedSCC


let printSccIDs sccG cg =
  IntMap.iter 
    (fun k scc ->
       L.logStatus ("SCC num: " ^ (string_of_int scc.scc_num));
       let buff = Buffer.create 16 in
       Buffer.add_string buff "funs :: [";
       FSet.iter (fun fk -> 
                    Buffer.add_string buff ((string_of_fkey fk) ^ ", ")
                 ) scc.scc_nodes;
       Buffer.add_string buff "]";
       L.logStatus (Buffer.contents buff)
    ) sccG

(********************* Entry Point ************************)
  
let main () =
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cgfile arg, so check manually *)
    if (!cgFile = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else begin
      let cg = readCalls !cgFile in
      let sccGraph = getSCCGraph cg in

      printFreqs sccGraph;
      
      if(!findCycles) then
        if(!pruneDone) then
          checkCyclesPruned sccGraph 
            (fun scc ->
               Dis.isSccDone scc.scc_num
            )
        else
          checkCycles sccGraph
      else
        ()
      ;

      if(!printRoots) then
        printRootFuns cg
      ;

      if(!printIDs) then
        printSccIDs sccGraph cg
      ;
      
      exit 0;
    end
  with e -> Printf.printf "Exc. in SCC: %s\n"
    (Printexc.to_string e) ; raise e
;;
main ()

