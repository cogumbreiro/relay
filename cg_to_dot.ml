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

(** Convert a callgraph to a dot file (for graphviz). Can slice the 
    call graph in various ways.  *)

open Gc_stats
open Callg
open Cil
open Pretty
open Fstructs
open Scc_cg
open Stdutil
open Cilinfos

module Dis = Distributions
module A = Alias
module Th = Threads
module FC = Filecache
module L = Logging


(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let cgFile = ref ""

let configFile = ref "client.cfg"

(** Where to store the generated dot files *)
let outDir = ref ""

let lower = ref []

let upper = ref []

(** Add a function [fname] to the "lower bound" of the generated call graph *)
let addLowerBound fname =
  lower := fname :: !lower

(** Add a function [fname] to the "upper bound" of the generated call graph *)
let addUpperBound fname =
  upper := fname :: !upper

(** List of callgraph edges (f1 -> f2) that are deemed infeasible *)
let infeasiblePairs = ref []

(** Expect input to be "i,foo,bar" *)
let addInfeasiblePair pairStr =
  let index, f1, f2 = 
    match Str.split (Str.regexp ",") pairStr with
      [i; f1; f2] -> 
        int_of_string (Strutil.strip i), 
        Strutil.strip f1, Strutil.strip f2
    | _ -> failwith "Infeasible edges must be of the form: indx,foo1,foo2"
  in
  infeasiblePairs := (index, f1, f2) :: !infeasiblePairs
        
let sccToShow = ref (-1)
let sccOnly = ref false

(** Set the ID of scc ([n]) to display *)
let setSCC (n:int) : unit = 
  sccToShow := n

let setSCCOnly (n:int) : unit = 
  sccToShow := n;
  sccOnly := true

(** Only print the shortest path between the L bound and U bound
    (if there's function each) *)
let shortestPath = ref false

let shortestPathBigHisto = ref false

let isContextSens = ref false

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "name of call graph file");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-l", Arg.String addLowerBound, "lowest parts of call graph needed");
   ("-u", Arg.String addUpperBound, "highest parts of call graph needed");
   ("-s", Arg.Int setSCC, "only show the SCC and it's neighbors");
   ("-sonly", Arg.Int setSCCOnly, "only show the SCC, not including neighbors");
   ("-p", Arg.Set shortestPath, "show the shortest path from U to L");
   ("-pstatL", Arg.Set shortestPathBigHisto, 
    "get common nodes in shortest paths between nodes in largest scc");
   ("-o", Arg.Set_string outDir, "name of output directory");
   ("-i", Arg.String addInfeasiblePair, 
    "add an infeasible call edge: i, f1, f2");
   ("-con", Arg.Set isContextSens, "callgraph is context-sensitive");
  ]
    
let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname -o outdir [options]\n"



(***************************************************)
(* Run                                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings cg =
  try
    let settings = Config.initSettings !configFile in
    DC.makeLCaches (!cgDir);
    A.initSettings settings !cgDir;
    Th.initSettings settings;
    Entry_points.initSettings settings; 

  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


    
(** If a relative [fname] is not given (None), pick an appropriate filename
    for the output dot graph file. Return the full path + filename
    for writing the output *)
let outFile (fname:string option) : string =
  let name = 
    match fname with 
      None ->
        (match !lower, !upper with
           fl :: _, fu :: _ ->
             fl ^ "_to_" ^ fu ^ ".dot"
         | fl :: _, [] ->
             fl ^ "_and_up.dot"
         | [], fu :: _ ->
             fu ^ "_and_down.dot"
         | [], [] -> "whole_thing.dot"
        )
    | Some (n) -> n
  in
  let full = (Filename.concat !outDir name) in
  L.logStatus ("writing to: " ^ full);
  full

(******************** Dump w/ Scc clusterings *********************)
 
module CGDot = struct

  open Dot_lib

  (** Write out the entire call graph *)
  let writeWholeDotFile cg sccCG  =
    let iterScc = 
      (fun iterFunc -> IntMap.iter (fun _ scc -> iterFunc scc) sccCG) in
    open_out_for (outFile None) (writeSccs iterScc cg)


  (** Write out the !sccToShow and neighbors *)
  let writeSCCDotFile cg sccCG =
    (* get the !sccToShow and sccs it calls *)
    try 
      let mainSCC = IntMap.find !sccToShow sccCG in
      let sccs = 
        IntSet.fold 
          (fun neighK curList ->
             try 
               let neighSCC = IntMap.find neighK sccCG in
               neighSCC :: curList
             with Not_found ->
               curList
          ) mainSCC.scc_callees [mainSCC] in
      let iterScc = (fun iterFunc -> List.iter iterFunc sccs) in
      open_out_for (outFile None) (writeSccs iterScc cg)
    with Not_found ->
      L.logError ("Can't find scc " ^ (string_of_int !sccToShow))

  (** Write out the !sccToShow and neighbors *)
  let writeSCCOnlyDotFile cg sccCG =
    (* get the !sccToShow and sccs it calls *)
    try 
      let mainSCC = IntMap.find !sccToShow sccCG in
      let sccs = [mainSCC] in
      let iterScc = (fun iterFunc -> List.iter iterFunc sccs) in
      open_out_for (outFile None) (writeSccs iterScc cg)
    with Not_found ->
      L.logError ("Can't find scc " ^ (string_of_int !sccToShow))



  (******************** Pruned call graph *********************)
        
  (** Find all sources in the [cg] *)
  let findSources cg : FSet.t = 
    FMap.fold 
      (fun fkey node cur ->
         if (List.mem node.name !upper) then
           FSet.add fkey cur
         else
           cur
      ) cg FSet.empty

  (** Find all sinks in the [cg] *)
  let findSinks cg : FSet.t = 
    FMap.fold
      (fun fkey node cur ->
         if (List.mem node.name !lower) then
           FSet.add fkey cur
         else
           cur
      ) cg FSet.empty

  (** Collect the set of functions that reach the [cur] set of functions *)
  let reachesSinks cg (cur : FSet.t) : FSet.t =
    let rec iterReaches oldSet = 
      let newSet, mbChanged = FMap.fold 
        (fun key v (cur, mbChanged) -> 
           List.fold_left 
             (fun (cur, mbChanged) calleeK ->
                if (FSet.mem calleeK cur) then
                  (FSet.add key cur, true)
                else
                  (cur, mbChanged)
             ) (cur, mbChanged) (calleeKeys v)
        ) cg (oldSet, false) in
      if (mbChanged && (not (FSet.equal oldSet newSet))) then
        iterReaches newSet
      else
        newSet
    in
    iterReaches cur


  (** Just write out the graph of functions that can reach the target *)
  let writeLowerBoundedDotFile cg =
    (* Find the needed nodes *)
    let baseNodes = findSinks cg in
    let neededNodes = reachesSinks cg baseNodes in
    let iterNodes = (fun iterFunc -> FSet.iter iterFunc neededNodes) in
    open_out_for (outFile None) (writeNodesWithEdges iterNodes cg)



  (** Just write out the graph of functions that can reach the target *)
  let writePathsDotFile cg  =
    (* Find the needed nodes *)
    let sinkNodes = FSet.elements (findSinks cg) in
    let sourceNodes = FSet.elements (findSources cg) in
    let (neededNodes, neededEdges) = 
      findPaths cg sourceNodes sinkNodes in
    let iterNodes = (fun iterFunc ->
                       FSet.iter iterFunc neededNodes) in
    let iterEdges = (fun iterFunc ->
                       ESet.iter iterFunc neededEdges) in
    open_out_for (outFile None) (writeNodesEdges iterNodes iterEdges cg)


  (** Just write out the graph of functions that can reach the target *)
  let writeShortestPath cg  =
    let handlePath sink source =
      let (neededNodes, neededEdges) = 
        graphShortestPath cg source sink in
      let iterNodes = (fun iterFunc ->
                         FSet.iter iterFunc neededNodes) in
      let iterEdges = (fun iterFunc ->
                         ESet.iter iterFunc neededEdges) in
      open_out_for (outFile None) (writeNodesEdges iterNodes iterEdges cg)
    in
    (* Find the needed nodes *)
    let sinkNodes = FSet.elements (findSinks cg) in
    let sourceNodes = FSet.elements (findSources cg) in
    match sinkNodes, sourceNodes with
      [sink], [source] ->
        handlePath sink source
    | [], _ -> failwith "empty set of sinks"
    | _, [] -> failwith "empty set of sources"
    | sink :: _, source :: _  ->
        Printf.printf "%d sinks, %d srcs, picking the first\n" 
          (List.length sinkNodes) (List.length sourceNodes);
        handlePath sink source


  let getShortestPathStats cg sccCG =
    (* TODO: use an all-pairs shortest path algo instead ?*)
    L.logStatus "Printing shortest path stats for biggest Scc\n";
    let nodes = FSet.elements (findBiggestScc sccCG) in
    let lenDist = Dis.makeDistro () in
    let funDist = Dis.makeDistro () in
    
    let len = List.length nodes in
    let expectedPairs = len in (* Only doing A -> A cycles *)
    let pairsDone = ref 0 in
    let printStatus () =
      if !pairsDone mod 100 == 0 then begin
        L.logStatusF "Pairs done: %d / %d\n" !pairsDone expectedPairs;
        L.flushStatus ();
      end
    in
    List.iter 
      (fun fk ->
         printStatus ();
         let neededNodes, _ = graphShortestPath cg fk fk in
         FSet.iter 
           (fun pathK ->
              try
                let pathN = FMap.find pathK cg in
                Dis.updateDistro funDist (pathN.name, pathK)
              with Not_found -> ()
           ) neededNodes;
         Dis.updateDistro lenDist (FSet.cardinal neededNodes);
         incr pairsDone
      ) nodes;
    Dis.printDistroSortFreq funDist 
      (fun (name, id) -> Printf.sprintf "%s (%s)" name (fid_to_string id)) 
      "Common shortest-path nodes";
    Dis.printDistroSortKey lenDist string_of_int "length of shortest self-cyc"

  (*** TODO: find all the chains if indirect -> indirect -> ... *)


  (* Use class as first-class interface *)
  class dotter = object 
    
    method writeSCCDotFile cg sccCG = 
      writeSCCDotFile cg sccCG

    method writeSCCOnlyDotFile cg sccCG = 
      writeSCCOnlyDotFile cg sccCG

    method getShortestPathStats cg sccCG =
      getShortestPathStats cg sccCG

    method writeWholeDotFile cg sccCG =
      writeWholeDotFile cg sccCG

    method writeLowerBoundedDotFile cg =
      writeLowerBoundedDotFile cg

    method writeShortestPath cg =
      writeShortestPath cg

    method writePathsDotFile cg =
      writePathsDotFile cg 

  end

end

(******************* Dispatching ****************)

let doOutputDot dotter cg sccCG =
  (* TODO: Figure out what the thread spawn edges in the graph looks like *)
  
  (* Write dot file depending on what bounds are given *)
  (if (!sccToShow >= 0) then
     if (!sccOnly) then
       dotter#writeSCCOnlyDotFile cg sccCG
     else 
       dotter#writeSCCDotFile cg sccCG
   else if !shortestPathBigHisto then
     dotter#getShortestPathStats cg sccCG
   else if (!lower = []) then
     dotter#writeWholeDotFile cg sccCG
   else if (!upper = []) then
     dotter#writeLowerBoundedDotFile cg 
   else if (!shortestPath) then
     dotter#writeShortestPath cg 
   else
     dotter#writePathsDotFile cg
  )


(** Convert cg, sccs, etc., to a dot graph *)
let doDot () : unit = begin
  let cg = readCalls !cgFile in
  let sccCG = Scc_cg.getSCCGraph cg in
  initSettings cg;
  let dotter = new CGDot.dotter in
  doOutputDot dotter cg sccCG
end


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgFile = "" || !configFile = "" || !outDir = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        cgDir := Filename.dirname !cgFile;
        Cil.initCIL ();
        setGCConfig ();

        L.logStatus "Preparing dot graph";
        L.logStatus "-----";
        doDot ();
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in cg_to_dot: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
