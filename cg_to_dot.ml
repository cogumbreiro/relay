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
    Convert a callgraph to a dot file (for graphviz). Can slice the 
    call graph in various ways.
*)

open Gc_stats
open Readcalls
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

let sccToShow = ref (-1)

(** Set the ID of scc ([n]) to display *)
let setSCC (n:int) : unit = 
  sccToShow := n

(** Only print the shortest path between the L bound and U bound
    (if there's function each) *)
let shortestPath = ref false

let shortestPathDist = ref (-1)

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "name of call graph file");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-l", Arg.String addLowerBound, "lowest parts of call graph needed");
   ("-u", Arg.String addUpperBound, "highest parts of call graph needed");
   ("-s", Arg.Int setSCC, "only show the SCC and it's neighbors");
   ("-p", Arg.Set shortestPath, "show the shortest path from U to L");
   ("-pstat", Arg.Set_int shortestPathDist, 
    "get common nodes in shortest paths between nodes in scc w/ given id");
   ("-o", Arg.Set_string outDir, "name of output directory")]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname -o outdir [options]\n"



(***************************************************)
(* Run                                             *)


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings (cg) =
  try
    let settings = Config.initSettings !configFile in
    DC.makeLCaches (!cgDir);
    A.initSettings settings !cgDir;
    Th.initSettings settings;
    Entry_points.initSettings settings; 

  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


(******************** Utility functions *********************)


let sccLabel (scc:scc) =
  ("scc #" ^ (string_of_int scc.scc_num))

let sccID (scc:scc) = 
  ("cluster_" ^ (string_of_int scc.scc_num))

let funAttrs (f:simpleCallN) (fkey: fKey) =
  ("label=\"{" ^ f.name ^ " | " ^ f.defFile ^ " | " ^ (string_of_fkey fkey) ^ 
     "}\", tooltip=\"" ^ f.typ ^ "\"")
    
let funID (f:simpleCallN) (fkey: fKey) = 
  (f.name ^ "_" ^ (string_of_fkey fkey))
    
let edgeColor max cur =
  let hue = (string_of_float ((float_of_int cur) /. (float_of_int max))) in
  let sat = (string_of_float ((Random.float 0.25) +. 0.5)) in
  let v = (string_of_float ((Random.float 0.5) +. 0.5)) in
  hue ^ ", " ^ sat ^ ", " ^ v

let randomEdgeColor () = 
  let hue = (string_of_float (Random.float 1.0)) in
  let sat = (string_of_float ((Random.float 0.25) +. 0.5)) in
  let v = (string_of_float ((Random.float 0.5) +. 0.5)) in
  hue ^ ", " ^ sat ^ ", " ^ v

let writeHeader out_chan = 
  output_string out_chan "digraph callgraph {\n\n";
  output_string out_chan "\tcompound=\"true\";\n";
  output_string out_chan "\tranksep=\"1\";\n";
  output_string out_chan 
    "\tnode [shape=\"record\", fontname=\"Verdana\"];\n\n";
  output_string out_chan "\tfontname=\"Verdana\";\n\n"
  
  
let writeTrailer out_chan =
  output_string out_chan "}\n";
  output_string out_chan "\n\n";
  (* Assume this is the end *)
  flush out_chan;
  let written = out_channel_length out_chan in
  L.logStatusF "Bytes written: %d\n" written

let writeEdges out_chan cg fkey =
  try
    let fnode = FMap.find fkey cg in
    let fid = funID fnode fkey in
    let numCallees = List.length fnode.callees in
    let _ = List.fold_left 
      (fun index neighK  ->
         try
           let neighNode = FMap.find neighK cg in
           let neighID = funID neighNode neighK in
           let color = edgeColor numCallees index in
           output_string out_chan ("\t" ^ fid ^ " -> " ^ 
                                     neighID ^ 
                                     "[color=\"" ^ color ^ "\"];\n");
           index + 1
         with Not_found -> 
           index + 1
      ) 0 fnode.callees in
    output_string out_chan "\n"
  with Not_found ->
    ()

let writeEdge out_chan cg (k1, n1, k2, n2) =
  let fid = funID n1 k1 in
  let sid = funID n2 k2 in
  let color = randomEdgeColor () in
  output_string out_chan ("\t" ^ fid ^ " -> " ^ 
                            sid ^ "[color=\"" ^ color ^ "\"];\n");
  output_string out_chan "\n"

let writeNodes out_chan cg iterNodes =
  iterNodes 
    (fun fkey -> try
       let fnode = FMap.find fkey cg in
       let fattrs = funAttrs fnode fkey in
       let fid = funID fnode fkey in
       output_string out_chan ("\t\t" ^ fid ^ " [" ^ fattrs ^ "];\n");
     with Not_found ->
       ()
    )

let writeSccHeader out_chan scc = 
  let sccL = sccLabel scc in
  let sccID = sccID scc in 
  output_string out_chan ("\tsubgraph " ^ sccID ^ 
                            " {\n\t\tlabel = \"" ^ sccL ^ "\";\n");
  output_string out_chan ("\t\tgraph [bgcolor=\"#cccccc\"];\n")
  
    
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
 

(** Write the chosen sccs *)
let writeSccs (iterScc : (scc -> unit) -> unit) cg = 
  let iterNodes scc func = 
    FSet.iter func scc.scc_nodes 
  in
  let doWrite out_chan =
    let wEdges = writeEdges out_chan cg in
    let wSccH = writeSccHeader out_chan in
    
    (* start *)
    writeHeader out_chan;
    
    (* Write out the scc clustering of nodes *)  
    iterScc
      (fun scc ->
         wSccH scc;
         writeNodes out_chan cg (iterNodes scc);
         output_string out_chan ("\t}\n\n");
      );
    
    (* Write out the function call edges *)
    iterScc
      (fun scc -> 
         FSet.iter wEdges scc.scc_nodes
      );
    
    (* Write out the thread creation edges *)
    
    (* finish up *)
    writeTrailer out_chan
  in
  open_out_for (outFile None) doWrite


(** Write out the entire call graph *)
let writeWholeDotFile cg sccCG threadCR  =
  let iterScc = 
    (fun iterFunc -> IntMap.iter (fun _ scc -> iterFunc scc) sccCG) in
  writeSccs iterScc cg


(** Write out the !sccToShow and neighbors *)
let writeSCCDotFile cg sccCG threadCR  =
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
    writeSccs iterScc cg
  with Not_found ->
    L.logError ("Can't find scc " ^ (string_of_int !sccToShow))



(******************** Pruned call graph *********************)

(** Write the chosen nodes and the accompanying edges *)
let writeNodesWithEdges iterNodes cg =
  let doWrite out_chan =
    let wEdges = writeEdges out_chan cg in
    
    (* start *)
    writeHeader out_chan;
    
    (* Write out the nodes *)  
    writeNodes out_chan cg iterNodes;
    
    (* Write out the function call edges *)
    iterNodes wEdges;
    
    (* finish up *)
    writeTrailer out_chan;
  in
  open_out_for (outFile None) doWrite


(** Write the chosen nodes and chosen edges *)
let writeNodesEdges iterNodes iterEdges cg =
  let doWrite out_chan =
    let wEdge = writeEdge out_chan cg in
    
    (* start *)
    writeHeader out_chan;
    
    (* Write out the nodes *)  
    writeNodes out_chan cg iterNodes;
    
    (* Write out the function call edges *)
    iterEdges wEdge;
    
    (* finish up *)
    writeTrailer out_chan;
  in
  open_out_for (outFile None) doWrite

      
module StringMap = Map.Make (String)

let addKey (fn, ft) cur =
  try 
    let old = StringMap.find fn cur in
    StringMap.add fn ((fn, ft) :: old) cur
  with Not_found ->
    StringMap.add fn [(fn, ft)] cur
    
let hasKey (fn, ft) cur =
  try 
    let bucket = StringMap.find fn cur in
    List.exists 
      (fun k ->
         compareNT (fn, ft) k == 0) bucket
  with Not_found ->
    false

let mapsEQ m1 m2 =
  StringMap.equal              
    (fun l1 l2 -> 
       try 
         List.for_all2 
           (fun k1 k2 -> 
              compareNT k1 k2 == 0
           ) l1 l2
       with Invalid_argument _ ->
         false
    )
    m1 m2

let toSet m =
  StringMap.fold 
    (fun n klist s ->
       List.fold_left 
         (fun curSet k ->
            FSet.add k curSet
         ) s klist
    ) m FSet.empty
    
(** Find all sources in the [cg] *)
let findSources cg : FSet.t = 
  let m = FMap.fold 
    (fun fkey node cur ->
       if (List.mem node.name !upper) then
         FSet.add fkey cur
       else
         cur
    ) cg FSet.empty in
  m

(** Find all sinks in the [cg] *)
let findSinks cg : FSet.t = 
  let m = FMap.fold
    (fun fkey node cur ->
       if (List.mem node.name !lower) then
         FSet.add fkey cur
       else
         cur
    ) cg FSet.empty in
  m


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
           ) (cur, mbChanged) v.callees
      ) cg (oldSet, false) in
    if (mbChanged && 
          (not (FSet.equal oldSet newSet))) then
      iterReaches newSet
    else
      newSet
  in
  iterReaches cur


(** Just write out the graph of functions that can reach the target *)
let writeLowerBoundedDotFile cg sccCG threadCR =
  (* Find the needed nodes *)
  let baseNodes = findSinks cg in
  let neededNodes = reachesSinks cg baseNodes in
  let iterNodes = (fun iterFunc -> FSet.iter iterFunc neededNodes) in
  writeNodesWithEdges iterNodes cg


module ESet = Set.Make(
  struct
    type t = fKey * simpleCallN * fKey * simpleCallN
    let compare (k1,_, k1', _) (k2, _, k2', _) =
      let f = compareFKey k1 k2 in
      if (f == 0) then
        compareFKey k1' k2'
      else
        f
  end
)

(** Find all callpaths in the [cg] between the [sources] and the [sinks] *)
let findPaths cg sources sinks : (FSet.t * ESet.t) = 
  let visited = Hashtbl.create 101 in
  let neededNodes = Hashtbl.create 101 in
  let neededEdges = Hashtbl.create 101 in
  let curPath = ref [] in
  
  (* Add the current path assuming it ends at lastFunc *)
  let addAPath lastFunc =
    let finalPath = 
      if (FMap.mem lastFunc cg) then
        lastFunc :: !curPath
      else
        !curPath
    in
    List.iter 
      (fun node ->
         Hashtbl.replace neededNodes node ();
      ) finalPath;
    match finalPath with
      fst :: tl ->
        let _ = List.fold_left 
          (fun prev cur ->
             Hashtbl.replace neededEdges (cur, prev) ();
             cur
          ) fst tl in
        ()
    | _ ->
        ()
  in
  
  (* Add any paths from src to any of the sinks *)
  let rec addPaths src =
    if (Hashtbl.mem visited src) then
      if (Hashtbl.mem neededNodes src) then
        addAPath src (* Add backedges to nodes in found paths? *)
      else ()
    else begin
      Hashtbl.add visited src ();
      
      (try
         let node = FMap.find src cg in
         curPath := src :: !curPath;
         List.iter
           (fun neighK ->
              if (List.exists (fun k ->
                                 compareFKey neighK k == 0) sinks) then begin
                addAPath neighK
              end
              else
                addPaths neighK
           ) node.callees;
         curPath := List.tl !curPath;
       with Not_found ->
         ()
      )
      ;
      
    end
  in
  (* Add any paths from any of the sources to any of the sinks *)
  List.iter 
    (fun src -> 
       addPaths src) sources;
  (
    Hashtbl.fold 
      (fun k _ curSet ->
         FSet.add k curSet
      ) neededNodes FSet.empty,
    Hashtbl.fold 
      (fun (k1, k2) _ curSet ->
         try 
           let node1 = FMap.find k1 cg in
           let node2 = FMap.find k2 cg in
           ESet.add (k1, node1, k2, node2) curSet
         with Not_found ->
           curSet
      ) neededEdges ESet.empty
  )



(** Just write out the graph of functions that can reach the target *)
let writePathsDotFile cg sccCG threadCR  =
  (* Find the needed nodes *)
  let sinkNodes = FSet.elements (findSinks cg) in
  let sourceNodes = FSet.elements (findSources cg) in
  let (neededNodes, neededEdges) = findPaths cg sourceNodes sinkNodes in
  let iterNodes = (fun iterFunc ->
                     FSet.iter iterFunc neededNodes) in
  let iterEdges = (fun iterFunc ->
                     ESet.iter iterFunc neededEdges) in
  writeNodesEdges iterNodes iterEdges cg



(** Find shortest path in [cg] between the [source] and the [sink],
    assuming edge weight of 1 throughout (gonna do BFS)... *)
let findShortestPaths cg source sink : (FSet.t * ESet.t) = 
  let neededNodes = Hashtbl.create 101 in
  let neededEdges = Hashtbl.create 101 in
  let reverseMap = Hashtbl.create 101 in
  let visited = Hashtbl.create 101 in

  let addReverseEdge back node =
    try 
      let cur = Hashtbl.find reverseMap node in
      Hashtbl.replace reverseMap node (back :: cur)
    with Not_found ->
      Hashtbl.add reverseMap node [back]
  in

  let addOnceReverse back node =
    if Hashtbl.mem reverseMap node then ()
    else Hashtbl.add reverseMap node [back]
  in

  let worklist = Queue.create () in
  let found = ref false in

  let findPaths () =
    Hashtbl.clear visited;
    Queue.add source worklist;
    
    while (not (Queue.is_empty worklist) && not !found) do
      let next = Queue.take worklist in
      if (Hashtbl.mem visited next) then ()
      else begin
        Hashtbl.add visited next ();

        (try
           let node = FMap.find next cg in
           List.iter
             (fun neighK ->
                if compareFKey neighK sink == 0 then begin
                  addReverseEdge next neighK;
                  found := true
                end
                else begin
                  addOnceReverse next neighK; (* others only have one *)
                  (* queue it up *)
                  Queue.add neighK worklist;
                end
             ) node.callees;
         with Not_found ->
           ()
        );
      end
    done;
  in
  
  (* Add found paths *)
  let rec addFoundPaths key =
    try
      if Hashtbl.mem visited key then ()
      else begin
        Hashtbl.add visited key ();
        let backNodes = Hashtbl.find reverseMap key in
        (* add self... possibly redundant *)
        Hashtbl.replace neededNodes key ();
        (* add neighbors *)
        List.iter 
          (fun neighK ->
             Hashtbl.replace neededNodes neighK ();
             Hashtbl.replace neededEdges (neighK, key) ();
             addFoundPaths neighK 
          ) backNodes
      end
    with Not_found ->
      ()
  in
  
  findPaths ();
  Hashtbl.clear visited;
  addFoundPaths sink;
  
  (* Convert the paths *)
  (
    Hashtbl.fold 
      (fun k _ curSet ->
         FSet.add k curSet
      ) neededNodes FSet.empty,
    Hashtbl.fold 
      (fun (k1, k2) _ curSet ->
         try 
           let node1 = FMap.find k1 cg in
           let node2 = FMap.find k2 cg in
           ESet.add (k1, node1, k2, node2) curSet
         with Not_found ->
           curSet
      ) neededEdges ESet.empty
  )


(** Just write out the graph of functions that can reach the target *)
let writeShortestPath cg sccCG threadCR  =
  (* Find the needed nodes *)
  let sinkNodes = FSet.elements (findSinks cg) in
  let sourceNodes = FSet.elements (findSources cg) in
  match sinkNodes, sourceNodes with
    [sink], [source] ->
      let (neededNodes, neededEdges) = 
        findShortestPaths cg source sink in
      let iterNodes = (fun iterFunc ->
                         FSet.iter iterFunc neededNodes) in
      let iterEdges = (fun iterFunc ->
                         ESet.iter iterFunc neededEdges) in
      writeNodesEdges iterNodes iterEdges cg
  | _ -> failwith "non-singular sources/sinks for writeShortestPath"


let getShortestPathStats cg sccCG =
  (* TODO: use an all-pairs shortest path algo instead... *)
  L.logStatusF "Printing shortest path stats for scc# %d\n" !shortestPathDist;
  try
    let scc = IntMap.find !shortestPathDist sccCG in
    let nodes = FSet.elements scc.scc_nodes in
    let dist = Dis.makeDistro () in

    let len = List.length nodes in
    let expectedPairs = (len * (len + 1)) / 2 in
    let pairsDone = ref 0 in
    let printStatus () = 
      if !pairsDone mod 100 == 0 then begin
        L.logStatusF "Pairs done: %d / %d\n" !pairsDone expectedPairs;
        L.flushStatus ();
      end
    in

    listIterPairs 
      (fun fk1 fk2 ->
         printStatus ();
         let neededNodes, _ = findShortestPaths cg fk1 fk2 in
         FSet.iter 
           (fun pathK ->
              if pathK == fk1 || pathK == fk2 then ()
              else 
                try
                  let pathN = FMap.find pathK cg in
                  Dis.updateDistro dist pathN.name
                with Not_found -> ()
           ) neededNodes;
         incr pairsDone
      ) nodes;
    Dis.printDistroSortFreq dist (fun x -> x) "Common shortest-path nodes"
  with Not_found ->
    L.logError "scc not found!" 

(******************* Dispatching ****************)


(** Convert cg, sccs, etc., to a dot graph *)
let doDot () : unit = begin
  (* Get Callgraph structures *)
  let cg = readCalls !cgFile in
  let sccCG = getSCCGraph cg in
  initSettings cg;
  
  (* TODO: Figure out what the thread spawn edges in the graph looks like *)
  let threadCreateToRoot = [] in
  
  (* Write dot file depending on what bounds are given *)
  (if (!sccToShow >= 0) then
     writeSCCDotFile cg sccCG threadCreateToRoot
   else if (!lower = []) then
     writeWholeDotFile cg sccCG threadCreateToRoot
   else if (!upper = []) then
     writeLowerBoundedDotFile cg sccCG threadCreateToRoot
   else if (!shortestPath) then
     writeShortestPath cg sccCG threadCreateToRoot
   else
     writePathsDotFile cg sccCG threadCreateToRoot
  );

  (* Print statistics *)
  if !shortestPathDist >= 0 then
    getShortestPathStats cg sccCG
  ;

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
