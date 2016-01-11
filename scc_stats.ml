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

open Scc_cg
open Callg
open Readcalls
open Fstructs
open Stdutil
open Pretty

module Dis = Distributed
module L = Logging
module Th = Threads
module DC = Default_cache
module A = Alias

let cgFile = ref ""

let cgDir = ref ""

let configFile = ref "client.cfg"

let setCGFile (fname:string) = 
  cgFile := fname;
  cgDir := Filename.dirname fname

let findCycles = ref false

let pruneDone = ref false

let printRoots = ref false

(** Print the SCC IDs and the functions within each SCC *)
let printIDs = ref false

(** Output the callgraph depth statistics *)
let dump_depths = ref false

(** Output the function reachability statistics *)
let reachable_stats = ref false

let coopDAG = ref false

let outDir = ref ""

(* Command-line argument parsing *)
 
let argSpecs = 
  [("-cg", Arg.String setCGFile, "the call graph file");
   ("-pr", Arg.Set printRoots, "print names of functions that are cg roots");
   ("-cy", Arg.Set findCycles, "test if there are cycles (debug)");
   ("-pd", Arg.Set pruneDone, "prune away sccs that have been analyzed");
   ("-id", Arg.Set printIDs, "print the scc ids + the fkeys in the scc");
   ("-tr", Arg.Set reachable_stats, "print thread root reachability stats");
   ("-d", Arg.Set dump_depths, "print max depth of each node in callgraph");
   ("-o", Arg.Set_string outDir, "set a different output directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap file");
   ("-coop", Arg.Set coopDAG, "format for Cooperate DAG consumption");
   ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg filename [options]\n"

let outFile baseName = 
  Filename.concat !outDir baseName

(************************ Frequency check for SCC sizes *************)

(** Returns a map from scc size -> frequency *)
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
    ) roots;
  L.logStatusF "(%d)\n\n" (List.length roots)

let printRootSccs scccg : unit =
  L.logStatus ("Root sccs:\n====================================");
  let numroots = IntMap.fold 
    (fun sccID scc tot ->
       if isRoot scc then begin 
         L.logStatusF "id: %d contents: %s\n"
           sccID
           (FSet.fold (fun fk c -> c ^ " " ^ string_of_fkey fk) 
              scc.scc_nodes ""); 
         tot + 1
       end else tot
    ) scccg 0 in
  L.logStatusF "(%d)\n\n" numroots

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

let checkHasBody cg sccCG =
  IntMap.iter
    (fun sccID scc ->
      FSet.iter 
        (fun fkey ->
           try
             let node = FMap.find fkey cg in
             if not node.hasBody then
               L.logError 
                (Printf.sprintf "scc %d has func %d w/ no body\n"
                   sccID fkey)
             else ()
           with Not_found ->
               L.logError
                (Printf.sprintf "scc %d has func %d no int cg\n"
                   sccID fkey)
        ) scc.scc_nodes
    ) sccCG

(**************** Print Scc contents ***************)

(** Get the in and out degree from function f to other functions in
    the same scc *)
let funcDegreesInScc fk scc cg =
  let edgesIntoScc curCount otherFk =
    if FSet.mem otherFk scc.scc_nodes then curCount + 1 else curCount
  in
  try 
    let finfo = FMap.find fk cg in
    let inD = List.fold_left edgesIntoScc 0 finfo.callers in
    let outD = List.fold_left edgesIntoScc 0 finfo.callees in
    (fk, finfo.name, inD, outD)
  with Not_found ->
    (fk, "", -1, -1)

let pFunStats scc cg : doc =
  (* Get in / out degrees *)
  let funStats = FSet.fold 
    (fun fk cur -> 
       funcDegreesInScc fk scc cg :: cur
    ) scc.scc_nodes [] in

  (* Sort by sum *)
  let funStats = List.sort 
    (fun (_, _, in1, out1) (_, _, in2, out2) ->
       (in2 + out2) - (in1 + out1)
    ) funStats in
  
  (* Print *)
  let printDistro funStats numFuncs =
    let header = dprintf "in deg\tout deg\tfkey\tfname\n" in
    List.fold_left
      (fun curDoc (fk, fn, inD, outD) ->
         curDoc ++ dprintf "%d\t%d\t%s\t%s\n" inD outD (string_of_fkey fk) fn
      ) header funStats ++
      dprintf "(%d)" numFuncs
  in

  let printSimple funStats =
    match funStats with
      [(fk, fn, _, _)] -> dprintf "%s\t%s" (string_of_fkey fk) fn
    | _ -> failwith "empty scc?"
  in

  let numFuncs = List.length funStats in
  if numFuncs <= 1 then
    printSimple funStats
  else
    printDistro funStats numFuncs

  

let printSccIDs sccG cg =
  IntMap.iter 
    (fun k scc ->
       let header = text ("SCC num: " ^ (string_of_int scc.scc_num)) ++ line ++ 
         text "  funs = [" ++ align in
       let body = pFunStats scc cg in
       let trailer = text "]" ++ unalign ++ line ++ line in
       L.logStatusD (header ++ body ++ trailer)
    ) sccG


(************ Other callgraph related stats ****************)


(** Print statistics on how deep each function is in the call graph
    (weighted by SCC size) *)
let printDepthStats file stats sccCG =
  let doWrite out_chan =
    Hashtbl.iter 
      (fun sccK stat -> try
         let scc = IntMap.find sccK sccCG in
         FSet.iter 
           (fun fk -> 
              output_string out_chan ((string_of_int fk) ^ "\t" ^ 
                                        (string_of_int stat) ^ "\n")) 
           scc.scc_nodes
       with Not_found ->
         ()
      ) stats;
  in
  open_out_for file doWrite


(** print the max depth of each node in the call graph 
    (based on the scc DAG *)
let printDepths cg sccCG =
  let depths = Hashtbl.create 227 in
  (* In (forward) topological order, process depths ("finishing times").
     To process in topological order, do BFS starting from roots. *)
  let visited = Hashtbl.create 227 in
  let worklist = Queue.create () in
  let _ = IntMap.iter (fun sccK scc -> 
                         if isRoot scc then Queue.add sccK worklist) sccCG in
  while not (Queue.is_empty worklist) do
    let sccK = Queue.take worklist in
    try 
      let scc = IntMap.find sccK sccCG in
      let myDepth = try Hashtbl.find depths sccK 
      with Not_found -> (Hashtbl.replace depths sccK 0; 0) in
      (* add the size of the SCC *)
      let nextDepth = myDepth + (FSet.cardinal scc.scc_nodes) in 
      IntSet.iter 
        (fun neighK ->
           let neighD = try Hashtbl.find depths neighK with Not_found -> 0 in
           if (neighD < nextDepth) then
             Hashtbl.replace depths neighK nextDepth
           ;
           if not (Hashtbl.mem visited neighK) then
             (Hashtbl.add visited neighK true;
              Queue.add neighK worklist)     
        ) scc.scc_callees
    with Not_found ->
      ()
  done;

  (* Finally, print the "finishing times" *)
  printDepthStats (outFile "depths.tmp") depths sccCG;

  (* Find the deepest child of each node? *)
  let deepestChild = Hashtbl.create 227 in

  let rec dfs rootK sccK =
    Hashtbl.add visited sccK true;
    try
      let scc = IntMap.find sccK sccCG in
      (* only the leafs can be the deepest children *)
      if isLeaf scc then (
        let oldD = try Hashtbl.find deepestChild rootK with Not_found -> 0 in
        let thisD = try Hashtbl.find depths sccK with Not_found -> 0 in
        if (thisD > oldD) then Hashtbl.replace deepestChild rootK thisD
      ) else (
        IntSet.iter 
          (fun childK -> if not (Hashtbl.mem visited childK) then
             dfs rootK childK) scc.scc_callees
      )
    with Not_found -> ()
  in

  IntMap.iter 
    (fun sccK scc ->
       Hashtbl.clear visited;
       dfs sccK sccK
    ) sccCG;
  printDepthStats (outFile "deepest_child.tmp") deepestChild sccCG

let hasBody cg fset =
  FSet.filter 
    (fun fk -> 
       try let fn = FMap.find fk cg in fn.hasBody with Not_found -> false) 
    fset  

(** Print thread root reachability stats *)
let printReachStats cg sccCG = begin
  let printSet fset header =
    let header = text (header ^ ": [") ++ line in
    let body = 
      L.seq_to_doc
        Pretty.line
        FSet.iter
        (fun fkey ->
           let temp = text (string_of_fkey fkey) in
           (try
              let fnode = FMap.find fkey cg in
              temp ++ text (" : " ^ fnode.name)
            with Not_found ->
              temp
           )
        )
        fset
        Pretty.nil in
    let num = FSet.cardinal fset in
    let tail = text ("] (" ^ (string_of_int num) ^ ")\n") ++ line in
    L.logStatusD (header ++ (indent 2 body) ++ tail);
  in
  
  (* Find all the roots *)
  let roots = getRoots cg in
  let roots = List.fold_left (fun cur (fk, _) -> FSet.add fk cur) 
    FSet.empty roots in
  printSet roots "Roots";

  (* Find which functions actually fork new threads *)
  let threadCreatorCallers = Th.findTCCallers cg in
  let tcc = List.fold_left 
    (fun cur (fkey, _) -> FSet.add fkey cur) FSet.empty threadCreatorCallers in
  printSet tcc "Thread creators";

  (* Find call graph roots that reach spawn sites *)
  let tccRoots = rootsThatReach cg tcc in
  printSet tccRoots "Roots reaching spawn (RRS)";
  
  (* Find the fork targets (i.e., thread roots) *)
  let threadRoots = Th.getThreadRoots cg threadCreatorCallers in
  printSet threadRoots "Thread roots (TR)";
  
  (* Find call graph roots that are "entry points" *)
  let entryRoots = Entry_points.getEntries !cgDir cg in
  printSet entryRoots "Entry points (EP)";
  
  (* TODO: make the above use the Entry_points module too *)
  let rooter = new Entry_points.rootGetter cg !cgDir in
  let rootSet = rooter#getRootKeys () in
  
  let reachable, _ = getReachableFunctions cg rootSet in
  printSet reachable "Reachable from EP, RRS and TR";

  let unusedRoots = hasBody cg (FSet.diff roots rootSet) in
  printSet unusedRoots "Unused roots";
  
  let allFuncs = FMap.fold (fun k v cur -> FSet.add k cur) cg FSet.empty in
  let nonreachable = FSet.diff allFuncs reachable in
  let nonreachable = hasBody cg nonreachable in
  printSet nonreachable "Not reachable from EP, RRS and TR (w/ body)";
  
  let rootList = rooter#getRoots () in
  let len = List.length rootList in
  L.logStatus ("Expected root pairs: " ^ 
                 string_of_int ((len * (len + 1)) / 2));
  
  let newSCCCG = Scc_cg.pruneUnreached sccCG reachable in
  L.logStatusF "Prev # SCCs: %d\t New: %d\n" (mapSize sccCG IntMap.fold)
    (mapSize newSCCCG IntMap.fold);

  let nonReachSCCCG = Scc_cg.pruneUnreached sccCG nonreachable in
  L.logStatusF "Non-reachable #: %d\n" (mapSize nonReachSCCCG IntMap.fold);
  L.flushStatus ();

end


(* RAVI printing out DAG to be read in by Cooperate *)
let printCooperate (dag:sccGraph) =
  Printf.printf "BEGIN DAG\n";
  IntMap.iter (fun _ node ->
    if IntSet.is_empty node.scc_callers then Printf.printf "source\n";
    if IntSet.is_empty node.scc_callees then Printf.printf "sink\n";
    Printf.printf "id %d kids " node.scc_num;
    IntSet.iter (fun kid -> Printf.printf "%d " kid) node.scc_callees;
    Printf.printf "\n"
  ) dag;
  Printf.printf "END DAG\n"


    
(********************* Entry Point ************************)
  
let initSettings (cg) =
  try
    let settings = Config.initSettings !configFile in
    DC.makeLCaches (!cgDir);
    A.initSettings settings !cgDir;
    Th.initSettings settings;
    Entry_points.initSettings settings; 

  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


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
      initSettings cg;

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

      checkHasBody cg sccGraph;

      if(!printIDs) then
        printSccIDs sccGraph cg
      ;
      
      (* print what and how many functions are reachable from thread roots *)
      if !reachable_stats then
        printReachStats cg sccGraph
      ;

      (* print max depths of each node in callgraph *)
      if !dump_depths then
        printDepths cg sccGraph
      ;
      
      if(!printRoots) then begin
        printRootFuns cg;
        printRootSccs sccGraph
      end;

      if !coopDAG then
        printCooperate sccGraph
      ;

      exit 0;
    end
  with e -> Printf.printf "Exc. in SCC: %s\n"
    (Printexc.to_string e) ; raise e
;;
main ()

