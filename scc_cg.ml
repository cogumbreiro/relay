

(** SCC DAGS adapted to call graphs *)

open Fstructs
open Callg
open Logging

(************************************************************
  Generate basic graph w/ Scc module
************************************************************)

module CGraph  = struct
  type key = funID
  type node = callN
  type graph = callG

  let getNode g k : node = FMap.find k g

  let listSuccs g n = (calleeKeys n)

  let iter = FMap.iter

end

let dummySCCID = -1

(***** Convert SCC DAG to something "more convenient" *****)

type 'a sccPoly = {
  scc_num : int;
  scc_nodes : 'a;
  scc_callees : IntSet.t;
  scc_callers : IntSet.t;
}


(** Implementation *)

(* Get an SCC DAG then convert it to something more specific to callgraphs *)
module SCCDAG = Scc.Make(CGraph)

type scc = FSet.t sccPoly
type sccGraph = scc IntMap.t 

let emptySCCCG = IntMap.empty

let convertNodeList sccN =
  List.fold_left 
    (fun curSet k -> FSet.add k curSet) 
    FSet.empty sccN.SCCDAG.scc_nodes

let convertSccList sccL =
  List.fold_left
    (fun curSet sccID -> IntSet.add sccID curSet) 
    IntSet.empty sccL

let convertScc (sccN : SCCDAG.sccNode) =
  { scc_num = sccN.SCCDAG.scc_num;
    scc_nodes = convertNodeList sccN;
    scc_callers = convertSccList sccN.SCCDAG.scc_in;
    scc_callees = convertSccList sccN.SCCDAG.scc_out;
  }

let convertSccCG (sccG : SCCDAG.sccDAG) : sccGraph =
  Hashtbl.fold 
    (fun k n curG -> IntMap.add k (convertScc n) curG)
    sccG emptySCCCG


(** @return true if the node should be included in the SCC graph *)
let filterNode n = n.hasBody

let pruneNode cg n =
  let filterTarget otherK =
    try
      let otherN = FMap.find otherK cg in
      filterNode otherN
    with Not_found -> false
  in
  (* Bleh... breaking abstraction here *)
  let filterDetailedNode callee =
    match callee with 
      CDirect (pp, k) -> filterTarget k
    | CIndirect (pp, ks) -> 
        (* if one should not be dropped, then don't *)
        List.exists filterTarget ks
  in
  if n.hasBody then
    { n with
        ccallers = List.filter filterTarget n.ccallers;
        ccallees = List.filter filterDetailedNode (calleeDetail n);
    }
  else begin
    assert (n.ccallees = []);
    n
  end

(** Get a call graph in terms of SCCs *)
let getSCCGraph cg : sccGraph =
  let smallerCG = 
    FMap.fold 
      (fun k n curCG ->
         if (filterNode n) then
           FMap.add k (pruneNode cg n) curCG
         else
           curCG
      ) cg FMap.empty in
  logStatusF "Scc_cg: Pruned %d funcs w/out bodies for scc DAG\n"
    (Stdutil.mapSize cg FMap.fold - Stdutil.mapSize smallerCG FMap.fold);
  let sccg = SCCDAG.makeSCCDAG smallerCG in
  convertSccCG sccg

(************************************************************)

(** Get a hashtable of reference counts based on the callgraph and a list of
    functions at the end of the analysis *)
let getDependencies cg finalFuncs =
  let pinCounts = Hashtbl.create 17 in
  FMap.iter
    (fun _ fNode ->
       List.iter 
         (fun calleeK ->
            try 
              let oldPinCount = Hashtbl.find pinCounts calleeK in
              Hashtbl.replace pinCounts calleeK (oldPinCount + 1)
            with Not_found ->
              Hashtbl.add pinCounts calleeK 1 
         ) (calleeKeys fNode)
    ) cg;
  List.iter 
    (fun k ->
       try 
         let oldPinCount = Hashtbl.find pinCounts k in
         Hashtbl.replace pinCounts k (oldPinCount + 1)
       with Not_found ->
         (* No callees. Assume dependence decr only happens when caller
            is processed *)
         Hashtbl.add pinCounts k 1 
    ) finalFuncs;
  pinCounts

(** Prune away the nodes in the SCC Graph that are not reachable *)
let pruneUnreached (sccCG:sccGraph) (reachableCG : FSet.t) : sccGraph =
  IntMap.fold
    (fun sccK sccN newSCCCG ->
       if not (FSet.is_empty 
                 (FSet.inter reachableCG sccN.scc_nodes)) then
         IntMap.add sccK sccN newSCCCG
       else newSCCCG
    ) sccCG emptySCCCG
    

let isRoot scc =
  IntSet.is_empty scc.scc_callers

let isLeaf scc =
  IntSet.is_empty scc.scc_callees

let filesOfPruned cg prunedSCCs =
  let fnames = Hashtbl.create 10 in
  IntMap.iter 
    (fun _ scc ->
       FSet.iter 
         (fun fkey ->
            try
              let callnode = FMap.find fkey cg in
              Hashtbl.replace fnames callnode.defFile ()
            with Not_found ->
              logError ("filesOfPruned: can't find call node for " ^ 
                          fid_to_string fkey)
         ) scc.scc_nodes
    ) prunedSCCs;
  fnames

let iterFiles (foo : Cil.file -> unit) fnames cgDir =
  Hashtbl.iter 
    (fun fname () ->
       let fname = Filename.concat cgDir fname in
       let ast = Cil.loadBinaryFile fname in
       foo ast
    ) fnames

let findBiggestScc sccCG =
  let biggestScc = ref FSet.empty in
  let size = IntMap.fold 
    (fun sccID scc m -> 
       let thisSize = FSet.cardinal scc.scc_nodes in
       if (thisSize > m) then begin
         biggestScc := scc.scc_nodes;
         thisSize
       end else m
    ) sccCG 0 in
  logStatusF "Found biggest scc w/ |x| = %d\n" size;
  !biggestScc    
    
let allFunctions sccCG = 
  IntMap.fold 
    (fun sccID scc funcs -> FSet.union funcs scc.scc_nodes) sccCG FSet.empty

