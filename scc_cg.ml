

(** SCC DAGS adapted to call graphs *)

open Callg
open Fstructs

(************************************************************
  Generate basic graph w/ Scc module
************************************************************)

module CallGraph = struct
  type key = fKey
  type node = simpleCallN
  type graph = simpleCallG

  let getNode g k = FMap.find k g

  let listSuccs g n = n.callees

  let iter = FMap.iter

end

module SCCCG = Scc.Make(CallGraph)

(***** Convert SCC DAG to something "more convenient" *****)

(* Assuming sccIDs are int... *)

type scc = {
  scc_num : Scc.sccID;
  scc_nodes : FSet.t;
  scc_callees : IntSet.t;
  scc_callers : IntSet.t;
}

type sccGraph = scc IntMap.t 

let convertNodeList sccN =
  List.fold_left 
    (fun curSet k -> FSet.add k curSet) FSet.empty sccN.SCCCG.scc_nodes

let convertSccList sccL =
  List.fold_left
    (fun curSet sccID -> IntSet.add sccID curSet) IntSet.empty sccL

let convertScc (sccN : SCCCG.sccNode) : scc =
  { scc_num = sccN.SCCCG.scc_num;
    scc_nodes = convertNodeList sccN;
    scc_callers = convertSccList sccN.SCCCG.scc_in;
    scc_callees = convertSccList sccN.SCCCG.scc_out;
  }

let convertSccCG (sccG : SCCCG.sccDAG) : sccGraph =
  Hashtbl.fold 
    (fun k n curG -> IntMap.add k (convertScc n) curG)
    sccG IntMap.empty


(** @return true if the node should be included in the SCC graph *)
let filterNode n = n.hasBody

let pruneNode cg n =
  let filterTarget otherK =
    try
      let otherN = FMap.find otherK cg in
      filterNode otherN
    with Not_found -> 
      false
  in

  if n.hasBody then
    { n with
      callers = List.filter filterTarget n.callers;
      callees = List.filter filterTarget n.callees;
    }
  else begin
    assert (n.callees = []);
    n
  end

(** Get a call graph in terms of SCCs *)
let getSCCGraph (cg:simpleCallG) : sccGraph =
  let smallerCG = 
    FMap.fold 
      (fun k n curCG ->
        if (filterNode n) then
           FMap.add k (pruneNode cg n) curCG
         else
           curCG
      ) cg FMap.empty in
  L.logStatusF "Scc_cg: Pruned %d funcs w/out bodies for scc DAG\n"
    (Stdutil.mapSize cg FMap.fold - Stdutil.mapSize smallerCG FMap.fold);
  let sccg = SCCCG.makeSCCDAG smallerCG in
  convertSccCG sccg


(** Get a hashtable of reference counts based on the callgraph and a list of
    functions at the end of the analysis *)
let getDependencies (cg:simpleCallG) (finalFuncs:fKey list) 
    : (fKey, int) Hashtbl.t =
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
         ) fNode.callees 
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
       if not (FSet.is_empty (FSet.inter reachableCG sccN.scc_nodes)) then
         IntMap.add sccK sccN newSCCCG
       else 
         newSCCCG
    ) sccCG IntMap.empty


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
              L.logError ("filesOfPruned: can't find call node for " ^ 
                            string_of_fkey fkey)
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

