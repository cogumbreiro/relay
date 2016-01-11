open Callg
open Fstructs

(**** call graph at the SCC level ****)

type sccID = int

type scc = {
  
  (* This SCC's ID number *)
  scc_num : sccID;
  
  (* Functions represented by this SCC *)
  mutable scc_nodes : FSet.t;
  
  (* sccs called from this node *)
  mutable scc_callees : IntSet.t;

  (* sccs calling this node *)
  mutable scc_callers : IntSet.t;
}
    
(* Call graph indexed by SCC numbering *)
type sccGraph = scc IntMap.t

(* Get a call DAG of SCCs *)
val getSCCGraph : simpleCallG -> sccGraph

val getDependencies : simpleCallG -> fKey list -> (fKey, int) Hashtbl.t

val isRoot : scc -> bool

val isLeaf : scc -> bool

val pruneUnreached : sccGraph -> FSet.t -> sccGraph
