

(** 
    Encode dependencies between analyses (identify which must run first
    and, if there are circular dependencies, which must run together).
    
    Generate a dependence graph and sort.

    TODO: look into using ocamlgraph instead?
*)

open Fstructs
open Callg

module BS = Backed_summary


(** Description of analysis *)
type analysisType = string
let makeAnalysisType (desc : string) : analysisType = desc
let string_of_anaType (t : analysisType) : string = t


(** Analyze a function using the "current" analysis. 
    Does not handle analyses that generate new summaries for new inputs.
    @return true if summary changes. 
*)
type intraProcCompute = funID -> callN -> bool


(** Describe how to "run" the analysis *)
type anaDescriptor = {
  anaTyp : analysisType;
  anaFunc : intraProcCompute;
} 


(* Should abstract construction + usage of these "descriptors" *)



(***** Edge of dependence graph labeled w/ semantics *****)

(** Describe a dependence between current analysis (A) and analysis B *)
type dependence =
    DCur of analysisType
      (** A depends on B for dataflow facts & summary of current function *)
  | DCallees of analysisType
      (** A depends on B for summary of functions called by current function *)
  | DCallers of analysisType
      (** A depends on B for summary of functions that call current function *)


(******* NODE of dependence graph *********)

(** Describe how to run own analysis and acquire own summary data *)
type analysisInfo = {
  summaryDesc : BS.dbManagement;      (** How to get at the summary descriptor *)
  analysisDesc : anaDescriptor;        (** How to run analysis *)
  mutable dependers : analysisType list; (** Analyses depending on this *)
  mutable dependees : dependence list; (** Analyses this depends upon + how *)
}


(***** Full graph of dependencies *****)

type dependenceGraph = (analysisType, analysisInfo) Hashtbl.t

let getInfo graph anaTyp =
  Hashtbl.find graph anaTyp

(** Add fresh analysis info to the dependence graph *)
let addInfo graph anaDesc anaRun anaSumDesc =
  let anaType = makeAnalysisType anaDesc in
  let anaInfo = {
    summaryDesc = anaSumDesc;
    analysisDesc = {
      anaTyp = anaType;
      anaFunc = anaRun;
    };
    dependers = [];
    dependees = [];
  } in
  Hashtbl.add graph anaType anaInfo


let addOutEdge graph info dep =
  info.dependees <- List_utils.addOnce info.dependees dep

let addInEdge graph ana dep =
  match dep with
    DCur targ
  | DCallees targ
  | DCallers targ ->
      try
        let targInfo = getInfo graph targ in
        targInfo.dependers <- List_utils.addOnce targInfo.dependers ana 
      with Not_found ->
        failwith "Dangling dependence edge?!"

(** Add a new analysis dependence for the given [ana] *)          
let addDependence graph ana dep =
  try
    let info = getInfo graph ana in
    addOutEdge graph info dep;
    addInEdge graph ana dep
  with Not_found ->
    failwith "Must add find analysis info prior to adding dependencies"


let listDependees (n : analysisInfo) : analysisType list =
  List.map 
    (fun dep -> match dep with
       DCur x
     | DCallers x
     | DCallees x -> x) n.dependees
   

(** Input graph module for using the SCC services *)      
module DepG = struct
  type key = analysisType
  type node = analysisInfo
  type graph = dependenceGraph

  let getNode = getInfo

  let listSuccs g n = listDependees n
   
  let iter = Hashtbl.iter
 
end

(** Get module for computing SCCs *)
module DepSCCG = Scc.Make(DepG)

type depSccs = DepSCCG.sccDAG

let getSccs graph : depSccs =
  DepSCCG.makeSCCDAG graph


(******** Higher-level operations *******)

(** @return true if the analysis has no dependers *)
let isRoot ana : bool =
  ana.dependers == []


(** List the type of summaries that may be needed (doesn't list which
    functions exactly). TODO: may need to modify to handle more 
    kinds of dependence structures *)
let listNeededSums (graph : dependenceGraph) : BS.dbManagement list =
  Hashtbl.fold (fun _ info curList ->
                  info.summaryDesc :: curList) graph []



(** Super-simple way to sort the analyses for now. Just make one
    pass on the given function, and return true if summaries changed,
    false if they don't change.

    TODO: Should use scc info

    TODO: Should have ways of flushing data that is no longer needed,
    be able to indicate which analyses should expect new summaries,
    etc... *)
let iterAnas funKey funNode (graph : dependenceGraph) : bool =
  let visited = Hashtbl.create 17 in
  (* Do a "bottom-up" traversal *)
  let rec visit anaType anaInfo : bool =
    (* first visit dependencies *)
    let kidsChanged = List.fold_left
      (fun changed neighT ->
         if Hashtbl.mem visited neighT then
           changed
         else
           (Hashtbl.add visited neighT ();
            try 
              let neighI = Hashtbl.find graph neighT in
              (visit neighT neighI) || changed
            with Not_found ->
              failwith "Can't find analysis info"
           )
      ) false (listDependees anaInfo) in
    (* then visit self *)
    (anaInfo.analysisDesc.anaFunc funKey funNode) ||
      kidsChanged
  in
  Hashtbl.fold
    (fun anaT anaI changed ->
       if isRoot anaI then
         (visit anaT anaI) || changed
       else
         changed
    ) graph false


