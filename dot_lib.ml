
(** Library for convert callgraphs, etc. to the graphviz dot format *)

open Fstructs
open Callg
open Scc_cg
open Pretty
open Logging


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

let fontname = "fontname=\"Verdana\""


(** Dot/graphviz utils for callgraphs *)

(******************** Utility functions *********************)
  
let sccLabel scc =
  "scc #" ^ (string_of_int scc.scc_num)
    
let sccID scc = 
  ("cluster_" ^ (string_of_int scc.scc_num))

let writeHeader out_chan = 
  output_string out_chan "digraph callgraph {\n\n";
  output_string out_chan "\tcompound=\"true\";\n";
  output_string out_chan "\tranksep=\"1\";\n";
  output_string out_chan 
    ("\tnode [shape=\"record\", " ^ fontname ^ "];\n\n");
  output_string out_chan ("\t" ^ fontname ^ ";\n\n")
    
let writeTrailer out_chan =
  output_string out_chan "}\n";
  output_string out_chan "\n\n";
  (* Assume this is the end *)
  flush out_chan;
  let written = out_channel_length out_chan in
  logStatusF "Bytes written: %d\n" written

let indirectStyle = " style=\"dashed\" "

let funAttrs fn fid =
  ("label=\"{" ^ fn.name ^ " | " ^ fn.defFile ^ " | " ^ 
     (fid_to_string fid) ^ "}\"")
    (* "}\", tooltip=\"" ^ fn.typ ^ "\"") *)

let disallowedChars = Str.regexp "[^_a-zA-Z0-9]"
    
let sanitize_str str = 
  Str.global_replace disallowedChars "_" str

let funID fn fid =
  let fid_str = sanitize_str (fid_to_string fid) in
  (fn.name ^ "_" ^ fid_str)
    
(********** Micro output **********)

let writeEdges out_chan cg fkey =
  try
    let fnode = FMap.find fkey cg in
    let fid = funID fnode fkey in
    let calleeKeyBools = calleeKeyBools fnode in
    let numCallees = List.length calleeKeyBools in
    let _ = List.fold_left 
      (fun index (neighK, indir)  ->
         try
           let neighNode = FMap.find neighK cg in
           let neighID = funID neighNode neighK in
           let color = edgeColor numCallees index in
           let style = if indir then indirectStyle else "" in
           output_string out_chan 
             ("\t" ^ fid ^ " -> " ^ neighID ^ 
                "[color=\"" ^ color ^ "\"" ^ style ^ "];\n");
           index + 1
         with Not_found -> 
           index + 1
      ) 0 calleeKeyBools in
    output_string out_chan "\n"
  with Not_found -> ()

let callsDirect n1 k2 : bool =
  List.exists (fun callee -> match callee with 
                 CDirect (_, k2') -> k2 = k2' | CIndirect _ -> false) 
    (calleeDetail n1)
    
let writeEdge out_chan cg (k1, n1, k2, n2) =
  let fid = funID n1 k1 in
  let sid = funID n2 k2 in
  let color = randomEdgeColor () in
  let style = if not (callsDirect n1 k2) then indirectStyle else "" in 
  output_string out_chan ("\t" ^ fid ^ " -> " 
                          ^ sid ^ "[color=\"" ^ color ^ "\"" 
                          ^ style ^ "];\n");
  output_string out_chan "\n"

let writeNodes out_chan cg iterNodes =
  iterNodes 
    (fun fkey -> try
       let fnode = FMap.find fkey cg in
       let fattrs = funAttrs fnode fkey in
       let fid = funID fnode fkey in
       output_string out_chan ("\t\t" ^ fid ^ " [" ^ fattrs ^ "];\n");
     with Not_found ->
       () )

let writeSccHeader out_chan scc = 
  let sccL = sccLabel scc in
  let sccID = sccID scc in 
  output_string out_chan ("\tsubgraph " ^ sccID ^ 
                            " {\n\t\tlabel = \"" ^ sccL ^ "\";\n");
  output_string out_chan ("\t\tgraph [bgcolor=\"#cccccc\"];\n")
    

(********** Macro output ***********)

(** Write all nodes in the cg and all of its edges, without SCC clustering *)
let writeWholeCg cg out_chan =
  let iterNodes func = FMap.iter (fun k n -> func k) cg in
  let wEdges = writeEdges out_chan cg in
  writeHeader out_chan;
  writeNodes out_chan cg iterNodes;
  iterNodes wEdges;
  writeTrailer out_chan


(** Write the chosen nodes and all of their out-edges *)
let writeNodesWithEdges iterNodes cg out_chan =
  let wEdges = writeEdges out_chan cg in
  writeHeader out_chan;
  writeNodes out_chan cg iterNodes;
  iterNodes wEdges;
  writeTrailer out_chan
    

(** Write the chosen nodes and chosen edges *)
let writeNodesEdges iterNodes iterEdges cg out_chan =
  let wEdge = writeEdge out_chan cg in
  writeHeader out_chan;
  writeNodes out_chan cg iterNodes;
  iterEdges wEdge;
  writeTrailer out_chan


(** Write the chosen sccs *)
let writeSccs (iterScc : (scc -> unit) -> unit) cg out_chan = 
  let iterNodes scc func = 
    FSet.iter func scc.scc_nodes 
  in
  let wEdges = writeEdges out_chan cg in
  let wSccH = writeSccHeader out_chan in
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
  
  writeTrailer out_chan


(********** Find paths in call graph **********)

let eqKey a b = compare a b == 0

module ESet = Set.Make(
  struct
    type t = funID * callN * funID * callN
    let compare (k1,_, k1', _) (k2, _, k2', _) =
      let f = compare k1 k2 in
      if (f == 0) then compare k1' k2'
      else f
  end)


(** Find all callpaths in the [cg] between the [sources] and the [sinks] *)
let findPaths cg sources sinks : (FSet.t * ESet.t) = 
  let visited = Hashtbl.create 101 in
  let neededNodes = Hashtbl.create 101 in
  let neededEdges = Hashtbl.create 101 in
  let curPath = ref [] in
  
  (* Add the current path assuming it ends at lastFunc *)
  let addAPath lastFunc =
    let finalPath = 
      if (FMap.mem lastFunc cg) then lastFunc :: !curPath
      else !curPath
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
          ) fst tl in ()
    | _ -> ()
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
              if (List.exists (eqKey neighK) sinks) then addAPath neighK
              else addPaths neighK
           ) (calleeKeys node);
         curPath := List.tl !curPath;
       with Not_found -> ()
      );
    end
  in
  (* Add any paths from any of the sources to any of the sinks *)
  List.iter addPaths sources;
  (Hashtbl.fold 
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
     ) neededEdges ESet.empty)

(** Convert a list of function keys to (key, node) pairs *)
let keysToKeyedNodes cg keys =
  List.map (fun k -> (k, FMap.find k cg)) keys

(** Convert a list of keys and node pairs to edges *)
let knToEdges kns =
  let _, res = List.fold_left 
    (fun (prev, cur) cn ->
       match prev with 
         None -> (Some cn, cur)
       | Some (pn) -> (Some cn, (pn, cn) :: cur)) (None, []) kns in
  List.rev res

(** Return int option where the int represents the max fan-out of an
    indirect call for the function prevNode *)
let callsIndirect prevNode k =
  List.fold_left 
    (fun maxI calleeT -> 
       match calleeT with 
         CDirect (pp, x) -> maxI 
       | CIndirect (pp, ks) -> 
           if List.mem k ks then
             let curMax = match maxI with None -> 0 | Some i -> i in
             Some (max curMax (List.length ks))
           else maxI
    ) None (calleeDetail prevNode)
    
let printPath (path : (funID * callN) list) =
  let _, doc = List.fold_left
    (fun (prev, curDoc) (cK, cN) ->
       match prev with
         None -> (Some (cK, cN), curDoc ++ dprintf "%s\n" cN.name)
       | Some(pK, pN) ->
           let prefix = (match callsIndirect pN cK with
                           None -> "(d)"
                         | Some n -> "(i " ^ string_of_int n ^ ")")
           in
           (Some (cK, cN), curDoc ++ dprintf "  -> %s %s\n" prefix cN.name)
    ) (None, nil) path in
  logStatusD (text "Path len: " ++ doc ++ 
                dprintf "(%d)\n" (List.length path))


let printModuleCrosses path =
  let count = ref 0 in
  let checkIfCrossed prevNode cur curK =
    (* if (Filename.dirname prevNode.defFile <> 
       Filename.dirname cur.defFile) then
    *)
    match callsIndirect prevNode curK with
      None -> ()
    | Some choices -> 
        Printf.printf "%s (%s) --(%d)--> %s (%s) \n" 
          prevNode.name prevNode.defFile choices cur.name cur.defFile;
        incr count;
  in
  Printf.printf "Indirect crossings: \n";
  ignore 
    (List.fold_left
       (fun prev (cK, cN) ->
          (match prev with 
             None -> Some (cK, cN)
           | Some (pK, pN) -> checkIfCrossed pN cN cK; Some (cK, cN)
          ); 
       ) None path);
  Printf.printf "(%d)\n\n" !count


(** Convert a path to a jumble of nodes and edges *)
let pathToNodesEdges path =
  let neededNodes = List.fold_left 
    (fun curSet (k, n) ->
       FSet.add k curSet
    ) FSet.empty path in
  let neededEdges, _ = List.fold_left
    (fun (curSet, kn1Opt) (k2, n2) ->
       match kn1Opt with
         None -> (curSet, Some(k2, n2))
       | Some (k1, n1) ->
           (ESet.add (k1, n1, k2, n2) curSet, Some (k2, n2))
    ) (ESet.empty, None) path
  in
  neededNodes, neededEdges


(** Find shortest path in [cg] between the [source] and the [sink],
    assuming edge weight of 1 throughout (gonna do BFS)... *)
let findShortestPath cg source sink : (funID * callN) list = 
  let backEdges = Hashtbl.create 101 in
  let visited = Hashtbl.create 101 in
  let addEdge node next =
    if Hashtbl.mem backEdges next then ()
    else Hashtbl.add backEdges next node
  in

  let worklist = Queue.create () in
  let found = ref false in

  (* Do one BFS to find how many hops needed *)
  let findShortestDistance () =
    Hashtbl.clear visited;
    Hashtbl.clear backEdges;
    Queue.add source worklist;
    while (not (Queue.is_empty worklist) && not !found) do
      let cur = Queue.take worklist in
      (try
         let node = FMap.find cur cg in
         List.iter
           (fun neighK ->
              if compare neighK sink == 0 then begin
                addEdge cur neighK;
                found := true
              end else if Hashtbl.mem visited neighK then ()
              else begin (* queue it up *)
                Hashtbl.add visited neighK ();
                addEdge cur neighK;
                Queue.add neighK worklist;
              end
           ) (List_utils.shuffleList (calleeKeys node)); 
         (* randomize it some *)
       with Not_found -> () );
    done;
  in
  
  (* Get the actual path *)
  let curPath = ref [] in
  let rec getPath key =
    let next = Hashtbl.find backEdges key in
    curPath := next :: !curPath;
    if next == source then ()
    else getPath next
  in
  findShortestDistance ();
  Hashtbl.clear visited;
  curPath := [sink];
  try
    getPath sink;
    keysToKeyedNodes cg !curPath
  with Not_found ->
    []


(** Find the shortest path and convert it to a graph *)
let graphShortestPath cg source sink : (FSet.t * ESet.t) =
  let path = findShortestPath cg source sink in
  (* Print the path while it's still in a list (linear) *)
  printPath path;
  printModuleCrosses path;
  pathToNodesEdges path

