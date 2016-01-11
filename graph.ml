

(** Simple graph algorithms. @see scc.ml for scc related algorithms. 
    TODO: Just use the ocamlgraph modules? 
    Make this class / object based instead? 
    Each guy can extend its nodes / edges in a different way...
*)



(** Input graph interface *)
module type Graph = sig
  
  type node (** Original graph nodes *)

  type key  (** Key for looking up graph nodes, given original graph --
                must be hashable by standard Hashtbl.hash *)

  type graph (** Original graph *)

  (** @return the node corresponding to the given key
      @raise  Not_found if no such node exists *)
  val getNode : graph -> key -> node
  
  (** @return a list of successor nodes for the given node *)
  val listSuccs : graph -> node -> key list

  (** Iterate through original graph (get pairs of keys and nodes) *)
  val iter : (key -> node -> unit) -> graph -> unit

end


(** Output module of graph algorithms *)
module type S = sig

  type graph  (** Input (external) graph representation *)
  type graph' (** Internal graph representation *)

  val getInternal : graph -> graph'
  val getExternal : graph' -> graph

  val reverse : graph' -> graph'
    (* val getSources  *)
    (* val getSinks *)
    
end


(*************************************************)

module type Dottable = sig
  include Graph

  val label_of_node : key -> node -> string

  val id_of_node : key -> node -> string

  val digraph : bool

  val graph_label : string

end

module type DOT = sig

  type graph

  val dotGraphBuffer : graph -> Buffer.t

  val dotGraphChannel : graph -> out_channel -> unit

end

(*************************************************)

(** Make an intermediate graph based on hashtables *)
module HashGraph (G:Graph) = struct

  type graph = G.graph

  type tempN = {
    (* Assume pre-visit, post-visit numbering is common to many apps *)
    mutable preVisit : int;
    mutable postVisit : int;

    myKey : G.key;
    mutable succKeys : G.key list;
  }

  type graph' = (G.key, tempN) Hashtbl.t
      
  let dummyNum = -1

  (** Make an intermediate graph *)
  let getInternal regG =
    let newG = Hashtbl.create 17 in
    G.iter
      (fun k node ->
         let newNode =
           { preVisit = dummyNum;
             postVisit = dummyNum;
             myKey = k;
             succKeys = G.listSuccs regG node;
           } in
         Hashtbl.add newG k newNode
      ) regG;
    newG

  (** Revert to the original graph. Hmm not actually possible as 
      we do not know its structure *)
  let getExternal myG =
    failwith "Impossible"


  (** Build the reverse/transpose graph *)
  let buildGT (regG: graph) =
    let transG = Hashtbl.create 17 in
    G.iter
      (fun k node ->
         List.iter
           (fun neighK ->
              try 
                let neighNode = Hashtbl.find transG neighK in
                neighNode.succKeys <- k :: neighNode.succKeys
              with Not_found ->
                let neighNode = 
                  { preVisit = dummyNum;
                    postVisit = dummyNum;
                    myKey = neighK;
                    succKeys = [k];
                  } in
                Hashtbl.add transG neighK neighNode
           ) (G.listSuccs regG node)
      ) regG;
    transG

  (** Reset bookkeeping info from the graph *)
  let reset graph =
    ()

end

module MakeDot (D:Dottable) = struct

  type graph = D.graph

  let nodeAttrs key node =
    ("label=\"{" ^ D.label_of_node key node ^ 
       "}\" ")
      
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
      
  let writeHeader outBuff = 
    let gType = if D.digraph then "digraph" else "graph" in
    Buffer.add_string outBuff (gType ^ " " ^ D.graph_label ^ " {\n\n");
    Buffer.add_string outBuff "\tcompound=\"true\";\n";
    Buffer.add_string outBuff "\tranksep=\"1\";\n";
    Buffer.add_string outBuff 
      "\tnode [shape=\"record\", fontname=\"Verdana\"];\n\n";
    Buffer.add_string outBuff "\tfontname=\"Verdana\";\n\n"
      
  
  let writeTrailer outBuff =
    Buffer.add_string outBuff "}\n";
    Buffer.add_string outBuff "\n\n"

  let writeEdges outBuff g =
    D.iter 
      (fun key node -> 
         let id = D.id_of_node key node in
         let neighs = D.listSuccs g node in
         let numNeighs = List.length neighs in
         let _ = List.fold_left 
           (fun index neighKey  ->
              try
                let neighNode = D.getNode g neighKey in
                let neighID = D.id_of_node neighKey neighNode in
                let color = edgeColor numNeighs index in
                Buffer.add_string outBuff ("\t" ^ id ^ " -> " ^ 
                                             neighID ^ 
                                             "[color=\"" ^ color ^ "\"];\n");
                index + 1
              with Not_found ->
                index + 1
           ) 0 neighs in
         Buffer.add_string outBuff "\n"
      ) g

  let writeNodes outBuff g =
    D.iter 
      (fun key node ->
         let attrs = nodeAttrs key node in
         let id = D.id_of_node key node in
         Buffer.add_string outBuff ("\t\t" ^ id ^ " [" ^ attrs ^ "];\n");
      ) g

  (*****************************)

  let dotGraphBuffer g =
    Printf.printf "Dot'ing graph for %s\n" D.graph_label;
    let outBuff = Buffer.create 16 in
    writeHeader outBuff;
    writeNodes outBuff g;
    writeEdges outBuff g;
    writeTrailer outBuff;
    let written = Buffer.length outBuff in
    Printf.printf "Bytes written: %d\n" written;
    outBuff

  let dotGraphChannel g oc =
    let outBuff = dotGraphBuffer g in
    output_string oc (Buffer.contents outBuff)


end
