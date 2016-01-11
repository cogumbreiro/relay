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
    Groups graph nodes into SCCs and creates DAGs based on the SCC grouping
    and the original graph. More specifically, this SCC'ing service
    is provided for callgraphs.

    Look into using ocamlgraph instead?
*)

open Graph
open Logging


(************************************************************
 Generic SCC computations (given interface -- @see graph.ml)
************************************************************)


(** IDs for SCCs *)
type sccID = int

let dummyID = -1

(** Output SCC info *)
module type S = sig
  
  type key

  type graph

  (** SCC super-nodes in an SCC DAG. Each SCC super-node represents
      a set of the original graph nodes *)
  type sccNode = {
    
    (** This SCC super-node's ID *)
    scc_num : sccID;
    
    (** Original nodes represented by this SCC super-node *)
    mutable scc_nodes : key list;
  
    (** sccs with edges into this scc *)
    mutable scc_in : sccID list;
    
    (** sccs adjacent via edges out of this scc *)
    mutable scc_out : sccID list;
  }

  type sccDAG = (sccID, sccNode) Hashtbl.t
      
  (** Create an SCC DAG out of the original graph *)
  val makeSCCDAG : graph -> sccDAG

  (** @return true if the SCC is a root in the SCC DAG *)
  val isRoot : sccNode -> bool

  (** @return true if the SCC is a leaf in the SCC DAG *)
  val isLeaf : sccNode -> bool

    
end


(** Generate the SCC service *)
module Make (G:Graph) = struct

  type key = G.key
  type graph = G.graph

  (** Intermediate scc info for each node in the original graph *)
  type sccInfo = {
    self : key;
    mutable rootNum : sccID;
  }

  type sccNumbering = (key, sccInfo) Hashtbl.t

  type sccNode = {
    scc_num : sccID;
    mutable scc_nodes : key list;
    mutable scc_in : sccID list;
    mutable scc_out : sccID list;    
  }
      
  type sccDAG = (sccID, sccNode) Hashtbl.t
      
  (*** UBER-simple version of SCC discovery algorithm (CLRS 22.5 ver) ***)
   
  (** Temp graph nodes used to calculate SCC numberings, etc. *)
  type tempN = {
    mutable pre : int;
    myKey : key;
    mutable succKeys : key list;
  }

  (** Build a simple temp version of the original graph *)
  let buildG (regG: graph) =
    let newG = Hashtbl.create 17 in
    G.iter
      (fun k node ->
         let newNode =
           { pre = dummyID;
             myKey = k;
             succKeys = G.listSuccs regG node;
           } in
         Hashtbl.add newG k newNode
      ) regG;
    newG

  (** Build the transpose graph *)
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
                  { pre = dummyID;
                    myKey = neighK;
                    succKeys = [k];
                  } in
                Hashtbl.add transG neighK neighNode
           ) (G.listSuccs regG node)
      ) regG;
    transG


  (** Partition the callgraph into SCCs *)
  let getSCCNums (graph:graph) : sccNumbering =
    let numberings = Hashtbl.create 17 in

    let initNumber k n =
      Hashtbl.add numberings k { self = k;
                                 rootNum = dummyID; }
    in  
    
    let curSCC = ref 0 in
    let curNumber = ref 0 in
    let postOrder = ref [] in
    
    let preVisit1 node =
      let num = !curNumber in
      incr curNumber;
      node.pre <- num;
    in
    
    let postVisit1 node =
      postOrder := node.myKey :: !postOrder
    in    
    
    let preVisit2 node =
      let num = !curSCC in
      node.pre <- num
    in
    
    let postVisit2 node =
      ()
    in

    let notVisited node =
      node.pre < 0
    in
    
    let rec visit curG preVis postVis node =
      if notVisited node then begin
        preVis node;
        List.iter 
          (fun neighK ->
             try
               let neighNode = Hashtbl.find curG neighK in
               visit curG preVis postVis neighNode
             with Not_found ->
               ()
          ) node.succKeys;
        postVis node;
      end
    in

    let () = G.iter initNumber graph in
    let g = buildG graph in
    Hashtbl.iter 
      (fun fk n ->
         visit g preVisit1 postVisit1 n
      ) g;
    let gt = buildGT graph in
    List.iter 
      (fun k ->
         try
           let node = Hashtbl.find gt k in
           let sccInfo = Hashtbl.find numberings k in 
           (* Not expecting to not find? *)
           visit gt preVisit2 postVisit2 node;
           incr curSCC;
           sccInfo.rootNum <- node.pre;
         with Not_found ->
           let num = !curSCC in
           incr curSCC;
           let newSCCInfo = 
             {
               self = k;
               rootNum = num;
             } in
           Hashtbl.replace numberings k newSCCInfo
      ) !postOrder;
    numberings
      (* end getSCCNums *)
      
      
  (** Build the SCC DAG (unify all nodes in each SCC) *)
  let makeSCCDAG (graph:graph) : sccDAG =
    let numberings = getSCCNums graph in
    let sccDAG = Hashtbl.create 17 in

    let getSCC (id:sccID) : sccNode = 
      try 
        Hashtbl.find sccDAG id
      with Not_found -> 
        let newSCC = {scc_num = id;
                      scc_nodes = [];
                      scc_in = [];
                      scc_out = [];} in
        Hashtbl.add sccDAG id newSCC;
        newSCC
    in
    
    let addInEdge (curNum:sccID) (neighk:key) : unit = 
      try 
        let neighInfo = Hashtbl.find numberings neighk in
        if (curNum <> neighInfo.rootNum) then
          let neighSCC = getSCC neighInfo.rootNum in
          neighSCC.scc_in <- List_utils.addOnce neighSCC.scc_in curNum
      with Not_found ->
        ()
    in
    
    let addOutEdge (curSCC:sccNode) (neighk:key) : unit =
      try
        let neighInfo = Hashtbl.find numberings neighk in
        if (curSCC.scc_num <> neighInfo.rootNum) then
          curSCC.scc_out <- List_utils.addOnce curSCC.scc_out neighInfo.rootNum
      with Not_found ->
        ()
    in
    
    let addToGraph (k:key) (info:sccInfo) : unit =
      if (info.rootNum == dummyID) then 
        logError "Scc: Node doesn't have an SCC?";
      let curSCC = getSCC info.rootNum in
      try
        let curNode = G.getNode graph k in
        let curSuccs = G.listSuccs graph curNode in
        
        curSCC.scc_nodes <- List_utils.addOnce curSCC.scc_nodes k;
        List.iter (addOutEdge curSCC) curSuccs;
        List.iter (addInEdge curSCC.scc_num) curSuccs; 
      with Not_found ->
        ()
    in
    Hashtbl.iter addToGraph numberings;
    sccDAG
      (* End makeSCCDAG *)


  (** @return true if the SCC is a root in the SCC DAG *)
  let isRoot scc =
    scc.scc_in == []
      
  (** @return true if the SCC is a leaf in the SCC DAG *)
  let isLeaf scc =
    scc.scc_out == []


end

