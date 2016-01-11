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


(** Groups callgraph nodes into SCCs, and creates a DAG out of the 
    SCCs & the callgraph
*)

open Callg
open Fstructs
open Readcalls

module L = Logging


let cgFile = ref ""

let setCGFile (fname:string) = 
  cgFile := fname


(*********************************************************************)

(* Per call node info for finding SCC structure *)
type sccInfo = {
  self : fKey;

  (* root for this node's component *)
  mutable rootNum : int;
    
  (* visit status & number: is -1 if not visited *)
  mutable number : int;
  
  (* true iff this node's component is known *)
  mutable inComponent : bool;
}

type sccMap = sccInfo FMap.t

(* global number for visiting nodes *)
let curNumber = ref 0

let tempStack = Stack.create ()

(* current sccData *)
let sccData = ref FMap.empty


(* return true if the node should be included in the scc graph *)
let filterNode (n : simpleCallN) : bool =
    n.hasBody

(* Initialize the sccData Map *)
let initSCCData (k:FMap.key) (n : simpleCallN) : unit =
  sccData := FMap.add k {self = k;
                         rootNum = -1;
                         number = -1;
                         inComponent = false} !sccData
    
(* Clear sccData Map *)
let clearSCCData (_:unit) = 
  curNumber := 0;
  sccData := FMap.empty;
  Stack.clear tempStack



(*** UBER-simple version of SCC discovery algorithm (CLRS 22.5 ver) *)

type tempN = {
  mutable pre : int;
  myKey : fKey;
  mutable succKeys : fKey list;
}

(* Build a simple version of the call graph *)
let buildG (cg: simpleCallG) =
  FMap.fold 
    (fun fk node curG ->
       let newNode = 
         { pre = -1;
           myKey = fk;
           succKeys = node.callees;
         } in
       FMap.add fk newNode curG
    ) cg FMap.empty

(* Build the transpose graph from the original graph *)
let buildGT regG =
  FMap.fold
    (fun fk node curG ->
       List.fold_left 
         (fun g neighK ->
            try 
              let neighNode = FMap.find neighK g in
              neighNode.succKeys <- fk :: neighNode.succKeys;
              g
            with Not_found ->
              let neighNode = 
                { pre = -1;
                  myKey = neighK;
                  succKeys = [fk];
                } in
              FMap.add neighK neighNode g
         ) curG node.succKeys
    ) regG FMap.empty


(** Partition the callgraph into SCCs *)
let findSCC (cg: simpleCallG) : unit =

  let curSCC = ref 0 in

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

  let rec visit curG preVis postVis node =
    if (node.pre < 0) then begin
      (* not visited *)
      preVis node;
      List.iter 
        (fun neighK ->
           try
             let neighNode = FMap.find neighK curG in
             visit curG preVis postVis neighNode
           with Not_found ->
             ()
        ) node.succKeys;
      postVis node;
    end
  in
  let g = buildG cg in
  FMap.iter 
    (fun fk n ->
       visit g preVisit1 postVisit1 n
    ) g;
  let gt = buildGT g in
  List.iter 
    (fun fk ->
       try
         let node = FMap.find fk gt in
         let sccInfo = FMap.find fk !sccData in
         visit gt preVisit2 postVisit2 node;
         incr curSCC;
         sccInfo.rootNum <- node.pre;
       with Not_found ->
         let num = !curSCC in
         incr curSCC;
         let newSCCInfo = 
           {
             self = fk;
             rootNum = num;
             number = -1;
             inComponent = false;
           } in
         sccData := FMap.add fk newSCCInfo !sccData 
    ) !postOrder


(*********************************************************************)
(* SCC-level call graph                                              *)
  
type sccID = int

(* Node for creating a call graph at the SCC level *)
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

let isRoot scc =
  IntSet.is_empty scc.scc_callers

let isLeaf scc =
  IntSet.is_empty scc.scc_callees


(* Unify all the nodes in each SCC to make one big SCC DAG *)
let makeSCCGraph (sccD:sccMap) (cg:simpleCallG)
    : sccGraph =
  let curGraph = ref IntMap.empty in
  
  let getSCC (num:int) : scc = 
    try 
      IntMap.find num !curGraph   
    with Not_found -> 
      let newSCC = {scc_num = num;
                    scc_nodes = FSet.empty;
                    scc_callees = IntSet.empty;
                    scc_callers = IntSet.empty;} in
      curGraph := IntMap.add num newSCC !curGraph;
      newSCC
  in

  let addToCallers (curNum:int) (neighk:fKey) : unit = 
    try 
      let neighInfo = FMap.find neighk sccD in
      if (curNum <> neighInfo.rootNum) then
        let neighSCC = getSCC neighInfo.rootNum in
        neighSCC.scc_callers <- IntSet.add curNum neighSCC.scc_callers
    with Not_found ->
      (* 
         L.logError ("Scc.addToCallers can't find SCC label for " ^
                      (string_of_nodeKey neighk) ^ "\n");
      *)
      ()
  in

  let addToCallees (curSCC:scc) (neighk:fKey) : unit =
    try
      let neighInfo = FMap.find neighk sccD in
      if (curSCC.scc_num <> neighInfo.rootNum) then
        curSCC.scc_callees <- IntSet.add neighInfo.rootNum curSCC.scc_callees
    with Not_found ->
      (*
      L.logError ("Scc.addToCallees can't find SCC label for " ^
                      (string_of_nodeKey neighk) ^ "\n");
      *)
      ()
  in

  let addToGraph (fk:fKey) (info:sccInfo) : unit =
    let curSCC = getSCC info.rootNum in
    try
      let curNode = FMap.find fk cg in
      let curSuccs = curNode.callees in
      
      curSCC.scc_nodes <- FSet.add fk curSCC.scc_nodes;
      List.iter (addToCallees curSCC) curSuccs;
      List.iter (addToCallers curSCC.scc_num) curSuccs; 
    with Not_found ->
      L.logError ("Scc.addToGraph can't find SCC label for fid: " ^
                      (string_of_fkey fk));
  in
  
  FMap.iter addToGraph sccD;
  !curGraph
  

(* Get a call graph in terms of SCCs *)
let getSCCGraph (cg:simpleCallG) : sccGraph =
  clearSCCData ();
  let smallerCG = 
    FMap.fold 
      (fun k n curCG ->
         if (filterNode n) then
           FMap.add k n curCG
         else
           curCG
      ) cg FMap.empty in
  FMap.iter initSCCData smallerCG;
  findSCC smallerCG;
  let sccg = makeSCCGraph !sccData smallerCG in
  clearSCCData ();
  sccg
  
(* Get a hashtable of reference counts based on the callgraph and a list of
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
    


let pruneUnreached (sccCG:sccGraph) (reachableCG : FSet.t) : sccGraph =
  IntMap.fold 
    (fun sccK sccN newSCCCG ->
       if not (FSet.is_empty (FSet.inter reachableCG sccN.scc_nodes)) then
         IntMap.add sccK sccN newSCCCG
       else 
         newSCCCG
    ) sccCG IntMap.empty
