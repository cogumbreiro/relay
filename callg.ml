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

(** Callgraph structures and simple reachability queries, misc. operations *)

open Fstructs

module L = Logging

(* a minimal callnode *)
type simpleCallN = {

  (* name of the function *)
  name: string;
  
  (* string describing the function's type *)
  typ: string;
  
  (* file where function is defined *)
  defFile: string;

  (* keys for functions this calls *)
  mutable callees: fKey list;

  (* keys for functions that call this function *)
  mutable callers : fKey list;

  (* true if we've seen the body for this function *)
  mutable hasBody : bool;
}


(* a call graph is a mapping from function key (see Fstructs.ml) to
   the node which describes that function's call structure *)
type simpleCallG = 
    (simpleCallN) FMap.t

      

(************* File interface info ***************)

let topDelim = "$"

let delim = ":"

let indirect_name = "*"


(************* Common Queries **************)

let getRoots cg : (fKey * simpleCallN) list =
  FMap.fold (fun fk node res -> 
               if (node.callers == [] && node.hasBody) 
               then (fk, node) :: res
               else res) cg []
                 

(************* Reachability test *****************)

(** Return the set of functions reachable from the given roots *)
let getReachableFunctions (cg:simpleCallG) (roots:FSet.t) : (FSet.t * int) =
  let reached = ref FSet.empty in
  let no_body = ref FSet.empty in
  let rec visit_children_of fkey =
    try 
      let node = FMap.find fkey cg in
      reached := FSet.add fkey !reached;
      List.iter
        (fun child ->
           if not (FSet.mem child !reached) then
              visit_children_of child
        ) node.callees;
        
    (* if we don't have the body for a function, there will be
       no entry for it in the call graph *)
    with Not_found ->
      no_body := FSet.add fkey !no_body;
      ()
  in
  FSet.iter (fun fkey -> visit_children_of fkey) roots;
  (!reached, FSet.cardinal !no_body)



(** Return the list of call graph roots that reach the given set of funcs *)
let rootsThatReach (cg:simpleCallG) (toReach:FSet.t) : FSet.t =
  let visited = ref FSet.empty in
  (* slow reachability check (restarts for each root...) *)
  let rec reaches (fk : fKey) =
    visited := FSet.add fk !visited;
    if FSet.mem fk toReach then
      true
    else (
      try
        let fnode = FMap.find fk cg in
        List.exists 
          (fun calleeK ->
             if not (FSet.mem calleeK !visited) then
               reaches calleeK
             else
               false
          ) fnode.callees
      with Not_found ->
        false
    )
  in
  let roots = getRoots cg in
  List.fold_left
    (fun cur (fk, fn) ->
       visited := FSet.empty;
       if reaches fk then (
         FSet.add fk cur
       )
       else cur
    ) FSet.empty roots
      
(** Test what happens if we try to treat unreached roots as roots as well? *)
