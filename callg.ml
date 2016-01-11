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

open Pretty 
open Logging
open Cil

(** Make all callgraphs capable of being context sensitive *)

type funID = Summary_keys.sumKey
let compareFunID a b = Summary_keys.compareSumKey a b
let fid_to_string a = Summary_keys.string_of_sumKey a
let fid_of_string s = Summary_keys.sumKey_of_string s

let dummyFID = (-12345, "dummy")

(** Use this to project out context-sensitivity, or use it to get the
    original function id to get the varinfo / cfg *)
let fid_to_fkey id = Summary_keys.fkey_of_sumKey id

(** Make up a context-free id *)
let fkey_to_fid fkey = Summary_keys.inputFreeSumKey fkey
  
type callTarget = 
    CDirect of prog_point * funID
  | CIndirect of prog_point * funID list

let getPPOfCall call =
  match call with
    CDirect (pp, _)
  | CIndirect (pp, _) -> pp

let compareTargets call1 call2 =
  comparePP (getPPOfCall call1) (getPPOfCall call2)

type callN = {
  name: string;
  typ: string;
  mutable defFile: string;
  mutable hasBody : bool;
  mutable ccallees: callTarget list;
  mutable ccallers : funID list;
}

let addIndirectTarg funID targs =
  List_utils.insertSort compareFunID funID targs

let mergeIndirectTargs targs1 targs2 = 
  List_utils.mergeUnique compareFunID targs1 targs2

exception MergeCalleeMismatch

let mergeCallTypes c1 c2 =
  match c1, c2 with
    CDirect (_, _), CDirect (_, _) -> c1
  | CIndirect (pp, c1), CIndirect (_, c2) ->
      CIndirect (pp, mergeIndirectTargs c1 c2)
  | _, _ -> raise MergeCalleeMismatch

let addCallee call callees =
  List_utils.insertSortCombine compareTargets mergeCallTypes call callees

let mergeCallees callees1 callees2 =
  List_utils.mergeUniqueCombine compareTargets mergeCallTypes callees1 callees2

module CompKey = struct
  type t = funID
  let compare a b = compareFunID a b
end

module FMap = Map.Make (CompKey)
module FSet = Set.Make (CompKey)
module FBSet = Set.Make 
  (struct
     type t = funID * bool
     let compare (k1, b1) (k2, b2) = 
       if b1 == b2 then compareFunID k1 k2
       else if b1 then 1 else -1
   end)

(* a call graph is a mapping from function key (see Fstructs.ml) to
   the node which describes that function's call structure *)
type callG = callN FMap.t


(************* File interface info ***************)

(* TODO: just use the sexp libraries to marshal this in and out? *)

(* Format is:
   
   File <- NodeDetail\n...
   NodeDetail <- defFile $ Node $ NodeCallees
   Node <- Name : Type : Key
   NodeCallees <- [] | NodeCallee $ NodeCallees
   NodeCallee <- false : pp { Node } | true : pp { IndirectList }
   IndirectList <- [] | Node / IndirectList
*)

let delim_0 = "$"
let delim_1 = ":"
let delim_2 = "/"
let delim_open = "{"
let delim_close = "}"

let splitter_0 = Str.split_delim (Str.regexp ("[" ^ delim_0 ^ "]"))
let splitter_1 = Str.split_delim (Str.regexp ("[" ^ delim_1 ^ "]"))
let splitter_2 = Str.split_delim (Str.regexp ("[" ^ delim_2 ^ "]"))
let splitter_targets = Str.split
  (Str.regexp ("[" ^ delim_open ^ delim_close ^ "]"))
  

(******* Implementation ********)

let emptyCG = FMap.empty
  
(** Project out only the keys of the callees *)
let calleeKeys callN = 
  let keySet = List.fold_left 
    (fun cur t -> match t with 
       CDirect (_, k) -> FSet.add k cur
     | CIndirect (_, ks) -> 
         List.fold_left (fun cur k -> FSet.add k cur) cur ks
    ) FSet.empty callN.ccallees in
  FSet.elements keySet

let calleeKeyBools callN =
  let keySet = List.fold_left 
    (fun cur t -> match t with 
       CDirect (_, k) -> FBSet.add (k, false) cur
     | CIndirect (_, ks) -> 
         List.fold_left (fun cur k -> FBSet.add (k, true) cur) cur ks
    ) FBSet.empty callN.ccallees in
  FBSet.elements keySet

let calleeDetail callN =
  callN.ccallees

let callerKeys callN =
  callN.ccallers

let callTargsAtPP cg curFun pp =
  if cg = emptyCG then 
    failwith "callTargsAtPP given empty callgraph"
  else try
    let n = FMap.find curFun cg in
    let callTarg = List.find 
      (fun x -> match x with
         CDirect (pp2, _) | CIndirect (pp2, _) -> pp = pp2) (calleeDetail n)
    in
    match callTarg with
      CDirect (_, x) -> [x]
    | CIndirect (_, xs) -> xs
  with Not_found ->
    logErrorF "callTargsAtPP no call info for %s at %s\n"
      (fid_to_string curFun) (string_of_pp pp);
    []

(************** simple printing ****************)

let d_funcset cg fset =
  let d = 
    (seq_to_doc 
       line 
       FSet.iter
       (fun fkey ->
          try 
            let node = FMap.find fkey cg in
            dprintf "%s(%s)" node.name (fid_to_string fkey)
          with Not_found -> text (fid_to_string fkey)) fset
       nil) ++ line in
  d ++ dprintf "Total %d\n\n" (FSet.cardinal fset)

(************* Common Queries **************)

let filterNodes filter cg = 
  FMap.fold (fun funID fn cur -> 
               if filter funID fn then FSet.add funID cur else cur
            ) cg FSet.empty
    
let getMatchingIDs cg fkeys = 
  let ids = 
    filterNodes (fun funID fn ->
                   let fk = fid_to_fkey funID in 
                   List.exists (fun fk2 -> fk = fk2) fkeys) cg in
  FSet.elements ids


let getRoots cg : (funID * callN) list =
  FMap.fold (fun fk node res -> 
               if (node.ccallers == [] && node.hasBody) 
               then (fk, node) :: res
               else res) cg []
    

(** Return the set of functions reachable from the given roots *)
let getReachableFunctions cg (roots:FSet.t) : FSet.t =
  let reached = ref FSet.empty in
  let rec visit_children_of fkey =
    try 
      let node = FMap.find fkey cg in
      reached := FSet.add fkey !reached;
      List.iter
        (fun child ->
           if not (FSet.mem child !reached) then
             visit_children_of child
        ) (calleeKeys node);
      (* if we don't have the body for a function, there will be
         no entry for it in the call graph *)
    with Not_found ->
      ()
  in
  FSet.iter (fun fkey -> visit_children_of fkey) roots;
  !reached


(** Return the list of call graph roots that reach the given set of funcs *)
let rootsThatReach cg (toReach:FSet.t) : FSet.t =
  let visited = ref FSet.empty in
  (* slow reachability check (restarts for each root...) *)
  let rec reaches fk =
    visited := FSet.add fk !visited;
    if FSet.mem fk toReach then
      true
    else (
      try
        let fnode = FMap.find fk cg in
        List.exists 
          (fun calleeK ->
             if not (FSet.mem calleeK !visited) then reaches calleeK
             else false
          ) (calleeKeys fnode)
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


(*********************** Slicing the cg ********************)

let sliceFList flist keepers =
  List.filter (fun fk -> FSet.mem fk keepers) flist

let sliceCallees callees keepers useIndirect =
  List.fold_left 
    (fun cur c -> 
       match c with 
         CDirect (pp, k) -> 
           if FSet.mem k keepers 
           then addCallee (CDirect (pp, k)) cur
           else cur
       | CIndirect (pp, ks) -> 
           if useIndirect then 
             (match sliceFList ks keepers with
                [] -> cur
              | l -> addCallee (CIndirect (pp, l)) cur)
           else cur
    ) [] callees
    
let sliceNode callNode keepers useIndirect =
  { callNode with
      ccallees = sliceCallees callNode.ccallees keepers useIndirect ;
      ccallers = sliceFList callNode.ccallers keepers;
  }

let sliceCg cg keepers useIndirect =
  FMap.fold 
    (fun fk n cur -> 
       if not (FSet.mem fk keepers) then cur
       else FMap.add fk (sliceNode n keepers useIndirect) cur
    ) cg FMap.empty


(** Modify the given callgraph to complete connections. That is, fill 
    the caller list (needed to wait for all callee edges to be loaded) *)
let completeCG partialCG : unit =
  (* Tell neighbors you're a caller *)
  let informNeighbors curFK node =
    (* fill caller fields of neighbors *)
    List.iter 
      (fun fk ->
         try
           let tempNode = FMap.find fk partialCG in
           let newCallers = List_utils.addOnce tempNode.ccallers curFK in
           tempNode.ccallers <- newCallers;
         with Not_found ->
           ()
      ) (calleeKeys node);
  in
  FMap.iter informNeighbors partialCG


let prePostNums cg : (int * int) FMap.t =
  let roots = getRoots cg in
  let curNum = ref 0 in
  let nextNum () = 
    let x = !curNum in
    incr curNum;
    x
  in
  let rec visit curNums (fID, fnode) =
    if FMap.mem fID curNums then curNums
    else 
      let pre = nextNum () in
      let curNums = FMap.add fID (pre, -1) curNums in
      let curNums = List.fold_left 
        (fun curNums neighID ->
           try
             let neighN = FMap.find neighID cg in
             visit curNums (neighID, neighN)
           with Not_found ->
             curNums
        ) curNums (calleeKeys fnode) in
      let post = nextNum () in
      FMap.add fID (pre, post) curNums
  in
  (* One pass from roots, another pass for the left-overs *)
  let curNums = List.fold_left visit FMap.empty roots in
  FMap.fold (fun fid fnode curNums -> visit curNums (fid, fnode)) cg curNums
    
(*************** File IO ***************)
    
exception CorruptCGFile

let warnCorrupt line = 
  logError ("Corrupt callee list in callg file: " ^ line);
  raise CorruptCGFile

let combineNodeInfo n1 n2 = 
  try 
    { n1 with
        ccallees = mergeCallees n1.ccallees n2.ccallees;
        ccallers = List_utils.union n1.ccallers n2.ccallers; 
        hasBody = n1.hasBody or n2.hasBody; }
  with MergeCalleeMismatch ->
    logErrorF "MergeCalleeMismatch for %s\n" n1.name;
    raise MergeCalleeMismatch
    
(** Read the call graph data from the given filename "readFrom" *)
let readCalls (readFrom:string) : callG =
  let curCG = ref FMap.empty in
  
  let rec makeSuccs (nodeStrs : string list) curCallees =
    match nodeStrs with
      [] -> curCallees
    | h :: t ->
        (match splitter_1 h with 
           [funcName ; typString ; funID] ->
             let fid = fid_of_string funID in
             (* check if there's a node for the neighbor yet *)
             (try
                let _ = FMap.find fid !curCG in
                () (* fill in caller info later *) 
              with Not_found -> 
                let newNode = 
                  {name = funcName;
                   defFile = "";
                   typ  = typString;
                   ccallees = [];
                   ccallers = []; (* fill in caller info later *)
                   hasBody = false;
                  } in
                curCG := FMap.add fid newNode !curCG
             );
             makeSuccs t (addIndirectTarg fid curCallees)
         | _ -> warnCorrupt h
        )
  in

  let parseIndirect indyStr =
    match splitter_1 indyStr with
      [isIndy; ppStr] ->
        let ind = bool_of_string isIndy in
        (pp_of_string ppStr, ind)
    | _ -> warnCorrupt indyStr
  in
  
  let rec makeCallees callees curCallees =
    match callees with
      [] -> curCallees
    | h :: t ->
        (match splitter_targets h with
           [indyStr ; targets] ->
             let pp, isIndir = parseIndirect indyStr in
             let targets = makeSuccs (splitter_2 targets) [] in
             let newCalless = 
               if isIndir then 
                 addCallee (CIndirect (pp, targets)) curCallees
               else 
                 (match targets with
                    [x] -> addCallee (CDirect (pp, x)) curCallees
                  | _ -> 
                      logError ("Direct call targets != 1? " ^ h);
                      curCallees)
             in
             makeCallees t newCalless
         | _ -> warnCorrupt h
        )
  in
  
  let inFile = open_in readFrom in
  try while (true) do 
    let curLine = input_line inFile in
    match splitter_0 curLine with
      fileName :: myNode :: callees -> begin
        match splitter_1 myNode with
          [ funcName ; typString ; funID ] -> begin
            let fid = fid_of_string funID in
            let newCallees = makeCallees callees [] in
            let called = 
              try
                (* Merge if there's an existing entry *)
                let curNode = FMap.find fid !curCG in
                mergeCallees curNode.ccallees newCallees
              with 
                Not_found -> newCallees
              | MergeCalleeMismatch ->
                  logErrorF "MergeCalleeMismatch for %s (%s)\n" funcName funID;
                  raise MergeCalleeMismatch
            in

            (* Just replace it all (already merged callees)*)
            curCG := FMap.add fid
              ({name = funcName;
                defFile = fileName;
                typ  = typString;
                ccallees = called;
                ccallers = [];
                hasBody = true;
               }) !curCG
          end
        | _ -> warnCorrupt curLine
      end
    | _ -> warnCorrupt curLine 
  done;
    failwith "Finished Cg w/ out reaching End_of_file?"
  with End_of_file ->
    logStatus ("Reached end of " ^ readFrom ^ " call graph file");
    logStatus ("Num funcs: " ^ 
                 (string_of_int (Stdutil.mapSize !curCG FMap.fold)));
    flushStatus ();
    close_in inFile;
    (* Add the backlinks (caller edges) *)
    let () = completeCG !curCG in
    !curCG
      
(************************************************************)


(** Write a function's callgraph info to the outFile
    @see callg.ml for the format *)
let writeFuncCalls cg (outFile:out_channel) curKey curNode =
  let writeNode key node =
    output_string outFile 
      (node.name ^ delim_1 ^ node.typ ^ delim_1 ^ (fid_to_string key))
  in
  let writeIsIndirect pp b =
    output_string outFile (delim_0 ^ string_of_bool b);
    output_string outFile (delim_1 ^ string_of_pp pp)
  in
  let writeTargets keys =
    let isFirst = ref true in
    output_string outFile delim_open;
    List.iter
      (fun succID ->
         try 
           let succNode = FMap.find succID cg in
           if !isFirst then isFirst := false
           else output_string outFile delim_2;
           writeNode succID succNode
         with Not_found ->
           logError ("Dumpcalls: No info for: " ^ (fid_to_string succID))
      ) keys;
    output_string outFile delim_close;
  in
  let writeSucc succ =
    match succ with
      CDirect (pp, k) -> writeIsIndirect pp false; writeTargets [k];
    | CIndirect (pp, ks) -> writeIsIndirect pp true; writeTargets ks
  in
  begin
    (* only write it if we have the body *)
    if (curNode.hasBody) then
      begin
        (* File that has function definition *)
        output_string outFile curNode.defFile;
        output_string outFile delim_0;

        (* Function name w/ signature *)
        writeNode curKey curNode;
        
        (* Successor names and sigs *)
        List.iter writeSucc (calleeDetail curNode);
        output_string outFile "\n";
      end
  end


let writeSomeCalls outFile cg =
  FMap.iter (writeFuncCalls cg outFile) cg

let projectContext funID =
  let fkey = fid_to_fkey funID in
  fkey_to_fid fkey

(** Convert to context-insensitive graph *)
let consSensNodeToInsens node = 
  let convertConsSensKeys ks =
    List.fold_left
      (fun cur id ->
         List_utils.addOnce cur (projectContext id)
      ) [] ks
  in
  { name = node.name;
    typ = node.typ;
    defFile = node.defFile;
    hasBody = node.hasBody;
    ccallees = List.fold_left
      (fun cur call ->
         match call with
           CDirect (pp, x) -> 
             addCallee (CDirect (pp, (projectContext x))) cur
         | CIndirect (pp, ks) -> 
             addCallee (CIndirect (pp, convertConsSensKeys ks)) cur
      ) [] node.ccallees;
    ccallers = convertConsSensKeys node.ccallers; }
    

let consSensToInsens consG =
  FMap.fold 
    (fun funID nodeInfo cur ->
       let funID = projectContext funID in
       let node = consSensNodeToInsens nodeInfo in
       try 
         let old = FMap.find funID cur in
         FMap.add funID (combineNodeInfo old node) cur
       with Not_found ->
         FMap.add funID node cur
    ) consG FMap.empty


(************************************************************)
module FH = Hashtbl.Make 
  (struct
    type t = funID
    let hash x = Hashtbl.hash x
    let equal a b = compareFunID a b == 0
   end)
