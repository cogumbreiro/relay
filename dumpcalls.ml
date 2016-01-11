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
   Dumps the call graph info of a C file to a file based on the
   chosen function pointer analysis (@see Alias)

   ASSUMES: Alias module is initialized (called initSettings, setRoot)
   ASSUMES: Cilinfos module is initialized (called reloadRanges)
 *)


open Cil
open Sys
open Pretty
open Filetools
open Fstructs
open Callg
open Stdutil

module CILCG = Mycallgraph

module D = Cildump
module A = Alias
module L = Logging

(** The directory in which the call graph file will be stored.
    Must set this before using the module *)
let dumpTo = ref ""

let setDumpTo (p:string) =
  dumpTo := p


(** The file currently being processed *)
let curFile = ref ""

let setCurFile (f:string) =
  curFile := f


(** Replacement for CIL's call graph computer *)
module CGComp = struct
  
  class cgComputer = object(self)
    inherit nopCilVisitor
      
    (* this is the partial (per-file) call graph we are computing;
     * it is created empty when the object is constructed, and will
     * be added-to during the computation *)
    val mutable graph: simpleCallG = FMap.empty

    (* the current function we're in, so when we visit a call node
     * we know who is the caller *)
    val mutable curFunc: (fKey * simpleCallN) option = None
      
    method getGraph = graph

    (* given the name of a function, retrieve its callnode; this
     * will create a node if one doesn't already exist *)
    method getNode ?(hasBod = false) (info:varinfo) : simpleCallN = begin
      let id = info.vid in
      try
        let ret = FMap.find id graph in
        (* possibly update the hasBody field *)
        ret.hasBody <- ret.hasBody || hasBod;
        ret
      with Not_found -> (
        (* make a new node *)
        let ret = {
          name = info.vname;
          typ = D.string_of_ftype info.vtype;
          callees = [];
          callers = [];
          defFile = "TBD"; 
          hasBody = hasBod;
        } in
        
        (* add it to the table, then return it *)
        graph <- FMap.add id ret graph;
        ret
      )
    end
      
    (* begin visiting a function definition *)
    method vfunc (f:fundec) : fundec visitAction = begin
      let hasBod = match f.sbody.bstmts with
        | [] -> false
        | _ -> true
      in
      L.logStatus ("entering function " ^  f.svar.vname);
      curFunc <- (Some (f.svar.vid, self#getNode ~hasBod:hasBod f.svar));
      DoChildren
    end
        
    (* visit an instruction; we're only interested in calls *)
    method vinst (i:instr) : instr list visitAction = begin
      (match curFunc, i with
       | Some(callerK, caller),       (* should always be set to something *)
         Call(_,callexp,_,_) -> 
           let handleFunc (finfo:varinfo) =
             (* get the callee's node *)
             let callee = (self#getNode finfo) in
             
             (* add one entry to each node's appropriate list *)
             caller.callees <- addOnce caller.callees finfo.vid;
             callee.callers <- addOnce callee.callers callerK;
           in
           let funIDs = A.funsForCall callexp in
           (* Get the fun infos *)
           let funInfos = List.fold_left
             (fun cur fid -> 
                try 
                  Cilinfos.getVarinfo fid :: cur
                with Not_found ->
                  L.logError ("Dumpcalls no varinfo for fid: " ^ 
                                (string_of_fkey fid));
                  cur
             ) [] funIDs in
           List.iter handleFunc funInfos
       | _ -> ()   (* ignore other kinds instructions *)
      ); 
      DoChildren
    end
      
  end (* end cgComputer class *)
    
    
  (** Compute the graph for this file. 
      ASSUMES the Alias module has been initialized *)
  let computeGraph (f:file) : simpleCallG =
    begin
      let obj:cgComputer = (new cgComputer) in
      
      (* visit the whole file, computing the graph *)
      (visitCilFileSameGlobals (obj :> cilVisitor) f);

      obj#getGraph
    end      
      
end


(** Write a function's callgraph info to the outFile
    The format is "curfile $ fname : ftype_string : id $ 
    succName1 : succType1 : succID1 $ succName2, ...\n"
*)
let writeFuncCalls (cg:simpleCallG) (outFile:out_channel) 
    (curKey:fKey) (curNode:simpleCallN) =

  let writeNode key node =
    output_string outFile 
      (topDelim ^ node.name ^ 
         delim ^ node.typ ^
         delim ^ (string_of_int key))
  in
  
  let writeSucc succID =
    try 
      let succNode = FMap.find succID cg in
      writeNode succID succNode
    with Not_found ->
      L.logError ("Dumpcalls: No info for callee? " ^ (string_of_fkey succID))
  in

  begin
    (* only write it if we have the body *)
    if (curNode.hasBody) then
      begin
        (* File that has function definition *)
        output_string outFile !curFile;

        (* Function name w/ signature *)
        writeNode curKey curNode;
        
        (* Successor names and sigs *)
        List.iter writeSucc curNode.callees;
        output_string outFile "\n";
      end
  end

(** Figure out which file to use to store the call graph *)
let getCallsFile baseDir =
  let ext = A.identifyFPA () in
  let cgFile = Filename.concat baseDir ("calls." ^ ext) in 
  cgFile

(** Dumps (appends) the call graph using all functions in the file *)
let dumpCalls (f:file) (fname:string) =
  setCurFile fname;
  let cgFile = getCallsFile !dumpTo in
  let outFile = (open_out_gen 
                   [Open_creat; Open_wronly; Open_append]
                   0o644 
                   cgFile) in
  try
    let callG = CGComp.computeGraph f in
    FMap.iter (writeFuncCalls callG outFile) callG;
    close_out outFile;
  with e -> 
    Printf.printf "Exc. in dumpCalls: %s\n" (Printexc.to_string e); 
    close_out_noerr outFile;
    raise e

      
(********** Main entry-point **************)
      
(** Make call graph for all ast files reachable from this root directory *)
let writeCallgraph root : unit =
  Filetools.walkDir 
    (fun ast file ->
       Alias.setCurrentFile ast;
       dumpCalls ast ast.fileName
    ) root
