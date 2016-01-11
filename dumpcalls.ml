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
open Pp_visitor
open Sys
open Pretty
open Filetools
open Fstructs
open Callg
open Logging


(************************************************************
  Options
************************************************************)

(** The directory in which the call graph file will be stored.
    Must set this before using the module *)
let dumpTo = ref ""

let setDumpTo (p:string) =
  dumpTo := p


(************************************************************
 DEBUG
************************************************************)

(** Filters to debug imprecision in call graph *)
class type cgFilter = object
  method filter : fKey list -> fundec -> fKey -> exp  -> fKey list
end

(** No filter *)
class noFilter = object
  method filter (funs: fKey list) (c:fundec) (f: fKey) (e: exp) = funs
end

let curFilter = ref (new noFilter)

let rec isIndirect callexp =
  match callexp with
    CastE(_, e) -> isIndirect e
  | Lval (Var(_), _)
  | AddrOf _
  | StartOf _ -> false
  | Lval (Mem(e), off) -> true
  | _ -> failwith "unknown call expression"

(************************************************************
 Actual dumper
************************************************************)

(** Replacement for CIL's call graph computer *)
module CGComp = struct
  
  class cgComputer (filter : cgFilter) curFile = object(self)
    inherit ppVisitor
      
    (* this is the partial (per-file) call graph we are computing;
     * it is created empty when the object is constructed, and will
     * be added-to during the computation *)
    val mutable graph = emptyCG
    method getGraph = graph

    (* the current function we're in, so when we visit a call node
     * we know who is the caller *)
    val mutable curFunc: (fundec * callN) option = None
      
    (* given the name of a function, retrieve its callnode; this
     * will create a node if one doesn't already exist *)
    method getNode ?(hasBod = false) (info:varinfo) = begin
      let id = fkey_to_fid info.vid in
      try
        let ret = FMap.find id graph in
        (* possibly update the hasBody field *)
        ret.hasBody <- ret.hasBody || hasBod;
        ret
      with Not_found -> (
        (* make a new node *)
        let ret = {
          name = info.vname;
          typ = Cildump.string_of_ftype info.vtype;
          ccallees = [];
          ccallers = [];
          defFile = curFile; 
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
      logStatus ("entering function " ^  f.svar.vname);
      curFunc <- (Some (f, self#getNode ~hasBod:hasBod f.svar));
      DoChildren
    end

    method private vidsToInfos ids =
      List.fold_left 
        (fun cur id -> Cilinfos.getVarinfo id :: cur) [] ids

    method private handleFuncs callerK caller calleeIDs calleeInfos indir =
      (* Make sure nodes exist first *)
      List.iter (fun vi -> let _ = self#getNode vi in ()) calleeInfos;

      (* Make up a context for each function *)
      let calleeIDs = List.map fkey_to_fid calleeIDs in
      let calleeIDs = List.sort compareFunID calleeIDs in

      (* then add 'em *)
      let pp = getCurrentPP () in
      let calleeToAdd = 
        if indir then CIndirect (pp, calleeIDs)
        else CDirect (pp, (match calleeIDs with [x] -> x 
                           | _ -> failwith "Too many callees"))
      in  
      caller.ccallees <- addCallee calleeToAdd caller.ccallees 
        (* Don't fill in the callers yet *)


    (* visit an instruction; we're only interested in calls *)
    method vinst (i:instr) : instr list visitAction = begin
      self#setInstrPP i;
      let caller, callerN = match curFunc with 
          Some(x) -> x
        | None -> failwith "caller not set?" in
      let callerK = caller.svar.vid in
      (match i with
         Call(_,callexp,_,loc) -> 
           let funIDs = Alias.funsForCall callexp in
           let funIDs = filter#filter funIDs caller callerK callexp in
           let isIndir = isIndirect callexp in
           self#handleFuncs callerK callerN funIDs 
             (self#vidsToInfos funIDs) isIndir
       | _ -> ()   (* ignore other kinds instructions *)
      );
      self#bumpInstr 1;
      DoChildren
    end
      
  end (* end cgComputer class *)
    
    
  (** Compute the graph for this file. 
      ASSUMES the Alias module has been initialized *)
  let computeGraph (f:file) filter curFile =
    begin
      let obj:cgComputer = (new cgComputer filter) curFile in
      (* visit the whole file, computing the graph *)
      (visitCilFileSameGlobals (obj :> cilVisitor) f);
      obj#getGraph
    end      
      
end


(** Figure out which file to use to store the call graph *)
let getCallsFile baseDir =
  let ext = Alias.identifyFPA () in
  let cgFile = Filename.concat baseDir ("calls." ^ ext) in 
  cgFile


(** Dumps (appends) the call graph using all functions in the file *)
let dumpCalls (f:file) (fname:string) =
  let cgFile = getCallsFile !dumpTo in
  let outFile = (open_out_gen 
                   [Open_creat; Open_wronly; Open_append]
                   0o644 
                   cgFile) in
  try
    let callG = CGComp.computeGraph f (!curFilter) fname in
    writeSomeCalls outFile callG;
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
