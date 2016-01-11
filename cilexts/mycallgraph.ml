(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
(* callgraph.ml *)
(* code for callgraph.mli *)

(* see copyright notice at end of this file *)

open Cil
open Trace
open Printf
module Dum = Cildump
module P = Pretty
module PTA = Myptranal



(* ------------------- interface ------------------- *)

(* set of function types, described by a string *)
module FTypeSet = Set.Make (
  struct
    type t = string
    let compare t1 t2 = Pervasives.compare t1 t2
  end
)



(* a call node describes the local calling structure for a
 * single function: which functions it calls, and which
 * functions call it *)
type callnode = {
  (* the function this node describes *)
  cnInfo: varinfo;

  (* set of functions this one calls *)
  cnCallees: (string, callnode) Hashtbl.t;

  (* set of functions that call this one *)
  cnCallers: (string, callnode) Hashtbl.t;

  (* set of indirect callexp types *)
  mutable callsIndirect : FTypeSet.t;

  (* true if the function body is known *)
  mutable hasBody : bool;
}


(* a call graph is a hashtable, mapping a function name to
 * the node which describes that function's call structure *)
type callgraph =
  (string, callnode) Hashtbl.t


(* Also need a set of functions which have had their address taken
 * "varinfo" has a flag for that purpose, but it may be limited due
 * to the per-file nature of the analysis
 *)
type addrTMap =
    (string, varinfo) Hashtbl.t

(* ------------------- implementation --------------------------- *)
(* add something to a hashtable if it's not already there, instead of
 * the default pushback behavior of Hashtbl.add *)
let add_if (table : ('a,'b) Hashtbl.t) (key:'a) (data:'b) : unit =
begin
  if (not (Hashtbl.mem table key)) then
    (Hashtbl.add table key data)
end


class cgComputer = object(self)
  inherit nopCilVisitor

  (* this is the graph we are computing, and will eventually return;
   * it is created empty when the object is constructed, and will
   * be added-to during the computation *)
  val graph: callgraph = (Hashtbl.create 117)

    
  val addrTaken: addrTMap = (Hashtbl.create 10)


  (* the current function we're in, so when we visit a call node
   * we know who is the caller *)
  val mutable curFunc: callnode option = None


  (* I don't know syntax for extracting a field directly, or else
   * OCaml doesn't allow it.. *)
  method getGraph () : callgraph = graph

  method getAddrTaken () : addrTMap = addrTaken

  (* given the name of a function, retrieve its callnode; this
   * will create a node if one doesn't already exist *)
  method getNode ?(hasBod = false) (info:varinfo) : callnode =
  begin
    let name = info.vname in
    try
      (* possibly update the hasBod field *)
      let ret = Hashtbl.find graph name in
      if hasBod then
        ret.hasBody <- hasBod;
      ret
    with Not_found -> (
      (* make a new node *)
      let ret:callnode = {
        cnInfo = info;
        cnCallees = (Hashtbl.create 5);
        cnCallers = (Hashtbl.create 5);
        callsIndirect = FTypeSet.empty;
        hasBody = hasBod
      } in

      (* add it to the table, then return it *)
      (Hashtbl.add graph name ret);
      ret
    )
  end

  (* begin visiting a function definition *)
  method vfunc (f:fundec) : fundec visitAction =
  begin
    let hasBod = match f.sbody.bstmts with
      | [] -> false
      | _ -> true
    in
    (trace "callgraph" (P.dprintf "entering function %s\n" f.svar.vname));
    curFunc <- (Some (self#getNode ~hasBod:hasBod f.svar));
    DoChildren
  end

  (* visit an instruction; we're only interested in calls *)
  method vinst (i:instr) : instr list visitAction =
    begin
      (match curFunc, i with
       | Some(caller),       (* should always be set to something *)
         Call(_,callexp,_,_) -> 
           let handleFunc (finfo:varinfo) =
             (* get the callee's node *)
             let callee:callnode = (self#getNode finfo) in
             (trace "callgraph" (P.dprintf "I see a call by %s to %s\n"
                                   caller.cnInfo.vname callee.cnInfo.vname));
             
             (* add one entry to each node's appropriate list *)
             (add_if caller.cnCallees callee.cnInfo.vname callee);
             (add_if callee.cnCallers caller.cnInfo.vname caller)
           in
           (
             (* match calls using named functions, thereby ignoring
              * function pointers (!) *)
             match callexp with
             | Lval(Var(vi),NoOffset) -> (
                 handleFunc vi
               )
                 
             (* Indirect call *)
             | Lval(Mem(derefExp),_) ->
                 let typestring = Dum.string_of_ftype (Cil.typeOf derefExp) in 
                 let possibleFuns = try
                   PTA.resolve_funptr derefExp
                 with Not_found ->
                   prerr_string ("couldn't resolve funptr: " ^ 
                                   (Pretty.sprint 80 (d_exp () derefExp)));
                   []
                 in
                 let aliasedFuns = List.filter 
                   (fun fdec ->
                      (typestring = Dum.string_of_ftype fdec.svar.vtype)) 
                   possibleFuns in
                 (* Handle each aliased function *)
                 List.iter (fun fdec -> 
                              handleFunc fdec.svar) aliasedFuns;
                 
                 (* Note type of indirect call, in case we need to widen 
                  * the set of possible functions called *)
                 caller.callsIndirect <- FTypeSet.add 
                   (Dum.string_of_cexp callexp) caller.callsIndirect;
                 
             (* Other indirect call? *)
             | _ -> 
                 let aliasedFuns = PTA.resolve_funptr callexp in

                 (* Handle each aliased function *)
                 List.iter (fun fdec -> 
                              handleFunc fdec.svar) aliasedFuns;
                 
                 (* Note type of indirect call, in case we need to widen 
                  * the set of possible functions called *)
                 caller.callsIndirect <- FTypeSet.add 
                   (Dum.string_of_cexp callexp) caller.callsIndirect;
                 
           )
       | _ -> ());     (* ignore other kinds instructions *)
      DoChildren
    end
      
  (* Just need to see if a function's address is taken *) 
  method vexpr (e:exp) : exp visitAction =
    begin
    (match e with 
      AddrOf (lval) -> begin
        match lval with
          (Var vinfo, _) -> if (Cil.isFunctionType vinfo.vtype) then
            (add_if addrTaken vinfo.vname vinfo)
        | (Mem mexp, _) -> 
            (* Might know it's a fun, but won't know the name, so ignore *)
            ()
      end
    |_ -> ());
    DoChildren
    end
end


(** ASSUMES PTA.analyze_file has been run *)
let computeGraph (f:file) : (callgraph * addrTMap) =
  begin
    let obj:cgComputer = (new cgComputer) in
    
    (* visit the whole file, computing the graph *)
    (visitCilFileSameGlobals (obj :> cilVisitor) f);
    
    (obj#getGraph (), obj#getAddrTaken ())
  end
    


let printGraph (out:out_channel) (g:callgraph) : unit =
  begin
    let printEntry (s:string) (n:callnode) : unit =
      (Printf.fprintf out " %s" s) in
    
    let printNode (name:string) (node:callnode) : unit =
      begin
    (fprintf out "%s:\n" name);
        (fprintf out "  calls:");
        (Hashtbl.iter printEntry node.cnCallees);
        (fprintf out "\n  is called by:");
        (Hashtbl.iter printEntry node.cnCallers);
        (fprintf out "\n")
      end in
    
    (Hashtbl.iter printNode g)
  end
    
let doCallGraph = ref false

let feature : featureDescr = 
  { fd_name = "callgraph";
    fd_enabled = doCallGraph;
    fd_description = "generation of a static call graph";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      let graph:callgraph = (fst (computeGraph f)) in
      printGraph stdout graph);
    fd_post_check = false;
  } 


(*
 *
 * Copyright (c) 2001-2002 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *  Ben Liblit          liblit@cs.berkeley.edu
 *
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1. XSRedistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products 
 * derived from  this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
