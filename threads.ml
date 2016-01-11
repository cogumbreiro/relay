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


(** Manages information about thread creation functions 
    (e.g., which argument to pthread_create is the function pointer
    for the thread root?)
*)

open Cil
open Fstructs
open Callg

module D = Cildump
module A = Alias
module L = Logging
module DC = Default_cache

(*************************************************
 * Thread creator attributes
 *************************************************)

type threadCreatorAttribs = {
  tcStarts : bool;         (* true if the function starts the thread also *)
  tcStartFunName : string; (* if fun doesn't start thread, which fun does? *)
  tcStartFunType : string;
  tcFPIndex : int;         (* index of arg list for function pointer *)
  tcArgIndex : int;        (* index of arg list for the args of new thread *)
}
    
let (threadCreators : (FNTMap.key * threadCreatorAttribs) list ref ) 
    = ref [] (* global list of known thread creators *)


(*************************************************
 * Splits a string based on different delimiters 
 *************************************************)

let ws = "[ \r\n\t]*"

(* Top level split, between the function name, type, lockset, correlations *)
let topSplitter = Str.split_delim (Str.regexp (ws ^ "[$]" ^ ws))


exception BadLine

(** Clear and initialize summaries *)
let initSettings settings =
  let threadSettings = Config.getGroup settings "THREAD_FUNCS" in
  threadCreators := [];
  Config.iter 
    (fun funcName details ->
       let fields = topSplitter details in
       try 
         (match fields with
            typString :: startStr :: rest ->
              (* Check if the creator also starts the thread.
                 If not, who does start the thread? *)
              let starts = bool_of_string startStr in
              let startFName, startFType, newRest = 
                if (starts) then
                  "", "", rest
                else
                  match rest with
                    sfn :: sft :: r ->
                      sfn, sft, r
                  | _ -> 
                      raise BadLine
              in

              (* Parse indexes to help find the function called on creation *)
              let fpInd, argInd = 
                match newRest with
                  fpIStr :: argIStr :: [] ->
                    if(fpIStr.[0] == 'f' &&
                         argIStr.[0] == 'f') then
                      (int_of_string (Str.string_after fpIStr 1),
                       int_of_string (Str.string_after argIStr 1))
                    else
                      raise BadLine
                | _ ->
                    raise BadLine
              in

              (* Only add if it hasn't already been added *)
              if (List.exists 
                    (fun (fkey,_) -> 
                       (compareNT fkey (funcName, typString)) == 0
                    ) !threadCreators) then
                ()
              else
                threadCreators := 
                  ((funcName, typString), 
                   {
                     tcStarts = starts;
                     tcStartFunName = startFName;
                     tcStartFunType = startFType;
                     tcFPIndex = fpInd;
                     tcArgIndex = argInd;
                   }
                  ) :: !threadCreators
              ;
          | _ ->
              raise BadLine
         )
       with BadLine ->
         L.logError ~prior:0 
           ("corrupt value in thread config " ^ details ^"\n")
    ) threadSettings


(********************************************************
 * Interface for getting thread creators / handling them
 ********************************************************)


(* Get TC attributes for the particular TC. 
   Raises Not_found if the function is not a TC *)    
let find (fkey:fNT) =
  let _, entry = 
    List.find 
      (fun (key, _) ->
         (compareNT fkey key) == 0) 
      !threadCreators in
  entry

type tccs = (fKey * simpleCallN) list

(* Get a list of all the functions that call thread creation functions *)
let findTCCallers (cg:simpleCallG) : (fKey * simpleCallN) list =
  FMap.fold 
    (fun callerKey callerNode res ->
       if (List.exists 
             (fun calleeKey ->
                try
                  let calleeNode = FMap.find calleeKey cg in
                  let (cn, ct) = calleeNode.name, calleeNode.typ in
                  let creates_thread = List.exists 
                    (fun ((tcn, tct), _) ->
                       (* (Fstructs.compareNT (cn, ct) (tcn, tct)) == 0 *)
                       (* Just compare the names for now... *)
                       cn = tcn
                    ) !threadCreators
                  in creates_thread
                with Not_found ->
                  false
             ) callerNode.callees
          ) 
       then (callerKey, callerNode) :: res
       else res
    ) cg []


(** A generic visitor that searches for function calls to 
    thread creation functions *)
class virtual threadCreateVisitor cg = object (self)
  inherit nopCilVisitor 

  val mutable curStmt = Cil.dummyStmt

  val curCG = cg

  (* handle given call to a thread creation function *)
  method handleThreadCreate i loc threadA actuals =
    let f = List.nth actuals threadA.tcFPIndex in
    let funs = A.funsFromAddr f in
    let arg = List.nth actuals threadA.tcArgIndex in
    
    (* What to do if we don't know what the FP leads to? *)
    if (List.length funs = 0) then
      L.logError ("unknown func ptr used as thread root: " 
                  ^ (D.string_of_exp f))
    else
      self#handleThreadRoots i loc f funs arg

    
  (* handle thread creation at instruction i and location l, with 
     root function f (may be a list of funs if funptr) and its argument *)
  method virtual handleThreadRoots : Cil.instr -> Cil.location -> 
    Cil.exp -> fKey list -> Cil.exp -> unit


  (* Just record current stmt and proceed *)
  method vstmt (s:Cil.stmt) =
    curStmt <- s;
    DoChildren
      
  (* Look for calls to thread creation functions and handle *)
  method vinst (i:Cil.instr) = 
    (* Look for calls to thread creators (e.g., create_pthread (...)) *)
    match i with
      (* Direct Call *)
      Call(ret, Lval(Var(va),NoOffset), actuals, loc) -> begin
        let (fname, ftyp) = 
          (va.vname,
           D.string_of_ftype va.vtype) in
        try
          let tcAttribs = find (fname, ftyp) in
          self#handleThreadCreate i loc tcAttribs actuals;
          SkipChildren
        with Not_found ->
          (* Most function calls aren't actually going to match... *)
          SkipChildren
      end

    (* Indirect Call *)
    | Call(ret, Lval(Mem(ptrExp), NoOffset), actuals, loc) ->
        (* Note: Do not conflate state from function pointer calls *)
        let aliasedFuns = 
          A.deref_funptr ptrExp in
        (* What to do if we don't know what the FP leads to? *)
        if (List.length aliasedFuns = 0) then
          L.logError ("Threads: No FP targets for: " ^ (D.string_of_exp ptrExp))
        else
          List.iter 
            (fun fid ->
               try
                 let fnode = FMap.find fid curCG in
                 let fname, ftyp = fnode.name, fnode.typ in
                 let tcAttribs = find (fname, ftyp) in
                 self#handleThreadCreate i loc tcAttribs actuals;
               with Not_found ->
                 (* Most function calls aren't going to match *)
                 ()
            ) aliasedFuns;
        SkipChildren

    (* Calls based on offsets *)
    | Call(_) ->
        L.logError "threadCreatorVisitor can't interp. a Call instr\n";
        SkipChildren
    | _ ->
        SkipChildren
            
end



(** A visitor that finds the names/types of possible thread roots 
    (functions "forked" by thread creators) *)
class threadRootFinder cg start = object (self)
  inherit threadCreateVisitor cg

  val mutable roots = start
        
  method getFuncs =
    roots

  (* handle given call to a thread creation function *)
  method handleThreadRoots i loc f funs arg =
    (* Record function key of each thread root *)
    List.iter (fun fkey -> roots <- FSet.add fkey roots) funs

end



(** Return a list of thread root functions *)
let collectThreadRoots cg curRoots (cfg:fundec) =
  let vis = new threadRootFinder cg curRoots in
  let _ = Cil.visitCilFunction (vis :> cilVisitor) cfg in
  vis#getFuncs


(** Return the list of thread roots (spawned at thread creation sites) *)
let getThreadRoots cg tcs =
  List.fold_left
    (fun cur (fk, tc) ->
       let ast = !DC.astFCache#getFile tc.defFile in
       A.setCurrentFile ast;
       match Cilinfos.getCFG fk ast with
         Some func ->
           (collectThreadRoots cg cur func)
       | None -> 
           cur
    ) FSet.empty tcs


