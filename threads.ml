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
    for the thread root?) *)

open Cil
open Cildump
open Fstructs
open Callg
open Logging

module A = Alias
module DC = Default_cache

(*************************************************
 * Thread creator attributes
 *************************************************)

type threadCreatorAttribs = {
  (* Not explicitly tracking the threadCreator's own function name 
     (implicitly tracked as the key in the indexed collection) *)

  tcStarts : bool;         (* true if the function starts the thread also *)
  tcStartFunName : string; (* if fun doesn't start thread, which fun does? *)
  tcStartFunType : string; 
  tcFPIndex : int;         (* index of arg list for function pointer *)
  tcArgIndex : int;        (* index of arg list for the actuals of new thread *)
  thArgIndex : int;        (* index of where formals are sent to spawned func *)

  (* 
     TODO: allow a list of arguments from different sources for example, 
     the GLib thread pools uses two sets of arguments 
     (one during pool creation, and another when the thread starts):
     
     // Make a pool that runs [func]
     a_pool = g_thread_pool_new (func, common_arg);

     // Run a thread on argument [new_arg]
     g_thread_pool_push (a_pool, new_arg);

     func (new_arg, common_arg) { ... }
  *)

}
 
(** global list of known thread creators and their arguments *)
let (threadCreators : (FNTMap.key * threadCreatorAttribs) list ref ) = ref [] 


(*************************************************
 * Splits a string based on different delimiters 
 *************************************************)

let ws = "[ \r\n\t]*"

(* Top level split, between the function name, type, lockset, correlations *)
let topSplitter = Str.split_delim (Str.regexp (ws ^ "[$]" ^ ws))


exception BadLine

let addThreadCreator (funcName, typString) tcData =
  threadCreators := List_utils.addOnceP 
    (fun (fnt1, _) (fnt2, _) -> compareNT fnt1 fnt2 == 0)
    !threadCreators ((funcName, typString), tcData) 

let parseArgIndexes options =
  match options with
    fpIStr :: argIStr :: argAIStrOpt ->
      let fpI = if (fpIStr.[0] == 'f') then
        int_of_string (Str.string_after fpIStr 1)
      else raise BadLine in
      let argI = if (argIStr.[0] == 'f') then
        int_of_string (Str.string_after argIStr 1)
      else raise BadLine in
      let argAI = 
        (match argAIStrOpt with
           [] -> 0
         | [x] -> 
             if (x.[0] == 'f') then int_of_string (Str.string_after x 1)
             else raise BadLine
         | _ -> raise BadLine) in
      fpI, argI, argAI
  | _ ->
      raise BadLine

 
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
              let fpInd, argInd, actInd = parseArgIndexes newRest in
              

              (* Only add if it hasn't already been added *)
              addThreadCreator (funcName, typString) 
                {
                   tcStarts = starts;
                   tcStartFunName = startFName;
                   tcStartFunType = startFType;
                   tcFPIndex = fpInd;
                   tcArgIndex = argInd;
                   thArgIndex = actInd;
                }
              ;
          | _ ->
              raise BadLine
         )
       with BadLine ->
         logError ~prior:0 
           ("corrupt value in thread config " ^ details ^"\n")
    ) threadSettings


(********************************************************
 * Interface for getting thread creators / handling them
 ********************************************************)


(* Get TC attributes for the particular TC. 
   Raises Not_found if the function is not a TC *)    
let find ((fname, ftyp):fNT) =
  let _, entry = 
    List.find 
      (fun ((fn, ft), _) ->
         (* just compare function names (not types) for now *)
         fname = fn)
      !threadCreators in
  entry

(** Thread creator callers (functions that call pthread_create, etc.) *)
type tcc = {
  tccID : funID;
  tccName : string;
  tccDefFile : string;
}

(** Get a list of all the functions that call thread creation functions *)
let findTCCallers cg : tcc list =
  let createsThread callerKey callerNode = 
    List.exists 
      (fun calleeKey ->
         try
           let calleeNode = FMap.find calleeKey cg in
           let (cn, ct) = calleeNode.name, calleeNode.typ in
           List.exists 
             (fun ((tcn, tct), _) ->
                (* (Fstructs.compareNT (cn, ct) (tcn, tct)) == 0 *)
                (* Just compare the names for now... *)
                cn = tcn
             ) !threadCreators
         with Not_found ->
           false
      ) (calleeKeys callerNode)
  in
  (* Drops context-sensitivity here... which may be bad if the call
     to thread_create was through a fun pointer *)
  FMap.fold 
    (fun callerKey callerNode res ->
       if createsThread callerKey callerNode then
         { tccID = callerKey;
           tccName = callerNode.name;
           tccDefFile = callerNode.defFile; } :: res
       else res
    ) cg []


let dummyExp = Cil.integer 1337

(** Place the args in the same position as the actual 
    thread root would expect them *)
let makeFakeActuals threadA arg =
  let index = threadA.thArgIndex in
  let rec makeList curList curIndex =
    if curIndex == index then curList
    else makeList (dummyExp :: curList) (curIndex + 1)
  in
  makeList [arg] 0


(** A generic visitor that searches for function calls to 
    thread creation functions *)
class virtual threadCreateVisitor (cg : callG) = object (self)
  inherit Pp_visitor.ppVisitor

  val mutable curFkey = (-1, "") 

  method vfunc fdec = 
    curFkey <- (Summary_keys.inputFreeSumKey (funToKey fdec));
    DoChildren

  (** handle given call to a thread creation function *)
  method handleThreadCreate i loc threadA actuals =
    let f = List.nth actuals threadA.tcFPIndex in
    let funs = A.funsFromAddr f in
    (* What to do if we don't know what the FP leads to? *)
    if (funs = []) then
      logErrorF "unknown func ptr used as thread root: %s @ %s\n" 
        (string_of_exp f) (string_of_loc !currentLoc)
    else
      let arg = List.nth actuals threadA.tcArgIndex in
      let args = makeFakeActuals threadA arg in
      let fids = getMatchingIDs cg funs in
      self#handleThreadRoots i loc f fids args
        
        
  (* handle thread creation at instruction i and location l, with 
     root function f (may be a list of funs if funptr) and its argument *)
  method virtual handleThreadRoots : Cil.instr -> Cil.location -> 
    Cil.exp -> funID list -> Cil.exp list -> unit


  (* Look for calls to thread creation functions and handle *)
  method vinst (i:Cil.instr) =
    self#setInstrPP i;
    (* Look for calls to thread creators (e.g., pthread_create (...)) *)
    let result = match i with
        (* Direct Call *)
        Call(ret, Lval(Var(va),NoOffset), actuals, loc) -> begin
          let (fname, ftyp) = 
            (va.vname,
             string_of_ftype va.vtype) in
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
          (* TODO: handle context-sensitive CG here... *)
          let pp = getCurrentPP () in
          let aliasedFuns = callTargsAtPP cg curFkey pp in
          let aliasedFuns = List.map Summary_keys.fkey_of_sumKey aliasedFuns in
(*          let aliasedFuns = A.deref_funptr ptrExp in *)
          (* What to do if we don't know what the FP leads to? *)
          if (aliasedFuns = []) then
            logError ("Threads: No FP targets for: " ^ 
                        (string_of_exp ptrExp))
          else begin
            let aliasedFuns = getMatchingIDs cg aliasedFuns in
            List.iter 
              (fun fid ->
                 try
                   let fnode = FMap.find fid cg in
                   let fname, ftyp = fnode.name, fnode.typ in
                   let tcAttribs = find (fname, ftyp) in
                   self#handleThreadCreate i loc tcAttribs actuals;
                 with Not_found ->
                   (* Most function calls aren't going to match *)
                   ()
              ) aliasedFuns
          end;
          SkipChildren
            
      (* Calls based on offsets *)
      | Call(_) ->
          logError "threadCreateVisitor: can't interp. a Call instr\n";
          SkipChildren
      | _ ->
          SkipChildren
    in
    self#bumpInstr 1;
    result
      
end

let getFundec fid cg =
  (* Ugh... really only needed the context insensitive version of
     the callgraph  *)
  let fkey = fid_to_fkey fid in
  try
    let fNode = FMap.find fid cg in
    Cilinfos.getFunc fkey fNode.defFile
  with Not_found ->
    logError ("threadArgFinder can't find fnode " ^ string_of_fkey fkey);
    None
      
(** A visitor that finds the funIDs of possible thread roots 
    (functions "forked" by thread creators) *)
class threadRootFinder cg start = object (self)
  inherit threadCreateVisitor cg

  val mutable roots = start
    
  method getFuncs =
    roots

  (* handle given call to a thread creation function *)
  method handleThreadRoots i loc f funs args =
    (* Record function all funIDs matching given fkeys of each thread root *)
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
    (fun cur tcc ->
       match Cilinfos.getFunc (fid_to_fkey tcc.tccID) tcc.tccDefFile with
         Some func -> (collectThreadRoots cg cur func)
       | None -> cur
    ) FSet.empty tcs


module ES = Set.Make(
  struct
    type t = Cil.exp
    let compare = Ciltools.compare_exp
  end
)

module VS = Set.Make(
  struct
    type t = Cil.varinfo
    let compare = Ciltools.compare_var
  end
)


(** Find actuals and formals passed to a thread *)
class threadArgFinder cg (startActs:ES.t) (startForms:VS.t) = object (self)
  inherit threadCreateVisitor cg

  val mutable foundActuals = startActs
  val mutable foundFormals = startForms

  method getActuals = foundActuals

  method getFormals = foundFormals

  method handleThreadRoots i loc f funs actuals =
    foundActuals <- 
      List.fold_left (fun cur arg -> ES.add arg cur) foundActuals actuals;
    foundFormals <- 
      List.fold_left 
      (fun cur fid ->
         match getFundec fid cg with
           None -> cur
         | Some fdec -> 
             List.fold_left (fun cur var -> VS.add var cur) cur fdec.sformals
      ) foundFormals funs

end
  
  
(** @return a list of the arguments that are ever passed to a thread root *)
let getThreadActuals cg tcs : Cil.exp list  * Cil.varinfo list =
  let actuals, formals = List.fold_left
    (fun (curActs, curForms) tcc ->
       match Cilinfos.getFunc (fid_to_fkey tcc.tccID) tcc.tccDefFile with
         Some func ->
           let vis = new threadArgFinder cg curActs curForms in
           let _ = Cil.visitCilFunction (vis :> cilVisitor) func in
           (vis#getActuals, vis#getFormals)
       | None -> 
           (curActs, curForms)
    ) (ES.empty, VS.empty) tcs in
  (ES.elements actuals, VS.elements formals)

