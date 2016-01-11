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
   TEST: Load a Cil AST File and run minimal analysis / transformations 
   to see how much RAM is used 
*)

open Gc_stats
open Cil
open Pretty
open Stdutil

module CILCG = Mycallgraph
module PTA = Myptranal
module L = Logging

let debug = false

let opsPerformed = "PTA, callgraph, cfgs"

(***************************************************)
(* Commandline handling                            *)
let in_file = ref ""

let setInFile (fname:string) = 
  in_file := fname

let isBinary = ref false

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-i", Arg.String setInFile, "file to process");
   ("-b", Arg.Set isBinary, "flag indicating sources are CIL binary ASTs")]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-i fname [flags]\n"


(***************************************************)
(* Test  Functions                                 *)

module OrderedLval = struct
  type t = lval
  let compare = Ciltools.compare_lval
end

module LvalSet = Set.Make(OrderedLval)

let lvalSet = ref LvalSet.empty

module OrderedExp = struct
  type t = exp
  let compare = Ciltools.compare_exp 
end

module ExpSet = Set.Make(OrderedExp)

let ptrExpSet = ref ExpSet.empty

class lvalVisitor = object
  inherit nopCilVisitor 

  method vlval (lv:lval) =
    lvalSet := LvalSet.add lv !lvalSet;
    (match lv with
       Mem (ptrExp), _ ->
         ptrExpSet := ExpSet.add ptrExp !ptrExpSet
     | _ ->
         ()
    );
    DoChildren
end

let getFile (fname:string) : file =
  if (!isBinary) then
    loadBinaryFile fname
  else
    failwith "Not tryign to parse file for now"
      (* Frontc.parse fname () *)

let doPTA (f:file) = begin
  PTA.reset_globals ();
  PTA.analyze_file f;
  PTA.compute_results false;
  (* check if may alias of each expression is the same as computations
     w/ their abslocs *)
  (* lvalSet := LvalSet.empty;
  let lvVisitor = new lvalVisitor in
  visitCilFileSameGlobals lvVisitor f;
  LvalSet.iter 
    (fun lv1 ->
       LvalSet.iter 
         (fun lv2 ->
            (try
               let exp1 = (Lval(lv1)) in
               let exp2 = (Lval(lv2)) in
               let absloc1 = PTA.absloc_of_lval lv1 in
               let absloc2 = PTA.absloc_of_lval lv2 in
               let isEQAbs = PTA.absloc_eq absloc1 absloc2 in
               let isEQMay = PTA.may_alias exp1 exp2 in
               if (not (isEQAbs == isEQMay)) then begin
                 L.logError ("Conflicting PTA answers for:\n\t" ^
                                 (sprint 80 (d_lval () lv1)) ^ "\t " ^
                                 (sprint 80 (d_lval () lv2)));
               end;
               if(isEQAbs) then
                 L.logStatus ("These lvals alias according to abs: " ^
                                 (sprint 80 (d_lval () lv1)) ^ "\t " ^
                                 (sprint 80 (d_lval () lv2)));
               if(isEQMay) then
                 L.logStatus ("These exps may_alias: " ^
                                 (sprint 80 (d_exp () exp1)) ^ "\t " ^
                                 (sprint 80 (d_exp () exp2)));
               
             with
               Not_found ->
                 L.logError ("Couldn't find alias result for: " ^
                                 (sprint 80 (d_lval () lv1)) ^ "\t " ^
                                 (sprint 80 (d_lval () lv2)));
                 
             | _ -> ()
            )
         )
         !lvalSet
    ) !lvalSet;
  ExpSet.iter
    (fun ptrExp ->
       let res = PTA.resolve_exp ptrExp in
       L.logStatus ("\n\nResolve_exp of: " ^ (sprint 80 (d_exp () ptrExp)) ^
                       " results in:");
       List.iter
         (fun vi ->
            L.logStatus ("\t" ^ sprint 80 (d_lval () (Var(vi),NoOffset)));
         ) res
    ) !ptrExpSet
    *)
  end
  
let doCallgraph (f:file) =
  CILCG.computeGraph f

let doCFG (f:file) =
  let computeCFG (func:fundec) =
    prepareCFG func;
    computeCFGInfo func false
  in
  iterGlobals f (fun glob ->
                       match glob with
                         GFun (fdec, _) -> 
                           computeCFG fdec
                       | _ -> ()) 
    
let runTest () = 
  try
    let f = getFile !in_file in
    doPTA f;
    let _ = doCallgraph f in
    doCFG f;
    ()
  with e -> L.logError ("Exc. in loadRunTest: " ^
                             (Printexc.to_string e));
    raise e
  


(***************************************************)
(* Execution / Program Entry Point                 *)

  

let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require files, so check manually *)
    if (!in_file = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        initCIL ();
        L.logStatus ("Checking mem usage for: " ^ !in_file);
        L.logStatus ("Operations: " ^ opsPerformed);
        L.logStatus "-----";
        runTest ();
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logError ("Exc. in LoadRunTest: " ^ (Printexc.to_string e));
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
