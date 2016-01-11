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
    Prints various statistics about instructions in a code base 
    (e.g., how many inline assembly instructions are present).
    TODO: Try to count "lines of code" -- right now it ignores 
    declarations / data structure definitions and statements.
*)

open Cil
open Trace
open Printf
open Gc_stats
open Stdutil

module Dum = Cildump
module P = Pretty
module PTA = Myptranal

(******** ARG MANAGEMENT ********)
let cgFile = ref ""

let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "name of call graph file")]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname -o outdir [options]\n"


(*********)

type instStats = {
  mutable numCalls : int;
  mutable numFPCalls : int;
  mutable numInstrs : int;
  mutable numASM : int;
  mutable numPtrArith : int; (* number of deref(X + A)... doesn't work if they 
                                do Y = X + A; deref(Y) *)
}


let freshStats () =
  {
    numCalls = 0;
    numFPCalls = 0;
    numInstrs = 0;
    numPtrArith = 0;
    numASM = 0;
  }

let addInstr s =
  s.numInstrs <- s.numInstrs + 1

let addASM s =
  s.numASM <- s.numASM + 1

let addCall s =
  s.numCalls <- s.numCalls + 1

let addFPCall s =
  s.numFPCalls <- s.numFPCalls + 1


let addPtrArith s =
  s.numPtrArith <- s.numPtrArith + 1


let combineStats s1 s2 =
  s1.numCalls <- s1.numCalls + s2.numCalls;
  s1.numFPCalls <- s1.numFPCalls + s2.numFPCalls;
  s1.numInstrs <- s1.numInstrs + s2.numInstrs;
  s1.numPtrArith <- s1.numPtrArith + s2.numPtrArith;
  s1.numASM <- s1.numASM + s2.numASM
    
let rec hasArithExp e =
  match e with 
    Lval (l) -> hasArithLval l
  | BinOp (PlusPI, _, _, _)
  | BinOp (MinusPI, _, _, _)
  | BinOp (MinusPP, _, _, _) -> true
  | BinOp (_, e1, e2, _) ->
      hasArithExp e1 or hasArithExp e2
  | _ -> false
  
and hasArithLval lv =
  match lv with
    (Var _,_) -> false
  | (Mem(e),_) -> hasArithExp e
      

class statGatherer = object(self)
  inherit nopCilVisitor

  val stats = freshStats ()

  method getStats = stats

  (* visit an instruction; *)
  method vinst (i:instr) : instr list visitAction =
    addInstr stats;
    print_string "I saw an instr!\n";
    (match i with
       Asm (_) ->
         addASM stats

     | Call(_,callexp,_,_) -> 
         addCall stats;
         (match callexp with
          | Lval(Var(vi),NoOffset) ->
              ()

          (* Indirect call *)
          | Lval(Mem(derefExp),_) ->
              addFPCall stats

          (* Other indirect call? *)
          | _ ->
              addFPCall stats
             
         )

     | Set(lv, exp, _)  -> 
         if hasArithLval lv or hasArithExp exp then
           addPtrArith stats
    );
    DoChildren
      

end

    

(** Get statistics on each instruction *)
let getStats (f:file) : instStats =
  let obj:statGatherer = (new statGatherer) in
  (* visit the whole file *)
  (visitCilFileSameGlobals (obj :> cilVisitor) f);
  (obj#getStats)
    

let printStats s =
  L.logStatus ("ptrArith: " ^ (string_of_int s.numPtrArith));
  L.logStatus ("ASM: " ^ (string_of_int s.numASM));
  L.logStatus ("fpCalls: " ^ (string_of_int s.numFPCalls));
  L.logStatus ("calls: " ^ (string_of_int s.numCalls));
  L.logStatus ("instructs: " ^ (string_of_int s.numInstrs))


(** Make call graph for all files starting at root *)
let statFiles root : unit =
  let stats = freshStats () in
  Filetools.walkDir 
    (fun ast filename ->
       let newStats = getStats ast in
       combineStats stats newStats
    ) root;
  printStats stats


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgFile = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Cil.initCIL ();
        setGCConfig ();

        L.logStatus "Gathering stats";
        L.logStatus "-----";
        statFiles (Filename.dirname !cgFile);
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in instr_stats: " ^
                   (Printexc.to_string e)) ;
    printStatistics ();
    L.flushStatus ();
    raise e
;;

main () ;;
