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

let cgDir = ref ""

let lineCount = ref false

let prune = ref false

let configFile = ref "client.cfg"

let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "call graph directory");
   ("-l", Arg.Set lineCount, "just count lines reached");
   ("-u", Arg.Set prune, "prune unreachable from line counts");]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname [options]\n"


(*********)

type instStats = {
  mutable numCalls : int;
  mutable numFPCalls : int;
  mutable numInstrs : int;
  mutable numASM : int;
  mutable numPtrArith : int; (* number of deref(X + A)... doesn't work if they 
                                do Y = X + A; deref(Y) *)
  mutable numAddrTaken : int; 
  mutable ptr2int : int;     (* pointer -> int conversions *)
  mutable int2ptr : int;     (* int -> pointer conversions  *)
}


let freshStats () =
  {
    numCalls = 0;
    numFPCalls = 0;
    numInstrs = 0;
    numPtrArith = 0;
    numASM = 0;
    numAddrTaken = 0;
    ptr2int = 0;
    int2ptr = 0;
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

let addAddrTaken s =
  s.numAddrTaken <- s.numAddrTaken + 1

let addPtr2Int s =
  s.ptr2int <- s.ptr2int + 1

let addInt2Ptr s =
  s.int2ptr <- s.int2ptr + 1

let combineStats s1 s2 =
  s1.numCalls <- s1.numCalls + s2.numCalls;
  s1.numFPCalls <- s1.numFPCalls + s2.numFPCalls;
  s1.numInstrs <- s1.numInstrs + s2.numInstrs;
  s1.numPtrArith <- s1.numPtrArith + s2.numPtrArith;
  s1.numASM <- s1.numASM + s2.numASM;
  s1.numAddrTaken <- s1.numAddrTaken + s2.numAddrTaken;
  s1.ptr2int <- s1.ptr2int + s2.ptr2int;
  s1.int2ptr <- s1.int2ptr + s2.int2ptr

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

let rec checkAddrTaken s e =
  match e with
    Lval l -> checkAddrTakenLv s l
  | AddrOf l
  | StartOf l -> addAddrTaken s; checkAddrTakenLv s l
  | Const (CStr str) ->
      (* Ignore taking addr of string constant for now? *)
      ()
  | Const _
  | SizeOf _
  | SizeOfStr _
  | AlignOf _ -> ()
  | AlignOfE e1
  | SizeOfE e1 -> checkAddrTaken s e1
  | UnOp (op, e1, t) -> checkAddrTaken s e1
  | BinOp (op, e1, e2, t) -> checkAddrTaken s e1; checkAddrTaken s e2
  | CastE (t, e1) -> checkAddrTaken s e1


and checkAddrTakenLv s l =
  match l with
    Var _, _ -> ()
  | Mem e, _ -> checkAddrTaken s e

let rec checkIntPtrCast s e =
  match e with
    CastE (typC, e1) ->
      let typE = typeOf e1 in
      (match typC, typE with
         TInt _, TPtr _ -> addPtr2Int s
       | TPtr _, TInt _ -> addInt2Ptr s
       | _, _ -> ())
  | Lval l   
  | AddrOf l
  | StartOf l -> checkIntPtrCastLv s l
  | Const _ 
  | AlignOf _
  | SizeOf _ 
  | SizeOfStr _ -> ()
  | AlignOfE _
  | SizeOfE _ -> () (* don't care about the possible cast here *)
  | UnOp (_, e1, _) -> checkIntPtrCast s e1
  | BinOp (_, e1, e2, _) -> 
      checkIntPtrCast s e1;
      checkIntPtrCast s e2

and checkIntPtrCastLv s lv =
  match lv with
    Var _, _ -> ()
  | Mem e, _ -> checkIntPtrCast s e


class statGatherer = object(self)
  inherit nopCilVisitor

  val stats = freshStats ()

  method getStats = stats

  (* visit an instruction; *)
  method vinst (i:instr) : instr list visitAction =
    addInstr stats;
    (match i with
       Asm (_) ->
         addASM stats

     | Call(_,callexp,args,_) -> 
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
             
         );
         
         List.iter (checkAddrTaken stats) args;

         checkIntPtrCast stats callexp;
         List.iter (checkIntPtrCast stats) args

     | Set(lv, exp, _)  -> 
         if hasArithLval lv or hasArithExp exp then
           addPtrArith stats
         ;
         checkAddrTaken stats exp;

         checkIntPtrCast stats exp;
         checkIntPtrCastLv stats lv
    );
    DoChildren

  method vstmt (s:stmt) : stmt visitAction =
    (match s.skind with
       Instr _ 
     | Goto _  (* not counted as an instruction *)
     | Break _
     | Continue _
     | Loop _
     | Block _
     | TryFinally _
     | TryExcept _ 
     | Return (None, _) -> ()
     | Return (Some e, _) ->
         checkAddrTaken stats e;
         checkIntPtrCast stats e
         (* not counted as an instruction *)
     | Switch (e, _, _, _)
     | If (e, _, _, _) -> 
         (* Don't count ptr cast here -- used w/ checking against null *)
         (* not counted as an instruction *)
         ()
    ); DoChildren

end


class approxLineCountVisitor = object(self)
  inherit nopCilVisitor

  val mutable maxLine = -1

  method processLoc loc =
    if loc.line > maxLine then
      maxLine <- loc.line

  method getMaxLine = maxLine

  method vinst i =
    match i with
      Set (_, _, loc)
    | Asm (_, _, _, _, _, loc) 
    | Call (_,  _, _, loc) ->
        self#processLoc loc
    ;
    DoChildren

  method vstmt s =
    (match s.skind with
       Return (_, loc)
     | Goto (_, loc)
     | Break loc
     | Continue loc
     | If (_, _, _, loc)
     | Switch (_, _, _, loc)
     | Loop (_, loc, _, _)
     | TryFinally (_, _, loc)
     | TryExcept (_, _, _, loc) ->
         self#processLoc loc
     | _ -> ()
    );
    DoChildren


end

let getMaxLine (f:file) : int =
  let obj:approxLineCountVisitor = (new approxLineCountVisitor) in
  (visitCilFileSameGlobals (obj :> cilVisitor) f);
  (obj#getMaxLine)


(** Get statistics on each instruction *)
let getStats (f:file) : instStats =
  let obj:statGatherer = (new statGatherer) in
  (* visit the whole file *)
  (visitCilFileSameGlobals (obj :> cilVisitor) f);
  (obj#getStats)
    

let printStats s =
  L.logStatusF "ptrArith: %d\n" s.numPtrArith;
  L.logStatusF "ASM: %d\n" s.numASM;
  L.logStatusF "fpCalls: %d\n" s.numFPCalls;
  L.logStatusF "calls: %d\n" s.numCalls;
  L.logStatusF "instructs: %d\n"  s.numInstrs;
  L.logStatusF "addrTaken: %d\n" s.numAddrTaken;
  L.logStatusF "ptr 2 int: %d\n" s.ptr2int;
  L.logStatusF "int 2 ptr: %d\n" s.int2ptr;
  L.logStatus "\n"


(** Make call graph for all files starting at root *)
let statFiles root : unit =
  let stats = freshStats () in
  Filetools.walkDir 
    (fun ast filename ->
       let newStats = getStats ast in
       combineStats stats newStats
    ) root;
  printStats stats


let doLineCount cgFile =
  let cg = Readcalls.readCalls cgFile in
  let sccCG = Scc_cg.getSCCGraph cg in
  let newSCCCG = if (!prune) then
    let rooter = new Entry_points.rootGetter cg !cgDir in
    let rootSet = rooter#getRootKeys () in
    let reachable, _ = Callg.getReachableFunctions cg rootSet in
    Scc_cg.pruneUnreached sccCG reachable
  else
    sccCG
  in
  let fnames = Scc_cg.filesOfPruned cg newSCCCG in
  let totalLines = ref 0 in
  Scc_cg.iterFiles (fun ast -> 
                      let more = getMaxLine ast in
                      if more < 0 then L.logError "getMaxLine return < 0?"
                      else totalLines := !totalLines + more
                   ) fnames !cgDir;
  L.logStatusF "Total files: %d \t lines processed: %d\n\n"
    (Hashtbl.length fnames) !totalLines
    

let initSettings () = begin
  Cilinfos.reloadRanges !cgDir;
  let clientSet = Config.initSettings !configFile in
  Default_cache.makeLCaches (!cgDir);
  Alias.initSettings clientSet !cgDir;
  Threads.initSettings clientSet;
  Entry_points.initSettings clientSet;
end

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
        
        cgDir := Filename.dirname !cgFile;
        initSettings ();
        if !lineCount then begin
          L.logStatus "\nCounting lines";
          L.logStatus "-----\n";
          doLineCount !cgFile;
        end
        else begin 
          L.logStatus "\nGathering stats";
          L.logStatus "-----\n";
          statFiles (Filename.dirname !cgFile);
        end;

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
