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
open Logging
open Stdutil
open Cil_lvals
open Pretty
open Obj_stats
open Oo_partition
open Malloc_stats
open Type_reach
open Cildump

module Dist = Distributions

(******** ARG MANAGEMENT ********)
let cgFile = ref ""

let cgDir = ref ""

let lineCount = ref false

let prune = ref false

let doFPStats = ref false

let doOOStats = ref false

let findBinders = ref false

let printConstructCg = ref false

let printMallocTypes = ref false

let outFile = ref ""

let configFile = ref "client.cfg"

let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "call graph file");
   ("-l", Arg.Set lineCount, "just count lines reached");
   ("-u", Arg.Set prune, "prune unreachable from line counts");
   ("-fp", Arg.Set doFPStats, "just print function pointer stats");
   ("-oo", Arg.Set doOOStats, "print OO-pattern stats");
   ("-pb", Arg.Set findBinders, "print functions that bind func ptrs");
   ("-o", Arg.Set_string outFile, "target file to write results");
   ("-pccg", Arg.Set printConstructCg, "print constructor callgraph");
   ("-malt", Arg.Set printMallocTypes, "print types of malloc cells");
  ]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname [options]\n"


(************************************************************)
(* Basic global instruction stats *)

type instStats = {
  mutable numCalls : int;
  mutable numFPCalls : int;
  mutable numAssign : int;
  mutable numControl : int;
  mutable numInstrs : int;
  mutable numASM : int;
  mutable numPtrArith : int; (* number of deref(X + A)... doesn't work if they 
                                do Y = X + A; deref(Y) *)
  mutable funAddrTaken : int; 
  mutable dataAddrTaken : int;
  mutable offsetOf : int ;   (* Instances of (0)->field *)
  mutable ptr2int : int;     (* pointer -> int conversions *)
  mutable int2ptr : int;     (* int -> pointer conversions  *)

  mutable numGlobalVars: int;
}

let freshStats () =
  {
    numCalls = 0;
    numFPCalls = 0;
    numAssign = 0;
    numControl = 0;
    numInstrs = 0;
    numPtrArith = 0;
    numASM = 0;
    funAddrTaken = 0;
    dataAddrTaken = 0;
    offsetOf = 0;
    ptr2int = 0;
    int2ptr = 0;
    numGlobalVars = 0;
  }

let addInstr s =
  s.numInstrs <- s.numInstrs + 1

let addAssign s =
  s.numAssign <- s.numAssign + 1

let addControl s =
  s.numControl <- s.numControl + 1

let addASM s =
  s.numASM <- s.numASM + 1

let addCall s =
  s.numCalls <- s.numCalls + 1

let addFPCall s =
  s.numFPCalls <- s.numFPCalls + 1

let addPtrArith s =
(*  logStatusF "PtrArith found @ %s\n" (string_of_loc !currentLoc); *)
  s.numPtrArith <- s.numPtrArith + 1

let addFunAddrTaken s =
  s.funAddrTaken <- s.funAddrTaken + 1

let addDataAddrTaken s =
  s.dataAddrTaken <- s.dataAddrTaken + 1

let addOffsetOf s =
  logStatusF "OffsetOf found @ %s\n" (string_of_loc !currentLoc);
  s.offsetOf <- s.offsetOf + 1

let addPtr2Int s =
  s.ptr2int <- s.ptr2int + 1

let addInt2Ptr s =
  s.int2ptr <- s.int2ptr + 1

let addGlobalVar s =
  s.numGlobalVars <- s.numGlobalVars + 1

let combineStats s1 s2 =
  s1.numCalls <- s1.numCalls + s2.numCalls;
  s1.numFPCalls <- s1.numFPCalls + s2.numFPCalls;
  s1.numAssign <- s1.numAssign + s2.numAssign;
  s1.numControl <- s1.numControl + s2.numControl;
  s1.numInstrs <- s1.numInstrs + s2.numInstrs;
  s1.numPtrArith <- s1.numPtrArith + s2.numPtrArith;
  s1.numASM <- s1.numASM + s2.numASM;
  s1.funAddrTaken <- s1.funAddrTaken + s2.funAddrTaken;
  s1.dataAddrTaken <- s1.dataAddrTaken + s2.dataAddrTaken;
  s1.offsetOf <- s1.offsetOf + s2.offsetOf;
  s1.ptr2int <- s1.ptr2int + s2.ptr2int;
  s1.int2ptr <- s1.int2ptr + s2.int2ptr;
  s1.numGlobalVars <- s1.numGlobalVars + s2.numGlobalVars

let rec hasArithExp e =
  match e with 
    Lval (l) -> hasArithLval l
  | BinOp (PlusPI, _, e2, _)
  | BinOp (MinusPI, _, e2, _) ->
      ( match isInteger e2 with
          Some c -> 
           if c = Int64.one then false (* Ignore + 1 *)
           else true
        | None -> 
            (* TODO: ignore array-like strides in general... *)
            true ) 
  | BinOp (MinusPP, _, _, _) -> true
  | BinOp (_, e1, e2, _) ->
      hasArithExp e1 or hasArithExp e2
  | CastE (_, e1) ->
      hasArithExp e1
  | _ -> false
  
and hasArithLval lv =
  match lv with
    (Var _,_) -> false
  | (Mem(e),_) -> hasArithExp e

let isOffsetOfPattern lv =
  let rec isZeroExp exp =
    match exp with
      CastE (t, e) -> isZeroExp e
    | Const (c) -> Cil.isZero exp
    | _ -> false
  in
  match lv with
    Mem (e), _ -> isZeroExp e
  | Var _, _ -> false
      
let rec checkAddrTaken s e =
  match e with
    Lval l -> checkAddrTakenLv s l
  | AddrOf l
  | StartOf l -> 
      if isFunctionType (typeOfLval l) then
        addFunAddrTaken s
      else if isOffsetOfPattern l then
        addOffsetOf s
      else
        addDataAddrTaken s
      ;
      checkAddrTakenLv s l
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


(************************************************************)

type perFunc = {
  mutable funNumInsts : int;
  mutable funLoops : int;
  mutable funIfs : int;
  mutable funFP : int;
}

let freshPerFunc () =
  { funNumInsts = 0;
    funLoops = 0;
    funIfs = 0; 
    funFP = 0;
  }

let combinePFStats pfs1 pfs2 =
  (* Assume disjoint keys *)
  Hashtbl.iter (Hashtbl.add pfs1) pfs2
       

(************************************************************)

let combineAllStats (globS1, pfs1) (globS2, pfs2) =
  combineStats globS1 globS2;
  combinePFStats pfs1 pfs2


(************************************************************)

class statGatherer = object(self)
  inherit nopCilVisitor

  val stats = freshStats ()
  val perFuncStats = Hashtbl.create 17
  val mutable curPerFunc = None


  method private checkExp exp =
    checkAddrTaken stats exp;
    checkIntPtrCast stats exp

  method private flushOldPerFunc () =
    match curPerFunc with
      None -> ()
    | Some (fname, fkey, fstats) ->
        Hashtbl.add perFuncStats (fname, fkey) fstats;
        curPerFunc <- None

  method getStats = 
    self#flushOldPerFunc ();
    (stats, perFuncStats)

  method vfunc fdec =
    self#flushOldPerFunc ();
    curPerFunc <- Some (fdec.svar.vname, fdec.svar.vid, freshPerFunc ());
    DoChildren

  method addPerFuncOrNot foo =
    match curPerFunc with 
      None -> failwith "no cur func?"
    | Some x -> foo x

  method private addInstr () =
    addInstr stats;
    self#addPerFuncOrNot 
      (fun (fname, fk, pf) ->
         pf.funNumInsts <- pf.funNumInsts + 1)
      
  method private addControl skind =
    addControl stats;
    self#addPerFuncOrNot 
      (fun (fname, fk, pf) ->
         match skind with
           If _ | Switch _ -> pf.funIfs <- pf.funIfs + 1
         | Loop _ -> pf.funLoops <- pf.funLoops + 1
         | Goto _ | Break _ | Continue _ | Block  _ | TryFinally _ 
         | TryExcept _ | Return _ | Instr _ -> ()
      )

  method private addFPCall =
    addFPCall stats;
    self#addPerFuncOrNot 
      (fun (fname, fk, pf) -> pf.funFP <- pf.funFP + 1)

  (* visit an instruction; *)
  method vinst (i:instr) : instr list visitAction =
    self#addInstr ();
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
              self#addFPCall 

          (* Other indirect call? *)
          | _ ->
              self#addFPCall
             
         );
         
         List.iter self#checkExp args;
         self#checkExp callexp;

     | Set(lv, exp, _)  ->
         addAssign stats;
         if hasArithLval lv or hasArithExp exp then
           addPtrArith stats
         ;
         self#checkExp exp;
         checkIntPtrCastLv stats lv
    );
    DoChildren

  method vstmt (s:stmt) : stmt visitAction =
    (match s.skind with
       Instr _ -> () (* already counted *)
     | Goto _  
     | Break _
     | Continue _ 
     | Loop _ -> self#addControl s.skind; self#addInstr ();
     | Block _
     | TryFinally _
     | TryExcept _ 
     | Return (None, _) -> 
         addControl stats; 
         self#addInstr ();
     | Return (Some e, _) -> 
         addControl stats; 
         self#addInstr ();
         self#checkExp e
     | Switch (e, _, _, _)
     | If (e, _, _, _) -> 
         (* Don't count ptr cast here -- used w/ checking against null *)
         self#addControl s.skind; 
         self#addInstr ();
    ); DoChildren

  method vinit var off init =
    (match init with
       SingleInit exp -> self#checkExp exp
     | CompoundInit (t, moreInit) -> () (* TODO handle CompoundInit *)
    );
    DoChildren

  method vvdec varinfo =
    if varinfo.vglob && not (isFunctionType varinfo.vtype) 
       && not (Trans_alloc.isAllocVar varinfo.vname) then
      addGlobalVar stats;
    DoChildren

end


(** Get statistics on each instruction *)
let getStats (f:file) =
  let obj:statGatherer = (new statGatherer) in
  (* visit the whole file *)
  (visitCilFileSameGlobals (obj :> cilVisitor) f);
  (obj#getStats)
    

let printStats s =
  logStatusF "ptrArith: %d\n" s.numPtrArith;
  logStatusF "ASM: %d\n" s.numASM;
  logStatusF "fpCalls: %d\n" s.numFPCalls;
  logStatusF "calls: %d\n" s.numCalls;
  logStatusF "ratio Indirect: %f\n" 
    ((float_of_int s.numFPCalls) /. (float_of_int s.numCalls));
  logStatusF "assigns: %d\n" s.numAssign;
  logStatusF "control: %d\n" s.numControl;
  logStatusF "instructs: %d\n"  s.numInstrs;
  logStatusF "funcs addrTaken: %d\n" s.funAddrTaken;
  logStatusF "data addrTaken: %d\n" s.dataAddrTaken;
  logStatusF "offsetOf pattern: %d\n" s.offsetOf;
  logStatusF "ptr 2 int: %d\n" s.ptr2int;
  logStatusF "int 2 ptr: %d\n" s.int2ptr;
  logStatusF "num global vars: %d\n" s.numGlobalVars;
  logStatus "\n"


let printPFStats pfStats =
  let listed = Stdutil.mapToList Hashtbl.fold pfStats in
  (* sort by instruction count *)
  let sorted = List.sort 
    (fun (_, d1) (_, d2) -> 
       Pervasives.compare d1.funNumInsts d2.funNumInsts) listed in
  logStatusD (seq_to_doc line List.iter
                  (fun ((fname, fkey), data) ->
                     dprintf "%s:%d -> instrs %d, loops %d, ifs %d, fp %d"
                       fname fkey 
                       data.funNumInsts data.funLoops data.funIfs data.funFP
                  ) sorted nil);
  logStatus ""
    
let printAllStats (globStats, pfStats) =
  printStats globStats;
  printPFStats pfStats

(** Check call graphs for all AST files stored under [root] dir *)
let statFiles (root:string) : unit =
  let stats = freshStats (), Hashtbl.create 17 in
  Filetools.walkDir 
    (fun ast filename ->
       let newStats = getStats ast in
       combineAllStats stats newStats
    ) root;
  printAllStats stats


(************************************************************)
(* Function pointer stats *)

      
(** Print histogram of field declarations that are function pointers *)
let printFPFieldDecls () =
  let fpFields = Hashtbl.create 17 in
  let unrolledDist = Dist.makeDistro () in
  let nonunrolledDist = Dist.makeDistro () in
  let commonFPNames = Dist.makeDistro () in 
  (* Should have done offset instead of name? 
     What if they use embed-base-struct pattern? *)
  let countIt unrolledType finfo =
    Dist.updateDistro unrolledDist (string_of_type unrolledType);
    Dist.updateDistro nonunrolledDist (string_of_type finfo.ftype);
    Dist.updateDistro commonFPNames finfo.fname;
    Hashtbl.add fpFields finfo.fname finfo.ftype
  in

  Cilinfos.iterCompinfos 
    (fun compinfo ->
       List.iter 
         (fun finfo ->
            let unrolledType = unrollTypeNoAttrs finfo.ftype in
            if hitsFunptr unrolledType then
              countIt unrolledType finfo
         ) (!Cil.getCfields compinfo)
    );
  Dist.printDistroSortFreq 
    unrolledDist (fun x -> x) "Funptr unrolled counts";
  Dist.printDistroSortFreq
    nonunrolledDist (fun x -> x) "Fps nonunrolled counts";
  Dist.printDistroSortFreq
    commonFPNames (fun x -> x) "Funcptr field names";
  print_string "\nFunptr name : type\n----------------------------\n";
  Hashtbl.iter 
    (fun fpname fptype ->
       Printf.printf "%s : %s\n" fpname (string_of_type fptype);
    ) fpFields;
  print_string "\n\n"


let addToList table key v =
  let old = try Hashtbl.find table key with Not_found -> [] in
  Hashtbl.replace table key (List_utils.addOnce old v)


let printTable title table strOfKey strOfVals =
  print_string "====================================\n";
  Printf.printf "%s\n" title;
  print_string "====================================\n";
  Hashtbl.iter 
    (fun key list ->
       Printf.printf "%s\n -> [%s]\n" (strOfKey key) (strOfVals list);
    ) table;
  print_string "\n"
    

(** Get histogram for each {field, variable} x {uses in calls, defs} *)
class fpDefUseVisitor = object(self)
  inherit nopCilVisitor

  val variablesCalled = Dist.makeDistro ()

  val fieldsCalled = Dist.makeDistro ()

  (* Hack to prevent initializer from being counted too much *)
  val varAssignVisited = Hashtbl.create 17
  val fieldAssignVisited = Hashtbl.create 17 

  val variablesAssigned = Dist.makeDistro ()

  val fieldsAssigned = Dist.makeDistro ()

  val mutable curFunc = dummyFunDec

  (* Offset -> location list *)
  val callsThroughFields = Hashtbl.create 17
  val assignsThroughFields = Hashtbl.create 17

  (* Fun name -> offset list *)
  val callsThroughFieldsFun = Hashtbl.create 17
  val assignsThroughFieldsFun = Hashtbl.create 17


  method private addLoc table off loc =
    addToList table off loc

  method private addOff table funInfo off =
    addToList table funInfo off

  method vfunc f = 
    curFunc <- f;
    DoChildren

  method vinst i =
    match i with
      Set (lv, _, loc) -> self#processAssign loc lv
    | Asm (_, _, _, _, _, _) -> ()
    | Call(lvopt, callexp, args, loc) ->
        (match lvopt with Some lv -> self#processAssign loc lv | None -> ());
        self#processCall loc callexp;
    ;
    DoChildren

  method vinit var off init : Cil.init Cil.visitAction =
    curFunc <- dummyFunDec;
    let rec checkInit curOff curInit =
      (match curInit with
         SingleInit _ -> self#processAssign var.vdecl (Var var, curOff)
       | CompoundInit (t, moreInitList) -> 
           List.iter (fun (moreOff, moreInit) ->
                        checkInit (addOffset moreOff curOff) moreInit) 
             moreInitList
      ) in
    checkInit off init;
    DoChildren

  method private normalizeOffset off =
    string_of_offset off

  method private checkOffsetlessFp loc e offset =
    if offset = "" then begin
      let loc = string_of_loc loc in
      Printf.printf "Offset-less exp is (%s : %s)\n" (string_of_exp e) loc;
      try let baseVar = findBaseVarinfoExp e in 
      if baseVar.vglob then "--global" 
      else if Ciltools.isFormal curFunc baseVar then "--formal"
      else "--local"
      with BaseVINotFound -> offset
    end else offset
      
  method private checkOffsetlessVarFp loc var offset =
    if offset = "" then begin
      let loc = string_of_loc loc in
      Printf.printf "Offset-less var is (%s : %s)\n" var.vname loc;
      if var.vglob then "--global"
      else if Ciltools.isFormal curFunc var then "--formal"
      else "--local"
    end else offset
    
  (* what else can look like an assignment? Skipping arg passing *)
  method private processAssign loc lv =
    if lvalIsFunptr lv then begin
      try
        let baseVar = findBaseVarinfoLval lv in
        if not (Hashtbl.mem varAssignVisited (baseVar.vname, loc)) then begin
          Dist.updateDistro variablesAssigned (baseVar.vname, baseVar.vdecl);
          Hashtbl.add varAssignVisited (baseVar.vname, loc) ()
        end;
        match lv with
          Mem (e), offset ->
            let offNorm = self#normalizeOffset offset in
            let offNorm = self#checkOffsetlessFp loc e offNorm in
            if not (Hashtbl.mem fieldAssignVisited loc) then begin
              Dist.updateDistro fieldsAssigned offNorm;
              Hashtbl.add fieldAssignVisited loc ()
            end;
            self#addLoc assignsThroughFields offNorm loc;
            self#addOff assignsThroughFieldsFun 
              (curFunc.svar.vname, curFunc.svar.vdecl) offNorm
        | Var v, offset -> 
            let offNorm = (self#normalizeOffset offset) in
            let offNorm = self#checkOffsetlessVarFp loc v offNorm in
            if not (Hashtbl.mem fieldAssignVisited loc) then begin
              Dist.updateDistro fieldsAssigned offNorm;
              Hashtbl.add fieldAssignVisited loc ()
            end;
            self#addLoc assignsThroughFields offNorm loc;
            self#addOff assignsThroughFieldsFun 
              (curFunc.svar.vname, curFunc.svar.vdecl) offNorm

      with BaseVINotFound ->
        Printf.printf "VI not found for %s\n" (string_of_lval lv)
    end

  method private processCall loc exp =
    match exp with
      Lval lv -> self#processCallLv loc lv
    | _ -> failwith "Skipping non-lval call exp\n"
        
  method private processCallLv loc lv =
    match lv with
      Mem e, _ ->
        (try
           let baseVar = findBaseVarinfoLval lv in
           Dist.updateDistro variablesCalled (baseVar.vname, baseVar.vdecl);
           let offset = findTopOffsetLvExp e in
           let offNorm = self#normalizeOffset offset in
           let offNorm = self#checkOffsetlessFp loc e offNorm in
           Dist.updateDistro fieldsCalled offNorm;
           self#addLoc callsThroughFields offNorm loc;
           self#addOff callsThroughFieldsFun 
             (curFunc.svar.vname, curFunc.svar.vdecl) offNorm

         with BaseVINotFound ->
           Printf.printf "VI not found for %s\n" (string_of_lval lv)
        )
    | Var _, NoOffset -> ()
    | Var _, _ -> failwith "function should not have offset"

  method varDeclString (vname, decl) =
    vname ^ " : " ^ string_of_loc decl
    
  method printStats () =
    Dist.printDistroSortFreq variablesCalled self#varDeclString
      "Called base variables holding funptrs";
    Dist.printDistroSortFreq fieldsCalled (fun x -> x)
      "Called fields holding funptrs";
    Dist.printDistroSortFreq variablesAssigned self#varDeclString
      "Assigned base variables holding funptrs";
    Dist.printDistroSortFreq fieldsAssigned (fun x -> x)
      "Assigned fields holding funptrs"
      

  method private printLocsTable title table =
    printTable title table 
      (fun x -> x) 
      (fun loclist ->
         let docList = List.fold_left 
           (fun curDoc loc ->
              curDoc ++ d_loc () loc ++ line
           ) nil loclist in
         (sprint 80 (indent 2 docList)) )

  method fnameFlocStr (fname, declloc) =
    Printf.sprintf "(%s, %s)" fname (string_of_loc declloc)
      
  method offListStr offlist =
    let docList = List.fold_left 
      (fun curDoc off ->
         curDoc ++ text off ++ line
      ) nil offlist in
    (sprint 80 (indent 2 docList))
    

  method private printOffsTable title table =
    printTable title table self#fnameFlocStr self#offListStr

  method private printFunsWithAssignAndCall assignT callT =
    print_string "====================================\n";
    Printf.printf "Functions w/ call and assign to funptr fields\n";
    print_string "====================================\n";
    Hashtbl.iter
      (fun (fn1, floc1) assignFields ->
         try
           let callFields = Hashtbl.find callT (fn1, floc1) in
           Printf.printf "%s @ %s\n" fn1 (string_of_loc floc1);
           Printf.printf "assign [%s]\nvs call [%s]\n" 
             (self#offListStr assignFields) (self#offListStr callFields)
         with Not_found -> ()
      ) assignT
      
  method printLocs () =
    self#printLocsTable "Assignments to fields (off -> loc list)" 
      assignsThroughFields;
    self#printLocsTable "Calls through fields (off -> loc list)" 
      callsThroughFields;
    self#printOffsTable "Assignments to fields (fun -> off list)" 
      assignsThroughFieldsFun;
    self#printOffsTable "Calls through fields (fun -> off list)" 
      callsThroughFieldsFun;
    self#printFunsWithAssignAndCall 
      assignsThroughFieldsFun callsThroughFieldsFun

end


let printFPDefUses root =
  let vis = new fpDefUseVisitor in
  Filetools.walkDir 
    (fun ast filename ->
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
    ) root;
  vis#printStats ();
  vis#printLocs ()

let printFPStats () =
  printFPFieldDecls ();
  printFPDefUses !cgDir

 
(************************************************************)

let initSettings () = begin
  Cilinfos.reloadRanges !cgDir;
  let clientSet = Config.initSettings !configFile in
  Default_cache.makeLCaches (!cgDir);
  Alias.initSettings clientSet !cgDir;
  Threads.initSettings clientSet;
  Entry_points.initSettings clientSet;
  let cgFile = Dumpcalls.getCallsFile !cgDir in
  let cg = Callg.readCalls cgFile in
  cg
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
        let cg = initSettings () in
        let () = if !lineCount then begin
          logStatus "\nCounting lines";
          logStatus "-----\n";
          Basic_line_count.doLineCount !prune !cgDir cg;
        end
        else if !doFPStats then begin
          logStatus "\nPrinting funptr field stats";
          logStatus "-----\n";
          printFPStats ();
        end 
        else if !doOOStats then begin
          logStatus "\nPrinting OO-style declaration stats";
          logStatus "-----\n";
          checkOOness !cgDir;
        end
        else if !findBinders then begin
          logStatus "\nPrinting binders for OO-style use";
          logStatus "-----\n";
          partitionFuncs !cgDir cg !outFile;
        end
        else if !printConstructCg then begin
          logStatus "\nPrinting sliced callgraph reaching FP binders";
          logStatus "-----\n";
          printConstructionCG !cgDir cg !outFile;
        end
        else if !printMallocTypes then begin
          logStatus "\nPrinting type stats for Malloc Targets";
          logStatus "-----\n";
          printAllocTypes !cgDir;
        end
        else begin 
          logStatus "\nGathering stats";
          logStatus "-----\n";
          statFiles !cgDir;
        end in

        printStatistics ();
        exit 0;
      end
  with e -> 
    logStatus ("Exc. in instr_stats: " ^
                   (Printexc.to_string e)) ;
    printStatistics ();
    flushStatus ();
    raise e
;;

main () ;;
