(*
  Copyright (c) 2008-2009, Regents of the University of California

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


(** Parse logs of each function call as "ENTER: foo / EXIT: foo" 
    and convert that into a dynamic callgraph.
    
    compile program with "cilly --dologcalls2" to log the ENTER and EXIT 

    The callgraph will be in a funny format
    (1) we will not be able to say if a call is indirect or direct
    (2) therefore, we will say everything is direct to avoid having
        to make up program points of indirect calls. 
    (3) also, because we are not running against the actual analyzed
        (and transformed) code the function identifiers will not match *)

open Stdutil 
open Pretty
open Gc_stats
open Logging
open Callg

(***************************************************)
(* Commandline handling                            *)

(** directory to dump dynamic call graph into *)
let cgDir = ref ""

(** file to dump dynamic call graph into *)
let cgFile = ref "calls.dynamic"
let baseCGFile = "calls.steens"

(** list of input files w/ the log of ENTER and EXITs *)
let (inLogs : string list ref) = ref []

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "output call graph directory");
   ("-o", Arg.Set_string cgFile, "output call graph file");
  ]
    
let anonArgFun (arg:string) : unit = 
  inLogs := List_utils.addOnce !inLogs arg
    
let usageMsg = getUsageString "-cg outdir inlog1 [inlog2...]\n"
  
(************************************************************)
(* utils *)

(** Take an existing CG and remove indirect calls (completely) *)
let emptyOutIndirect cg = 
  FMap.map 
    (fun node -> 
       { node with
           ccallees = List.map
           (fun call ->
              match call with 
                CDirect _ -> call | CIndirect (pp, _) -> CIndirect (pp, [])
           ) node.ccallees;
       }
    ) cg

(** Build an map from fun name -> fun ID *)
let indexFunNames index callg =
  FMap.iter 
    (fun fid fnode ->
       let fname = fnode.name in
       try
         let oldID = Hashtbl.find index fname in
         logErrorF "Duplicate funname %s -> %s or %s\n" fname
           (fid_to_string fid) (fid_to_string oldID)
       with Not_found -> 
         Hashtbl.add index fname fid
    ) callg

let funOfName index callg fname =
  let fid = Hashtbl.find index fname in
  let fnode = FMap.find fid callg in
  (fid, fnode)

let funStrsToFuns index callg (funStrs : string list) : (funID * Cil.typ) list = 
  List.fold_left 
    (fun cur funName ->
       try
         let fid, fnode = funOfName index callg funName in
         let fvar = Cilinfos.getVarinfo (fid_to_fkey fid) in
         (fid, fvar.Cil.vtype) :: cur
       with Not_found ->
         logErrorF "Can't find info for %s\n" funName;
         cur
    ) [] funStrs


(** Estimate the coverage of the dynamic callgraph as
    (#Funs w/ Indirect Covered / #Fun w/ Indirect) *)
let measureCoverage dynamicCG =
  let numCov, numIndir = 
    FMap.fold 
      (fun fkey call (numCov, numIndir) -> 
         let hasCov, hasIndir = List.fold_left 
           (fun (hasCov, hasIndir) callTarg ->
              match callTarg with
                CIndirect (_, l) -> 
                  (hasCov || not (l = [])), true
              | CDirect _ ->
                  hasCov, hasIndir
           ) (false, false) (calleeDetail call) in
         if hasCov then begin
           assert hasIndir;
           (numCov + 1, numIndir + 1)
         end else begin
           if hasIndir then
             (numCov, numIndir + 1)
           else 
             (numCov, numIndir)
         end
      ) dynamicCG (0, 0)
  in
  if numIndir <> 0 then
    logStatusF "Coverage of Funs with Indirect Calls: %d / %d = %f\n" 
      numCov numIndir ((float_of_int numCov) /. (float_of_int numIndir))
  else 
    logStatus "No indirect calls in benchmark!"


(***************************************************)
(* Run                                             *)

(** Make our own simple call graph then convert it afterwards *)
  
let rec genDynamicCG inLogs (outFile : string) : unit = 
  open_out_for outFile (genWithOut inLogs) 
    
and genWithOut inLogs oc : unit =

  (** Only track indirect calls for now *)
  let (simplecg : (string, string list) Hashtbl.t) = Hashtbl.create 17 in
  let (name_to_id : (string, funID) Hashtbl.t) = Hashtbl.create 17 in 
  let curID = ref 1 in
  
  let ensureID name =
    if Hashtbl.mem name_to_id name then ()
    else 
      let myID = !curID in
      incr curID;
      Hashtbl.add name_to_id name (fkey_to_fid myID)
  in

  let ensureNode name =
    if Hashtbl.mem simplecg name then ()
    else Hashtbl.add simplecg name []
  in
  
  let printStack callstack = 
    logErrorD 
      (text "Stack:\n" ++ 
         indent 2 (seq_to_doc (text "\n") Stack.iter
                   (fun fname -> text fname) callstack nil) ++ 
         line)
  in

  let isIndirectCall lastCalled =
    String.contains lastCalled '*' || 
      (String.contains lastCalled '-' && String.contains lastCalled '>')
  in

  let splitter = Str.split_delim (Str.regexp "[:]") in
  List.iter 
    (fun inFile ->
       let callstack = Stack.create () in
       let curLine = ref 0 in

       (* Hack to make sure we pop enough... for now... *)
       let lastPopped = ref "" in
       let lastCalled = ref "" in
       let doPop () =
         let p = Stack.pop callstack in
         lastPopped := p;
         p
       in
       let checkCallReturn nextLine = 
         try 
           let funname = Scanf.sscanf nextLine "return from %s" 
             (fun x -> x) in
           if !lastPopped <> "" && !lastPopped <> funname then begin
             let top = Stack.top callstack in
             if top = funname then begin
               logErrorF "[%s:%d] Delay in pop (%s)\n" 
                 inFile !curLine funname;
               ignore (doPop ());
             end else if funname = !lastCalled then
               () (* okay. called and returned from external function *)
             else begin
               (* May be different if we printed an indirect call exp *)
               if String.contains funname '*' then
                 () (*logErrorF "[%s:%d] Funptr call? (%s vs %s)\n"
                      inFile !curLine funname top *)
               else
                 logErrorF "[%s:%d] Haven't cleared pop (%s vs %s vs %s)\n" 
                   inFile !curLine !lastPopped funname top   
             end
           end
         with Scanf.Scan_failure _ ->
           try
             let funname = Scanf.sscanf nextLine "call %s" 
               (fun x -> x) in
             lastCalled := funname
           with Scanf.Scan_failure _ ->
             unmatchedLine nextLine
       in
       (* /Hack *)

       let processIn ic =
         try
           while true do
             let nextLine = input_line ic in
             incr curLine;
             match splitter nextLine with
               [kind; funname] ->
                 let kind = Strutil.strip kind in
                 let funname = Strutil.strip funname in
                 if kind = "ENTER" then begin
                   (try
                      let caller = Stack.top callstack in
                      if isIndirectCall !lastCalled then
                        addCall simplecg caller funname;
                      
                      (* DEBUG *)
                      if funname = "main" then
                        logErrorF "[%s:%d] Adding call to main?\n" 
                          inFile !curLine;

                      if caller = "die_builtin" &&
                        funname = "sideband_demux" then begin
                          logErrorF "[%s:%d] FROM die_b. TO sideband_demux?\n" 
                            inFile !curLine;
                          printStack callstack
                        end;
                      
                    with Stack.Empty ->
                      logStatusF "Root function is %s\n" funname);
                   ensureID funname;
                   ensureNode funname;
                   Stack.push funname callstack;
                 end else if kind = "EXIT" then begin
                   let poppedGuy = doPop () in
                   if poppedGuy <> funname then begin
                     logErrorF "[%s:%d] Popped guy (%s) diff from EXIT (%s)\n"
                       inFile !curLine poppedGuy funname;
                     printStack callstack;
                     raise End_of_file (* just end it for now... *)
                   end;
                 end else
                   unmatchedLine nextLine
             | _ -> 
                 (* Hack to make sure we pop enough... for now... *)
                 checkCallReturn nextLine;
           done;
         with End_of_file ->
           logStatusF "DONE reading file: %s\n" inFile;
           flushStatus ()
       in
       logStatusF "Processing file %s\n" inFile;
       flushStatus ();
       open_in_for inFile processIn;
       let leftOnStack = Stack.length callstack in
       if leftOnStack <> 0 then
         logErrorF "Still have %d in stack\n" leftOnStack;
       Stack.clear callstack
    ) inLogs;
  logStatus "DONE reading all -- now converting";
  convertCG simplecg name_to_id oc
      
and unmatchedLine line =
  logStatusF "Skipping: %s\n" line
    
and addCall simplecg caller callee = 
  let old = 
    try Hashtbl.find simplecg caller with Not_found -> [] in
  Hashtbl.replace simplecg caller (List_utils.addOnce old callee)

and convertCG simplecg name_to_id oc =
  let builtCG = ref (readCalls (Filename.concat !cgDir baseCGFile)) in
  builtCG := emptyOutIndirect !builtCG;
  let funName2funID = Hashtbl.create 17 in
  indexFunNames funName2funID !builtCG;
  let dummyPP = Cil.ppUnknown in
  let finalCG = Hashtbl.fold 
    (fun caller callees cg ->
       try
         let callerID, callerNode = funOfName funName2funID cg caller in
         let callees = List.fold_left
           (fun callees callee -> 
              try
                let calleeID, _ = funOfName funName2funID cg callee in
                addCallee (CIndirect (dummyPP, [calleeID])) callees
              with Not_found ->
                logErrorF "Couldn't find callee %s\n" callee;
                callees
           ) (calleeDetail callerNode) callees in
         let callerNode = { callerNode with
                              ccallees = callees; 
                              hasBody = true;
                          } in
         FMap.add callerID callerNode cg
       with Not_found ->
         logErrorF "Couldn't find caller %s\n" caller;
         cg
    ) simplecg !builtCG in
  logStatus "DONE converting -- now writing!";
  writeSomeCalls oc finalCG;
  logStatus "DONE writing!";
  measureCoverage finalCG


(************************************************************)

let doCleanup () =
  printStatistics ()

let main () = 
  Arg.parse argSpecs anonArgFun usageMsg;
  (* Didn't know how to require the -cg file, etc., so check manually *)
  if (!cgDir = "" || !inLogs = []) then begin
    Arg.usage argSpecs usageMsg;
    exit 1
  end else begin
    combineLogs ();
    Stdutil.printCmdline ();
    Cil.initCIL ();
    Pervasives.at_exit doCleanup;

    let outFile = Filename.concat !cgDir !cgFile in
    logStatusF "Making a dynamic callgraph into %s from %s" outFile
      (sprint 80 (text "[" ++ 
                    seq_to_doc (text " ") List.iter
                    (fun fname -> text fname) !inLogs nil ++ 
                    text "]")) ;
    logStatus "-----";
    genDynamicCG !inLogs outFile;

    exit 0
  end

;;
main () ;;
