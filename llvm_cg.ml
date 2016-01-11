(*
  Copyright (c) 2009, Regents of the University of California

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


(** Read a log of LLVM function call data + a starting callgraph 
    (for the direct calls) to generate an LLVM callgraph file *)

open Cil
open Stdutil 
open Logging
open Callg

(***************************************************)
(* Commandline handling                            *)

(** directory to dump call graph into *)
let cgDir = ref ""

(** file to dump call graph into *)
let cgFile = ref "calls.llvm"

(** file with raw LLVM call data *)
let llvmLog = ref ""

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "output call graph directory");
   ("-o", Arg.Set_string cgFile, "output call graph file");
   ("-i", Arg.Set_string llvmLog, "input raw LLVM log of calls");
  ]
    
let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-i llvmlog -cg outdir [opts]\n"

(************************************************************)

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

let baseCGFile = "calls.steens"

let startPattern = Str.regexp "BEGIN_LLVM_CG===.*"
let endPattern = Str.regexp "END_LLVM_CG===.*"
let funSplitter = Str.split (Str.regexp " ")

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

let funStrsToFuns index callg (funStrs : string list) : (funID * typ) list = 
  List.fold_left 
    (fun cur funName ->
       try
         let fid, fnode = funOfName index callg funName in
         let fvar = Cilinfos.getVarinfo (fid_to_fkey fid) in
         List_utils.addOnce cur (fid, fvar.vtype)
       with Not_found ->
         logErrorF "Can't find info for %s\n" funName;
         cur
    ) [] funStrs

let isLocUnknown loc = compareLoc locUnknown loc == 0

type linePPInfo = 
  { lppLoc : location;
    lppPP : prog_point;
    lppInst : instr; }

let closestLinePP oldOpt lpp2 lineNum =
  match oldOpt with
    None -> Some lpp2
  | Some lpp1 ->
      if isLocUnknown lpp1.lppLoc then Some lpp2
      else if isLocUnknown lpp2.lppLoc then oldOpt
      else 
        let diff1 = abs (lpp1.lppLoc.line - lineNum) in
        let diff2 = abs (lpp2.lppLoc.line - lineNum) in
        if diff2 <= diff1 then begin
          if diff1 == 0 && diff2 == 0 then begin
            logError "Two points w/ same lineNum, taking later one"
          end;
          Some lpp2
        end else oldOpt

let findCallPP callerName callerFkey fileName lineNum = 
  match Cilinfos.getFunc callerFkey fileName with
    Some fdec ->
      List.fold_left 
        (fun cur stmt ->
           let stmtPP = getStmtPP stmt in
           match stmt.skind with
             Instr il -> 
               let closer, _ = List.fold_left 
                 (fun (cur, idx) instr ->
                    match instr with
                      Call (_, Lval (Mem (_), _), _, _) ->
                        let instrLoc = get_instrLoc instr in
                        let instrPP = getInstrPP stmtPP instr idx in
                        let newInfo = { lppLoc = instrLoc;
                                        lppPP = instrPP;
                                        lppInst = instr; } in
                        (closestLinePP cur newInfo lineNum, idx + 1)
                    | _ -> (cur, idx + 1)
                 ) (cur, 0) il in
               closer
           | _ -> cur
        ) None fdec.sallstmts
  | None -> failwith ("findCallPP can't find func: " ^ callerName)

let filterByType callees lpp =
  let callSiteSig = 
    match lpp.lppInst with
      Call (_, Lval (Mem (exp), _), _, _) ->
        Type_utils.getFunSigPtrExp exp
    | _ -> failwith "filterByType given non-indirect call"
  in
  List.fold_left
    (fun cur (fid, typ) -> 
      if (Type_utils.functionTypeMatches typ callSiteSig) 
      then fid :: cur else cur)
    [] callees

let addIndirectCall cg callerID callerNode lpp callees = 
  let callees = filterByType callees lpp in
  let newCall = CIndirect (lpp.lppPP, callees) in
  let newCallees = 
    try 
      let _, newList = List_utils.listFindReplace
        (fun call _ ->
           match call with
             CDirect _ -> false
           | CIndirect (pp, _) -> pp = lpp.lppPP ) 
        (fun oldCall newCall ->
           match oldCall, newCall with
             CIndirect (pp1, callees1), CIndirect (pp2, callees2) ->
               assert (pp1 = pp2);
               assert (lpp.lppPP = pp1);
               CIndirect (pp1, mergeIndirectTargs callees1 callees2)
           | _, _ -> failwith "addIndirectCall given direct calls")
        newCall callerNode.ccallees in
      newList
    with Not_found ->
      addCallee newCall callerNode.ccallees in
  let newNode = { callerNode with ccallees = newCallees } in
  FMap.add callerID newNode cg



let genLLVMCG (inlog : string) (outFile : string) =
  let builtCG = ref (readCalls (Filename.concat !cgDir baseCGFile)) in
  builtCG := emptyOutIndirect !builtCG;
  let funName2funID = Hashtbl.create 17 in
  indexFunNames funName2funID !builtCG;
  let doReadLLVMLog (ic : in_channel) : unit =
    let processCall (callerName : string) (fileName : string) (lineNum:int) =
      let funLine = input_line ic in
      let callees = 
        funStrsToFuns funName2funID !builtCG (funSplitter funLine) in
      if callees = [] then ()
      else 
        let callees = List.sort (fun (fid1, _) (fid2, _) -> 
                                   compareFunID fid1 fid2) callees in
        let callerID, callerNode = 
          funOfName funName2funID !builtCG callerName in
        let callPPOpt = 
          findCallPP callerName (fid_to_fkey callerID) fileName lineNum in
        match callPPOpt with
          None -> failwith (Printf.sprintf "Can't find callPP for %s @ %s:%d\n"
                              callerName fileName lineNum)
        | Some lpp ->
            if lpp.lppLoc.line != lineNum then
              logErrorF "Line num of call not exact: %s @ %s:%d vs %d\n"
                callerName fileName lineNum lpp.lppLoc.line ;
            builtCG := 
              addIndirectCall !builtCG callerID callerNode lpp callees
    in
    let startedYet = ref false in
    let doneYet = ref false in
    (try
       while not (!doneYet) do
         let line = input_line ic in
         if Str.string_match startPattern line 0 then
           startedYet := true
         else if Str.string_match endPattern line 0 then
           doneYet := true
         else 
           if !startedYet && not !doneYet then begin
             Scanf.sscanf line "%s # %s@:%d" processCall
           end
       done
     with End_of_file ->
       ()
    );
    if !doneYet then logStatus "Done reading LLVM LOG"
    else failwith "LLVM log is truncated?"
  in
  open_in_for inlog doReadLLVMLog;
  open_out_for outFile (fun oc -> writeSomeCalls oc !builtCG)


(************************************************************)

let doCleanup () =
  Gc_stats.printStatistics ()

let main () = 
  Arg.parse argSpecs anonArgFun usageMsg;
  (* Didn't know how to require the -cg file, etc., so check manually *)
  if (!cgDir = "" || !llvmLog = "") then begin
    Arg.usage argSpecs usageMsg;
    exit 1
  end else begin
    combineLogs ();
    Stdutil.printCmdline ();
    Cil.initCIL ();
    Pervasives.at_exit doCleanup;
    Cilinfos.reloadRanges !cgDir;
    Default_cache.makeLCaches !cgDir;
    let outFile = Filename.concat !cgDir !cgFile in
    logStatusF "Making LLVM callgraph %s based on %s\n" outFile !llvmLog;
    logStatus "-----";
    genLLVMCG !llvmLog outFile;

    exit 0
  end

;;
main () ;;
