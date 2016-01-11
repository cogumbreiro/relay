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
    Merge multiple source files and output resulting AST (in text form).
    
    Warning: untested on large examples... (may be very inefficient)
    Also, make sure that the input files can actually be linked (duh).
    Namely, do not merge files that may have namespace conflicts.

    @see scripts/merge.sh (for an alternative that uses the CIL merger)
*)

open Gc_stats
open Stdutil
open Logging

let debug = false


(***************************************************)
(* Commandline handling                            *)


let in_file = ref ""

let out_file = ref ""

let cgDir = ref ""

(* Indicates whether input files are CIL binary ASTs. Even if input is
   text, they must be preprocessed *)
let inBinary = ref true

let outBinary = ref true

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-i", Arg.Set_string in_file, "file w/ source file names, one per line");
   ("-o", Arg.Set_string out_file, "output file name");
   ("-cg", Arg.Set_string cgDir, "use files in callgraph directory as input");
   ("-nib", Arg.Clear inBinary, "input files are NOT binary ASTs");
   ("-nob", Arg.Clear outBinary, "output file should NOT be a binary AST");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "[-i fname | -cg cgDir] -o fname [opts]\n"


let useInFile () = !in_file <> ""
let useCGDir () = !cgDir <> "" 

(***************************************************
   Custom pretty printer that doesn't print 
   original line nums at EVERY instruction... 
 ***************************************************)

class simplePrinter = object (self)
  inherit Cil.defaultCilPrinterClass as super
    
  val mutable printingInstr = false
      
  method pInstr () (i:Cil.instr) : Pretty.doc  =
    printingInstr <- true;
    let result = super#pInstr () i in
    printingInstr <- false;
    result
    
  method pLineDirective ?(forcefile = false) (loc:Cil.location) : Pretty.doc =
    if (printingInstr) then Pretty.nil
    else super#pLineDirective ~forcefile:forcefile loc

(** TODO: reduce messy type-casting when printing *)

end

let myPrinter = new simplePrinter


(***************************************************)
(* Merge Functions                                 *)

(** Process a chunk of files (of this number) at a time *)
let chunkSize = 10      

(** Load a file from disk *)
let getFile (fname:string) : Cil.file =
  try
    if(!inBinary) then
      Cil.loadBinaryFile fname
    else
      let p = Frontc.parse fname in
      p ();
  with Frontc.ParseError s ->
    (logStatus ("Exception in getFile: " ^ fname ^ " : " ^ s));
    raise (Frontc.ParseError s)


(** Wrapper for merging a list of files for the output *)
let doMerge (files : Cil.file list) : Cil.file =
  let result = Mergecil.merge files !out_file in
  if !Errormsg.hadErrors then
    Errormsg.warn "There were errors during merging\n"
  ;
  result
      

(** Process the next set of files (output is the last in the list) *)
let iterChunks (curChunk : Cil.file list) (nextFile:Cil.file) : Cil.file list =
  if (List.length curChunk > chunkSize) then
    [doMerge curChunk]
  else
    (nextFile :: curChunk)


(** Handle the last chunk of files *)
let finalizeChunk (curChunk : Cil.file list) : Cil.file =
  match curChunk with
    [aFile] -> aFile
  | h :: _ -> 
      doMerge curChunk
  | [] -> failwith "No input files?"
      

(** Output the result *)
let writeOutput (merged:Cil.file) : unit =
  if (!outBinary) then
    Cil.saveBinaryFile merged !out_file
  else
    (let out_channel = open_out !out_file in
     Cil.dumpFile myPrinter out_channel !out_file merged;
     close_out out_channel)


(* TODO incrementally merge via divide and conquer instead of
   using this chunk-by-chunk merging? *)


(** Iterate through the input files (depending on the source) *)
let iterInputs () =
  let sourcesFromFile () : Cil.file list =
    let inFile = (open_in !in_file) in
    let rec loop (curFileList:Cil.file list) : Cil.file list =
    try
      let fname = Strutil.strip (input_line inFile) in
      let nextResult = iterChunks curFileList (getFile fname) in
      loop nextResult
    with End_of_file ->
      logStatus "Reached end of input file";
      logStatus ("Num sources: " ^ 
                     (string_of_int (List.length curFileList)));
      flush stdout;
      curFileList
    in
    let result = loop [] in
    close_in inFile;
    result
  in
  let sourcesFromDir () : Cil.file list =
    let curResults = ref [] in
    let processFile (f:Cil.file) (_:string) : unit =
      curResults := iterChunks !curResults f
    in
    Filetools.walkDir processFile !cgDir;
    !curResults
  in
  let lastChunk = 
    if (useInFile ()) then sourcesFromFile ()
    else if (useCGDir ()) then sourcesFromDir ()
    else failwith "No inputs!"
  in
  finalizeChunk lastChunk



let mergeSources () = 
  let merged = iterInputs () in
  writeOutput merged


(***************************************************)
(* Execution / Program Entry Point                 *)



(** boolean XOR *)
let bxor b1 b2 : bool =
  (not b1 && b2) || (b1 && not b2)


(** Return true if the settings are fine *)
let validateSettings () : bool=
  (* Didn't know how to require files, so check manually *)
  let hasOut = !out_file <> "" in
  (bxor (useInFile ()) (useCGDir ())) && hasOut


let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    if not (validateSettings ()) then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Cil.initCIL ();
        logStatus ("Merging sources to: " ^ !out_file);
        logStatus "-----";
        mergeSources ();
        printStatistics ();
        exit 0;
      end
  with e -> 
    logError ("Exc. in Merge Sources: " ^ (Printexc.to_string e)); 
    printStatistics ();
    flushStatus ();
    raise e
;;
main () ;;
