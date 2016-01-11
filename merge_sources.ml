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


(** Merge multiple source files and output resulting AST.

    Warning: untested on large examples... (read, very inefficient)
*)

open Gc_stats
open Stdutil

module E = Errormsg
module L = Logging

let debug = false


(***************************************************)
(* Commandline handling                            *)
let in_file = ref ""

let out_file = ref ""

let setInFile (fname:string) = 
  in_file := fname

let setOutFile (fname:string) = 
  out_file := fname


(* Indicates whether sources are CIL binary ASTs. Keep in mind that
   sources must be preprocessed *)
let isBinary = ref true


(* Command-line argument parsing *)
    
let argSpecs = 
  [("-i", Arg.String setInFile, "file w/ CIL source file names, one per line");
   ("-o", Arg.String setOutFile, "output file name");
   ("-nb", Arg.Clear isBinary, "flag indicating srcs are NOT CIL binary ASTs")]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-i fname -o fname [flags]\n"


(***************************************************)
(* Merge Functions                                 *)

let getFile (fname:string) : Cil.file =
  try
    if(!isBinary) then
      Cil.loadBinaryFile fname
    else
      Frontc.parse fname ();
  with Frontc.ParseError s ->
    (L.logStatus ("Exception in getFile: " ^ fname ^ " : " ^ s));
    raise (Frontc.ParseError s)
      


let mergeSources () = 
  let rec getSources (inFile:in_channel)
      (curFileList:Cil.file list) : Cil.file list =
    try
      let l = input_line inFile in
      getSources inFile ((getFile l) :: curFileList)       
    with End_of_file ->
      L.logStatus "Reached end of input file";
      L.logStatus ("Num sources: " ^ 
                      (string_of_int (List.length curFileList)));
      flush stdout;
      curFileList
  in

  try
    (* TODO incrementally merge via divide and conquer? *)
    let inChannel = (open_in !in_file) in
    let sources = getSources inChannel [] in
    let merged = 
      Mergecil.merge sources !out_file in
    if !E.hadErrors then
      E.s (E.error "There were errors during merging\n");
    let outChannel = (open_out !out_file) in
    Cil.saveBinaryFileChannel merged outChannel;
    close_out outChannel;
    close_in inChannel; 
    ()
  with e -> (L.logError ("Exc. in mergeSources: " ^
                             (Printexc.to_string e)));
    raise e
  


(***************************************************)
(* Execution / Program Entry Point                 *)


let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require files, so check manually *)
    if (!in_file = "" || !out_file = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Cil.initCIL ();
        L.logStatus ("Merging sources to: " ^ !out_file);
        L.logStatus "-----";
        mergeSources ();
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logError ("Exc. in Merge Sources: " ^ (Printexc.to_string e)); 
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
