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


(** Load the lock/guarded access summaries, and symex return value summaries
    and print them *)

open Cil
open Pretty
open Fstructs
open Stdutil

module RS = Racesummary
module SS = Symsummary
module ST = Sym_types
module L = Logging
module FC = Filecache
module DC = Default_cache
module A = Alias

(***************************************************)
(* Commandline handling                            *)
let in_file = ref ""

let configFile = ref "client.cfg"

let cgFile = ref ""

let cgDir = ref ""

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-i", Arg.Set_string in_file, "file containing the summary");
   ("-cg", Arg.Set_string cgFile, "call graph file")]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-i fname -cg dir [flags]\n"


(***************************************************)
(* Test Functions                                  *)

let printSum (fname:string) = begin
  (try
     let resultSumm = RS.deserializeFromFile fname in
     L.logStatus ("Lock Summary: "); 
     RS.printState (RS.summOutstate resultSumm);
   with e ->
     L.logStatus ("Lock Summary: Unreadable " ^ (Printexc.to_string e))
  );
  (try
     let returnVal = SS.sum#getFromFile fname in
     L.logStatus ("Return value:");
     ST.printVal returnVal;
   with e ->
     L.logStatus ("Return value: Unreadable " ^ (Printexc.to_string e))
  )
end


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () =
  try
    let settings = Config.initSettings !configFile in
    A.initSettings settings !cgDir;

    DC.makeLCaches (!cgDir);
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e



(***************************************************)
(* Execution / Program Entry Point                 *)

let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require files, so check manually *)
    if (!cgFile = "" || !in_file = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        initCIL ();
        cgDir := Filename.dirname !cgFile;
        Cilinfos.reloadRanges !cgDir;
        initSettings ();
        printSum !in_file;
        exit 0;
      end
  with e -> 
    L.logError ("Exc. in PrintSummary: " ^ (Printexc.to_string e));
    raise e
;;
main () ;;
