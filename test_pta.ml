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
(** Test the pta (post-compile phase) *)

open Gc_stats
open Readcalls
open Cil
open Pretty
open Fstructs
open Stdutil
open Cilfiles

module PTA = Pta_fi_eq
module FC = Filecache
module DC = Default_cache
module L = Logging

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let printResults = ref false

let printVars = ref false

let printFP = ref false

let noRestart = ref false

let printConstraints = ref false

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-ps", Arg.Set printResults, "print pts to sets");
   ("-pv", Arg.Set printVars, "print var id -> varname / decl mappings");
   ("-pf", Arg.Set printFP, "print function pointer mappings");
   ("-pc", Arg.Set printConstraints, "print constraints");
   ("-nr", Arg.Set noRestart, "don't restart -- use old results")]


let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg dirname [options]\n"


(***************************************************)
(* Run                                             *)


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" ) then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Cil.initCIL ();
        setGCConfig ();
        
        DC.makeLCaches !cgDir;
        Cilinfos.reloadRanges !cgDir;

        if (!printConstraints) then
          PTA.printConstraints !cgDir;

        if (!printVars) then
          PTA.printVarIDs !cgDir;
        
        L.logStatus "Loading / linking PTA info";
        PTA.analyzeAll !cgDir (not !noRestart);
        L.logStatus "Pre-pass done";

        if (!printResults) then
          PTA.printPtsToSets ()
        ;

        if (!printFP) then
          PTA.testFunPtrs !cgDir
        ;
        
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in test_pta: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    flush stdout;
    raise e
;;
main () ;;
