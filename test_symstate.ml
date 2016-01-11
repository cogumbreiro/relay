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
(** Test the symbolic execution analysis (@see symstate2.ml).
    May be out of sync at the moment. *)

open Gc_stats
open Callg
open Readcalls
open Scc
open Fstructs
open Cilinfos
open Stdutil

module A = Alias
module SS = Symstate2
module FC = Filecache
module L = Logging

(***************************************************)
(* Commandline handling                            *)

(** File containing pre-processed call graph info *)
let cgFile = ref ""

let setCGFile (fname:string) = 
  cgFile := fname


(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.String setCGFile, "name of call graph file");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg fname [-o] \n"


(***************************************************)
(* Run                                             *)        
  

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

        (* Get Callgraph structures *)
        let cg = readCalls !cgFile in
          
        L.logStatus "Testing symstate intra-procedurally";
        L.logStatus "-----";

        (* TODO: get reads/writes from racestate *)

        (* Then run symstate on each individual function *)
        let fCache = (new FC.fcache FC.cilLoader 
                        (Filename.dirname !cgFile) 11) in
        FMap.iter 
          (fun fkey node ->
             let ast = fCache#getFile node.defFile in
             A.setCurrentFile ast;
             match getCFG fkey ast with
               Some (fn) -> 
                 let _ = SS.doSymState fn in (* ignore ret value for now *)
                 (* print results *)
                 L.logStatus "===========================";
                 L.logStatus ("Store for function: " ^ node.name);
                 L.logStatus "===========================";
                 SS.printExitState ()
             | None -> 
                 ()
          ) cg;
        
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in Test Symstate: " ^ (Printexc.to_string e)); 
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
