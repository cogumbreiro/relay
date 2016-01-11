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
(** Pre-pass to make IDs unique in files, run the PTA, 
    and make the callgraph file in terms of the fixed function IDs.
    TODO: May want to separate the different phases into different
    executables, but right now we just use command-line options
*)

open Gc_stats
open Readcalls
open Cil
open Pretty
open Fstructs
open Stdutil

module L = Logging
module DC = Default_cache
module A = Alias

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

let idFix = ref true

let pta = ref true

let doCG = ref true

let configFile = ref "client.cfg"

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "target path to store call graph file");
   ("-ng", Arg.Clear doCG, "do not generate the call graph");
   ("-ni", Arg.Clear idFix, "do not fix ids");
   ("-np", Arg.Clear pta, "do not generate PTA info")]
    

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg desiredCgfile [options]\n"


(***************************************************)
(* Run                                             *)

(** Initialize watchlist of special functions (e.g., pthread_create) *)
let initSettings () = begin
  let settings = Config.initSettings !configFile in
  DC.makeLCaches (!cgDir);
  Cilinfos.reloadRanges !cgDir;
  A.initSettings settings !cgDir;
end



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

        if (!idFix) then begin
          L.logStatus "Fixing IDs";
          Id_fixer.ensureUniqueIDs !cgDir;
          L.logStatus "IDs are now fixed";
          flush stdout;
        end;
        

        if (!pta) then begin
          L.logStatus "Pre-pass analysis of pta / value flow";
          Pta_compile.analyzeAll !cgDir;
          L.logStatus "Pre-pass done";
          flush stdout;
        end;
        
        if (!doCG) then begin
          (* Do this after the PTA pre-pass to choose which PTA to initialize *)
          initSettings ();
          (* Finally, dump the call graph based on the PTA results *)
          L.logStatus "Preparing call graph file";
          L.logStatus "-----";
          Dumpcalls.setDumpTo !cgDir;
          Dumpcalls.writeCallgraph !cgDir;
          printStatistics ();
        end;

        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in fix_id_cg: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
