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
open Scc_cg
open Fstructs
open Cilinfos
open Stdutil 

module A = Alias
module Intra = IntraDataflow
module BS = Backed_summary
module SPTA1 = Symstate2
module SPTA2 = Symex
module RS = Racestate.RS

module FC = Filecache
module L = Logging
module I = Inspect

(***************************************************)
(* Commandline handling                            *)

(** directory containing pre-processed call graph info *)
let cgDir = ref ""

let configFile = ref "client.cfg"

let userName = ref "xyz"

let restart = ref false

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap");
   ("-i", Arg.String 
      I.inspector#addInspect, "inspect state of function (given name)");
   ("-u", Arg.Set_string userName, "username to use");
   ("-r", Arg.Set restart, "restart analysis");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg fname [-o] \n"


(***************************************************)
(* Run                                             *)        
  

let initSettings () =
  Cil.initCIL ();
  try
    Cilinfos.reloadRanges !cgDir;
    let settings = Config.initSettings !configFile in
    Request.init settings;
    DC.makeLCaches (!cgDir);
    Threads.initSettings settings;
    A.initSettings settings !cgDir;
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = readCalls cgFile in
    let sccCG = Scc_cg.getSCCGraph cg in

    let () = Backed_summary.init settings !cgDir cg sccCG in

    SPTA1.init settings cg (RS.sum :> Modsummaryi.absModSumm);
    SPTA2.init settings cg (RS.sum :> Modsummaryi.absModSumm);

    Distributed.init settings !cgDir;
    Entry_points.initSettings settings;
    let _ = File_serv.init settings in (* ignore thread created *)
    Request.setUser !userName;
    let gen_num = Request.initServer () in
    if ( !restart ) then begin
      L.logStatus "trying to clear old summaries / local srcs, etc.";
      L.flushStatus ();
      Backed_summary.clearState gen_num;
      Request.clearState gen_num;
    end;
    cg
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e


let printWithBars printer =
  let bars = "===================================" in
  L.logStatus bars;
  printer ();
  L.logStatus bars


(************************************************************)
    
let runAnalyses cg sym1 sym2 =

  let string_of_fidnode fid fnode = 
    Printf.sprintf "%s(%s)" fnode.name (fid_to_string fid)
  in

  let setInspect fid fnode =
    if I.inspector#mem fnode.name then begin
      L.logStatusF "Trying to inspect %s\n" (string_of_fidnode fid fnode);
      sym1#setInspect true;
      sym2#setInspect true
    end else begin
      sym1#setInspect false;
      sym2#setInspect false
    end
  in
  
  (* Then run symstate on each individual function *)
  FMap.iter 
    (fun fid node ->
       if node.hasBody then begin
         printWithBars 
           (fun () -> L.logStatusF "Testing %s in %s\n"  
              (string_of_fidnode fid node) node.defFile);
         setInspect fid node;
         L.flushStatus ();

         let ast = !DC.astFCache#getFile node.defFile in
         A.setCurrentFile ast;
         match getCFG (fid_to_fkey fid) ast with
           Some (fn) ->
             let sumKey = fid in
             if not (sym2#isFinal sumKey) then sym2#compute fid fn;
             let changed = sym2#summarize sumKey fn in
             (* print results *)
             if changed then L.logStatus "Summary CHANGED"
             else L.logStatus "Summary UNCHANGED";
             printWithBars 
               (fun () -> L.logStatusF "Out State for %s\n"
                  (string_of_fidnode fid node));
             
             SPTA2.printExitState ();
             L.flushStatus ()
         | None -> 
             L.logError ("No ast for " ^ node.name)
       end else
         L.logStatusF "Skipping (no body) %s in %s\n" 
           (string_of_fidnode fid node) node.defFile;
    ) cg
    

let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        
        (* Get Callgraph structures *)
        let cg = initSettings () in 
          
        L.logStatus "Testing symstate intra-procedurally";
        L.logStatus "-----";

        (* TODO: get mods filled up from racestate or some other analysis *)

        let symAna1 = new SPTA1.symexAnalysis in
        let symAna2 = new SPTA2.symexAnalysis in

        runAnalyses cg symAna1 symAna2;

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
