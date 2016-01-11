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

(** Util to find differences in data race warning results *)

open Logging
module LocHash = Race_reports.RaceWarnOut.KH
module DC = Default_cache


(***************************************************)
(* Commandline handling                            *)

(** directory containing pre-processed call graph / variable / struct info *)
let cgDir = ref ""

let configFile = ref "client.cfg"

let raceFile1 = ref ""

let raceFile2 = ref ""

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "call graph directory");
   ("-su", Arg.Set_string configFile, "name of config/summary bootstrap");
  ]

let anonArgFun (arg:string) : unit = 
  if !raceFile1 <> "" then
    raceFile2 := arg
  else 
    raceFile1 := arg

let usageMsg = Stdutil.getUsageString "-cg astdir [options] file1 file2 \n"

(***************************************************)
(* Run                                             *)        
          
let initSettings () =
  Cilinfos.reloadRanges !cgDir;
  DC.makeLCaches (!cgDir);
  let settings = Config.initSettings !configFile in

  (* Just using this old cg file to get the list of functions w/ bodies *)
  Alias.initSettings settings !cgDir

let printDiffs file1 file2 =
  logStatus "Printing Diffs:\n================================\n";
  let rep1 = Race_reports.RaceWarnOut.deserialize file1 in
  let rep2 = Race_reports.RaceWarnOut.deserialize file2 in
  let diffData = rep1#diffReport rep2 in
  let listedDiffs = Stdutil.mapToList LocHash.fold diffData in
  let numDiffs = List.fold_left 
    (fun count (locKey, diffKind) ->
       logStatus "******";
       (match diffKind with
          Race_reports.RaceWarnOut.OnlyFirst rep ->
            logStatus "Diff Kind: Only First\n<<<";
            Race_reports.printRaceRep rep 0;
            
        | Race_reports.RaceWarnOut.OnlySecond rep ->
            logStatus "Diff Kind: Only Second\n>>>";
            Race_reports.printRaceRep rep 0;
            
        | Race_reports.RaceWarnOut.BothButDiff (rep1, rep2) -> 
            logStatus "Diff Kind: Both Diff\n<<<";
            Race_reports.printRaceRep rep1 0;
            logStatus ">>>";
            Race_reports.printRaceRep rep2 0
       );
       count + 1
    ) 0 listedDiffs in
  logStatusF "Total Diffs: %d\n" numDiffs
  

let main () = 
  Arg.parse argSpecs anonArgFun usageMsg;
  (* Didn't know how to require the -cg file, etc., so check manually *)
  if (!cgDir = "" || !raceFile1 = "" || !raceFile2 = "") then begin
    Arg.usage argSpecs usageMsg;
    exit 1
  end else begin
    combineLogs ();
    Stdutil.printCmdline ();
    Cil.initCIL ();

    initSettings ();
    printDiffs !raceFile1 !raceFile2;
    exit 0
  end
;;
main () ;;
