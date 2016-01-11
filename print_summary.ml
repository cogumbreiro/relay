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
open Callg

open Lvals

module RS = Racesummary
module SS = Symsummary
module ST = Sym_types
module L = Logging
module DC = Default_cache
module A = Alias
module LP = Lockset_partitioner
module BS = Backed_summary
module Par = Pseudo_access
module NW = Null_warnings

(***************************************************)
(* Commandline handling                            *)

let in_file = ref ""

let configFile = ref "client.cfg"

let cgDir = ref ""

let doAll = ref false

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-i", Arg.Set_string in_file, "file containing the summary");
   ("-cg", Arg.Set_string cgDir, "call graph directory");
   ("-all", Arg.Set doAll, "input is a directory, and print all its summaries");
   ("-su", Arg.Set_string configFile, "config/summary bootstrap file");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-i fname -cg dir [flags]\n"


(***************************************************)
(* Test Functions                                  *)

let sz = Osize.size_w

let rec d_lval_sizeInfo lv =
  dprintf "overall words: %d\n" (sz lv) ++
    indent 2 
    (match lv with
       CVar vi, off ->
         let offSize = sz off in
         dprintf "Vinfo: %d, Off: %d\n" (sz vi) offSize ++
           if offSize > 1000 then d_off_sizeInfo off else nil

     | AbsHost h, off ->
         let offSize = sz off in
         dprintf "AbsHost: %d, Off: %d\n" (sz h) offSize ++
           if offSize > 1000 then d_off_sizeInfo off else nil

     | CMem exp, off ->
         let offSize = sz off in
         dprintf "Exp: %d, Off: %d\n" (sz exp) offSize ++
           (if offSize > 1000 then d_off_sizeInfo off else nil)
         ++ indent 2 (d_exp_sizeInfo exp)
    )

and d_exp_sizeInfo exp =
  match exp with
    CLval lv 
  | CAddrOf lv
  | CStartOf lv ->
      dprintf "Lv: %d\n" (sz lv) ++
        indent 2 (d_lval_sizeInfo lv)
  | CCastE (t, e) ->
      dprintf "Cast type: %d, Exp: %d\n" (sz t) (sz e) ++
        indent 2 (d_exp_sizeInfo e)
  | CSizeOf typ
  | CAlignOf typ ->
      dprintf "Align/Sizeof type: %d\n" (sz typ)

  | CSizeOfE e
  | CAlignOfE e ->
      dprintf "Align/Sizeof exp: %d\n" (sz e) ++
        indent 2 (d_exp_sizeInfo e)
      
  | CSizeOfStr str ->
      dprintf "SizeOfStr: %d\n" (sz str)
  | CBinOp (op, e1, e2, t) ->
      dprintf "BOp op: %d, e1: %d, e2: %d, t: %d\n" 
        (sz op) (sz e1) (sz e2) (sz t) ++
        indent 2 (d_exp_sizeInfo e1) ++ line ++
        indent 2 (d_exp_sizeInfo e2)
  | CUnOp (op, e1, t) ->
      dprintf "UOp op: %d, e: %d, t: %d\n" 
        (sz op) (sz e1) (sz t) ++
        indent 2 (d_exp_sizeInfo e1)
  | CConst c ->
      dprintf "Const: %d\n" (sz c)

and d_off_sizeInfo off =
  match off with
    NoOffset -> nil
  | Index (iexp, moreOff) ->
      dprintf "Index exp: %d, off: %d\n" (sz iexp) (sz moreOff)
  | Field (fi, moreOff) ->
      dprintf "Field fi: %d, off: %d\n" (sz fi) (sz moreOff) ++
        d_field_sizeInfo fi

and d_field_sizeInfo fi =
  dprintf "comp %d, name %d, typ %d, bitf %d, attr %d, loc %d\n"
    (sz fi.fcomp) (sz fi.fname) (sz fi.ftype) (sz fi.fbitfield) 
    (sz fi.fattr) (sz fi.floc)

let p_au_sizeInfo au =
  Osize.p_size "All unlocks total words: " au;
  let doc = Lockset.LS.S.fold
    (fun lv v doc ->
       doc ++ text (string_of_lval lv ^ ":\n") ++
         indent 2 (d_lval_sizeInfo lv) ++ line
    ) au nil in
  L.logStatusD doc


let printSum cg (fname:string) = begin
  let fkey = Summary_keys.sumKey_of_string (Filename.basename fname) in
  let funcName = 
    try 
      let fnode = FMap.find fkey cg in
      fnode.name
    with Not_found -> "???" in
  L.logStatusF "Summaries for %s(%s) are:\n\n" funcName fname;
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
  );
  (try
     let au = BS.deserializeFromFile fname "au" in
     L.logStatus "";
     L.logStatus "AllUnlocks Summary:";
     let fullLS = Lockset.LS.composeNew (Lockset.LS.S.empty) au in
     RS.printLockset fullLS;
     
     (* Check out sizing *)
     p_au_sizeInfo au
   with e ->
     L.logStatus ("Return value: Unreadable " ^ (Printexc.to_string e))
  );
  (try
     let sum = BS.deserializeFromFile fname "lsp" in
     L.logStatus "";
     L.logStatus "LSP Summary:";
     LP.printSumm (-1) sum
   with e ->
     L.logStatus ("Return value: Unreadable " ^ (Printexc.to_string e))
  );
  (try
     let sum = BS.deserializeFromFile fname "par" in
     L.logStatus "";
     L.logStatus "PAR Summary:";
     Par.printSumm sum fkey
   with e ->
     L.logStatus ("Return value: Unreadable " ^ (Printexc.to_string e))
  );

  (try
     let sum = BS.deserializeFromFile fname "nwarn1" in
     L.logStatus "";
     L.logStatus "Null Warn 1 (delayed) summary:";
     let doc = NW.pReport sum.NW.nDelayed in
     L.logStatusD doc;
     L.logStatus "";

     L.logStatus "";
     L.logStatus "Null Warn 1 (safe) summary:";
     let doc = NW.pReport sum.NW.nSafe in
     L.logStatusD doc;
     L.logStatus "";

     L.logStatus "";
     L.logStatus "Null Warn 1 (unsafe) summary:";
     let doc = NW.pReport sum.NW.nUnsafe in
     L.logStatusD doc;
     L.logStatus "";

   with e ->
     L.logStatus ("Return value: Unreadable " ^ (Printexc.to_string e))
  )

end

module StrSet = Set.Make (String)

let printAllSums cg dirname =
  let allFiles = ref StrSet.empty in
  Filetools.walkDirSimple 
    (fun fname ->
       let chopped = try Filename.chop_extension fname
       with Invalid_argument _ -> fname in
       allFiles := StrSet.add chopped !allFiles
    ) dirname;
  let printer = printSum cg in
  StrSet.iter printer !allFiles


(** Initialize function summaries, and watchlist of special 
    functions (e.g., pthread_create) *)
let initSettings () =
  try
    Osize.checkSizes := true;
    let settings = Config.initSettings !configFile in
    A.initSettings settings !cgDir;
    DC.makeLCaches (!cgDir);
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = readCalls cgFile in
    cg
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e



(***************************************************)
(* Execution / Program Entry Point                 *)

let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require files, so check manually *)
    if (!cgDir = "" || !in_file = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        initCIL ();
        Cilinfos.reloadRanges !cgDir;
        let cg = initSettings () in
        if not (!doAll) then
          printSum cg !in_file
        else
          printAllSums cg !in_file
        ;
        exit 0;
      end
  with e -> 
    L.logError ("Exc. in PrintSummary: " ^ (Printexc.to_string e));
    raise e
;;
main () ;;
