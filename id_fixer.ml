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


(** Utility to ensure uniqueness and consistency of varinfo VIDs
    across different CIL AST object files. Reassigns IDs.

    WARNING: Make a copy of files beforehand. A crash could leave
    IDs in an unstable state  
*)

open Cil
open Filetools
open Intrange

module CF = Cilfiles
module IH = Inthash
module D = Cildump
module L = Logging

(**************** "Flags" **************)

let setFile (fname:string) =
  let out_chan = open_out fname in
  output_char out_chan '!';
  close_out out_chan

(** dummy file that is dumped into the directory to indicate that 
    id fixing is in progress. Its existence after completion or a crash
    indicates that the files are now corrupt *)
let inProgressFile = ".doing_id_fixins"

let setInProgress (root:string) =
  setFile (Filename.concat root inProgressFile)

let clearInProgress (root:string) =
  Sys.remove (Filename.concat root inProgressFile)



(******* VID / CKEY range stuff *******)


(** file that is dumped into the directory to indicate that the
    ids have been fixed. Also contains ID/Key range info *)
let doneFile = ".vids_ckeys_fixed"

(** make a file to indicate ids are fixed. piggyback id ranges
    in this file as well *)
let setDone (root:string) =
  CF.saveRanges (Filename.concat root doneFile)


let loadRanges (root:string) = 
  CF.loadRanges (Filename.concat root doneFile)

(**************** Visitor to do the work **************)

let nextGlobVID = ref 0

let nextCompKey = ref 0


(* Consistently reassign vids to each global var. For local var,
   just make sure it doesn't clash *)
let (seenGlobalVars : ((string * string), int) Hashtbl.t) = Hashtbl.create 173

let (compKeys : (bool * string, int) Hashtbl.t) = Hashtbl.create 173

let getNextVID () =
  let result = !nextGlobVID in
  incr nextGlobVID;
  result

let getNextCKey () =
  let result = !nextCompKey in
  incr nextCompKey;
  result


let isInline vi =
  vi.vinline || 
  List.exists 
    (fun att ->
       match att with
         Attr (name, _) ->
           name = "inline"
    ) vi.vattr



(** A visitor that searches for varinfo IDs and compinfo IDs to make 
    sure that if we make another info, it will not clash w/ any of 
    the IDs in the visited file (or previously loaded files) *)
class globalIDVisitor = object (self)
  inherit nopCilVisitor 
    
  val seenVIDs = Hashtbl.create 173
    
  val mutable seenVinfos = IH.create 173
    
  val mutable seenCinfos = IH.create 173

  method getVinfos () =
    seenVinfos

  method getCinfos () =
    seenCinfos

  method handleVI (vi:varinfo) =
    let name = vi.vname in
    let typs = D.string_of_ftype vi.vtype in
    (try
       (* seenVIDs for this file *)
       let oldID = Hashtbl.find seenVIDs (vi.vid, name, typs) in 
       vi.vid <- oldID
         
     with Not_found -> begin
       
       (* Fix the ID (depending on whether it's a global + extern
          or not). Also, do no inline the inlined functions. *)
       let newID = 
         if (vi.vglob && (vi.vstorage <> Static || isInline (vi))) then begin
           try
             let oldID = Hashtbl.find seenGlobalVars (name, typs) in
             vi.vid <- oldID;
             oldID
           with Not_found ->
             let newID = getNextVID () in
             vi.vid <- newID;
             Hashtbl.add seenGlobalVars (name, typs) newID;
             newID
         end
         else begin
           let newID = getNextVID () in
           vi.vid <- newID;
           newID
         end
       in
       
       (* remember the fixed id for this file *)
       Hashtbl.add seenVIDs (vi.vid, name, typs) newID;
       (* make index of varinfos *)
       IH.add seenVinfos vi.vid vi;
     end
    )

  method vvdec (vi:varinfo) =
    self#handleVI vi;
    DoChildren

  method vvrbl (vi:varinfo) =
    self#handleVI vi;
    DoChildren


  method vglob = function
      GCompTag (ci, _)
    | GCompTagDecl (ci, _) ->
        (try 
           let oldCKey = Hashtbl.find compKeys (ci.cstruct, ci.cname) in
           ci.ckey <- oldCKey
         with Not_found ->
           let newCKey = getNextCKey () in
           
           (* re-assign *)
           ci.ckey <- newCKey;
           
           (* remember that it's done *)
           Hashtbl.add compKeys (ci.cstruct, ci.cname) newCKey;
           (* make index of compinfos *)
           IH.add seenCinfos ci.ckey ci;
        );
        DoChildren
    | _ ->
        DoChildren    
end



(**************** Interface **************)


let doEnsureUniqueIDs (root : string) =
  Filetools.walkDir 
    (fun ast file ->
       let startVid = !nextGlobVID in
       let startCkey = !nextCompKey in
       let vis = new globalIDVisitor in (* must be fresh for every file *)
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       (* Write back the result! *)
       Cil.saveBinaryFile ast file;

       (* Write just the varinfos/cinfos to a separate file *)
       let vinfos = vis#getVinfos () in
       let cinfos = vis#getCinfos () in
       CF.writeIndexes vinfos cinfos file;

       (* Record the vid / ckey range *)
       let endVid = !nextGlobVID - 1 in
       let endCkey = !nextCompKey - 1 in

       CF.addRanges startVid endVid startCkey endCkey ast.fileName;

    ) root
    

let initState () =
  CF.initRanges ();
  Hashtbl.clear seenGlobalVars;
  Hashtbl.clear compKeys
  

(** Apply ID fixer to each file rooted at the given path. *)
let ensureUniqueIDs (root : string) : unit =
  (* Maybe it's already done *)
  if (Sys.file_exists (Filename.concat root doneFile)) then
    L.logStatus "IDs are already fixed"
  else begin
    setInProgress root;
    initState ();
    doEnsureUniqueIDs root;
    setDone root;
    clearInProgress root;

    (* Don't need state anymore, so clear *)
    initState ();
  end
    
