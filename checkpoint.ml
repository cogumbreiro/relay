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


(** Module to track the current unit of work (which analysis and which scc) *)


open Scc_cg
open Scc
open Stdutil

module L = Logging

let statusFile = ref ""

type anaType = string

(*** TODO: log the progress of the SCC worklist as well *)


(** Configure checkpointing to use [tempFile] for recording info *)
let init tempFile =
  statusFile := tempFile;
  L.logStatus ("using: " ^ !statusFile ^ " to track status")


(** Return true if checkpointing should be done *)
let shouldDo () : bool =
  !statusFile <> ""

  
(************************************************************
     General checkpointing (hmm ensure no key clashes how?)
 ************************************************************)

(** Get previous values. Private use *)
let getOldTable () =
  if Sys.file_exists !statusFile then
    fileToTable !statusFile
  else
    Hashtbl.create 11

(** Add a checkpoint for the given key with a given value *)
let addCheck (key:string) (value: string) : unit =
  if shouldDo () then begin
    let tab = getOldTable () in
    Hashtbl.replace tab key value;
    tableToFile tab !statusFile
  end

(** Clear the checkpoint corresponding to the given key *)
let clearCheck (key:string) : unit =
  if shouldDo () then begin
    let tab = getOldTable () in
    Hashtbl.remove tab key;
    tableToFile tab !statusFile
  end


(** Check if there is any left-over values for the given key *)
let getPrevCheck (key:string) : string option =
  if shouldDo () then try
    let tab = getOldTable () in
    let v = Hashtbl.find tab key in
    Some v
  with Sys_error _  | End_of_file | Not_found | Failure _ ->
    L.logError "Checkpoint: No left-over status file";
    None
  else None
    

  
(************************************************************
   Special case for top level checkpoint (per-SCC/analysis)
 ************************************************************)

let sccKey = "curSCC"
let anaKey = "curAna"

(** Record that we started the analysis analysisType on scc *)
let record scc (analysisType:anaType) =
  (* TODO: don't special-case it? *)
  if shouldDo () then begin
    let tab = getOldTable () in
    Hashtbl.replace tab sccKey (string_of_int scc.scc_num);
    Hashtbl.replace tab anaKey analysisType;
    tableToFile tab !statusFile
  end


(** Read in the previous status. *)
let readStatus () =
  let tab = getOldTable () in
  let sccn = (int_of_string (Hashtbl.find tab sccKey)) in
  let anaT = Hashtbl.find tab anaKey in
  (sccn, anaT)      


(** Clear out the status file (to indicate that we're not working on anything *)
let clearStatus () =
  if shouldDo () then begin
    let tab = getOldTable () in
    Hashtbl.remove tab sccKey;
    Hashtbl.remove tab anaKey;
    tableToFile tab !statusFile
  end


(** Record the fact that the given unit of work is now done *)
let complete scc (analysisType:anaType) : unit =
  if shouldDo () then
    clearStatus ()
  else ()


(** Returns status of previous crashed process (if it crashed) *)
let whatsLeft () : (sccID * anaType) option =
  if shouldDo () then
    try
      Some (readStatus ())
    with 
      Sys_error _ 
    | End_of_file
    | Not_found
    | Failure _ ->
        L.logError "Checkpoint: No left-over status file";
        None
  else 
    None

