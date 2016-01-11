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


(** 
    Manage logging of messages to different channels. 
    One channel is for status messages (is buffered, but can be flushed),
    and one is for error messages (not buffered).

    Provides different priority levels.
*)


(* TODO: choose a fixed number of priority levels? Make an enum? *)

(* TODO: compress duplicate messages (if one follows the other) *)


let slog' = stdout
let elog' = stderr

let stat_log = ref slog'

let err_log = ref elog'

let withTime = ref false


(**** Priority/Verbosity levels ****)

(** Only print warning messages up to this priority (lower number is 
    greater priority (so that it maxes at 0)) *)
let priority = ref 1
let defaultPrior = 0
let printPriority = ref false


(************************************************************
 Changing Settings
************************************************************)


(** Create a temp file in the given directory w/ a given prefix 
    (first 5 chars). ASSUMES "tempfile" program exists. *)
let tmpFile dir prefix =
  let result = ref "" in
  let in_chan = 
    Unix.open_process_in ("tempfile -d " ^ dir ^ 
                            " -p " ^ prefix ^ " -m 0600") in
  result := input_line in_chan;
  match Unix.close_process_in in_chan with
    Unix.WEXITED i when i == 0 ->
      !result
  | _ ->
      prerr_string "couldn't make tempfile\n";
      "/tmp/_123BLAH"


(** Set file to store status logs. Keeps file open! *)
let setStatLog dir =
  let fileName = tmpFile dir "rstat" in
  print_string ("logging status to " ^ fileName ^ "\n");
  if (!stat_log == slog') then 
    (* previously stdout *)
    stat_log := open_out fileName
  else (
    close_out !stat_log;
    stat_log := open_out fileName
  )


(** Set file to store error logs *)
let setErrLog dir =
  let fileName = tmpFile dir "rerr" in
  print_string ("logging errors to " ^ fileName ^ "\n");
  if (!err_log == elog') then
    (* previously stderr, don't need to close stderr *)
    err_log := open_out fileName
  else (
    (* not previously stderr, close previous err_log *)
    close_out !err_log;
    err_log := open_out fileName
  )

(** Set error log to be the same as stat log. Do not change the stat log
    or error log after this, otherwise the invariant that they are the
    same won't be maintained + other bad stuff *)
let combineLogs () =
  err_log := !stat_log

(** Toggle time-stamps in log messages *)
let doWithTime b =
  withTime := b



(************************************************************)

(** Prepend output with timestamp *)
let timestampOut chan =
  if !withTime then
    output_string chan ((string_of_float (Sys.time ())) ^ "> ")
  
(** Prepend output w/ priority level *)
let priorityOut chan prior =
  if (!printPriority) then
    Printf.fprintf chan "[Er]: <%d>" prior
  else 
    output_string chan "[Er]: " 

(* Internal status logger *)
let doStatus doer item =
  let () = timestampOut !stat_log in
  doer !stat_log item
    
(** Writes a status message (w/ a newline) *)
let logStatus s : unit = 
  doStatus output_string s;
  output_char !stat_log '\n'

(** Write a status message using printf-style arguments *)
let logStatusF (fmt:('a, out_channel, unit) format) : 'a =
  doStatus Printf.fprintf fmt

(** Flushes the status log *)
let flushStatus () : unit =
  flush !stat_log


(** Writes an error message (along with a timestamp and newline) *)
let logError ?(prior=defaultPrior) s =
  if prior <= !priority then begin
    priorityOut !err_log prior;
    timestampOut !err_log;
    Printf.fprintf !err_log "%s\n" s;
    flush !err_log
  end else ()
    
(** logErrorF only works on ocaml 3.10+ because we need Printf.ifprintf 
    to consume the args in the case where the priority is not enough *)
let logErrorF ?(prior=defaultPrior) (fmt:('a, out_channel, unit) format) : 'a =
  if prior <= !priority then begin
    priorityOut !err_log prior;
    timestampOut !err_log;
    Printf.fprintf !err_log fmt
  end else 
    Printf.ifprintf !err_log fmt


(************************************************************
 Higher-level printing
************************************************************)  

(**** Write collections to buffers ****)

(* Core *)

let seq_to_ sep iter doElem seq add_to target =
  let firstElem = ref true in
  iter (fun elem ->
          if not (!firstElem) then add_to target sep
          else firstElem := false;
          add_to target (doElem elem);
       ) seq

let map_to_ sep iter doKeyVal map add_to target =
  let firstElem = ref true in
  iter (fun k v ->
          if not (!firstElem) then add_to target sep
          else firstElem := false;
          add_to target (doKeyVal k v);
       ) map


let seq_to_limit limit sep iter doElem seq add_to target =
  let firstElem = ref true in
  let numDone = ref 0 in
  iter (fun elem ->
          if !numDone < limit then begin
            incr numDone;
            if not (!firstElem) then add_to target sep
            else firstElem := false;
            add_to target (doElem elem);
          end
       ) seq

let map_to_limit limit sep iter doKeyVal map add_to target =
  let firstElem = ref true in
  let numDone = ref 0 in
  iter (fun k v ->
          if !numDone < limit then begin
            incr numDone;
            if not (!firstElem) then add_to target sep
            else firstElem := false;
            add_to target (doKeyVal k v);
          end
       ) map
    
(**** String buffs ****)

let seq_to_buf ?(sep=", ") iter doElem seq buff = 
  seq_to_ sep iter doElem seq Buffer.add_string buff
    (* TODO: Hmm, by using Buffer.add_string, we make it so that
       doElem is not recursive, but returns a string representation? *)
    
let map_to_buf ?(sep=", ") iter doKeyVal map buff =
  map_to_ sep iter doKeyVal map Buffer.add_string buff

let seq_to_buf_limit ?(sep=", ") iter doElem seq buff lim = 
  seq_to_limit lim sep iter doElem seq Buffer.add_string buff
    (* TODO: Hmm, by using Buffer.add_string, we make it so that
       doElem is not recursive, but returns a string representation? *)
    
let map_to_buf_limit ?(sep=", ") iter doKeyVal map buff lim =
  map_to_limit lim sep iter doKeyVal map Buffer.add_string buff

let logStatusB buf =
  doStatus Buffer.output_buffer buf

(**** Same as string buffer operations, but for Pretty.doc ****)

open Pretty

let concatToRef cur newTail =
  cur := !cur ++ newTail

let seq_to_doc sep iter doElem seq doc =
  (* Use mutation, or make the seq_to_x / map_to_x 
     return new x instead of unit *)
  let result = ref doc in
  seq_to_ sep iter doElem seq concatToRef result; 
  !result

let map_to_doc sep iter doElem map doc =
  let result = ref doc in
  map_to_ sep iter doElem map concatToRef result;
  !result

let seq_to_doc_limit sep iter doElem seq doc lim =
  let result = ref doc in
  seq_to_limit lim sep iter doElem seq concatToRef result; 
  !result

let map_to_doc_limit sep iter doElem map doc lim =
  let result = ref doc in
  map_to_limit lim sep iter doElem map concatToRef result;
  !result

let logStatusD doc =
  doStatus (Pretty.fprint ~width:80) doc

let logErrorD ?(prior=defaultPrior) doc = 
  logError ~prior:prior (Pretty.sprint ~width:80 doc)
