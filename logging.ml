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


(************************************************************)


(** Writes a status message (along with a timestamp and newline) *)
let logStatus s =
  if !withTime then
    output_string !stat_log ((string_of_float (Sys.time ())) ^ "> ")
  ;
  output_string !stat_log s;
  output_char !stat_log '\n'
    

(** Flushes the status log *)
let flushStatus () =
  flush !stat_log


(** Writes an error message (along with a timestamp and newline) *)
let logError ?(prior=0) s =
  if prior <= !priority then begin
    output_string !err_log ("<" ^ (string_of_int prior) ^ "> ");
    (if !withTime then
       output_string !err_log ((string_of_float (Sys.time ())) ^ "> ")
    );
    output_string !err_log s;
    output_char !err_log '\n';
    flush !err_log
  end


(** Create a temp file in the given directory w/ a given prefix 
    (first 5 chars) *)
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
    (* previously stderr *)
    err_log := open_out fileName
  else (
    close_out !err_log;
    err_log := open_out fileName
  )    

(** Toggle time-stamps in log messages *)
let doWithTime b =
  withTime := b
