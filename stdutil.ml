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

(** Basic utilities to augment the Ocaml standard library *)

open Cil
open Logging

(***************************************************
 * Augment the standard library
 ***************************************************)


(** Return a string for printing command-line usage of programs.
    (Prepends the program name to the supplied [tail] string) *)
let getUsageString tail = 
  "Usage: " ^ (Filename.basename Sys.executable_name) ^ " " ^ tail

(** Print the list of arguments that this binary was run with *)
let printCmdline () =
  logStatus "Args: ";
  logStatus (Array.fold_left (fun cur s -> cur ^ " " ^ s) "" Sys.argv)

(** remove all files in a directory that statisfy the filter (non-recursive) *)
let clearDir dir filter =
  let files = Sys.readdir dir in
  Array.iter 
    (fun fname ->
       if (filter fname) then
         try Sys.remove (Filename.concat dir fname)
         with Sys_error e -> logError ("couldn't clear dir: " ^ e)
    ) files


(** conditionally clear out a directory, given a generation number,
    generation file, and the code to clear the directory *)
let clearDirGen genNum genFile clearFunc =
  (* check the old generation number -- if same, don't clear *)
  let clearP = 
    if (Sys.file_exists genFile) then
      let oldNum = Gen_num.getNum genFile in
      genNum <> oldNum
    else true
  in
  if (clearP) then
    (clearFunc ();
     Gen_num.setNum genNum genFile ) 
  else logStatus "not clearing persistent state, same generation num"


(******************** Utils for collections ************************)

let countMapEntry (_) (_) curCount = curCount + 1

(** Return the size of a map [m], based on the given [fold] function *)
let mapSize (m) (fold) : int =
  fold countMapEntry m 0

let seqToString iter seq doElem sep =
  let firstElem = ref true in
  let buff = Buffer.create 10 in
  iter (fun elem ->
          if not (!firstElem) then Buffer.add_string buff sep
          else firstElem := false
          ;
          Buffer.add_string buff (doElem elem);
       ) seq;
  Buffer.contents buff

let mapToList fold m =
  fold (fun k v cur -> (k, v) :: cur) m []


(***************************************************
 * File / resource functions
 ***************************************************)

(** Return the file extension from the given filename. *)
let get_extension name =
  let non_ext = Filename.chop_extension name in
  (* Skip the dot *)
  let non_ext_len_p1 = String.length non_ext + 1 in
  let orig_len = String.length name in
  String.sub name non_ext_len_p1 (orig_len - non_ext_len_p1)


let finaliseResource closeFunc = fun res ->
  logError "finaliseResource was called?!";
  closeFunc res

(** Open the resource (identified by [resourceName]), using 
    the given [openFunc], and call the given function [foo] 
    with the io_channel. Close the resource with given [closeFunc]
    on completion or failure *)
let open_for openFunc closeFunc resourceName foo =
  let chan = openFunc resourceName in
(*  let _ = Gc.finalise (finaliseResource closeFunc) chan in *)
  try
    let result = foo chan in
    let _ = closeFunc chan in
    result
  with e ->
    closeFunc chan;
    raise e


(** Some shortcuts for files *)

let open_in_bin_for foo = open_for open_in_bin close_in_noerr foo

let open_in_for foo = open_for open_in close_in_noerr foo

let open_out_bin_for foo = open_for open_out_bin close_out_noerr foo

let open_out_for foo = open_for open_out close_out_noerr foo


(** Some shortcuts for sockets *)

let open_connect addr = 
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in (* File opened! *)
  try
    Unix.connect sock addr;
    (sock, Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
  with e ->
    Unix.close sock (* at least close sock if connect fails! *);
    raise e
      
let close_connect (sock, sock_in, sock_out) =
  (try Unix.shutdown sock Unix.SHUTDOWN_ALL
   with e -> ()
  );
  try Unix.close sock
  with e -> ()

let open_conn_for foo = open_for open_connect close_connect foo

(* open for accept? *)


(************************************************************
    Read/write simple data structures into simple text files
 ***********************************************************)

(** Write a hashtable of strings to a text file *)
let tableToFile ?(sep=":") (tab  : (string, string) Hashtbl.t) (fname:string) =
  let doWrite oc =
    Hashtbl.iter
      (fun k v -> output_string oc (k ^ sep ^ v ^ "\n")) tab
  in
  open_out_for fname doWrite

(** Read a text file representing a table into a new table *)
let fileToTable ?(sep=":") (fname:string) : (string, string) Hashtbl.t =
  let result = Hashtbl.create 11 in
  let splitter = Str.regexp sep in
  let doRead ic =
    try while true do
      let line = input_line ic in
      match Str.split splitter line with
        [k; v] -> Hashtbl.add result k v
      | _ -> logError ("fileToTable: corrupt entry - " ^ line)
    done; with End_of_file ->
      ()
  in
  open_in_for fname doRead;
  result

(* User must coordinate to make sure the "sep"arator char is the same *)


(************************* Hashtbl utils ***************************)

let string_of_hashstats statsFun hashtbl caption : string =
  let tlen, entries, sum_buck_lens, min_buck, med_buck, max_buck =
    statsFun hashtbl in
  Printf.sprintf "%s hash stats (len, ents, sum/min/median/max bucket)\n\t%d\t%d\t%d\t%d\t%d\t%d\t" caption tlen entries sum_buck_lens min_buck med_buck max_buck


(************************************************************)

let negF foo = 
  function x -> not (foo x)
  
  
