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


(** Marshaling objects with gzip compression, or not. 
    (Compression currently turned off... issue w/ streaming 
    the marshaling + compression instead of marshaling to a buffer first) *)

module L = Logging

(* For now, don't compress at all... *)

(** Write obj to the file fname, using the marshal flags *)
let to_file fname obj flags =
  let out_chan = open_out_gen
    [Open_creat; Open_wronly; Open_binary] 0o666 fname in
  try
    Marshal.to_channel out_chan obj flags;
    close_out out_chan;
  with e ->
    close_out out_chan;
    raise e

(** Read the marshalled value from a file (fname) *)
let from_file fname =
  let in_chan = open_in_bin fname in
  (try
     let v = Marshal.from_channel in_chan in
     close_in in_chan;
     v
   with e ->
     close_in in_chan;
     raise e)  

(** Delete the file (fname) containing marshalled data *)
let remove fname =
  Sys.remove fname


(** Assume the file (fname) contains a single marshalled value. 
    Check if the file length, etc. is Kosher. Return true if fine. *)
let check_file fname =
  let ic = open_in_bin fname in
  let header_buff = String.create (Marshal.header_size) in
  try
    (* Read header into buffer and read expected size from that, then compare *)
    really_input ic header_buff 0 (Marshal.header_size);
    let expected_size = Marshal.total_size header_buff 0 in 
    (* Marshal.total_size is off by 1 sometimes ?! If I add "1", 
       it ends up adding "2" sometimes as well... *)
    let actual_size = in_channel_length ic in
    close_in_noerr ic;
    let good = expected_size == actual_size in
    if (not good) then
      L.logError ("Gz_marshal: check_file size is off: " ^ 
                    (string_of_int expected_size) ^ "/" ^
                    (string_of_int actual_size))
    ;
    good
  with e ->
    close_in_noerr ic;
    L.logError ("Gz_marshal: error in check_file: " ^ (Printexc.to_string e));
    false
  


(*

let to_file fname obj flags =
  let out_chan = open_out_gen 
    [Open_creat; Open_wronly; Open_binary] 0o666 fname in
  try
    Marshal.to_channel out_chan obj flags;
    close_out out_chan;
    if Sys.command ("gzip -f " ^ fname) == 0 then () 
    else L.logError ("gzip " ^ fname ^ " failed\n")
  with e ->
    close_out out_chan;
    raise e

let from_file fname =
  if (Sys.command ("gunzip " ^ fname) == 0) then
    let in_chan = open_in_bin fname in
    (try
       let v = Marshal.from_channel in_chan in
       close_in in_chan;
       let _ = Sys.command ("gzip -f " ^ fname) in
       v
     with e ->
       close_in in_chan;
       raise e)  
  else
    (L.logError ("gunzip " ^ fname ^ " failed\n");
     raise Not_found)

let remove fname =
  Sys.remove (fname ^ ".gz")

*)

(*

let to_channel reg_chan obj flags =
  let out_chan = Gzip.open_out_chan reg_chan in
  try
    let obj_string = Marshal.to_string obj flags in
    let len = String.length obj_string in
    Gzip.output out_chan obj_string 0 len;
    Gzip.close_out out_chan  
  with e ->
    Gzip.close_out out_chan;
    raise e
      

let from_file fname =
  let in_chan = Gzip.open_in fname in
  let obj = try
    let obj_sbuff = Buffer.create 1024 in
    let buf = String.create 1024 in
    let rec loop chars_read =
      if (chars_read == 0) then
        ()
      else
        (Buffer.add_substring obj_sbuff buf 0 chars_read;
         loop (Gzip.input in_chan buf 0 1024))
    in
    loop (Gzip.input in_chan buf 0 1024);
    Gzip.close_in in_chan;
    Marshal.from_string (Buffer.contents obj_sbuff) 0
  with e ->
    Gzip.close_in in_chan;
    raise e
  in
  obj

*)
