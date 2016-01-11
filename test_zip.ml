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


(* 

   Test how large of an obj you can make and still Marshal.to_string...
   The ocaml interpreter used up 800MB making a string that was 
   2MB. Zipped and output, it was only 22KB (w/ compression).

   The native version blows up at a tree height of 17

   Non-zipped (doesn't use Marshal.to_string) can handle much larger objs

*)

open Gc

(** Print time/memory usage statistics *)
let printStatistics () =
  print_string ("\n\nElapsed time (secs): " ^ 
                 (string_of_float (Sys.time ())) ^ "\n");
  let gcStat = quick_stat () in
  print_string ("Top heap size (words): " ^
                 (string_of_int gcStat.top_heap_words) ^ "\n");
  print_string ("Current heap size (words): " ^
                 (string_of_int gcStat.heap_words) ^ "\n");
  print_string ("Lifetime allocated bytes: " ^
                 (string_of_float (allocated_bytes ())) ^"\n");
  print_string ("Collections -- minor: " ^ 
                 (string_of_int gcStat.minor_collections) ^ 
                 " major: " ^ (string_of_int gcStat.major_collections) ^ 
                 " compactions: " ^ (string_of_int gcStat.compactions) ^ "\n")


(** Tree struct *)
type testT = 
    TestLeaf of string
  | TestNode of int * string * testT * testT


(** Prepare exponentially growing struct (if no sharing) *)
let rec makeTree height =
  if height == 0 then
    TestLeaf "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  else
    (let subTree = makeTree (height - 1) in
     TestNode (height, "gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg", subTree, subTree))


(** GO *)
let test () =
  if (Array.length Sys.argv <> 3) then
    print_string ("usage: " ^ (Filename.basename Sys.executable_name) 
                  ^ " tree_size outfile\n")
  else
    let tree = makeTree (int_of_string (Sys.argv.(1))) in
    let out_chan = open_out_bin Sys.argv.(2) in

(* With zip *)
    let gz_out = Gzip.open_out_chan out_chan in
    (try
       let obj_string = Marshal.to_string tree [Marshal.No_sharing] in
       let len = String.length obj_string in
       Gzip.output gz_out obj_string 0 len;
       Gzip.close_out gz_out
     with e ->
       Gzip.close_out gz_out;
       raise e
    );

(* Without zip

  Marshal.to_channel out_chan tree [Marshal.No_sharing];
  close_out out_chan;
  printStatistics ()
*)
;;

test ()
