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


(** Can I marshal multiple things into the same channel, in sequence? *)

module IS = Set.Make (struct
                        type t = int
                        let compare a b = a - b
                      end)

module SS = Set.Make (String)

type kinds =
    String
  | List
  | Int
  | IntSet
  | StringSet

let testFile = "/tmp/testMarshal"

let testMarshal () =
  let oc = open_out_bin testFile in
  let order = [String; List; Int; StringSet] in
  let a = "Hello!" in
  let l = [1; 2; 3; 4] in
  let i = 3149832 in
  let ss = SS.add "Other daf32 3k2j43294" (SS.add "World!" SS.empty) in
  let flags = [Marshal.Closures] in
  Marshal.to_channel oc order flags;
  Marshal.to_channel oc a flags;
  Marshal.to_channel oc l flags;
  Marshal.to_channel oc i flags;
  Marshal.to_channel oc ss flags;
  flush oc;
  close_out oc

let printThing ic = function
    String ->
      let s = Marshal.from_channel ic in
      print_string (s ^ "\n")

  | List ->
      let il = Marshal.from_channel ic in
      List.iter (fun i -> print_string ((string_of_int i) ^ "\n")) il

  | Int ->
      let i = Marshal.from_channel ic in
      print_string ((string_of_int i) ^ "\n")

  | IntSet ->
      let is = Marshal.from_channel ic in
      IS.iter (fun i -> print_string ((string_of_int i) ^ "\n")) is

  | StringSet -> 
      let ss = Marshal.from_channel ic in
      SS.iter (fun s -> print_string (s ^ "\n")) ss

let inputMarshal () =
  let ic = open_in_bin testFile in
  let order = Marshal.from_channel ic in
  List.iter (printThing ic) order;
  close_in ic


(*** 
     YEAH it works!... Can read in header_size worth of data into
     buffer, then check the buffer's data_size and skip ahead if 
     I wanted a way to seek out the right parts of a binary
     channel without marshal'ing in the junk in between...
     Could also be nice if the actual I/O was done lazily?
*)

let skipFirst () =
  let ic = open_in_bin testFile in
  let order = Marshal.from_channel ic in
  let buff = String.create (Marshal.header_size + 1) in
  really_input ic buff 0 (Marshal.header_size);
  let ds = Marshal.data_size buff 0 in
  seek_in ic ((pos_in ic) + ds);
  match order with
    h :: t -> List.iter (printThing ic) t
  | _ -> print_string "rest is empty!\n"

(* Yup, it works... *)
