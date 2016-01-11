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


(** Load from a user-supplied config file of program entry-points.
    See {!rootFile} for the name of the user-supplied config file 
    we expect in the call graph directory... *)

open Stdutil
open Fstructs
open Callg

module L = Logging

let rootFile = "roots.txt"
 
type patterns =
    NamePat of Str.regexp
  | TypePat of Str.regexp

let ws = "[ \r\n\t]*"

let splitter = Str.split_delim (Str.regexp (ws ^ "[:]" ^ ws))

let name_field = "name"

let type_field = "type"

exception CorruptLine

(** Return the set of functions in the call graph that match the 
    entry points read from a listing in cgDir *)
let getEntries cgDir cg : FSet.t =
  let entries = ref [] in

  (* read in the entry point name patterns *)
  let readEntries ic =
    try while true do
      let line = input_line ic in
      try 
        match splitter line with
          [kind; patString] ->
            (* Just use the inputted line as a regexp. TODO: check syntax? *)
            let pat = Str.regexp patString in
            let tagged = 
              if kind = name_field then
                NamePat pat
              else if kind = type_field then
                TypePat pat
              else 
                raise CorruptLine
            in
            entries := tagged :: !entries
        | [] -> () (* skip blank line *)
        | _ -> raise CorruptLine
      with CorruptLine as e ->
        L.logError ("Entry_points: corrupt input file - " ^ line);
        raise e
    done; with End_of_file -> 
      let numEntries = List.length !entries in
      L.logStatus ("Finished reading in entry points: " ^ 
        (string_of_int numEntries))
  in

  (* return true if the function node matches one of the entry points *)
  let filterNode fk fn =
    List.exists 
      (fun taggedPat -> 
         match taggedPat with
           NamePat pat ->
             Str.string_match pat fn.name 0
         | TypePat pat ->
             Str.string_match pat fn.typ 0) !entries
  in
  
  (* first, read in the patterns from the rootFile *)
  let fname = (Filename.concat cgDir rootFile) in
  if (Sys.file_exists fname) then begin
    open_in_for fname readEntries;
    (* Then get the functions from the call graph that match *)
    FMap.fold (fun fk fn cur -> 
                 if filterNode fk fn 
                 then FSet.add fk cur
                 else cur) cg FSet.empty
  end else
    (L.logError "No entry-points file!";
     FSet.empty)
      


(** Add any dangling roots to the set of entries *)
let addRootEntries baseEntries cg sccCG =
  ()
