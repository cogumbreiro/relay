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

(* TODO: allow comments of the form "# comment" *)

(** Parse a config file and give access to key -> value mappings. 
    Example of a well-formatted config file:

    //Begin example

    SECTION_A {
    
    key1 : value1 is any string without a colon (the separator) or newline
    key2 : value2
    
    }
  
    SECTION_B {
    
    key1 : value1
    
    key2 : value2
    
    }

    include(other.cfg)
    
    //End of example. 

    Whitespace is tolerable (except for newlines in
    the middle of a key/value pair). Should add a slash for escaping
    colons and for continuing a line... Section names can only
    be letters + the underscore (easy to change if needed). Should
    also add comment indicators.

    Includes are handled by running the m4 preprocessor first.
*)

module L = Logging

(************ Patterns for tokenizing *************)

let ws = "[ \r\n\t]*"

(* Tolerable section names *)
let secNames = "\\([_a-zA-Z]+\\)"

let groupStart = Str.regexp (ws ^ secNames ^ ws ^ "[{]" ^ ws)

let groupEnd = Str.regexp (ws ^ "[}]" ^ ws)

(* separator between field name and value  *)
let fieldSplit = Str.split_delim (Str.regexp (ws ^ "[:]" ^ ws))



(************ Table of tables of settings *********)

type groupSettings = (string, string) Hashtbl.t

type settings = (string, groupSettings) Hashtbl.t


(************ The toplevel parsing routines *******)

let handleGroup settings groupName groupSettings inFile =
  try while (true) do
    let curLine = input_line inFile in
    
    let informError () = 
      L.logError ("Corrupt line in config group: " ^ groupName);
      L.logError curLine
    in
    
    let fields = fieldSplit curLine in
    (match fields with
       fieldName :: [values] ->
         Hashtbl.add groupSettings fieldName values
     | [] -> (* blank line, skip *)
         ()
     | [str] -> (* some other string *)
         if (Str.string_match groupEnd str 0) then begin
           raise End_of_file
         end
         else
           informError ()
     | _ ->
         informError () 
    )
  done
  with End_of_file ->
    Hashtbl.replace settings groupName groupSettings;
    L.logStatus ("Finished parsing config group: " ^ groupName)

let getOldGroup settings groupName =
  try Hashtbl.find settings groupName
  with Not_found -> Hashtbl.create 37

let rec findGroups settings inFile =
  try while (true) do
    let line = input_line inFile in
    if(Str.string_match groupStart line 0) then
      let groupName = Str.matched_group 1 line in
      let groupSettings = getOldGroup settings groupName in
      handleGroup settings groupName groupSettings inFile;
  done
  with End_of_file ->
    L.logStatus "Done reading config file"



(***************** Interface *****************)

(** Run the m4 preprocessor on the config file first *)
let preprocess (configFile : string) : in_channel =
  Unix.open_process_in ("m4 " ^ configFile)

(** Create a settings table from the [configFile] (filename) *)
let initSettings (configFile : string) : settings =
  try
    let settings = Hashtbl.create 37 in
    let inFile = preprocess configFile in
    findGroups settings inFile;
    close_in inFile;
    settings
  with e -> Printf.printf "Exc. in initSettings: %s\n"
    (Printexc.to_string e) ; raise e

(** Project out the settings for the given [groupName] *)
let getGroup (settings : settings) (groupName : string) : groupSettings =
  Hashtbl.find settings groupName
    

let iter = Hashtbl.iter
