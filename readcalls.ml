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

(** Reads a call graph from a file (@see dumpcall.ml for the format) *)

open Sys
open Fstructs
open Cil
open Stdutil
open Callg

module L = Logging

(*********************************************************************)
(* Callgraph functions                                               *)


(** 
    Modify the given callgraph to complete connections.
    That is, fill the caller list (needed to wait for all callee edges 
    to be loaded first)
*)
let completeCG (partialCG: simpleCallG) : unit =
  (* Tell neighbors you're a caller *)
  let informNeighbors curFK node =
    (* fill caller fields of neighbors *)
    List.iter 
      (fun fk ->
         try
           let tempNode = FMap.find fk partialCG in
           let newCallers = addOnce tempNode.callers curFK in
           tempNode.callers <- newCallers;
         with Not_found ->
           ()
      ) node.callees;
  in
  FMap.iter informNeighbors partialCG
    


(*******************************************************************)
(* READ functions                                            *)



(* Splits a string based on a delimiter *)

let topSplitter = Str.split_delim (Str.regexp ("[" ^ topDelim ^ "]"))

let splitter = Str.split_delim (Str.regexp ("[" ^ delim ^ "]"))

exception CorruptCGFile

(** Read the call graph data from the given filename "readFrom" *)
let readCalls (readFrom:string) : simpleCallG =
  let curCG = ref FMap.empty in
  
  let rec makeSuccs (succList : string list) curCallees =
    match succList with
      [] -> 
        curCallees
    | hd :: tl -> begin
        match splitter hd with 
          [funcName ; typString ; funID] ->
            let fid = int_of_string funID in
            (* check if there's a node for the neighbor yet *)
            (try
               let _ = FMap.find fid !curCG in
               (* fill in caller info later *)
               ()
             with Not_found -> 
               let newNode = 
                 {name = funcName;
                  defFile = "";
                  typ  = typString;
                  callees = [];
                  callers = []; (* fill in caller info later *)
                  hasBody = false;
                 }
               in
               curCG := FMap.add fid newNode !curCG
            );
            makeSuccs tl (fid :: curCallees)
        | _ ->
            L.logError "Corrupt successor list in callgraph file";
            raise CorruptCGFile
      end
  in
  let inFile = (open_in readFrom) in
  try while (true) do 
    let curLine = input_line inFile in
    match topSplitter curLine with
      fileName :: myNode :: succs -> begin
        match splitter myNode with
          [ funcName ; typString ; funID ] -> begin
            let fid = int_of_string funID in
            try
              (* If there's an existing entry *)
              let curNode = FMap.find fid !curCG in
              let newCallees = makeSuccs succs [] in
              let called = union curNode.callees newCallees in
              (* Replace entry with updated entry... 
                 could have made fields mutable? *)
              let newCG = FMap.add fid
                ({curNode with
                    defFile = fileName;
                    callees = called;
                    hasBody = true;
                 }) !curCG in
              curCG := newCG

            with Not_found -> 
              (* otherwise, make a new entry *)
              let called = makeSuccs succs [] in
              let newCG = FMap.add fid 
                ({name = funcName;
                  defFile = fileName;
                  typ  = typString;
                  callees = called;
                  callers = [];
                  hasBody = true;
                 }) !curCG in
              curCG := newCG
          end
        | _ ->
            L.logError "Corrupt line in callgraph file:";
            L.logError (curLine);
            raise CorruptCGFile
      end
    | _ ->
        L.logError "Corrupt line in callgraph file:";
        L.logError (curLine);
        raise CorruptCGFile 
  done;
    !curCG
  with End_of_file ->
    L.logStatus ("Reached end of " ^ readFrom ^ " call graph file");
    L.logStatus ("Num funcs: " ^ 
                   (string_of_int (mapSize !curCG FMap.fold)));
    L.flushStatus ();
    close_in inFile;
    (* Add the backlinks (caller edges) *)
    let () = completeCG !curCG in
    !curCG

