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
open Cil

module L = Logging
module Th = Threads

let rootFile = "roots.txt"
 
type patterns =
    NamePat of Str.regexp
  | TypePat of Str.regexp

let ws = "[ \r\n\t]*"

let splitter = Str.split_delim (Str.regexp (ws ^ "[:]" ^ ws))

let name_field = "name"

let type_field = "type"

(************************************************************)

let useEntrypoints = ref true

let initSettings config =
  let settings = Config.getGroup config "ENTRY_POINTS" in
  Config.iter 
    (fun fieldName value ->
       let informError () = 
         L.logError "Corrupt line in entry_points settings file:\n";
         L.logError (fieldName ^ "\n")
       in
       try
         (match fieldName with
            "USE_ROOTS" ->
              useEntrypoints := bool_of_string (Strutil.strip value);
              L.logStatus ("Entry points use roots: " ^ 
                             string_of_bool !useEntrypoints)
          | _ ->
              informError ()
         ) 
       with e ->
         L.logError ("initSettings: " ^ (Printexc.to_string e));
         informError ();
         raise e
    ) settings

(************************************************************)

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
  failwith "not used"


(************************************************************)

type root =
    Entry of (fKey * simpleCallN)
  | Thread of (fKey * simpleCallN)


let tagRootsWith cg (tagger : (fKey * simpleCallN) -> 'a) roots : 'a list =
  let (results : 'a list) = FSet.fold 
    (fun fkey cur ->
       try 
         let n = FMap.find fkey cg in 
         tagger (fkey, n) :: cur
       with Not_found -> 
         L.logError ("tagRootsWith: no node for " ^ (string_of_fkey fkey));
         cur
    ) roots [] in
  results


class rootGetter cg cgDir = object (self)

  (* Find which functions actually fork new threads *)
  val threadCreatorCallers = Th.findTCCallers cg



  (** Get "roots" that are tagged as Thread. These are really just
      functions that spawn the thread roots, not the roots themselves *)
  method getThreadRoots () : root list =
    let threadCreators = List.fold_left
      (fun cur (fkey, _) -> FSet.add fkey cur) 
      FSet.empty threadCreatorCallers in
    (* Tag them *)
    tagRootsWith cg (fun (fkey, fnode) -> Thread (fkey, fnode)) threadCreators


  method getEntryFKeys () : FSet.t = 
    (* [a] Get user-specified entry points (in case they aren't roots)
       [b] Find call graph roots that reach spawn sites. *)
    let entryRoots = getEntries cgDir cg in
    let threadCreatorsRoots = List.fold_left
      (fun cur (fkey, _) -> FSet.add fkey cur) 
      FSet.empty threadCreatorCallers in
    let spawnRoots = rootsThatReach cg threadCreatorsRoots in
    let roots = FSet.union entryRoots spawnRoots in
    roots
         
  (** Get roots that are tagged as Entry, and not already handled as Thread.
      The thread roots themselves will be handled by the spawning functions
      (which are tagged as Thread) *)
  method getEntryRoots () : root list =
    let roots = self#getEntryFKeys () in
    (* Thread roots may be "entry-points" as well, but they're already
       handled by the "Thread n" tagging above, so filter *)
    let threadRoots = Th.getThreadRoots cg threadCreatorCallers in
    let roots = FSet.filter
      (fun f -> not (FSet.mem f threadRoots)) roots in

    (* Tag them *)
    tagRootsWith cg (fun (fkey, fnode) -> Entry (fkey, fnode)) roots

  method getRootKeys () : FSet.t =
    if !useEntrypoints then
      let threadRoots = Th.getThreadRoots cg threadCreatorCallers in
      let roots = FSet.union threadRoots (self#getEntryFKeys ()) in
      roots
    else
      let threadRoots = Th.getThreadRoots cg threadCreatorCallers in
      (* Also need the thread creator callers *)
      let threadCreators = List.fold_left
        (fun cur (fkey, _) -> FSet.add fkey cur)
        FSet.empty threadCreatorCallers in
      FSet.union threadRoots threadCreators

  (** Get function keys of relevant roots *)
  method getUntaggedRoots () : (fKey * simpleCallN) list =
    let roots = self#getRootKeys () in
    tagRootsWith cg (fun (fkey, fnode) -> (fkey, fnode)) roots
      

  (** Return the list of tagged roots that are relevant to the analysis. *)
  method getRoots () : root list =
    if !useEntrypoints then
      (self#getThreadRoots ()) @ (self#getEntryRoots ())
    else
      self#getThreadRoots ()

end



(************* UnCommon Queries **************)

(* Hack alert: This prunes out functions from the callgraph that
    1) has Static storage 2, or
    2) starts with a lower case function.
   The remaining functions are then printed out.
   This was designed to extract the "thread roots" for the OpenSSL library
 *)
let printNonStaticAndUpperCaseRoots cg =
  let roots = getRoots cg in
  let cfgs =
    List.fold_left (fun res (fk, node) ->
      let file = !Default_cache.astFCache#getFile node.defFile in
      match Cilinfos.getCFG fk file with
        | None -> res
        | Some cfg -> cfg :: res) [] roots in
  let cfgs2 =  
    List.filter (fun cfg ->
      match cfg.svar.vstorage with
        | Static
        | Register -> false
        | _ -> true) cfgs in
  let cfgs3 =
    List.filter (fun cfg ->
      if cfg.svar.vname.[0] <= 'Z' then true
      else false ) cfgs2 in

    let a, b = List.length cfgs, List.length cfgs3 in
    L.logStatus (string_of_int (a-b) ^ " of " ^ string_of_int a
                   ^ " roots removed because Static and/or lowercase");
    List.iter (fun cfg ->
      L.logStatus ("non-static-root: " ^ cfg.svar.vname);
    ) cfgs3;

    roots
                 
