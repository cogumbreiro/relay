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

(** This module handles writing summaries to disk, and tracking 
    the storage locations. It also tracks which summaries are considered 
    finalized (with-respect-to a particular analysis).  E.g., functions w/out 
    bodies can be set to the "empty" summary and finalized 
    (we can't analyze them w/out the body)
*)

open Fstructs
open Statfs

module L = Logging
module Stat = Mystats



(************************************************************
     Summary Storage Locations
 ************************************************************)

(** Opaque type representing a storage path for summaries *)
type dbToken = int

(** Low-level conversion of the token to an abstract string 
    (the converted the string may not be the path -- it is only 
    a temporary intermediate representation) *)
let string_of_token = string_of_int

(** Low-level conversion for the low-level string back to the token *)
let token_of_string = int_of_string


(*************** Map entry for tracking presence of summaries ***************)

type 'a sumStub = (* parameter is summary type *)
    InMemSumm of (dbToken option * 'a) 
      (** Summary (of type 'a) is in memory. Also tracks the
          old storage location if any. TODO: add a "dirty" bit 
          to skip writing in some cases *)

  | OnDiskSumm of dbToken
      (** Summary is on disk, stored at the path represented by dbToken *)

(** Exception thrown when there is not enough space to 
    store summaries on disk*)
exception Not_enough_space


(** A list of summary paths eligible for storage. Load balance between them *)
let summPaths = ref []


(** The "null" storage path token. TODO: actually check against this *)
let dummyToken = 0


(************************************************************
    Kinds of summaries
 ************************************************************)

type sumType = string

(** Generate a sumType given a very short name (e.g. "rs") *)
let makeSumType strRep : sumType =
  strRep (* TODO: check uniqueness and simplicity *)

(** Convert sumType to a printable string *)
let string_of_sumType typ = 
  typ


(************************************************************
      Initialization and parsing of the config file
 ************************************************************)

(* Top level split, between the field name and data *)
let ws = "[ \r\n\t]*"

let pathSeparator = Str.split_delim (Str.regexp (ws ^ "[;]" ^ ws))

let pathFieldName = "PATHS"


(** Choose the given list of summary storage paths for use *)
let setPaths paths =
  List.iter (fun s ->
               Filetools.ensurePath s) paths;
  L.logStatus "summary database will use:";
  List.iter (fun p -> 
               L.logStatus ("\t" ^ p)) paths;
  summPaths := paths
    
(** Make up a location for storing summaries, specific to a given 
    input program (by using its call graph directory) *)
let makePaths cgDir =
  [Filename.concat cgDir "relay_sums"]


(** Initialize summary manager based on parsed config file [settings] *)
let init settings cgDir =
  let _ = Unix.umask 0o000 in
  let bsSettings = Config.getGroup settings "SUMMARY_DB" in
  let usePath = ref false in
  let paths = ref [] in
  Config.iter
    (fun fieldName values ->
       let informError () = 
         L.logError ~prior:0 "Corrupt value in backing store config:";
         L.logError values;
       in
       try 
         (match fieldName with
            "PATHS" -> paths := pathSeparator values
          | "CENTRAL" -> usePath := bool_of_string values
          | _ -> informError ()
         )
       with e -> 
         L.logError ("init BS settings: " ^ (Printexc.to_string e));
         informError ();
         raise e
    ) bsSettings;
  if (!usePath && (!paths <> [])) then
    setPaths !paths
  else
    setPaths (makePaths cgDir)



(************************************************************
   Track finalized function summaries (for each analysis)
 ************************************************************)

(** Table of functions which have finalized summaries wrt to an analysis *)
let (final_funcs : (fKey, sumType list) Hashtbl.t) = Hashtbl.create 213

(** Mark the function's ([f]) summary as finalized 
    for the analysis type [t] *)
let setFinal (f:fKey) (t:sumType) : unit =
  let oldList = try Hashtbl.find final_funcs f with Not_found -> [] in
  let newList = Stdutil.addOnce oldList t in
  Hashtbl.replace final_funcs f newList
    
(** True if the function's ([fkey]) summary is finalized for analysis [t] *)
let isFinal (fkey:fKey) (t:sumType) : bool =
  try
    let curFinal = Hashtbl.find final_funcs fkey in
    List.mem t curFinal
  with Not_found ->
    false


(************************************************************
  Low-level operations: Assign directories, serialize, etc. 
 ************************************************************)


(** If the storage locations are relative, assume it is relative to
    the current working directory. (DO NOT CHDIR DURING EXECUTION) *)
let cwd = Sys.getcwd ()

(** Estimate of upper bound on the size of summary file 
    (lower bound on storage requirement) *)
let minSpace = Int64.of_int 209715200 (* 200 MB *)


(** Approximate whether there will be enough space for the next summary *)
let enoughSpace (path:string) =
  let stats = Statfs.statfs path in
  Int64.compare (Int64.mul stats.f_bsize stats.f_bavail) minSpace > 0


(** Translate a token to the base path of a summary *)
let pathFromToken (t:dbToken) : string =
  let path = List.nth !summPaths t in
  if (Filename.is_relative path) then
    Filename.concat cwd path
  else
    path


(** Convert a path to a token (possibly appending the given path 
    to the list of summary storage path choices if previously unknown) *)
let pathToToken (path:string) : dbToken = 
  try
    (* Check if the given path is part of the list of known paths. 
       If not, add to list and convert to a token first *)
    Stdutil.indexOf (fun sp -> path = sp) !summPaths
  with Not_found ->
    let tok = List.length !summPaths in
    summPaths := !summPaths @ [path]; (* add to end so existing toks unfazed *)
    Filetools.ensurePath path;
    tok


(** Choose the next storage path, given the current one ([curToken]) 
    and the number of choices to begin with ([listLen]) *)
let nextToken listLen curToken = 
  curToken + 1 mod listLen
    (* Could just have it check the list length every time... *)


(** Make sure the chosen path has enough space *)
let rec checkPath listLen firstToken curToken : (string * dbToken)=
  let p = pathFromToken curToken in
  if (enoughSpace p) then
    (p, curToken)
  else 
    let newToken = nextToken listLen curToken in
    if (newToken == firstToken) then
      raise Not_enough_space
    else
      checkPath listLen firstToken newToken


let getBasename (fid:fKey) (summType:sumType) : string =
  ((string_of_int fid) ^ "." ^ (string_of_sumType summType))
    
    
(** Create the filename used to store a summary *)
let getFName (fid:fKey) (summType:sumType) (path:string) = 
  Filename.concat path (getBasename fid summType)


(** Get the FID associated w/ the chosen filename *)
let key_of_name fname : fKey =
  let base = Filename.basename fname in
  (int_of_string (Filename.chop_extension base))


(** Get the sumType associated w/ the chosen filename *)
let stype_of_name fname : sumType =
  makeSumType (Stdutil.get_extension fname)


(** Choose a directory to store the summary, 
    return a token to represent which directory was chosen 
    in the end *)
let chooseDBPath (fid: fKey) : (string * dbToken) =
  let listLen = List.length !summPaths in
  let tok = fid mod listLen in
  let (path, actualTok) = checkPath listLen tok tok in
  (path, actualTok)


(** Pick any random storage location. TODO: also check if 
    it has enough space.*)
let lastTok = ref 0
let anyDBPath () : (string * dbToken) =
  let listLen = List.length !summPaths in
  lastTok := (!lastTok + 1) mod listLen;
  let (path, actualTok) = checkPath listLen !lastTok !lastTok in
  (path, actualTok)


(** See if a summary already exists on disk (we just didn't know about it
    because of restart, or what-not). Raises Not_found if not found. *)
let find (fid: fKey) (summType:sumType) : (string * dbToken) =
  (* Search all local dbPaths *)
  let p = List.find 
    (fun path ->
       let fname = getFName fid summType path in
       Sys.file_exists fname;
    ) !summPaths in
  (p, pathToToken p) 



(** Discover any summaries (from disk) for function [f] and analysis
    type [typ] that weren't already known in mem. If it isn't found, 
    invoke the notFoundFunc. *)
let discover (f:fKey) (typ:sumType) 
    (notFoundFunc: fKey -> sumType -> dbToken option) : dbToken option =
  if (isFinal f typ) then 
    None (* no-op, globally known to be finalized (assuming all 
            workers are using the same config + ASTs) *)
  else
    (* Check if some of summary is on disk *)
    try 
      let path, tok = find f typ in
      Some tok
    with Not_found ->
      notFoundFunc f typ


(** Get the file used to store a summary previously *)
let getDBFile (fid: fKey) (summType:string) (token: dbToken) : string =
  let path = pathFromToken token in
  getFName fid summType path


(** Get a previously serialized summary for a function *) 
let deserializeFromToken (fkey:fKey) (summType:string) (tok: dbToken) : 'a =
  let fileName = getDBFile fkey summType tok in
  Gz_marshal.from_file fileName
 

(** Get a previously serialized summary from a basepath + fkey.
    Also return the dbToken as reference to the path in the future *)
let deserializeFromPath (fkey:fKey) (summType:string) (path:string) 
    : 'a * dbToken =
  let token = pathToToken path in
  let fileName = getDBFile fkey summType token in
  let result = Gz_marshal.from_file fileName in
  (result, token)


(** Just deserialize the file, return the value of any summaries 
    stored in that file *)
let deserializeFromFile (fname:string) (summType:string) : 'a =
  Gz_marshal.from_file (fname ^ "." ^ summType)


(** Store summary [value] for function [fkey] and analysis
    type [summType] in file. Return the storage location  *)
let serializeSummary (fkey:fKey) (summType:string) (value:'a) : dbToken =
  (* First, pick a place to store *)
  let listLen = List.length !summPaths in
  let path, firstToken = chooseDBPath fkey in
  let fileName = getFName fkey summType path in
  (* Then try, (may fail if there isn't enough space *)
  let rec trySerializing curFName curToken =
    try (* Assume no name collisions *)
      Gz_marshal.to_file curFName value [Marshal.Closures] ;
      curToken
    with
      Sys_error _
    | Unix.Unix_error (Unix.ENOSPC, _, _) ->
        (* channel closed by Gz_marshal *)
        (* manually advance token and check if we've run out of choices *)
        let newToken = nextToken listLen curToken in
        if (newToken == firstToken) then
          raise Not_enough_space
        else let newF, newT = checkPath listLen firstToken newToken in
        trySerializing newF newT
  in
  trySerializing fileName firstToken


(** Delete summary for function [fkey] and analysis [sumType],
    given that it exists on disk in the storage location represented 
    by the [token] *)
let removeSummary (fkey:fKey) (summType:sumType) (token:dbToken) : unit =
  try
    let fileName = getDBFile fkey summType token in
    Gz_marshal.remove fileName;
    L.logStatus ("Deleted summary for: " ^ (string_of_fkey fkey));
  with (Sys_error x) as e ->
    L.logError ~prior:0 ("removeSummary failed: " ^ x);
    raise e


(** Delete all summaries in summary storage paths that do not match
    the current generation ([gen_num]). Store the current generation *)
let clearState gen_num =
  List.iter
    (fun sumPath ->
       let gen_file = Filename.concat sumPath "gen_num.txt" in
       let clearFunc = 
         (fun () ->
            Stdutil.clearDir sumPath (fun _ -> true)) in
       Stdutil.clearDirGen gen_num gen_file clearFunc
    ) !summPaths



(************************************************************
     Generic serialization / flushing ops for summaries
 ************************************************************)

(** Input module for generating a module that will manage 
    storing summaries to disk *)
module type Summarizeable = sig

  (** Type of the summary *)
  type t 

  (** Type of a simplified summary *)
  type simpleSum 

  (** Unique identifier / dynamic type of the summary *) 
  val id : sumType

  (** Function for simplifying the summary before writing to disk *)
  val simplify : t -> simpleSum

  (** Function for reversing simplification (used after loading) *)
  val desimplify : simpleSum -> t

  (** value to use for initialization when summary isn't found the first time *)
  val initVal : t

end

  
(** Interface to summary database *)
class type ['sum] base = object
  
  (** Handles any cleanup of partially read/written summaries on reboot *)
  method cleanup : unit -> unit
    
  (** Find and return the summary for the given function. If "Not_found",
      return a specified initial value instead of raising the exception *)
  method find : fKey -> 'sum
    
  (** Replace an old summary (if any) for the given function w/ a new one *)
  method addReplace : fKey -> 'sum -> unit
    
  (** Save all (in-memory) summaries to disk, and allow garbage collection *)
  method serializeAndFlush : unit
    
  (** evict all in-memory summaries which have already been written to disk *)
  method evictSummaries : unit
    
  (** Load the summary from the given file *)
  method getFromFile : string -> 'sum
    
  (** Given a list of functions and storage locations, assume 
      the summaries for those functions can be found at 
      the corresponding locations *)
  method assumeComplete : ((fKey * dbToken) list) -> unit 
    
  (** Low-level serialization. Avoid using, but feel free to extend *)
  method private serialize : fKey -> 'sum -> dbToken
    
  (** Low-level deserialization. Avoid using, but feel free to extend *)
  method private deserialize : fKey -> dbToken -> 'sum * dbToken
    
  (** Log an error, given the body of the message *)
  method err : string-> unit
    
  (** Write the summary for the given function to disk *)
  method flushOne : fKey -> unit
    
  (** Given a list of functions, find the storage locations of each
      function's summary. May be omitted in the resulting list if
      there is no such summary. 
      TODO: maybe raise an exception instead. Not sure if it's worth
      the trouble to have some kinds of summaries NOT written to disk 
      (e.g., the initial/bottom summaries) *)
  method locate : fKey list -> (fKey * dbToken) list
    
  (** Bit of reflection to identify the kind of summary tracked *)
  method typ : sumType
    
end


(** Output module that will manage storing summaries to disk *)
module type S  = sig

  (** The type of summaries *)
  type sum

  (** Implementation for the database interface *)
  class data : [sum] base

end


module Make (I:Summarizeable) = struct
  
  type sum = I.t

  let sumStr = string_of_sumType I.id

  (** Implementation of the interface *)
  class data : [sum] base = object (self)
    
    (* TODO: Should allow only a single instance to be registered *)
        

    (* The summary map. TODO: Is it better to consolidate
       all the summaries for each function into one file per function? 
       One map entry per function (reduces number of intermediate index
       nodes, but will have the same number of leaves)? *)
    val mutable summs = FMap.empty

    (** Bit of reflection to identify the kind of summary tracked *)
    method typ : sumType =
      I.id

    (** Post-reboot cleanup *)
    method cleanup () = L.logStatus "BS: Not doing any cleanup"

    (** Shortcut for tagging, then logging error messages *)
    method err str =
      L.logError (sumStr ^  ": " ^ str)

    (** Serialize the summary (given the associated function ID).
        Return the chosen storage location. *)
    method private serialize (fkey:fKey) (v:sum) : dbToken =
      try serializeSummary fkey I.id (I.simplify v);
      with e -> self#err ("serialization failed: " ^  
                            (string_of_fkey fkey) ^ " " ^
                            (Printexc.to_string e));  
        raise e

    (** Read in the summary for function fkey given the storage 
        location (token). Returns a new token, which may be different 
        if the summary was moved. *)
    method private deserialize (fkey:fKey) (token:dbToken) : sum * dbToken =
      try
        (I.desimplify (deserializeFromToken fkey I.id token), token)
      with e -> self#err ("deserialization failed for: " ^ 
                            (string_of_fkey fkey));
        raise e

    (** Deserialize a summary, given the filename (w/ extension stripped) *)
    method getFromFile (path:string) : sum =
      try deserializeFromFile path I.id
      with e -> self#err ("deserialization failed for : " ^ path);
        raise e

    (** Replace the summary given the (optional) storage location (tokOpt) 
        in which the summary should use for storage in the future *)
    method private addReplace2 (k:fKey) 
      ((tokOpt, newVal):dbToken option * sum) : unit =
      summs <- FMap.add k (InMemSumm (tokOpt, newVal)) summs


    (** Replace the old value in the summary w/ this one *)          
    method addReplace (k:fKey) (newVal:sum) : unit =
      let tok =
        try
          let stub = FMap.find k summs in
          match stub with
            OnDiskSumm t -> Some t
          | InMemSumm (t, _) -> t
        with Not_found -> None
      in
      summs <- FMap.add k (InMemSumm (tok, newVal)) summs

    (** Find the summary for function with fkey k. 
        Returns I.initVal if it isn't found, and uses that in the future *)
    method find (k:fKey) : sum =
      try
        let stub = FMap.find k summs in
        match stub with
          OnDiskSumm t ->
            let v, tok = self#deserialize k t in
            self#addReplace2 k (Some tok, v);
            v
        | InMemSumm (_, v) -> 
            v
      with Not_found ->
        self#addReplace2 k (None, I.initVal);
        I.initVal

    (** Serialize and flush one function summary from memory *)
    method flushOne fkey =
      try 
        match FMap.find fkey summs with
        OnDiskSumm _ -> ()
        | InMemSumm (_,v) ->
            let newTok = self#serialize fkey v in
            summs <- FMap.add fkey (OnDiskSumm newTok) summs
      with Not_found ->
        ()
          
    (** serialize all the summaries and clear from memory *)
    method serializeAndFlush =
      summs <- FMap.mapi
        (fun fkey summStub ->
           match summStub with
             InMemSumm (_,v) ->
               let newTok = self#serialize fkey v in
               (OnDiskSumm newTok)
           | OnDiskSumm _ ->
               summStub
        ) summs
    
    (** Clear all in-memory summaries that have already been written out *)
    method evictSummaries =
      summs <- FMap.mapi
        (fun fkey summStub ->
           match summStub with
             InMemSumm (Some(t), s) ->
               OnDiskSumm (t)
           | OnDiskSumm t ->
               summStub
           | InMemSumm (None, s) ->
               summStub
        ) summs


    (** Assume functions listed in have finished summaries 
        (also given location in which summary is stored)   *)
    method assumeComplete (fk_toks_list : (fKey * dbToken) list) : unit  =
      List.iter (fun (f,tok) ->
                   summs <- FMap.add f (OnDiskSumm tok) summs) fk_toks_list


    (** Find the locations of the summaries for the given list of functions.
        If the storage location for a function is unknown, it is omitted. *)
    method locate (fkeys : fKey list) : (fKey * dbToken) list =
      List.fold_left
        (fun res fk -> 
           try match FMap.find fk summs with
             InMemSumm (Some(t), _)
           | OnDiskSumm (t) ->
               (fk, t) :: res
           | _ -> res
           with Not_found -> res
        ) [] fkeys
        
        
  end (* end of class *)
  
end

(************************************************************
  Interface / callbacks that are not parametric 
  (and therefore can be placed in collections (like lists)
 ************************************************************)

type sumDescriptor = {
  sumTyp : sumType;
  sumCompletor : (fKey * dbToken) list -> unit;
  (* locator, etc ? *)
  (* make a link between the sumType and the analysis type/id? *)
}


(** List of all known sumDescriptors *)
let (allTypes : sumDescriptor list ref) = ref []

(** Add another sumType to the list of known types. 
    Whenever a database instance is created, it MUST be registered! *)
let registerType (db : ('a) base) : unit =
  (* register a descriptor with the system *)
  let myDescriptor = 
    { sumTyp = db#typ;
      sumCompletor = db#assumeComplete; } in
  if List.exists (fun x -> myDescriptor.sumTyp = x.sumTyp) !allTypes then ()
  else allTypes := myDescriptor :: !allTypes

(** Get a list of summary filenames for given function ID.
    TODO: remove this and use newer interface *)
let possibleNames fid =
  List.map (fun sumDescr -> getBasename fid sumDescr.sumTyp) !allTypes
