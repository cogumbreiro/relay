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

(** Support for indexing Cil varinfo / compinfos *)

open Cil
open Filetools
open Intrange
open Fstructs
open Logging 

module FC = Filecache
module DC = Default_cache

(********** Cil varinfos ***********)

let vidToFile = ref RangeMap.empty

let ckeyToFile = ref RangeMap.empty

(** cache of id -> varinfo mappings *)

module IDC = Cache.Make (
  struct 
    type t = int
    let equal = (=)
    let hash = Hashtbl.hash
  end
)

let minIDCacheSize = 1024
let minCKCacheSize = 1024

let (vidToVar : varinfo IDC.t) = IDC.create minIDCacheSize
let (ckeyToCinfo : compinfo IDC.t) = IDC.create minCKCacheSize

let rangesLoaded = ref false
let cilIDsSet = ref false


(** Find the largest IDs from files that may be loaded, and set the
    CIL IDs out of that range *)
let initCilIDs () =
  if not !cilIDsSet then begin
    logStatus "Setting Cil Varinfo ID ranges";
    if not !rangesLoaded then 
      failwith "Need to load VID ranges first!"
    ;
    let maxVID = RangeMap.fold
      (fun {r_upper = u;} _ curMax ->
         max curMax u
      ) !vidToFile (-1) in
    let maxCKey = RangeMap.fold
      (fun {r_upper = u;} _ curMax ->
         max curMax u
      ) !ckeyToFile (-1) in
    Cil.setNextVID (maxVID + 1);
    Cil.setNextCKey (maxCKey + 1);
    cilIDsSet := true
  end

let initCache cache ranges minSize =
  (* look at the var ranges for each file, and allow the cache to be 
     at least that big to avoid thrashing! TODO: also compare to largest 
     points-to-set size *)
  let maxRange = RangeMap.fold
    (fun {r_lower = l; r_upper = u;} _ curMax ->
       let range = u - l in
       max curMax range
    ) !ranges (-1) in
  if (maxRange <= minSize) then ()
  else begin
    let newSize = maxRange * 2 in
    logStatusF "Resizing VID/CKEY cache to: %d\n" newSize;
    IDC.resize cache newSize
  end
  

(** Initialize the size of the VID cache based on the max range of ids
    for a file *)
let initVidCache () =
  initCache vidToVar vidToFile minIDCacheSize

let initCKeyCache () =
  initCache ckeyToCinfo ckeyToFile minCKCacheSize

exception NoCilinfo of int

let getInfo id cache distiller ranges fnameCache =
  try
    IDC.find cache id
  with Not_found ->
    try
      let filename = RangeMap.find { r_lower = id;
                                     r_upper = id; } !ranges in
      let index = !fnameCache#getFile filename in
      let info = Inthash.find index id in
      distiller info;
      IDC.add cache id info;
      info
    with Not_found -> (* Translate exception just in case... *)
      raise (NoCilinfo id)


(** get the varinfo matching the given vid *)
let getVarinfo (id:int) : varinfo =
  getInfo id vidToVar Cil_lvals.distillVar vidToFile DC.viFCache

let getCinfo (id:int) : compinfo =
  getInfo id ckeyToCinfo Cil_lvals.distillCompinfo ckeyToFile DC.ciFCache
  
(** look for a global w/ the given name *)
let varinfo_of_name (n:string) : varinfo option =
  RangeMap.fold 
    (fun _ fname result -> 
       match result with 
         Some _ -> result
       | None -> 
           try let index = !DC.viFCache#getFile fname in
           Inthash.fold 
             (fun _ vi result ->
                match result with 
                  Some _ -> result
                | None -> 
                    if (vi.vglob && vi.vname = n) then Some (vi) else None
             ) index result
           with 
           | FC.File_not_found f 
           | Failure f -> result (* file probably had 0 vars *)
    ) !vidToFile None    


(** Load the mapping between VID/Compinfo key ranges and source file *)
let reloadRanges fname = begin
  logStatus "Loading Cil Varinfo ID ranges for lookup tables";
  let ids, cks = Id_fixer.loadRanges fname in
  vidToFile := ids;
  ckeyToFile := cks;
  rangesLoaded := true;
  initCilIDs ();
  initVidCache ();
  initCKeyCache ();
end



(********** Cil.File / fundec ops **********)

module CFGC = Cache.Make
  (struct
     type t = fKey
     let equal a b = a = b
     let hash a = Hashtbl.hash a
   end)

let cfgCache = CFGC.create 64

(** Get a fundec given the id and file *)
let getFundec fkey file : Cil.fundec option =
  (* Find function we are looking for in a compilation unit *)
  let findF (curFind:Cil.fundec option) (globl:Cil.global) =
    match curFind, globl with
      None, Cil.GFun (fdec, _) ->
        if(fdec.svar.vid == fkey) then
          Some fdec
        else
          curFind
    | _ -> curFind
  in
  Cil.foldGlobals file findF None
    
    
(** Get the fundec w/ its control flow graph initialized *)
let getCFG fkey file : Cil.fundec option =
  match getFundec fkey file with
    Some f ->
      (* already in CFG form
         Cil.prepareCFG f;
       Cil.computeCFGInfo f false;
      *)
      Some f
  | None -> None


let getFuncListeners = ref []

let addGetFuncListener (lis : fKey -> Cil.file -> unit) = 
  getFuncListeners := List_utils.addOnce !getFuncListeners lis


(** Get the AST + CFG for the given function that is defined in [deffile] *)
let getFunc fKey deffile : fundec option =
  try 
    let ast = !DC.astFCache#getFile deffile in
    List.iter (fun lis -> lis fKey ast) !getFuncListeners;
    try CFGC.find cfgCache fKey
    with Not_found ->
      let oldLoc = !currentLoc in
      let cfg = getCFG fKey ast in (* may modify currentLoc *)
      currentLoc := oldLoc;
      CFGC.add cfgCache fKey cfg;
      cfg
  with FC.File_not_found fname ->
    logError ("getFunc/CFG: can't find " ^ fname);
    None
  

let iterInfos (foo: 'a -> unit) ranges distiller fnameCache =
  RangeMap.iter 
    (fun { r_lower = l; r_upper = r } file ->
       let index = !fnameCache#getFile file in
       for id = l to r do  
         try 
           let info = Inthash.find index id in
           distiller info;
           foo info
         with Not_found -> () (* may catch Not_founds from foo too... *)
       done
    ) !ranges

let iterCompinfos (foo: compinfo -> unit) =
  iterInfos foo ckeyToFile Cil_lvals.distillCompinfo DC.ciFCache
