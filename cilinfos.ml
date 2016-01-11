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

module IH = Inthash
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

let minIDCacheSize = 120001

let (vidToVar : varinfo IDC.t) = IDC.create minIDCacheSize


(** Find the largest IDs from files that may be loaded, and set the
    CIL IDs out of that range *)
let initCilIDs () =
  let maxVID = RangeMap.fold
    (fun {r_upper = u;} _ curMax ->
       if (u > curMax) then
         u
       else
         curMax
    ) !vidToFile (-1) in
  let maxCKey = RangeMap.fold
    (fun {r_upper = u;} _ curMax ->
       if (u > curMax) then
         u
       else
         curMax
    ) !ckeyToFile (-1) in
  Cil.setNextVID (maxVID + 1);
  Cil.setNextCKey (maxCKey + 1)


(** Initialize the size of the VID cache based on the max range of ids
    for a file *)
let initVidCache () =
  (* look at the var ranges for each file, and allow the cache to be 
     at least that big to avoid thrashing! TODO: also compare to largest 
     points-to-set size *)
  let maxRange = RangeMap.fold
    (fun {r_lower = l; r_upper = u;} _ curMax ->
       let range = u - l in
       if (range > curMax) then
         range
       else
         curMax
    ) !vidToFile (-1) in
  if (maxRange <= minIDCacheSize) then
    ()
  else
    IDC.resize vidToVar (maxRange * 2)


(** get the varinfo matching the given vid *)
let getVarinfo (id:int) : varinfo =
  try
    IDC.find vidToVar id
  with Not_found ->
    let filename = RangeMap.find { r_lower = id;
                                   r_upper = id; } !vidToFile in
    let index = !DC.viFCache#getFile filename in
    let vi = IH.find index id in
    Cil_lvals.distillVar vi;
    IDC.add vidToVar id vi;
    vi

(** look for a global w/ the given name *)
let varinfo_of_name (n:string) : varinfo option =
  RangeMap.fold 
    (fun _ fname result -> 
       match result with 
         Some _ -> result
       | None -> 
           try let index = !DC.viFCache#getFile fname in
           IH.fold 
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
    

let reloadRanges fname =
  let ids, cks = Id_fixer.loadRanges fname in
  vidToFile := ids;
  ckeyToFile := cks;
  initCilIDs ();
  initVidCache ()



(********** Cil.File / fundec ops **********)


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
      (* TODO figure out if the CFG info is cached also? *)
      Cil.prepareCFG f;
      Cil.computeCFGInfo f true;
      Some f
  | None -> 
      None


