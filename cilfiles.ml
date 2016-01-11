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


(** Support for indexing and mapping between IDs for varinfo / compinfo
    and the files that contain the actual varinfos and compinfos.
    Varinfos and compinfos have been stored are separated from original
    CIL AST file (@see vinfoExt, cinfoExt)
*)

open Cil
open Filetools
open Intrange

(*********** ID -> varinfo / compinfo indices *****)

(** File extensions for storing indexes of varinfos / compinfos *)
let vinfoExt = ".vi"

let cinfoExt = ".ci"

type viMap = varinfo Inthash.t

type ciMap = compinfo Inthash.t

(** Write the mapping to a file w/ an extension like [vinfoExt] *)
let doWrites index origFile newExt =
  let newFile = changeExtension origFile newExt in
  let out_chan = open_out_bin newFile in
  Marshal.to_channel out_chan 
    index [Marshal.Closures] ;
  close_out out_chan

(** Write the mappings for varinfos and compinfos to a file w/ a
    filename that is based on the the original CIL AST object file *)
let writeIndexes vis cis origFile =
  doWrites vis origFile vinfoExt;
  doWrites cis origFile cinfoExt

(** Load the mapping (either varinfo or compinfo) from a file *)
let doLoads fname =
  let in_chan = open_in_bin fname in
  let res = Marshal.from_channel in_chan in
  close_in in_chan;
  res


(************ ID range -> filename maps ************)

(** Map from VID range to filename *)
let vidMap = ref RangeMap.empty

(** Map from CKey range to filename *)
let ckeyMap = ref RangeMap.empty

(** Clear the temp/global range maps *)
let initRanges () =
  vidMap := RangeMap.empty;
  ckeyMap := RangeMap.empty

(** Add a (id range -> file) mapping to the global range maps *)
let addRanges startVid endVid startCkey endCkey curFile =
  (* Only add if it is a non-empty set *)
  if (startVid <= endVid) then
    vidMap := RangeMap.add 
      {r_lower = startVid; r_upper = endVid;} 
      (changeExtension curFile vinfoExt) !vidMap
  ;
  if (startCkey <= endCkey) then
    ckeyMap := RangeMap.add 
      {r_lower = startCkey; r_upper = endCkey;} 
      (changeExtension curFile cinfoExt) !ckeyMap


(** Return true if the given file is supposed to own the "golden" copy
    of the given kind of info *)
let ownsIDKind (id:int) map (kindExt:string) (file:string) : bool =
  try
    let owningFile = RangeMap.find { r_lower = id; r_upper = id; } map in
    let chExt = changeExtension file kindExt in
(*  Printf.printf "ownsID comparing query(%s) to owner(%s)\n" chExt owningFile;
*)
    chExt = owningFile
  with Not_found ->
    failwith 
      (Printf.sprintf "ownsIDKind given unknown ID %d for %s\n" id kindExt)

  
(** Return true if the given file is supposed to own the "golden" copy
    of the varinfo *)
let ownsVID (vid: int) (file:string) : bool =
  ownsIDKind vid !vidMap vinfoExt file 
    
let ownsCKey (ckey: int) (file:string) : bool =
  ownsIDKind ckey !ckeyMap cinfoExt file

(************************************************************)

(** Save the ranges *)
let saveRanges (fname: string) =
  let out_chan = 
    open_out_gen 
      [Open_creat; Open_wronly; Open_binary] 
      0o666 
      fname in
  (* save vid ranges *)
  Marshal.to_channel out_chan (!vidMap, !ckeyMap) [Marshal.Closures] ;
  close_out out_chan
  

(** Load back the ID ranges *)
let loadRanges fname : ((string RangeMap.t) * (string RangeMap.t)) = 
  let in_chan = open_in_bin fname in
  let (vids, ckeys) = Marshal.from_channel in_chan in
  close_in in_chan;
  (vids, ckeys)
