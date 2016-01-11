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

(** Default cache of CIL AST files, and CIL varinfo files *)

open Cilfiles

module FC = Filecache

(***** Place to look for global instances of file caches *****)

(* Cache of parsed ast files *)
let astFCache = ref (new FC.fcache FC.cilLoader "" 79)

let setASTFCache fc = 
  astFCache := fc

(* Cache of varinfo files *)
let viFCache = ref (new FC.fcache FC.viLoader "" 29)

let setViFCache fc =
  viFCache := fc

(* Cache of compinfo files *)
let ciFCache = ref (new FC.fcache FC.ciLoader "" 29)

let setCiFCache fc =
  ciFCache := fc


(* REMOVED this... cause circular dependencies...

module RFC = Remote_fcache

(* Cache of possibly remote files *)
let makeRCaches localScratch localSrc =
  let astFCache = new RFC.rfcache FC.cilLoader localScratch localSrc 127 in
  let viFCache = new RFC.rfcache FC.viLoader localScratch localSrc 43 in
  setASTFCache (astFCache :> Cil.file FC.fcache);
  setViFCache (viFCache :> viMap FC.fcache)

*)

(* Local-file based caches *)
let makeLCaches localSrc = begin
  let astFCache = new FC.fcache FC.cilLoader localSrc 20 in
  let viFCache = new FC.fcache FC.viLoader localSrc 29 in
  let ciFCache = new FC.fcache FC.ciLoader localSrc 29 in
  setASTFCache astFCache;
  setViFCache viFCache;
  setCiFCache ciFCache;
end
