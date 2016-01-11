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


(** Cache of various files (AST files, varinfo index files, etc.) *)

open Fstructs
open Cil

module Stat = Mystats

(** Polymorphic file caches *)
module C = Cache.Make(
  struct
    type t = string           (* relative file name *)
    let equal = (=)
    let hash = Hashtbl.hash
  end
)

(************** Cache Ops **************)

exception File_not_found of string

class ['a] fcache (ld : string -> 'a) (rt:string) (sz:int) = object (self)
  
  (** Root directory in which the files are expected to be stored *)
  val mutable root = rt

  (* The cache *)
  val cache = C.create sz

  (** A function that loads the contents of the file 
      (and possibly unmarshal, possibly parse) *)
  val loader = ld

  method setRoot r =
    root <- r

  method resize i =
    C.resize cache i

  method addFile fname newF =
    ignore (C.add cache fname newF)

  method getFile fname =
    try
      C.find cache fname
    with Not_found ->
      let fullName = (Filename.concat root fname) in
      try
        match Sys.file_exists fullName with
          | true -> begin
              let newF = loader fullName in
              self#addFile fname newF;
              newF
            end
          | false ->
              raise (File_not_found fullName)
      (* rkc: trying to manually handle another kind of lookup error *)
      with Sys_error e -> begin
        Logging.logStatusF "getFile: Sys_error %s getFile %s\n" e fullName;
        raise (File_not_found fullName)
        end

  method clear () =
    C.clear cache

end
      
(************** Known loaders ************)

(** Loader for cil files (given absolute filename) *)
let cilLoader fname =
  Cil.loadBinaryFile fname


(** Loader for varinfo files *)
let viLoader fname : Cilfiles.viMap =
  Cilfiles.doLoads fname


(** Loader for compinfo files *)
let ciLoader fname : Cilfiles.ciMap =
  Cilfiles.doLoads fname

