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
(** Extension of filecache that looks at both the local store 
    and a server's store if the file does not exist locally. *) 

open Cil

module FC = Filecache
module Req = Request



class ['a] rfcache (ld : string -> 'a) (srt:string) (lrt : string) (sz : int) =
object (self) 
  inherit ['a] FC.fcache ld lrt sz as super
    
  val mutable scratchRoot = srt

  method setScratch p =
    scratchRoot <- p
    
  method getFile fname =
    try
      (* Check local dir *)
      super#getFile fname
    with FC.File_not_found _ ->
      (* Check dir of files acquired from server *)
      let fullName = Filename.concat scratchRoot fname in
      if (not (Sys.file_exists fullName)) then
        (let dest = Filename.dirname fullName in
         let dest = if (Filename.is_relative dest) then
           Filename.concat (Sys.getcwd ()) dest
         else dest in
         Filetools.ensurePath dest;
         Req.requestData fname dest;
        )
      ;
      if (not (Sys.file_exists fullName)) then
        raise (FC.File_not_found fullName)
      else
        (* success... *)
        let newF = loader fullName in
        super#addFile fname newF;
        newF

end

