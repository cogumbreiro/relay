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


(** Additional operations related to Cil offsets *)

open Cil

module L = Logging
module Lv = Cil_lvals


exception UnknownOffset

(** Given a type and the number of bits of offset, return the field offset. 
    May raise UnknownOffset if the bits don't make sense *)
let rec bitsToOffset (t:typ) (bits:int) : offset = 
  if (bits < 0) then
    (* TODO Need a way to handle offsets that are "behind" the named struct *)
    NoOffset
  else 
    let t = Lv.unrollTypeNoAttrs t in
    (* first see if the bits offset is a multiple of the sizeof base 
     object (may be striding an array) *)
    let baseSize = bitsSizeOf t in
    let bits = bits mod baseSize in (* assume baseSize != 0!!! *)
    if (bits == 0) then
      NoOffset
    else match t with
      TComp (ci, _) ->
        (* Check if bits offset matches the offset of one of the fields *)
        let fi = List.find
          (fun fi ->
             let base, width = bitsOffset t (Field (fi, NoOffset)) in
             (bits >= base) && (bits < base + width)
          ) ci.cfields in
        let base, width = bitsOffset t (Field (fi, NoOffset)) in
        Field (fi, 
               if (bits == base) then NoOffset
             else bitsToOffset fi.ftype (bits - base)
              )
    | t ->
        (* otherwise, not sure what to do... *)
        L.logError "bitsToOffset: can't compute field on non-struct\n";
        raise UnknownOffset
