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


(** Simple Set/Map/Hashtbl modules for storing function keys. *)

open Cil

(************************************************************
   Function data structures 
 ************************************************************)

(** Function ID/key *)
type fKey = int

let funToKey (f:fundec) : fKey =
  f.svar.vid
let compareFKey a b = a - b
let dummyFKey = -1

let string_of_fkey = string_of_int

let fkey_of_string = int_of_string

let getFkey cilF =
  cilF.svar.vid

(**** Basic comparators / data structures ****)

module OrderedInt = 
  struct
    type t = int
    let compare a b = a - b
  end

module OrderedFKeys = OrderedInt

(* Map from integers to --> 'a *)
module IntMap = Map.Make(OrderedInt)
  
(* Set of integers *)
module IntSet = Sparsebitv

(* faze this out to force users to use the correct funID for cgs?? *)
module FKMap = IntMap

module FKSet = IntSet

(*** Name + type string data structures *)

(** Function type in string form *)
type fTS = string

(** Function (name, type) pair *)
type fNT = (string * fTS)

let string_of_fNT (fn,ft) =
  fn ^ " : " ^ ft 

let compareNT (n1,t1) (n2,t2) =
  let ncomp = Pervasives.compare n1 n2 in
  if (ncomp == 0) then
    Pervasives.compare t1 t2
  else
    ncomp
      
module OrderedFNTs =
  struct
    type t = fNT
    let compare = compareNT
  end

module HashedFNTs = 
  struct
    type t = fNT
    let equal x y = compareNT x y == 0
    let hash = Hashtbl.hash
  end

(* For map from (function name, type string) to 'a *)
module FNTMap = Map.Make(OrderedFNTs)

module FNTHash = Hashtbl.Make(HashedFNTs)

(* Set of (function name, types) *)
module FNTSet = Set.Make(OrderedFNTs)

