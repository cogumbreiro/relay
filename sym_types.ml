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


(** Collection of the symbolic types used by the symbolic execution
    analysis (@see symstate2.ml) *)

open Stdutil
open Cil
open Pretty
open Lvals


module CLv = Cil_lvals
module L = Logging

(*********************************************************
 * Cells, values, expressions etc.
 *********************************************************)

(* symbolic cells that can be looked up in the store *)
type symAddr = 
    { saHost : aHost;
      mutable saSummary : bool; }

let hostOfAddr (addr:symAddr) =
  addr.saHost

type symExp = aExp


let compAddrApprox 
    { saHost = v1; } 
    { saHost = v2; } =      
  compare_host v1 v2     (* ignores saSummary *)


module OrderedAddr = struct
  type t = symAddr
  let compare = compAddrApprox
end

module AddrMap = Mapset.Make(OrderedAddr)

module HashAddr = struct
  type t = symAddr
  let equal x y = compAddrApprox x y == 0
  let hash x = hash_host x.saHost
end

module AddrHash = Weak.Make(HashAddr)

let compareAddrOff (a1,o1) (a2,o2) =
  let comp_a = compAddrApprox a1 a2 in
  if (comp_a <> 0) then
    comp_a 
  else
    Ciltools.compare_offset o1 o2
      
module OrderedAddrOffset = struct
  type t =  symAddr * Cil.offset
  let compare = compareAddrOff
end

module AddrOffSet = Set.Make(OrderedAddrOffset)


module OrderedOffset = struct
  type t = Cil.offset
  let compare = Ciltools.compare_offset
end

module OffsetMap = Mapset.Make(OrderedOffset)



(* symbolic value held at a symbolic address *)
type symVal =
    Vtop
  | Vbot
  | Vval of symExp                    (** probably non-pointer value *)
  | Vstruct of symVal OffsetMap.t      (** structure: indexable by offset
                                          (include array indexing here) *)
  | Vmustptr of (symAddr * Cil.offset) (** pointer to another symbolic cell *)
  | Vmayptr of (int * AddrOffSet.t)    (** pointer to multiple cells, 
                                          w/ an id to approx. linearity *)
  | Vextptr of (int * AddrOffSet.t)    (** pointer to external cell, 
                                          which may actually point to 
                                          other existing cells. Be sure 
                                          to ask coarse ptr analysis *)

let isConst e =
  match e with
    CConst _ -> true
  | _ -> false

let makeNullExp () =
  CConst (CStr "$SSBOT")

let nullVar = CLv.mkVarinfo true "$NULL" (TVoid [])

let nullAddr =
  { saHost = Lvals.CMem (Lvals.mkLval 
                           (Lvals.hostOfVar nullVar) NoOffset);
    saSummary = true; }

let isNullAddr addr =
  Lvals.compare_host nullAddr.saHost addr.saHost == 0

let isNullHost h =
  Lvals.compare_host nullAddr.saHost h == 0

(*********************************************************
 * Test / Debug code
 *********************************************************)

let d_addr addr =
  let host = hostOfAddr addr in
  let lval = (host, NoOffset) in
  let str = "lv = " ^ (string_of_lval lval) in
  let str = if addr.saSummary then str ^ "(sum)"
    else str in
  text str

let string_of_addr (addr:symAddr) : string = 
  sprint 80 (d_addr addr)

let printAddr addr =
  L.logStatus (string_of_addr addr)

let d_pointerTarg (addr, off) =
  d_offset (d_addr addr) () off

let string_of_pointer (addr, off) =
  sprint 80 (d_pointerTarg (addr, off))


let d_ptrSet header addrOffSet =
  header ++ indent 2 
    (AddrOffSet.fold
       (fun target cur ->
          cur ++ d_pointerTarg target ++ line 
       ) addrOffSet nil)

let rec d_value v =
  match v with
     Vtop -> text "$SSTOP"
   | Vbot -> text "$SSBOT"
       
   | Vval exp -> 
       dprintf "Vval: %s" (string_of_exp exp)
         
   | Vmustptr target -> 
       text "Mustptr (" ++  d_pointerTarg target ++ text ")"

   | Vmayptr (id, addrOffSet) ->
       let header = text ("Mayptr (" ^ (string_of_int id) ^ "): ") in
       d_ptrSet header addrOffSet
         
   | Vextptr (id, addrOffSet) ->
       let header = text ("Extptr (" ^ (string_of_int id) ^ "): ") in
       d_ptrSet header addrOffSet

   | Vstruct offMap ->
       let header = text "Struct:\n"in
       header ++ indent 2
         (OffsetMap.fold 
            (fun off v cur ->
               cur ++ 
                 (d_offset Pretty.nil () off) ++ text " ->" ++
                 (indent 2 (d_value v)) ++ line
            ) offMap nil
         )

let string_of_val v =
  sprint 80 (d_value v)

let prefix = " |-> "

let printVal v =
  L.logStatus (prefix ^ string_of_val v)
