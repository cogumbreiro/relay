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

  
let rec scopeValue op (v:symVal) = 
  match v with
    Vtop
  | Vbot ->
      ()

  | Vval e ->
      Scope.AL.scopeExp op e

  | Vstruct oMap ->
      OffsetMap.iter 
        (fun _ v ->
           scopeValue op v) oMap

  | Vmustptr ( {saHost = h; } , _ ) ->
      Scope.AL.scopeHost op h
        
  | Vmayptr (_, aoSet) ->
      AddrOffSet.iter 
        (fun ( {saHost = h; }, _) ->
           Scope.AL.scopeHost op h) aoSet
        
  | Vextptr (_, aoMap) ->
      AddrOffSet.iter 
        (fun ( {saHost = h; }, _) ->
           Scope.AL.scopeHost op h) aoMap




(*********************************************************
 * Test / Debug code
 *********************************************************)

let string_of_addr (addr:symAddr) : string = 
  let host = hostOfAddr addr in
  let lval = (host, NoOffset) in
  ("Cell :: lval = " ^ (string_of_lval lval) ^ " ; isSum = " ^
     (string_of_bool addr.saSummary))

    
let printAddr addr =
  L.logStatus (string_of_addr addr)

let string_of_pointer (addr,off) =
  (string_of_addr addr) ^
    (sprint 80 (d_offset Pretty.nil () off))


(* Should probably use a pretty printer to handle nested structs *)
let prefix = "Value :: "

let rec printVal v =
  let buff = Buffer.create 16 in
  Buffer.add_string buff prefix;
  (match v with
     Vtop -> Buffer.add_string buff "$SSTOP"
   | Vbot -> Buffer.add_string buff "$SSBOT"
       
   | Vval exp -> 
       Buffer.add_string buff ("Vval: \n\t" ^ (string_of_exp exp))

   | Vmustptr target -> 
       Buffer.add_string buff ("Mustptr: \n\t" ^ (string_of_pointer target))

   | Vmayptr (id, addrOffSet) ->
       Buffer.add_string buff ("Mayptr (" ^ (string_of_int id) ^ "): \n");
       AddrOffSet.iter
         (fun target ->
            Buffer.add_string buff ("\t" ^ (string_of_pointer target) ^ "\n")
         ) addrOffSet

   | Vextptr (id, addrOffSet) ->
       Buffer.add_string buff ("Extptr (" ^ (string_of_int id) ^ "): \n");
       AddrOffSet.iter
         (fun target ->
            Buffer.add_string buff ("\t" ^ (string_of_pointer target) ^ "\n")
         ) addrOffSet

   | Vstruct offMap ->
       Buffer.add_string buff "Struct: \n";
       OffsetMap.iter 
         (fun off v ->
            Buffer.add_string buff ("\t" ^ 
                            (sprint 80 (d_offset Pretty.nil () off)) ^ "=\n");
            printVal v;
         ) offMap
  );
  Buffer.add_string buff "\n";
  L.logStatus (Buffer.contents buff)
