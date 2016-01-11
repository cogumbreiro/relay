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
(** Function summaries that track the symbolic return value. 
    TODO: see if we need to track any other information to replace
    the modsummaries? (@see modsummary.ml and @see racesummary.ml)
*)

open Fstructs
open Sym_types
open Callg

module L = Logging
module Stat = Mystats
module BS = Backed_summary

(*********************************************************
 * Function return value summaries
 *********************************************************)

type sumval = symVal

module SymSum = struct

  type t = sumval

  type simpleSum = t

  let simplify sum = sum

  let desimplify simp = simp

  let initVal = Vbot

  let unknownSummary = Vtop

end

module SS = Safer_sum.Make (SymSum)

type sumdb = SS.data

class data = SS.data

let sum = new data (BS.makeSumType "ss")

let _ = BS.registerType sum

(*********************************************************
 * Test / Debug code
 *********************************************************)

let printSummary sum fKey = 
  let sval = sum#find fKey in
  L.logStatus "SS Sum:";
  printVal sval

