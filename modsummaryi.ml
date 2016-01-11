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

open Summary_keys
open Lvals

(** Basic Interface for mod summaries (track what externally visible 
    writes occur in a function). *)

class type modSum = object 

  method getMods : sumKey -> (aLval * Scope.scope ) list

  method getGlobalMods : sumKey -> LvalSet.t
  method getLocalMods : sumKey -> (aLval * Scope.scope) list

  method iterMods : sumKey -> (aLval * Scope.scope -> unit) -> unit

  method foldMods : 'a.  sumKey -> (aLval * Scope.scope -> 'a -> 'a) -> 'a -> 'a

  method evictSummaries : unit

end

let init_error_msg =
  "absModSumm: you forgot to choose a real mod sum (init symex)"

class absModSumm : modSum = object


  method getMods fkey =
    failwith init_error_msg

  method getGlobalMods fkey =
    failwith init_error_msg

  method getLocalMods fkey =
    failwith init_error_msg

  method iterMods fkey foo =
    failwith init_error_msg

  method foldMods : 'a.  sumKey -> (aLval * Scope.scope -> 'a -> 'a) -> 'a -> 'a =
    fun fkey foo acc ->
      failwith init_error_msg

  method evictSummaries =
    failwith init_error_msg

end

exception BottomSummary

