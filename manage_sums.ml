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


(** Module responsible for managing the {i acquisition } / discovery of 
    function summaries for various analyses. *)


open Fstructs
open Summary_keys
open Callg
open Scc_cg

module BS = Backed_summary

(** Helper function to track where function summaries can be found. 
    Bookkeeping is to be stored in the given [dict]. 
    The given [sumTyp], [key], [tok] has just been discovered *)
let trackSum dict sumTyp (key, tok) : unit =
  let oldList = try Hashtbl.find dict sumTyp with Not_found -> [] in
  let newList = (key, tok) :: oldList in
  Hashtbl.replace dict sumTyp newList

(* Instead of allowing summaries w/ the same key but different summary 
   type to be written to a different directory, may be simpler to enforce
   writing all summaries for the same key to the same directory...
   Still need confirmation that the summary for the different analyses
   were actually acquired though... *)

(** Helper function to find the original descriptor *)
let getDescriptor descrips sumTyp = 
  List.find (fun sumD -> sumD#sumTyp = sumTyp) descrips


(** Helper function to notify summary managers which summaries are complete,
    and where the files can be found *)
let completeSums dict =
  Hashtbl.iter (fun sumT completed -> sumT#assumeComplete completed) dict


(** Know that at least funs * sums are needed, but get any 
    other dependencies that may be needed too. *)
let getDependencies funs sums : ((sumKey * BS.dbManagement), unit) Hashtbl.t = 
  let dependents = Hashtbl.create 17 in
  List.iter
    (fun sum ->
       let moreDeps = sum#getDependentKeys funs in 
       List.iter (fun (k, s) -> Hashtbl.replace dependents (k, s) ()) moreDeps
    ) sums;
  dependents

(** Given a list of funs that need summaries. Acquire them 
    and mark them as ready for use. 
    @raise SummariesNotFound if some are not acquired -- 
    (this means no summary should be too trivial to be exist on disk...
    or you have some method of detecting such summaries on the
    peer and have the peer reply)
*)
let prepareSumms (funs : sumKey list) (sums : BS.dbManagement list) =
  (* Identify list of func sums that need to be downloaded *)
  let toGet = ref [] in
  let addToGet fkey styp =
    toGet := (fkey, styp) :: !toGet;
    None
  in
  (* Identify list of func sums available locally or after acquiring, 
     and where they're stored, separated by func summary type *)
  let availSums = Hashtbl.create (List.length sums * 3) in
  (* Iter over funs to see what's available locally, 
     and what needs to be acquired *)
  let depends = getDependencies funs sums in
  Hashtbl.iter
    (fun (fkey, sum) () ->
       match BS.discover fkey sum#sumTyp addToGet with
         None -> ()
       | Some tok -> trackSum availSums sum (fkey, tok)
    ) depends;
  (* DL the rest and track *)
  if (!toGet <> []) then begin
    let acquired = Request.requestSumm !toGet in
    List.iter 
      (fun (fkey, sumTyp, tok) -> 
         let sum = getDescriptor sums sumTyp in
         trackSum availSums sum (fkey, tok)
      ) acquired;
  end
  else () ;
  completeSums availSums

(******************** Higher level versions ******************)


let sumKeysOfScc scc curList =
  FSet.fold (fun f curList -> f :: curList) scc.scc_nodes curList
    
let sumKeysOfCallees sccCG scc curList = 
  IntSet.fold
    (fun neighSCCID curList ->
       let neighSCC = IntMap.find neighSCCID sccCG in
       sumKeysOfScc neighSCC curList
    ) scc.scc_callees curList
    
let prepareSCCCalleeSums sccCG scc sumTyps =
  let callees = sumKeysOfCallees sccCG scc [] in
  prepareSumms callees sumTyps

let prepareSCCSums scc sumTyps =
  let ownFuns = sumKeysOfScc scc [] in
  prepareSumms ownFuns sumTyps
