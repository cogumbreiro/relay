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

module Req = Request
module BS = Backed_summary

(** Helper function to track where function summaries can be found. 
    Bookkeeping is to be stored in the given [dict]. 
    The given [sumTyp], [fkey], [tok] has just been discovered *)
let trackSum dict sumTyp (fkey, tok) =
  let oldList = try Hashtbl.find dict sumTyp with Not_found -> [] in
  let newList = (fkey, tok) :: oldList in
  Hashtbl.replace dict sumTyp newList

(* Instead of allowing summaries w/ the same fkey but different summary 
   type to be written to a different directory, may be simpler to enforce
   writing all summaries for the same fkey to the same directory...
   Still need confirmation that the summary for the different analyses
   were actually acquired though... *)

(** Helper function to find the original descriptor *)
let getDescriptor descrips sumTyp = 
  List.find (fun sumD -> sumD#sumTyp = sumTyp) descrips


(** Helper function to notify summary managers which summaries are complete,
    and where the files can be found *)
let completeSums dict =
  Hashtbl.iter 
    (fun sumT completed ->
       sumT#assumeComplete completed
    ) dict


(** Given a list of funs that need summaries. Acquire them 
    and mark them as ready for use. 
    @raise SummariesNotFound if some are not acquired -- 
    (this means no summary should be too trivial to be exist on disk...
    or you have some method of detecting such summaries on the
    peer and have the peer reply)
*)
let prepareSumms (funs : fKey list) (sums : BS.dbManagement list) =
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
  List.iter
    (fun fkey ->
       (* Iter over typs *)
       List.iter
         (fun sum ->
            match BS.discover fkey sum#sumTyp
              addToGet with
                None -> ()
              | Some tok -> trackSum availSums sum (fkey, tok)
         ) sums
    ) funs;
  (* DL the rest and track *)
  if (!toGet <> []) then begin
    let acquired = Req.requestSumm !toGet in
    List.iter 
      (fun (fkey, sumTyp, tok) -> 
         let sum = getDescriptor sums sumTyp in
         trackSum availSums sum (fkey, tok)
      )
      acquired;
  end
  else
    ()
  ;
  completeSums availSums


(** Given a list of functions (maybe the functions in the current SCC),
    check the disk to see if the summaries are already there. If they
    are, great! Use them. If they aren't, oh well. Just let the 
    analysis run and recompute them *)

(*
let discoverSumms funs =
  let local = List.fold_left 
    (fun curList fkey ->
       match BS.discover fkey (fun f -> None) with
         None -> curList
       | Some tok -> (fkey, tok) :: curList 
    ) [] funs in
  (* Inform each of the summary structures that the summaries are prepared *)
  RS.sum#assumeComplete local; 
  (* TODO make this more uniform... actually, have each of the summary
     classes contain their own BS.discover method, but have them only discover
     their own summary? *)
  SS.sum#assumeComplete local
*)
