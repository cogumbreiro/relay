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


(** A pair of sets representing relative changes. The two sets are the
    "plus" and the "minus" set. The "plus" set contains things that
    MUST have been changed, and the "minus" set contains things that
    MAY have been changed. The two sets are kept disjoint, so adding to
    the MUST set means removing from the MAY set as well.

    Historically, the names plus and minus (for MUST and MAY), 
    come from its original use for locked and unlocked sets *)


(** Input module for the Make functor *)
module type RelType = sig
  
  type key
    
  type value

  val compareK : key -> key -> int

  val compareV : value -> value -> int option (* None if incomparable *)

  val combineV : value -> value -> value

  val equalV : value -> value -> bool (* Can be different from compareV... *)

  val hashK : key -> int (* if (equalV v1 v2) then hashK v1 == hashK v2 *)

  val uniqueK : key -> key (* canonicize key *)

(* canoncized key, but don't need to canonicize value? *)

end


(** Output module for the Make functor *)
module type S = sig

  type key

  type value

  (** An individual set in the relative pair. Should not tie to Mapset? *)
  module S : Mapset.S with type key = key

  (** Fold over one of the sets. Just use the S module or 
      only allow fold specifically? *)
  val fold : (key -> value -> 'a -> 'a) -> value S.t -> 'a -> 'a

  (** Type for relative pairs *)
  type relSet 

  val inter : relSet -> relSet -> relSet

  val subset : relSet -> relSet -> bool 

  val doPlus : relSet -> key -> value -> relSet

  val doMinus : relSet -> key -> value -> relSet

  val getPlus : relSet -> value S.t

  val getMinus : relSet -> value S.t 


  (*********** Hash to single copies of these sets *************)

  val hash : relSet -> int

  val equal : relSet -> relSet -> bool

  module HashedRS : Hashtbl.HashedType with type t = relSet
    
  val unique : relSet ->  relSet

  (****** special singleton instances *******)

  val empty : relSet

  val is_empty : relSet -> bool

  (****** make new instances from pairs ******)
  val composeNew : value S.t -> value S.t -> relSet

  (****** simplification / simplified operations ******)
  val ditchMinus : relSet -> relSet

  val ditchPlus : relSet -> relSet

  val emptyPlus : relSet -> bool

  val emptyMinus : relSet -> bool

end



(** More of a Map / Set. It's a set in that there's only one instance of 
    the key, but it's a map in that keys map to values. 
    The values get merged when there's a collision          
*)
module Make (T:RelType) = struct

  type key = T.key

  type value = T.value

  module S = Mapset.Make (struct
                            type t = T.key
                            let compare = T.compareK
                          end)
 
  let fold = S.fold

  type relSet = {
    plus : T.value S.t;   (* What's been added *)
    minus : T.value S.t;  (* What's been removed *)
  }

  (* Assumes plus is MUST and minus is MAY *)
  let inter a b = 
    if (a == b) then a
    else { plus = S.inter T.combineV a.plus b.plus; 
           minus = S.union T.combineV a.minus b.minus; }

  let subset a b = 
    (* reversed a and b for the PLUS... by subset, we mean more restricting *)
    ((S.subset T.compareV b.plus a.plus) &&  
       (S.subset T.compareV a.minus b.minus))

  let doPlus s k v =
    { plus = S.add k v s.plus;        (* replaces if already existed *)
      minus = S.remove k s.minus;
    }

  let doMinus s k v =
    { plus = S.remove k s.plus;
      minus = S.add k v s.minus;      (* replaces *)
    }

  let getPlus s = 
    s.plus

  let getMinus s =
    s.minus

  (*********** Hash to single copies of these sets *************)

  (** TODO: factor out to a general MapSet hash... *)

  (** Hash function for S that only samples it *)
  let hash (x:relSet) =
    let p_hash = if (S.is_empty x.plus) then
      0
    else
      (* Hash on size, and a sample of possibly 3 keys *)
      let size_p_h = Hashtbl.hash (S.cardinal x.plus) in
      let min_k_p_h = T.hashK (fst (S.min_binding x.plus)) in
      let med_k_p_h = T.hashK (fst (S.choose x.plus)) in
      let max_k_p_h = T.hashK (fst (S.max_binding x.plus)) in
      size_p_h lxor min_k_p_h lxor med_k_p_h lxor max_k_p_h
    in  
    if (S.is_empty x.minus) then 
      p_hash
    else 
      let size_m_h = Hashtbl.hash (S.cardinal x.minus) in
      let min_k_m_h = T.hashK (fst (S.min_binding x.minus)) in
      let med_k_m_h = T.hashK (fst (S.choose x.minus)) in
      let max_k_m_h = T.hashK (fst (S.max_binding x.minus)) in
      p_hash lxor size_m_h lxor min_k_m_h lxor med_k_m_h lxor max_k_m_h 
        
  
  let equal (x:relSet) (y:relSet) : bool=
    (x == y) ||
      ((S.equal T.equalV x.plus y.plus) && 
         (S.equal T.equalV x.minus y.minus))


  module HashedRS = 
  struct
    type t = relSet
    let equal = equal
    let hash = hash
  end
    
  module WH = Weak.Make (HashedRS)
    
  let cache = WH.create 237
    
  let unique (possiblyNew : relSet) : relSet =
    try 
      WH.find cache possiblyNew
    with Not_found ->
      let uniq = { plus = S.mapk (fun k -> T.uniqueK k) possiblyNew.plus;
                   minus = S.mapk (fun k -> T.uniqueK k) possiblyNew.minus; } in
      WH.add cache uniq;
      uniq

  (****** special singleton instances *******)

  let empty =
    { plus = S.empty; 
      minus = S.empty; }

  let is_empty rs =
    S.is_empty rs.plus &&
      S.is_empty rs.minus

  let composeNew p m =
    { plus = p;
      minus = m;
    }

  (****** simplification operations ******)
  let ditchMinus rs =
    { rs with minus = S.empty; }

  let ditchPlus rs =
    { rs with plus = S.empty; }

  let emptyPlus rs : bool =
    S.is_empty rs.plus

  let emptyMinus rs : bool =
    S.is_empty rs.minus

end


(************************************************************
   Annotate scope of elements of sets (if they are lvals)
 ************************************************************)


(* Move this to another module? *)


