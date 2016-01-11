(*
 *
 * Copyright (c) 2001-2002, 
 *  John Kodumal        <jkodumal@eecs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
exception Bad_find

module type UNIONFIND = sig

  type 'a uref

  val find : 'a uref -> 'a  uref
          
  val uref : 'a -> 'a uref 

  val deref : 'a uref -> 'a
   
  val equal : 'a uref * 'a uref -> bool

  val update : 'a uref * 'a -> unit
   
  val unify : ('a uref * 'a uref -> 'a) -> 'a uref * 'a uref -> unit
   
  val union : 'a uref * 'a uref -> 'a

  (* Debug stuff included =( *)
  type step

  val whyLinked : 'a uref -> 'a uref -> step list

end

(* TODO: have a version where the parameter is separate from the
   ref, and the ref is really an integer? *)



(************************************************************
  Non-tracking version
************************************************************)


module NotTracked  : UNIONFIND = struct
  
  type 'a urefC =
      Ecr of 'a * int
    | Link of 'a uref
  and 'a uref = 'a urefC ref
      
  let rec find p = 
    match !p with
      Ecr _ -> p
    | Link p' ->
	    let p'' = find p' in 
        p := Link p''; 
        p''
          
  let uref x = ref (Ecr(x,0))

  let equal (p,p') = (find p == find p')
    
  let deref p = 
    match ! (find p) with 
      Ecr (x,_) -> x
    | _ -> raise Bad_find

  let update (p,x) = 
    let p' = find p in
    match !p' with
      Ecr (_,rank) -> p' := Ecr(x,rank)
    | _ -> raise Bad_find

  let unify f (p,q) : unit = 
    let p',q' = find p, find q in
    match (!p',!q') with 
      (Ecr(px,pr), Ecr(qx,qr)) ->
	    if (p' == q') then
	      ()         (* changed to not run f (px, qx) yet *)
	    else let x = f(p,q) in (* changed to pass original nodes *)
        if pr == qr then
	      (q' := Ecr(x,qr+1); 
           p' := Link q';)
	    else if pr < qr then
	      (q' := Ecr(x,qr); 
           p' := Link q';)
	    else (* pr > qr *)
	      (p' := Ecr(x,pr); 
           q' := Link p';)
    | _ -> raise Bad_find

  let union (p,q) : 'a = 
    let p',q' = find p, find q in
    match (!p',!q') with 
      (Ecr(px,pr),Ecr(qx,qr)) ->
	    if (p' == q') then 
	      px
	    else if pr == qr then
	      (q' := Ecr(qx, qr+1); 
           p' := Link q';
           qx)
	    else if pr < qr then
	      (p' := Link q';
           qx)
	    else (* pr > qr *)
	      (q' := Link p';
           px)
    | _ -> raise Bad_find

  type step = unit

  let whyLinked _ = failwith "Not available"

end

(******* Version that tracks when links are made and why **)

module type Trackable = sig

  type step

  val curStep : step ref 
    (* Use this ref to hide a parameter that is not passed when
       the tracked mode is not used *)

end

module Tracked (T:Trackable) : UNIONFIND with type step = T.step = struct

  type step = T.step

  type 'a urefC =
      Ecr of 'a * int
    | Link of ('a uref * step)
  and 'a uref = 'a urefC ref

  let rec find p = 
    match !p with
      Ecr _ -> p
    | Link (p', step) ->
	    let p'' = find p' in 
        (* No path compression 
           p := Link p'';
        *)
        p''
          
  let uref x = ref (Ecr(x,0))

  let equal (p,p') = (find p == find p')
    
  let deref p = 
    match ! (find p) with 
      Ecr (x,_) -> x
    | _ -> raise Bad_find

  let update (p,x) = 
    let p' = find p in
    match !p' with
      Ecr (_,rank) -> p' := Ecr(x,rank)
    | _ -> raise Bad_find

  let unify f (p,q) : unit = 
    let p',q' = find p, find q in
	if (p' == q') then () (* already unified *)
    else match (!p',!q') with 
      (Ecr(px,pr), Ecr(qx,qr)) ->
	    let x = f(p,q) in (* get new data *)
        let curStep = !T.curStep in
        if pr == qr then
	      (q' := Ecr(x,qr+1); 
           p' := Link (q', curStep);)
	    else if pr < qr then
	      (q' := Ecr(x,qr); 
           p' := Link (q', curStep);)
	    else (* pr > qr *)
	      (p' := Ecr(x,pr);
           q' := Link (p', curStep);)
    | _ -> raise Bad_find

  let union (p,q) : 'a = 
    let p',q' = find p, find q in
    match (!p',!q') with 
      (Ecr(px,pr),Ecr(qx,qr)) ->
    	if (p' == q') then px (* already unified *) 
        else let curStep = !T.curStep in
	    if pr == qr then
	      (q' := Ecr(qx, qr+1); 
           p' := Link (q', curStep);
           qx)
	    else if pr < qr then
	      (p' := Link (q', curStep);
           qx)
	    else (* pr > qr *)
	      (q' := Link (p', curStep);
           px)
    | _ -> raise Bad_find

  let whyLinked p q : step list =
    let rec foldUp cur path = 
      match !cur with
        Ecr _ -> (cur, path)
      | Link (p', step) ->
	      foldUp p' (step :: path)
    in
    let _, pToTop = foldUp p [] in
    let _, qToTop = foldUp q [] in
    (* reverse one of the paths and concat, so the "top" parts meet *)
    List.rev_append pToTop qToTop

end

(** Version where references are known to be int -- more dangerous... *)

module type Typed = sig 
  type t
end

(* Type signature currently not compatible w/ other Uf signatures *)
module IntRefs (T:Typed) = struct

  module Arr = GrowArray
  
  type uref = int

  type urefC =
      Ecr of T.t * int
    | Link of uref

  (* Dummy ref is a self-loop... watch out! *)
  let dummyRef i = Link i

  let internalData = Arr.make 128 (Arr.Susp dummyRef)

  let nextIndex = ref 0

  let getNextIndex () : uref =
    let cur = !nextIndex in
    incr nextIndex;
    cur

  let internalDeref r =
    Arr.getg internalData r
  
  let internalSet r data =
    Arr.setg internalData r data

  let uref x = 
    let data = (Ecr(x,0)) in
    let r = getNextIndex () in
    internalSet r data;
    r

  let rec find p = 
    match internalDeref p with
      Ecr _ -> p
    | Link p' ->
	    let p'' = find p' in
        internalSet p (Link p''); (* path compress *) 
        p''

  let equal (p,p') = (find p == find p')
    
  let deref p = 
    match internalDeref (find p) with 
      Ecr (x,_) -> x
    | _ -> raise Bad_find

  let update (p,x) = 
    let p' = find p in
    match internalDeref p' with
      Ecr (_,rank) -> 
        internalSet p' (Ecr(x,rank))
    | _ -> raise Bad_find

  let unify f (p,q) : unit = 
    let p', q' = find p, find q in
    match (internalDeref p', internalDeref q') with 
      (Ecr(px,pr), Ecr(qx,qr)) ->
	    if (p' == q') then
	      ()         (* changed to not run f (px, qx) yet *)
	    else 
          let x = f(p,q) in (* changed to pass original nodes *)
          if pr == qr then
	        (internalSet q' (Ecr(x,qr+1)); 
             internalSet p' (Link q');)
	      else if pr < qr then
	        (internalSet q' (Ecr(x,qr)); 
             internalSet p' (Link q');)
	      else (* pr > qr *)
	        (internalSet p' (Ecr(x,pr)); 
             internalSet q' (Link p');)
    | _ -> raise Bad_find

  let union (p,q) : 'a = 
    let p',q' = find p, find q in
    match (internalDeref p', internalDeref q') with 
      (Ecr(px,pr),Ecr(qx,qr)) ->
	    if (p' == q') then 
	      px
	    else if pr == qr then
	      (internalSet q' (Ecr(qx, qr+1)); 
           internalSet p' (Link q');
           qx)
	    else if pr < qr then
	      (internalSet p' (Link q');
           qx)
	    else (* pr > qr *)
	      (internalSet q' (Link p');
           px)
    | _ -> raise Bad_find

  type step = unit

  let whyLinked (r1:uref) (r2:uref) = failwith "Not available"

end
