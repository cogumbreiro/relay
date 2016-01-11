(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)


(** Map with Set-like operations (union, isect, subset).  
    Based on the Ocaml std library implementations.
    Specialized by Jan Voung *)


module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val mem:  key -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val mapk: (key -> key) -> 'a t -> 'a t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    (* New operation *)
    val addComb: ('a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t  

    (* Borrowed / Adapted from Set *)
    val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val inter: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val diff: 'a t -> 'a t -> 'a t
    val subset: ('a -> 'a -> int option) -> 'a t -> 'a t -> bool
    val subsetWithKey: (key -> 'a -> 'a -> int option) -> 'a t -> 'a t -> bool
    val min_binding: 'a t -> key * 'a
    val max_binding: 'a t -> key * 'a
    val choose: 'a t -> key * 'a
    val cardinal: 'a t -> int
    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    val exists: (key -> 'a -> bool) -> 'a t -> bool


  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Mapset.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Mapset.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Mapset.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Mapset.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, v, data, r, h)
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _) -> (x, d)
      | Node(l, _, _, _, _) -> min_binding l

    let rec max_binding = function
        Empty -> raise Not_found
      | Node(_, x, d, Empty, _) -> (x, d)
      | Node(_, _, _, r, _) -> max_binding r

    let rec remove_min_binding = function
        Empty -> invalid_arg "Mapset.remove_min_elt"
      | Node(Empty, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

    let rec mapi f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

    let rec mapk f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(mapk f l, f v, d, mapk f r, h)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)


    (* New operations *)

    (** Adds a mapping from (x to data). If a mapping already exists,
        (x to oldD) replace w/ the result of (combiner data oldD) *)
    let rec addComb combiner x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, v, combiner d data, r, h)
          else if c < 0 then
            bal (addComb combiner x data l) v d r
          else
            bal l v d (addComb combiner x data r)


    (* Borrowed / Adapted from Set *)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. There should be no binding
       of (x,d) in either of the l and r trees *)
              
    let rec join l x d r =
      match (l, r) with
        (Empty, _) -> add x d r
      | (_, Empty) -> add x d l
      | (Node(ll, lx, ld, lr, lh), Node(rl, rx, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lx ld (join lr x d r) else
            if rh > lh + 2 then bal (join l x d rl) rx rd rr else
              create l x d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)
                
    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> 
          let mx2, md2 = min_binding t2 in
          join t1 mx2 md2 (remove_min_binding t2)


    (* Splitting.  split x m returns a triple (l, md, r) where
        - l is the set of elements of m with key < x
        - r is the set of elements of m with key > x
        - md is None if m contains no mapping with key = x,
          or Some(d) where d is the current value bound to x. *)
              
    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, Some(d), r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in 
            (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in 
            (join l v d lr, pres, rr)
              
              
    let rec union combiner m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node(l1, k1, d1, r1, h1), Node(l2, k2, d2, r2, h2)) ->
          if h1 >= h2 then
            if h2 = 1 then addComb combiner k2 d2 m1 else begin
              let (l2, k1ValInM2, r2) = split k1 m2 in
              let d' = match k1ValInM2 with
                  None -> d1
                | Some(d) -> combiner d d1
              in
              join (union combiner l1 l2) k1 d' (union combiner r1 r2)
            end
          else
            if h1 = 1 then addComb combiner k1 d1 m2 else begin
              let (l1, k2ValInM1, r1) = split k2 m1 in
              let d' = match k2ValInM1 with
                  None -> d2
                | Some (d) -> combiner d d2
              in
              join (union combiner l1 l2) k2 d' (union combiner r1 r2)
            end

    let rec inter combiner m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, v1, d1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              concat (inter combiner l1 l2) (inter combiner r1 r2)
          | (l2, Some(d), r2) ->
              let d' = combiner d d1 in
              join (inter combiner l1 l2) v1 d' (inter combiner r1 r2)

    let rec diff m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> t1
      | (Node(l1, v1, d1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              join (diff l1 l2) v1 d1 (diff r1 r2)
          | (l2, Some(_), r2) ->   (* TODO should check if data eq or not *)
              concat (diff l1 l2) (diff r1 r2)


    let rec subset cmp m1 m2 =
      match (m1, m2) with
        Empty, _ ->
          true
      | _, Empty ->
          false
      | Node (l1, k1, d1, r1, _), (Node (l2, k2, d2, r2, _) as t2) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then begin
            (* Compare values first *)
            match cmp d1 d2 with
              None -> 
                false
            | Some (i) when i > 0 ->
                false
            | _ ->
                subset cmp l1 l2 && 
                  subset cmp r1 r2
          end
          else if c < 0 then
            subset cmp (Node (l1, k1, d1, Empty, 0)) l2 && 
              subset cmp r1 t2
          else
            subset cmp (Node (Empty, k1, d1, r1, 0)) r2 && 
              subset cmp l1 t2

    let rec subsetWithKey cmp m1 m2 =
      match (m1, m2) with
        Empty, _ ->
          true
      | _, Empty ->
          false
      | Node (l1, k1, d1, r1, _), (Node (l2, k2, d2, r2, _) as t2) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then begin
            (* Compare values first *)
            match cmp k1 d1 d2 with
              None -> 
                false
            | Some (i) when i > 0 ->
                false
            | _ ->
                subsetWithKey cmp l1 l2 && 
                  subsetWithKey cmp r1 r2
          end
          else if c < 0 then
            subsetWithKey cmp (Node (l1, k1, d1, Empty, 0)) l2 && 
              subsetWithKey cmp r1 t2
          else
            subsetWithKey cmp (Node (Empty, k1, d1, r1, 0)) r2 && 
              subsetWithKey cmp l1 t2

    let choose = function
        Empty -> raise Not_found
      | Node(_, x, d, _, _) -> (x, d)

    let rec cardinal = function
        Empty -> 0
      | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

    let rec for_all p = function
        Empty -> true
      | Node(l, k, v, r, _) -> p k v && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node(l, k, v, r, _) -> p k v || exists p l || exists p r


end


