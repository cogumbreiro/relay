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
    val is_singleton: 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t 
    (* Like add but when keys are the same and data not, combines data *)
    val addComb: ('a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t  

    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val mem:  key -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val mapk: (key -> key) -> 'a t -> 'a t

    val mapCh: ('a -> 'a) -> 'a t -> 'a t
    val mapChI: (key -> 'a -> 'a) -> 'a t -> 'a t
    val mapChK: (key -> key) -> 'a t -> 'a t

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (* Like compare, but creates default values for unbound keys *)
    val compareC: ('a -> 'a -> int) -> (key -> 'a) -> 'a t -> 'a t -> int

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (* Like equal, but creates default values for unbound keys *)
    val equalC: ('a -> 'a -> bool) -> (key -> 'a) -> 'a t -> 'a t -> bool

    val min_binding: 'a t -> key * 'a
    val max_binding: 'a t -> key * 'a
    val remove_min_binding: 'a t -> key * 'a * 'a t


    val choose: 'a t -> key * 'a
    val cardinal: 'a t -> int
    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    val exists: (key -> 'a -> bool) -> 'a t -> bool

    (* Adapted from set to take a combiner function for data w/ same key  *)
    val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val inter: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val diff: 'a t -> 'a t -> 'a t

    (* Like intersect but drops elements if the combiner can't come up
       with something *)
    val interO: ('a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

    (* New operation -- comb func should return differences in 
       arg1 vs arg2, or return arg2 if there is no difference *)
    val diffComb: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val diffEq: ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
    val diffBoth: ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t

    (* Like above operations but creates default values when key is
       bound in one set, but not the other *)
    val unionC: ('a -> 'a -> 'a) -> (key -> 'a) -> 'a t -> 'a t -> 'a t
    val unionC2: ('a -> 'a -> 'a) -> (key -> 'a) -> (key -> 'a) -> 'a t -> 'a t -> 'a t

    val unionCK: (key -> 'a -> 'a -> 'a) -> (key -> 'a) -> 'a t -> 'a t -> 'a t

    (* add a hook to collect keys for interesting changes *)
    val unionCN : ('a -> 'a -> 'a * bool) -> (key -> 'a) -> 'a t -> 'a t -> ('a t * key list)
      
    (* Subset's value comparer returns int option so that usual
       comparison functions (that return int) can be used... *)
    val subset: ('a -> 'a -> int option) -> 'a t -> 'a t -> bool
    val subsetWithKey: (key -> 'a -> 'a -> int option) -> 'a t -> 'a t -> bool
    (* Like subset, but where the cmp returns true if val is subset *)
    val subsetB: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val subsetBK: (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool

    (* Like subsetB, but creates default values for unbound keys *)
    val subsetC: ('a -> 'a -> bool) -> (key -> 'a) -> 'a t -> 'a t -> bool
    val subsetC2: ('a -> 'a -> bool) -> (key -> 'a) -> (key -> 'a) -> 'a t -> 'a t -> bool


    val sampleHash: (key -> 'a -> int) -> 'a t -> int

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

    let is_singleton = function 
        Empty -> false 
      | Node (Empty, _, _, Empty, _) -> true
      | _ -> false


    (* Old version *)
(*
    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then  Node(l, v, data, r, h)
          else if c < 0 then 
            bal (add x data l) v d r
          else 
            bal l v d (add x data r)
*)

    (* New version of add that checks if data is new *)
    let rec add x data m = 
      match m with
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then 
            if d == data then m
            else Node(l, v, data, r, h)
          else if c < 0 then 
            let newl = add x data l in
            if l == newl then m else bal newl v d r
          else 
            let newr = add x data r in 
            if newr == r then m else bal l v d newr

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
      | Node(Empty, x, d, r, _) -> (x, d, r)
      | Node(l, x, d, r, _) -> 
          let x1, d1, newl = remove_min_binding l in
          x1, d1, bal newl x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d, newt2) = remove_min_binding t2 in
          bal t1 x d newt2

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

    (** Watch out! It might be wrong to actually use this 
        _IF_ it disrupts the ordering of keys!
        Currently only used to hash-cons the keys (so it's ok) *)
    let rec mapk f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(mapk f l, f v, d, mapk f r, h)

    (** Only replace nodes if they change *)
    let rec mapCh f m =
      match m with
        Empty               -> m
      | Node(l, v, d, r, h) -> 
          let newD = f d in
          let newL = mapCh f l in
          let newR = mapCh f r in
          if d == newD && l == newL && r == newR then m
          else Node(newL, v, newD, newR, h)

    let rec mapChI f m =
      match m with
        Empty               -> m
      | Node(l, v, d, r, h) -> 
          let newD = f v d in
          let newL = mapChI f l in
          let newR = mapChI f r in
          if d == newD && l == newL && r == newR then m
          else Node(newL, v, newD, newR, h)

    let rec mapChK f m = 
      match m with
        Empty               -> m
      | Node(l, v, d, r, h) -> 
          let newV = f v in
          let newL = mapChK f l in
          let newR = mapChK f r in
          if v == newV && l == newL && r == newR then m
          else Node(newL, newV, d, newR, h)


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

    let rec compare cmp m1 m2 =
      compare_aux cmp (cons_enum m1 End) (cons_enum m2 End)

    and compare_aux cmp e1 e2 =
      match (e1, e2) with
        (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
          let c = Ord.compare v1 v2 in
          if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
              compare_aux cmp (cons_enum r1 e1) (cons_enum r2 e2)

    let rec compareC cmp def m1 m2 =
      compare_auxC cmp def (cons_enum m1 End) (cons_enum m2 End)

    and compare_auxC cmp def oldE1 oldE2 =
      match (oldE1, oldE2) with
        (End, End) -> 0
      | End, More(v2, d2, r2, e2) ->
          let d1 = def v2 in
          let c = cmp d1 d2 in
          if c <> 0 then c else
            compare_auxC cmp def oldE1 (cons_enum r2 e2)
      | More(v1, d1, r1, e1), End ->
          let d2 = def v1 in
          let c = cmp d1 d2 in
          if c <> 0 then c else
            compare_auxC cmp def (cons_enum r1 e1) oldE2
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
          let cv = Ord.compare v1 v2 in
          if cv == 0 then 
            let c = cmp d1 d2 in
            if c <> 0 then c
            else compare_auxC cmp def (cons_enum r1 e1) (cons_enum r2 e2)
          else if cv < 0 then
            let d2 = def v1 in
            let c = cmp d1 d2 in
            if c <> 0 then c
            else compare_auxC cmp def (cons_enum r1 e1) oldE2
          else 
            let d1 = def v2 in
            let c = cmp d1 d2 in
            if c <> 0 then c
            else compare_auxC cmp def oldE1 (cons_enum r2 e2)
         

    let rec equal cmp m1 m2 =
      equal_aux cmp (cons_enum m1 End) (cons_enum m2 End)

    and equal_aux cmp e1 e2 =
      match (e1, e2) with
        (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
      equal_aux cmp (cons_enum r1 e1) (cons_enum r2 e2)
    
    let rec equalC cmp def m1 m2 =
      equal_auxC cmp def (cons_enum m1 End) (cons_enum m2 End)

    and equal_auxC cmp def origE1 origE2  =
      match (origE1, origE2) with
        (End, End) -> true
      | ((End as endE), More(v2, d2, r2, e2)) 
      | (More(v2, d2, r2, e2), (End as endE)) ->
          let d1 = def v2 in
          cmp d1 d2 && equal_auxC cmp def endE (cons_enum r2 e2)
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
          let cv = Ord.compare v1 v2 in
          if cv == 0 then 
            cmp d1 d2 && equal_auxC cmp def (cons_enum r1 e1) (cons_enum r2 e2)
          else if cv < 0 then
            checkMissing cmp def v1 d1 (cons_enum r1 e1) origE2
          else 
            checkMissing cmp def v2 d2 origE1 (cons_enum r2 e2)

    and checkMissing cmp def v d e1 e2 =
      let d2 = def v in
      cmp d d2 && equal_auxC cmp def e1 e2

           
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
          let mx2, md2, newt2 = remove_min_binding t2 in
          join t1 mx2 md2 newt2


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
                | Some(d) -> combiner d1 d
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

    let rec interO combiner m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, v1, d1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              concat (interO combiner l1 l2) (interO combiner r1 r2)
          | (l2, Some(d), r2) ->
              match combiner d d1 with
                Some d' ->
                  join (interO combiner l1 l2) v1 d' (interO combiner r1 r2)
              | None ->
                  concat (interO combiner l1 l2) (interO combiner r1 r2)

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

    let rec diffEq eqVals m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> t1
      | (Node(l1, v1, d1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              join (diffEq eqVals l1 l2) v1 d1 (diffEq eqVals r1 r2)
          | (l2, Some(d2), r2) -> 
              if eqVals d1 d2 then
                concat (diffEq eqVals l1 l2) (diffEq eqVals r1 r2)
              else 
                join (diffEq eqVals l1 l2) v1 d1 (diffEq eqVals r1 r2)


    let rec diffBoth eqVals m1 m2 =
      if m1 == m2 then Empty
      else match (m1, m2) with
        (Empty, t2) -> t2 (* Includes stuff only in m2 as well *)
      | (t1, Empty) -> t1
      | (Node(l1, v1, d1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              join (diffBoth eqVals l1 l2) v1 d1 (diffBoth eqVals r1 r2)
          | (l2, Some(d2), r2) -> 
              if eqVals d1 d2 then
                concat (diffBoth eqVals l1 l2) (diffBoth eqVals r1 r2)
              else 
                join (diffBoth eqVals l1 l2) v1 d1 (diffBoth eqVals r1 r2)

    (** Find the diffs between the two maps, generating a diff val with
        "combiner" if a k -> v1 in m1 and k -> v2 in m2.
        The combiner should return v2 if the key should simply be omitted *)
    let rec diffComb combiner m1 m2 =
      if m1 == m2 then Empty
      else match (m1, m2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> t1
      | (Node(l1, v1, d1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              join (diffComb combiner l1 l2) v1 d1 (diffComb combiner r1 r2)
          | (l2, Some(d2), r2) ->
              let comb = combiner d1 d2 in 
              if comb == d2 then (* really is the same -- leave out *)
                concat (diffComb combiner l1 l2) (diffComb combiner r1 r2)
              else (* not the same, keep the comb value *)
                join (diffComb combiner l1 l2) v1 comb (diffComb combiner r1 r2)


    let rec unionC combiner default m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> mapi (fun k2 v2 -> combiner (default k2) v2) t2
      | (t1, Empty) -> mapi (fun k1 v1 -> combiner v1 (default k1)) t1
      | (Node(l1, k1, d1, r1, h1), Node(l2, k2, d2, r2, h2)) ->
          if h1 >= h2 then
            let (l2, k1ValInM2, r2) = split k1 m2 in
            let d' = match k1ValInM2 with
                None -> combiner d1 (default k1) 
              | Some(d) -> combiner d1 d
            in
            join (unionC combiner default l1 l2) 
              k1 d' (unionC combiner default r1 r2)
          else
            let (l1, k2ValInM1, r1) = split k2 m1 in
            let d' = match k2ValInM1 with
                None -> combiner d2 (default k2)
              | Some (d) -> combiner d d2
            in
            join (unionC combiner default l1 l2) 
              k2 d' (unionC combiner default r1 r2)
              
    let rec unionC2 combiner def1 def2 m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> mapi (fun k2 v2 -> combiner (def2 k2) v2) t2
      | (t1, Empty) -> mapi (fun k1 v1 -> combiner v1 (def1 k1)) t1
      | (Node(l1, k1, d1, r1, h1), Node(l2, k2, d2, r2, h2)) ->
          if h1 >= h2 then
            let (l2, k1ValInM2, r2) = split k1 m2 in
            let d' = match k1ValInM2 with
                None -> combiner d1 (def2 k1)
              | Some(d) -> combiner d1 d
            in
            join (unionC2 combiner def1 def2 l1 l2) 
              k1 d' (unionC2 combiner def1 def2 r1 r2)
          else
            let (l1, k2ValInM1, r1) = split k2 m1 in
            let d' = match k2ValInM1 with
                None -> combiner d2 (def1 k2) 
              | Some (d) -> combiner d d2
            in
            join (unionC2 combiner def1 def2 l1 l2) 
              k2 d' (unionC2 combiner def1 def2 r1 r2)

    let rec unionCK combiner default m1 m2 =
      match (m1, m2) with
        (Empty, t2) -> mapi (fun k2 v2 -> combiner k2 (default k2) v2) t2
      | (t1, Empty) -> mapi (fun k1 v1 -> combiner k1 v1 (default k1)) t1
      | (Node(l1, k1, d1, r1, h1), Node(l2, k2, d2, r2, h2)) ->
          if h1 >= h2 then
            let (l2, k1ValInM2, r2) = split k1 m2 in
            let d' = match k1ValInM2 with
                None -> combiner k1 d1 (default k1) 
              | Some(d) -> combiner k1 d1 d
            in
            join (unionCK combiner default l1 l2) 
              k1 d' (unionCK combiner default r1 r2)
          else
            let (l1, k2ValInM1, r1) = split k2 m1 in
            let d' = match k2ValInM1 with
                None -> combiner k2 (default k2) d2 
              | Some (d) -> combiner k2 d d2
            in
            join (unionCK combiner default l1 l2) 
              k2 d' (unionCK combiner default r1 r2)
              
              
    (* Version of union that also records keys w/ new vals *)
    let rec unionCN combiner default m1 m2 =
      let newKeys = ref [] in
      let applyDefaults k v =
        let newV, interesting = combiner (default k) v in
        if interesting then newKeys := k :: !newKeys;
        newV
      in
      let rec recurse m1 m2 =
        match (m1, m2) with
          (Empty, t2) -> mapi applyDefaults t2
        | (t1, Empty) -> mapi applyDefaults t1
        | (Node(l1, k1, d1, r1, h1), Node(l2, k2, d2, r2, h2)) ->
            if h1 >= h2 then
              let (l2, k1ValInM2, r2) = split k1 m2 in
              let d', interesting = match k1ValInM2 with
                  None -> combiner d1 (default k1)
                | Some(d) -> combiner d1 d
              in
              if interesting then newKeys := k1 :: !newKeys; 
              join (recurse l1 l2) k1 d' (recurse r1 r2)
            else
              let (l1, k2ValInM1, r1) = split k2 m1 in
              let d', interesting = match k2ValInM1 with
                  None -> combiner (default k2) d2
                | Some (d) -> combiner d d2
              in
              if interesting then newKeys := k2 :: !newKeys; 
              join (recurse l1 l2) k2 d' (recurse r1 r2)
      in
      let nm = recurse m1 m2 in
      nm, !newKeys
        
    (********** Subset ops **********)
                  
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

    let rec subsetB cmp m1 m2 =
      match (m1, m2) with
        Empty, _ -> true
      | _, Empty -> false
      | Node (l1, k1, d1, r1, _), (Node (l2, k2, d2, r2, _) as t2) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then begin
            if cmp d1 d2 then subsetB cmp l1 l2 && subsetB cmp r1 r2
            else false
          end
          else if c < 0 then
            subsetB cmp (Node (l1, k1, d1, Empty, 0)) l2 && 
              subsetB cmp r1 t2
          else
            subsetB cmp (Node (Empty, k1, d1, r1, 0)) r2 && 
              subsetB cmp l1 t2

    let rec subsetBK cmp m1 m2 =
      match (m1, m2) with
        Empty, _ -> true
      | _, Empty -> false
      | Node (l1, k1, d1, r1, _), (Node (l2, k2, d2, r2, _) as t2) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then begin
            if cmp k1 d1 d2 then subsetBK cmp l1 l2 && subsetBK cmp r1 r2
            else false
          end
          else if c < 0 then
            subsetBK cmp (Node (l1, k1, d1, Empty, 0)) l2 && 
              subsetBK cmp r1 t2
          else
            subsetBK cmp (Node (Empty, k1, d1, r1, 0)) r2 && 
              subsetBK cmp l1 t2


    let rec subsetC cmp default m1 m2 =
      match (m1, m2) with
        Empty, _ -> for_all (fun k2 v2 -> cmp (default k2) v2) m2
      | _, Empty -> for_all (fun k1 v1 -> cmp v1 (default k1)) m1
      | Node (l1, k1, d1, r1, _), Node (l2, k2, d2, r2, _) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then begin
            if cmp d1 d2 
            then subsetC cmp default l1 l2 && subsetC cmp default r1 r2
            else false
          end
          else if c < 0 then
            subsetC cmp default (Node (l1, k1, d1, Empty, 0)) l2 && 
              subsetC cmp default r1 (Node (Empty, k2, d2, r2, 0))
          else
            subsetC cmp default (Node (Empty, k1, d1, r1, 0)) r2 && 
              subsetC cmp default l1 (Node (l2, k2, d2, Empty, 0))
              (* Can't reuse t2 (unlike the normal subset code) because 
                 we care when entries are actually missing! *)
        
    let rec subsetC2 cmp def1 def2 m1 m2 =
      match (m1, m2) with
        Empty, _ -> for_all (fun k2 v2 -> cmp (def2 k2) v2) m2
      | _, Empty -> for_all (fun k1 v1 -> cmp v1 (def1 k1)) m1
      | Node (l1, k1, d1, r1, _), Node (l2, k2, d2, r2, _) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then begin
            if cmp d1 d2 
            then subsetC2 cmp def1 def2 l1 l2 && subsetC2 cmp def1 def2 r1 r2
            else false
          end
          else if c < 0 then
            subsetC2 cmp def1 def2 (Node (l1, k1, d1, Empty, 0)) l2 && 
              subsetC2 cmp def1 def2 r1 (Node (Empty, k2, d2, r2, 0))
          else
            subsetC2 cmp def1 def2 (Node (Empty, k1, d1, r1, 0)) r2 && 
              subsetC2 cmp def1 def2 l1 (Node (l2, k2, d2, Empty, 0))


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

    let rec sampleHelper hashElem m cur goLeft =
      match m with 
        Empty -> cur
      | Node (l, k, d, r, h) ->
          let newHash = cur lxor (hashElem k d) in
          if goLeft then
            sampleHelper hashElem l newHash false
          else
            sampleHelper hashElem r newHash false (* go left 1x *)   

    let sampleHash hashElem x =
      if (is_empty x) then 0
      else
        let size = (cardinal x) in
        let sizeH = Hashtbl.hash size in
        if size == 1 then
          let lv, corr = choose x in
          sizeH lxor (hashElem lv corr)
        else
          let min_lv, min_corr = min_binding x in
          let minH = sizeH lxor (hashElem min_lv min_corr) in
          let medH = sampleHelper hashElem x minH true in
          let max_lv, max_corr = max_binding x in
          medH lxor (hashElem max_lv max_corr)
            
end


(**** super-basic sanity checks

module IC = struct
  type t = int
  let compare = Pervasives.compare
end

module IM = Make(IC)
;;

(* EqualC *)
let m1 = IM.add 0 0 (IM.add 2 2 (IM.add 3 3 (IM.add 4 4 IM.empty)))

let m2 = IM.add 0 0 (IM.add 1 1 (IM.add 2 2 (IM.add 3 3 (IM.add 4 4 IM.empty))))

let m3 = IM.add 0 0 (IM.add 1 0 (IM.add 2 2 (IM.add 3 3 (IM.add 4 4 IM.empty))))
let m4 = IM.add 0 0 (IM.add 1 0 (IM.add 2 2 (IM.add 4 3 (IM.add 6 6 IM.empty))))

let default k = 0
;;

IM.equalC (=) default m1 m1;; (* true *)
IM.equalC (=) default m1 m2;; (* false *)
IM.equalC (=) default m1 m3;; (* true *)
IM.equalC (=) default m2 m2;; (* true *)
IM.equalC (=) default m2 m3;; (* false *)
IM.equalC (=) default m3 m3;; (* true *)


(* CompareC *)

let cmpMod2 v1 v2 =
 (v1 mod 2) - (v2 mod 2)
;;

IM.compareC cmpMod2 default m1 m1;; (* 0 *)
IM.compareC cmpMod2 default m1 m2;; (* -1 *)
IM.compareC cmpMod2 default m1 m3;; (* 0 *)
IM.compareC cmpMod2 default m2 m1;; (* 1 *)
IM.compareC cmpMod2 default m2 m3;; (* 1 *)
IM.compareC cmpMod2 default m3 m3;; (* 0 *)

(* Diff *)


IM.diffBoth (=) m1 m1;; (* empty *)
IM.diffBoth (=) m1 m2;; (* {(1, 1)} *)
IM.diffBoth (=) m1 m3;; (* {(1, 0)} *)
IM.diffBoth (=) m2 m1;; (* {(1, 1)} *)
IM.diffBoth (=) m2 m3;; (* {(1, 1)} *)
IM.diffBoth (=) m3 m3;; (* empty *)
IM.diffBoth (=) m3 m4;; (* {(3, 3), (4, 4), (6, 6)} *)

*)
