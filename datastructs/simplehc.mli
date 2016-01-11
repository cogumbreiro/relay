(*
 * hashcons: hash tables for hash consing
 * simplified (no tag, and hkey not memoized)
 *)

type 'a hash_consed = private {
  node : 'a; }

(*s Generic part, using ocaml generic equality and hash function. *)

type 'a t

val create : int -> 'a t
  (** [create n] creates an empty table of initial size [n]. The table
      will grow as needed. *)  
val clear : 'a t -> unit
  (** Removes all elements from the table. *)
val hashcons : 'a t -> 'a -> 'a hash_consed
  (** [hashcons t n] hash-cons the value [n] using table [t] i.e. returns
      any existing value in [t] equal to [n], if any; otherwise, allocates
      a new one hash-consed value of node [n] and returns it. 
      As a consequence the returned value is physically equal to
      any equal value already hash-consed using table [t]. *)
val iter : ('a hash_consed -> unit) -> 'a t -> unit
  (** [iter f t] iterates [f] over all elements of [t]. *)
val stats : 'a t -> int * int * int * int * int * int
  (** Return statistics on the table.  The numbers are, in order:
      table length, number of entries, sum of bucket lengths,
      smallest bucket length, median bucket length, biggest bucket length. *)

(*s Functorial interface. *) 

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type key
    type t
    val create : int -> t
    val clear : t -> unit
    val hashcons : t -> key -> key hash_consed
    val iter : (key hash_consed -> unit) -> t -> unit
    val stats : t -> int * int * int * int * int * int
  end

module Make(H : HashedType) : (S with type key = H.t)
