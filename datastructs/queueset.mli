(* A queue w/ set semantics (only one copy of the same thing exists on 
 * the queue) *)

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =  
sig
  
  (** The type of elements *)
  type elt
    
  (** The type of queue sets *)
  type t

  (** Get an empty queue *)
  val create : unit -> t

  val clear : t -> unit
    
  (** remove from front *)
  val pop : t -> elt

  val peek : t -> elt
    
  (** add to back, unless it's already on the queue *) 
  val addOnce : elt -> t -> unit
    
  (** true if the queue is empty *)
  val is_empty : t -> bool

  (** (iter f q) applies f in turn to all elements of q, from the front
   *  of the queue to the back *)
  val iter : (elt -> unit) -> t -> unit

  (** Return the number of elements in a queue. *)
  val length : t -> int

  val mem : t -> elt -> bool
end


module Make (Ord:OrderedType) : S with type elt = Ord.t
