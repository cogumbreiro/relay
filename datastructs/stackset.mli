(* A stack w/ set semantics (only one copy of the same thing exists on 
 * the stack) *)

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =  
sig
  
  (** The type of elements *)
  type elt
    
  (** The type of StackSets *)
  type t

  (** Get an empty stack *)
  val create : unit -> t

  val clear : t -> unit
    
  (** remove from front *)
  val pop : t -> elt
    
  val peek : t -> elt

  (** add to front, unless it's already on the stack *) 
  val pushOnce : elt -> t -> unit
    
  (** true if the stack is empty *)
  val is_empty : t -> bool

  (** iter f s applies f in turn to all elements of s, from the top
   *  of the stack to the bottom *)
  val iter : (elt -> unit) -> t -> unit

  (** Return the number of elements in a stack. *)
  val length : t -> int

  (** True if the elt is already a member of the stack *)
  val mem : elt -> t -> bool

end


module Make (Ord:OrderedType) : S with type elt = Ord.t
