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


module Make (Ord:OrderedType) =
struct

  type elt = Ord.t
    
  module QSet = Set.Make(Ord)


  type t = {
    queue : elt Queue.t;
    mutable set : QSet.t;
  }



  let create (_:unit) : t = 
    { queue = Queue.create ();
      set = QSet.empty;
    }
    
  let clear (qs:t) : unit =
    qs.set <- QSet.empty;
    Queue.clear qs.queue

  let pop (qs:t) : elt =
    let x = Queue.pop qs.queue in
    qs.set <- QSet.remove x qs.set;
    x

  let peek qs =
    Queue.peek qs.queue

  let addOnce (newVal:elt) (qs:t) : unit =
    if (not (QSet.mem newVal qs.set)) then
      begin
        qs.set <- QSet.add newVal qs.set;
        Queue.add newVal qs.queue
      end


  let is_empty (qs:t) : bool =
    Queue.is_empty qs.queue
      
  let iter f q = 
    Queue.iter f q.queue

  let length q =
    Queue.length q.queue

  let mem qs v =
    QSet.mem v qs.set

end
  
