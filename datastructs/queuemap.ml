(* A queue w/ map semantics (only one copy of the same thing exists on 
   the queue and the elements are separated by key and actual data,
   where the actual data can be modified) *)

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =  
sig
  
  (** The type of keys and elements *)
  type key

  (** The type of queue sets *)
  type 'a t

  (** Get an empty queue *)
  val create : unit -> 'a t

  val clear : 'a t -> unit

  (** remove from front *)
  val pop : 'a t -> key * 'a

  val peek : 'a t -> key * 'a

  (** add to back, unless it's already on the queue... 
      will at least update the key -> value binding *) 
  val addOnce : 'a t -> key -> 'a  -> unit

  val find : key -> 'a t -> 'a

  (** true if the queue is empty *)
  val is_empty : 'a t -> bool

  (** (iter f q) applies f in turn to all elements of q, from the front
   *  of the queue to the back *)
  val iter : (key -> 'a -> unit) -> 'a t -> unit
    
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** Return the number of elements in a queue. *)
  val length : 'a t -> int

  val mem : 'a t -> key -> bool

end


module Make (Ord:OrderedType) : S with type key = Ord.t =
struct

  type key = Ord.t
    
  module QMap = Map.Make(Ord)

  type 'a t = {
    queue : key Queue.t;
    mutable map : 'a QMap.t;
  }
      
  let create (_:unit) : 'a t = 
    { queue = Queue.create ();
      map = QMap.empty; }
    
  let clear (q: 'a t) : unit =
    q.map <- QMap.empty;
    Queue.clear q.queue

  let qmapnf () =
    failwith "Queuemap not found!"

  let find (k:key) (q:'a t) : 'a =
    let v = QMap.find k q.map in
    v

  let pop (q:'a t) : key * 'a =
    let k = Queue.pop q.queue in
    try 
      let v = find k q in 
      q.map <- QMap.remove k q.map;
      (k, v)
    with Not_found -> qmapnf ()

  let peek (q:'a t) : key * 'a =
    let k = Queue.peek q.queue in
    try (k, find k q)
    with Not_found -> qmapnf ()

  let addOnce (q: 'a t) (k: key) (v: 'a) : unit =
    if not (QMap.mem k q.map) then
      Queue.add k q.queue;
    q.map <- QMap.add k v q.map

  let is_empty q : bool =
    Queue.is_empty q.queue
      
  let iter f q = 
    Queue.iter 
      (fun k -> 
         try f k (find k q)
         with Not_found -> qmapnf ()
      ) q.queue

  let fold f q acc =
    Queue.fold
      (fun acc k ->
         try f k (find k q) acc
         with Not_found -> qmapnf ()
      ) acc q.queue 

  let length q =
    Queue.length q.queue

  let mem q v =
    QMap.mem v q.map

end
  
