(* A queue w/ map semantics and sorted by key-ordering given for the map *)

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
    
  module QMap = Mapset.Make(Ord)

  type 'a t = {
    mutable map : 'a QMap.t;
  }
      
  let create (_:unit) : 'a t = 
    { map = QMap.empty; }
    
  let clear (q: 'a t) : unit =
    q.map <- QMap.empty

  let qmapnf () =
    failwith "Queuemap not found!"

  let find (k:key) (q:'a t) : 'a =
    let v = QMap.find k q.map in
    v

  let pop (q:'a t) : key * 'a =
    try
      let k, v, newm = QMap.remove_min_binding q.map in
      q.map <- newm;
      (k, v)
    with Not_found -> raise Queue.Empty
      
  let peek (q:'a t) : key * 'a =
    try QMap.min_binding q.map
    with Not_found -> raise Queue.Empty

  let addOnce (q: 'a t) (k: key) (v: 'a) : unit =
    q.map <- QMap.add k v q.map

  let is_empty q : bool =
    QMap.is_empty q.map
      
  let iter f q = 
    QMap.iter f q.map

  let fold f q acc =
    QMap.fold f q.map acc

  let length q =
    QMap.fold (fun k v c -> 1 + c) q.map 0

  let mem q k =
    QMap.mem k q.map

end
  
