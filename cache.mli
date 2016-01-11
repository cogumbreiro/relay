(* A bounded cache of objects *)

module type S = 
sig

  type key
    
  type 'a t 

  (** Create a cache of size n *) 
  val create : int -> 'a t
      
  (** Clear the cache *)
  val clear : 'a t -> unit
      
  val replace : 'a t -> key -> 'a -> key option

  (** Add new entry to the cache, evicting an item if needed *)
  val add : 'a t -> key -> 'a -> key option

  (** Get an entry from the cache, may raise Not_found *)
  val find : 'a t -> key -> 'a

  val resize : 'a t -> int -> unit
        
end


(** Cache w/ FIFO replacement policy (not LRU) *)
module Make (T:Hashtbl.HashedType) : S with type key = T.t 
                                       
