(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following 
  conditions are met:
  
  1. Redistributions of source code must retain the above copyright 
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above 
  copyright notice, this list of conditions and the following disclaimer 
  in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the University of California, San Diego, nor 
  the names of its contributors may be used to endorse or promote 
  products derived from this software without specific prior 
  written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
*)

(** A generic, bounded cache of objects *)


module type S = 
sig

  type key
    
  type 'a t 

  (** Create a cache of size n *) 
  val create : int -> 'a t
      
  (** Clear the cache *)
  val clear : 'a t -> unit

  (** Replace an entry in the cache, adding it if no binding existed
      before and evicting an item if needed.
      Return the evicted key if anything is evicted *)
  val replace : 'a t -> key -> 'a -> key option

  (** Add new entry to the cache, evicting an item if needed. 
      If the binding already existed, no change is made.
      Return the evicted key if anything is evicted *)
  val add : 'a t -> key -> 'a -> key option

  (** Get an entry from the cache, may raise Not_found *)
  val find : 'a t -> key -> 'a
        
  val resize : 'a t -> int -> unit

end

(** Cache w/ FIFO replacement policy (not LRU) *)
module Make(T:Hashtbl.HashedType) =
struct

  type key = T.t

  module Hash = Hashtbl.Make (T)


  (** Cache of objects *)
  type 'a t =
      { mutable maxSize : int;
        table : 'a Hash.t;
        fifo : key Queue.t;
      }

  (** Create a cache of size n *) 
  let create (n:int) : 'a t =
      { maxSize = n;
        table = Hash.create (n + 13);
        fifo = Queue.create ();
      }
      
  (** Clear the cache *)
  let clear (cache: 'a t) : unit =
    Hash.clear cache.table;
    Queue.clear cache.fifo

  let enforceSize (cache : 'a t) : key option = 
    (* Prepare to add the object, possibly making room *)
    let curSize = Queue.length cache.fifo in
    if (curSize >= cache.maxSize) then begin
      let kickK = Queue.take cache.fifo in
      Hash.remove cache.table kickK;
      Some kickK
    end else 
      None
    
  (** Add new entry to the cache, evicting an item if needed. 
      If the binding already existed, no change is made *)
  let add (cache: 'a t) (key: key) (value: 'a) : key option =
    (* Check if object is already in the cache *)
    if (Hash.mem cache.table key) then
      (* Since the replacement policy is just FIFO, do nothing *)
      None
    else
      (* Add the object, possibly making room *)
      let kicked = enforceSize cache in
      Hash.add cache.table key value;
      Queue.add key cache.fifo;
      kicked


  (** Replace an entry in the cache, adding it if no binding existed
      before and evicting an item if needed *)
  let replace (cache: 'a t) (key: key) (value: 'a) : key option =
    (* Check if object is already in the cache *)
    if (Hash.mem cache.table key) then begin
      (* Since the replacement policy is just FIFO, do nothing *)
      Hash.replace cache.table key value;
      None
    end else
      (* Add the object, possibly making room *)
      let kicked = enforceSize cache in
      Hash.add cache.table key value;
      Queue.add key cache.fifo;
      kicked
        
        
  (** Get an entry from the cache, may raise Not_found *)
  let find (cache: 'a t) (key: key) : 'a =
    Hash.find cache.table key


  let resize (cache: 'a t) (size: int) : unit =
    cache.maxSize <- size;
    while ((Queue.length cache.fifo) > size) do
      let kickK = Queue.take cache.fifo in
      Hash.remove cache.table kickK
    done

end
  
