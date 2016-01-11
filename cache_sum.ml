

(** Version of Backed_summary w/ a limit on the # of in-memory summaries
    (i.e., it that automatically flushes once past the limit)  *)

open Stdutil
open Summary_keys
open Backed_summary

module L = Logging

module Make (I:Summarizeable) = struct
  
  module Base = Make(I) (* Backed_summary's Make *)

  type sum = I.t

  class data (limit: int) sumID : [sum] base = object(self)
    inherit Base.data sumID as super

    (* In-memory summaries to be flushed when limit is reached? 
       Actually, need to be able to find+remove crap from the middle too
       if we want to be precise when clients use flushOne and evictOne also *)
    val inMem = Queue.create ()

    (* Hmm... don't track uses of this so the queue may be an over-estimate? *) 
    method flushOne key =
      super#flushOne key
        
    method evictOne key =
      super#evictOne key

    (** Bringing something into memory, possibly evict another summary 
        to make room *)
    method private willAdd (key : sumKey) =
      if Queue.length inMem >= limit then
        let toEvict = Queue.take inMem in
        self#flushOne toEvict
      else ();
      Queue.push key inMem

    method serializeAndFlush =
      super#serializeAndFlush;
      Queue.clear inMem

    method evictSummaries =
      super#evictSummaries;
      Queue.clear inMem

  (* Assume fold methods do not bring extra folks into memory / 
     should not affect the queue *)
        

  end (* End class: data *)

end 
