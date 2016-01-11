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

  (** Clear the stack *)
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

module Make (Ord:OrderedType) =
struct
  (** The type of elements *)
  type elt = Ord.t
    
  module SSet = Set.Make(Ord)

  (** The type of StackSets *)
  type t = {
    stack : elt Stack.t;
    mutable set : SSet.t;
  }


  (** Get an empty stack *)
  let create (_:unit) : t = 
    { stack = Stack.create ();
      set = SSet.empty;
    }
    

  let clear (curStack:t) : unit =
    curStack.set <- SSet.empty;
    Stack.clear curStack.stack
    
  (** remove from front *)
  let pop (curStack:t) : elt =
    let x = Stack.pop curStack.stack in
    curStack.set <- SSet.remove x curStack.set;
    x

  let peek curStack =
    let x = Stack.pop curStack.stack in
    Stack.push x curStack.stack;
    x
    
  (** add to front, unless it's already on the stack *) 
  let pushOnce (newVal:elt) (curStack:t) : unit =
    if (not (SSet.mem newVal curStack.set)) then
      begin
        curStack.set <- SSet.add newVal curStack.set;
        Stack.push newVal curStack.stack
      end

  (** true if the stack is empty *)
  let is_empty (curStack:t) : bool =
    Stack.is_empty curStack.stack

  let iter f s = 
    Stack.iter f s.stack

  let length s =
    Stack.length s.stack

  let mem e s =
    SSet.mem e s.set

end
  
