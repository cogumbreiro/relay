(** Data structure to track integer ranges & map integer ranges to 
    other objects (assuming the ranges are disjoint) *)


type range = {
  r_lower : int;
  r_upper : int;
}


module RangeMap = Map.Make (
  struct 
    type t = range
    let compare 
        ({r_lower = l1; r_upper = u1;})
        ({r_lower = l2; r_upper = u2;}) = 
      if ( l2 >= l1 && u2 <= u1 ) then
        0
      else if ( l1 >= l2 && u1 <= u2) then
        0
      else
        Pervasives.compare l1 l2
  end
)        
