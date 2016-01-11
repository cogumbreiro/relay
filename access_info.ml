
(** Common information that can be tracked for all "accesses" *)

open Fstructs

module Du = Cildump
module L = Logging
module CLv = Cil_lvals


(** Function + source-code location *)
type funLoc = fKey * Cil.location

let compareFunLoc (k1, l1) (k2, l2) = 
  let c = k1 - k2 in 
  if c == 0 then Cil.compareLoc l1 l2 else c

(** Set of Function + File + Line number locations *)
module Locs = Set.Make (
  struct 
    type t = funLoc
    let compare = compareFunLoc
  end
)

type accessInfo = Locs.t

                    
(********************** Access Info Funcs ***********************)

(** Pick out the first access location in the access info *)
let firstLocation a =
  let _, l = Locs.min_elt a in
  l

let string_loc (fkey, loc) =
  Du.string_of_loc loc

let string_of_accesses a =
  let outString = Buffer.create 10 in
  Buffer.add_string outString "[";
  L.seq_to_buf Locs.iter string_loc a outString;
  Buffer.add_string outString "]";
  Buffer.contents outString

let hcAccLocation (fk, loc) =
  (fk, CLv.distillLoc loc)

(** Hash-cons the info related to an access *)
let hcAccesses a = 
  Locs.fold (fun funLoc s -> Locs.add (hcAccLocation funLoc) s) a Locs.empty

(** True if the given accesses infos are equal *)
let eqAccesses (a:accessInfo) (b:accessInfo) =
  Locs.equal a b

(** True if a location has as much info as other *)
let locSubs l1 l2 =
  if (l1 == Cil.locUnknown) then true
  else if (l2 == Cil.locUnknown) then false
  else true (* neither are unknown, but don't compare the two *) 


(** Merge two access infos *)
let combineAccs a1 a2 =
  Locs.union a1 a2


