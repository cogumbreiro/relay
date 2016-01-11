
(** Sparse bit set w/ no blit operations. The provided operations will
    be more like set operations. *)

module IS = Mapset.Make
  (struct
     type t = int
     let compare a b = a - b
   end)

module type S = sig
  type t
  val empty : t
  val add : int -> t -> t 
  val remove : int -> t -> t
  val mem : int -> t -> bool
  val subset : t -> t -> bool
  val equal : t -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val iter : (int -> unit) -> t -> unit
  val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
  val elements : t -> int list
  val exists : (int -> bool) -> t -> bool
  val for_all : (int -> bool) -> t -> bool
  val inter_isEmpty : t -> t -> bool
  val cardinal : t -> int
  val singleton : int -> t
  val is_empty : t -> bool
  val filter : (int -> bool) -> t -> t
end

type t = int IS.t
  
let bpi = Sys.word_size - 2

(* From bitv:
   Access and assignment. The [n]th bit of a bit vector is the [j]th
   bit of the [i]th integer, where [i = n / bpi] and [j = n mod
   bpi]. Both [i] and [j] and computed by the function [pos].
   Accessing a bit is testing whether the result of the corresponding
   mask operation is non-zero, and assigning it is done with a
   bitwise operation: an {\em or} with [bit_j] to set it, and an {\em
   and} with [bit_not_j] to unset it. *)
  
let pos n = 
  let i = n / bpi and j = n mod bpi in
  if j < 0 then (i - 1, j + bpi) else (i,j)

let invPos i j =
  bpi * i + j

(** All 0s except at bit j *)
let maskAt j =
  1 lsl j

(** All 1s except at bit j *)
let notMaskAt j =
  max_int - (maskAt j)

let setBit theInt j =
  theInt lor maskAt j

let checkBit theInt j =
  (theInt land maskAt j) != 0

let beyondBits theInt j =
  maskAt j > theInt

let empty = 
  IS.empty


let add n bs = 
  let i, j = pos n in
(*
  let old = try IS.find i bs with Not_found -> 0 in
  IS.add i (old lor maskAt j) bs
*)
  IS.addComb (lor) i (maskAt j) bs


let remove n bs =
  let i, j = pos n in
  let old = try IS.find i bs with Not_found -> 0 in
  let res = (old land notMaskAt j) in 
  if res != 0 then IS.add i res bs else IS.remove i bs

let mem n bs =
  let i, j = pos n in
  try 
    let bits = IS.find i bs in
    checkBit bits j
  with Not_found -> false

(***** Comparisons ******)

(* Check subset at the bit level w/ a weird ass return value *)
let subsetBits int1 int2 =
  int1 == (int1 land int2) (* then Some (-1) else None *)
    
let subset b1 b2 =
  IS.subsetB subsetBits b1 b2

let equal b1 b2 = 
  IS.equal (=) b1 b2

(***** Merging ******)

let unionBits int1 int2 =
  int1 lor int2

let union b1 b2 =
  if b1 == b2 then b1
  else IS.union unionBits b1 b2
    
let interBits int1 int2 =
  int1 land int2

let inter b1 b2 =
  if b1 == b2 then b1
  else IS.inter interBits b1 b2

(* Bit is set if set in int1, but not in int2... 
   return int2 if the result is that nothing is set... 
   funky way to signal that nothing is diff to the IS module... *)
let diffBits int1 int2 =
  let res = int1 land (lnot int2) in
  if res == 0 then int2 else res

let diff b1 b2 =
  IS.diffComb diffBits b1 b2

(****** Iteration *******)

(* Looks like the dumb for-loop is faster *)
let iterBits foo i bits =
  for j = 0 to bpi - 1 do
    if checkBit bits j then 
      foo (invPos i j)
  done

(*
let rec iterBits foo i bits =
  iterBitsLoop foo i bits 0

and iterBitsLoop foo i bits j =
  if j = bpi || beyondBits bits j then ()
  else begin
    if checkBit bits j then 
      foo (invPos i j);
    iterBitsLoop foo i bits (j + 1)
  end
*)

let iter foo b =
  IS.iter (iterBits foo) b


let foldBits foo i bits accum =
  let result = ref accum in
  for j = 0 to bpi - 1 do
    if checkBit bits j then 
      result := foo (invPos i j) !result
  done;
  !result

(*
let rec foldBits foo i bits accum = 
  foldBitsLoop foo i bits 0 accum

and foldBitsLoop foo i bits j accum =
  if j = bpi || beyondBits bits j then accum
  else begin
    let accum = if checkBit bits j then foo (invPos i j) accum else accum in
    foldBitsLoop foo i bits (j + 1) accum
  end
*)

let fold foo b accum =
  IS.fold (foldBits foo) b accum

let existsBits pred i bits =
  let result = ref false in
  for j = 0 to bpi - 1 do
    if checkBit bits j then
      result := !result || pred (invPos i j)
  done;
  !result
(*
  let rec loop j =
    if j = bpi || beyondBits bits j then false
    else begin
      if checkBit bits j then 
        if pred (invPos i j) then true
        else loop (j + 1) 
      else loop (j + 1)
    end
  in
  loop 0
*)

let exists pred b =
  IS.exists (existsBits pred) b

let forallBits pred i bits =
  let result = ref true in
  for j = 0 to bpi - 1 do
    if checkBit bits j then
      result := !result && pred (invPos i j)
  done;
  !result

let for_all pred b =
  IS.for_all (forallBits pred) b

let inter_isEmpty b1 b2 =
  let result = inter b1 b2 in
  if result == empty then true
  else
    not (exists (fun x -> x != 0) result)

let cardinal b =
  fold (fun someBit cur -> cur + 1) b 0

let singleton n =
  add n empty

let is_empty b =
  (b = IS.empty) ||
    (not (IS.exists (fun n bits -> bits != 0) b))

(* Lame-o versions *)

let elements b =
  fold (fun i acc -> i :: acc) b []

let filter p b =
  fold (fun i acc -> if p i then remove i acc else acc) b b
