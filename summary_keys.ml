open Fstructs

(************************************************************
  Summary keys for lookup
************************************************************)

(** User must convert summary key to a string w/ alphanumeric chars only *)
type sumKey = (fKey * string)

let compareSumKey a b = Pervasives.compare a b

module SM = Map.Make
  (struct
     type t = sumKey
     let compare a b = compareSumKey a b
   end)

(* HACK: for now make this identifier the same as the one used
   for inputFreeSumKey. Otherwise, we would need to guess that
   wildCard summaries existed and try to transport those as well 
   (which isn't TOO bad) *)
let wildCardStr = "f"

(** Returns a sumKey representing a summary for an given function *)
let wildCardkey (fkey:fKey) : sumKey =
  (fkey, wildCardStr)

let isWildCard (fk, str) = (str = wildCardStr)

let separator = "."

let splitter = Str.split_delim (Str.regexp ("[" ^ separator ^ "]"))

let string_of_sumKey (fk, s) =
  (string_of_fkey fk) ^ separator ^ s

let sumKey_of_string str =
  match splitter str with
    [fk; s] -> (fkey_of_string fk, s)
  | _ -> failwith "sumKey_of_string: malformed"

(** Creates a sumKey for functions that don't care about their input state *)
let inputFreeSumKey fkey = wildCardkey fkey

let fkey_of_sumKey ((fk, _):sumKey) = fk

(** Convenient module for making funIDs out of sumKeys *)
module FunIDSumKey = struct

  type funID = sumKey
  let compare a b = compareSumKey a b
  let equal a b = compareSumKey a b == 0
  let hash x = Hashtbl.hash x

  let funIDToSumKey x = x

end
