
(*
open Objsize

let p_size o = 
  let x = objsize o in
  "obj bytes : " ^ (string_of_int (size_with_headers x))
    ^ ", " ^ (string_of_int (size_without_headers x))

*)

module L  = Logging

let checkSizes = ref false

let size_kb o =
  if !checkSizes then Size.size_kb o
  else -1

let size_w o =
  if !checkSizes then Size.size_w o
  else -1

let p_size_kb caption kb =
  L.logStatus (caption ^ " size (kb): " ^ string_of_int kb)

let p_size caption o =
  if !checkSizes then p_size_kb caption (size_kb o)
  else ()

