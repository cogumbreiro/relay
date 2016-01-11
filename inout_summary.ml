
(** Utils for making summary keys when summaries depend on more than
    just the function ID. E.g., if the summary depends on the input
    state as well *)

open Fstructs
open Summary_keys

let sumKeyOfKeyData fkey data : sumKey =
  (* Hopefully we don't hit Sys.max_string_length *)
  let dstr = Marshal.to_string data [Marshal.Closures] in
  (fkey, Digest.to_hex (Digest.string dstr))

let sumKeyOfKeyString fkey str =
  (fkey, Digest.to_hex (Digest.string str))
