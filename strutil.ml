

(** String utilities from various sources *)



(* arch-tag: String utilities main file
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

let wschars = [' '; '\t'; '\r'; '\n'];;

let wsregexp = Str.regexp "[ \n\t]+";;

let rec lstrip s = 
  if String.length s < 1 then s else
  if List.mem (String.get s 0) wschars then
    lstrip (String.sub s 1 ((String.length s) - 1))
  else
    s;;

let rec rstrip s =
  if String.length s < 1 then s else
    let len = String.length s in
    if List.mem (String.get s (len - 1)) wschars then
      rstrip (String.sub s 0 (len - 1))
    else
      s;;

let strip s = rstrip (lstrip s);;




(********************************************)
(**** More Str Utils added by Jan Voung *****)


(**************** String pool ******************)

module StringHash = 
struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end

module SH = Weak.Make(StringHash)


let strings = SH.create 237
let stringMutex = Mutex.create ()

(** add/get a string to/from the string pool (thread-unsafe) *)
let addString s =
  SH.merge strings s


(** thread-safe addString *)
let addStringMT s =
  Mutex.lock stringMutex;
  let newS = SH.merge strings s in
  Mutex.unlock stringMutex;
  newS

(* TODO: allow/make different string pools *)


module StringSet = Set.Make(String)


(*********** substringing *********)

(** Summarize a string, only keeping the header *)
let sumStrHead ?(maxlen=15) str =
  if String.length str > maxlen then
    let tail = "..." in
    let taillen = String.length tail in
    String.sub str 0 (maxlen - taillen) ^ tail
  else str

(** Summarize a string, keeping only the tail *)
let sumStrTail ?(maxlen=15) str =
  let len = String.length str in
  if len > maxlen then
    let header = "..." in
    let headerlen = String.length header in
    let bodylen = maxlen - headerlen in
    header ^ (String.sub str (len - bodylen) bodylen)
  else str

