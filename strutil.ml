

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


(** More str utils (from a difference source ...) *)

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


(** Returns true if [s] contains the prefix [p] *)
let prefix p s = 
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p


(** Returns true if [str] contains the substring [sub]. Regular expression
    characters in [sub] must be escaped (unless it is to be used as a regexp).
    Warning: if [sub] is indeed a regexp, this function prepends ".*" so
    [sub] cannot ask to match from the beginning of a string *)
let containsSub str sub = 
  Str.string_match (Str.regexp (".*" ^ sub)) str 0
    
    
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

