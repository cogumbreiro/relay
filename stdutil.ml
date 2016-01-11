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

(** Basic utilities to augment the Ocaml standard library *)

open Cil

module D = Cildump
module E = Errormsg
module L = Logging
module Stat = Mystats

(***************************************************
 * Augment the standard library
 ***************************************************)

(** Enhance the exit() function to print stats first *)
let quit exit_code =
  Stat.print Pervasives.stdout "STATS:\n";
  Gc_stats.printStatistics ();
  exit exit_code


(** Return a string for printing command-line usage of programs.
    (Prepends the program name to the supplied [tail] string) *)
let getUsageString tail = 
  "Usage: " ^ (Filename.basename Sys.executable_name) ^ " " ^ tail


(** remove all files in a directory that statisfy the filter (non-recursive) *)
let clearDir dir filter =
  let files = Sys.readdir dir in
  Array.iter 
    (fun fname ->
       if (filter fname) then
         try
           Sys.remove (Filename.concat dir fname)
         with
           Sys_error e ->
             L.logError ("couldn't clear dir: " ^ e)) files


(** conditionally clear out a directory, given a generation number,
    generation file, and the code to clear the directory *)
let clearDirGen genNum genFile clearFunc =
  (* check the old generation number -- if same, don't clear *)
  let clearP = 
    if (Sys.file_exists genFile) then
      let oldNum = Gen_num.getNum genFile in
      genNum <> oldNum
    else
      true
  in
  if (clearP) then
    (clearFunc ();
     Gen_num.setNum genNum genFile
    ) 
  else
    L.logStatus "not clearing persistent state, same generation num"
      
(*************************** List utils ****************************)
  
(** add given [item] if it is not already in the given [lst] *)
let addOnce (lst: 'a list) (item: 'a) =
  if (not (List.mem item lst)) then
    item :: lst
  else 
    lst

(** add given [item] if it is not already in the given [lst]. Test for
    equality of items with the given [eq] func *)
let addOnceP eq lst item =
  if (List.exists (fun other -> eq item other) lst) then lst
  else item :: lst



(** Take the union of two lists, slowly *)
let union l1 l2 =
  List.fold_left addOnce l1 l2


(** Return the size of a map [m], based on the given [fold] function *)
let mapSize (m) (fold) : int =
  let count (_) (_) curCount =
      curCount + 1
  in
  fold count m 0


(** Return the index of the first element that satisfies pred *)
let indexOf pred list = 
  let rec indexHelp curIndex = function
      [] -> raise Not_found
    | x :: l -> if pred x then curIndex else indexHelp (curIndex + 1) l
  in
  indexHelp 0 list

(** Remove n elements from the head of the list *)
let rec listSliceHead curL n =
  if n == 0 then curL
  else match curL with
    [] -> raise (Invalid_argument "out of bounds")
  | _ :: t -> 
      listSliceHead t (n - 1)

(** Keep n elements from the list, truncating the rest from the tail.
    The returned list is reverse *)
let rec listSliceTailRev curL rest n =
  if n == 0 then curL
  else match rest with
    [] -> raise (Invalid_argument "out of bounds")
  | h :: t ->
      listSliceTailRev (h :: curL) t (n - 1)
        
(** Get a slice of a list given the bounds. Bounds are 0-based and
    does includes all up to [last - 1]. No fancy negative-indexing.
    @raise Invalid_argument if out of bounds *)
let listSlice lis first last =
  let len = (last - first) in
  if (len < 0) then raise (Invalid_argument "out of bounds")
  else
    let truncH = listSliceHead lis first in
    let truncL = listSliceTailRev [] truncH len in
    List.rev truncL

let pickFromList ls randomize =
  if randomize then begin
    let len = List.length ls in
    if len == 0 then None
    else
      let picked = Random.int len in
      Some (List.nth ls picked)
  end else
    (* Pick first *)
    match ls with
      [] -> None
    | x :: _ -> Some x

let removeNth ls n =
  let rec loop curLs curN curHead =
    match ls with
      [] -> failwith "removeNth"
    | x :: t ->
        if curN == 0 then x, t, curHead
        else
          loop t (n - 1) (x :: curHead)
  in
  let ele, finalTail, revHead = loop ls n [] in
  (ele, List.rev_append revHead finalTail)

let stealFromList ls randomize =
  if randomize then begin
    let len = List.length ls in
    if len == 0 then (None, ls)
    else
      let picked = Random.int len in
      let x, newLS = removeNth ls picked in
      (Some x, newLS)
  end else
    (* Pick first *)
    match ls with
      [] -> None, ls
    | x :: t -> (Some x, t)

let pickK n k =
  if n <= 0 || k > n then 
    raise (Invalid_argument "pickK")
  ;
  let rec loop chosen left =
    if left = 0 then chosen
    else
      let next = Random.int n in
      if List.mem next chosen then loop chosen left
      else loop (next :: chosen) (left - 1)
  in
  loop [] k


(** Iterate through ordered pairs of [l1] and [l2] *)
let listIterOrderedPairs foo l1 l2 =
  List.iter (fun x1 -> List.iter (fun x2 -> foo x1 x2) l2) l1

(** Iterate unordered pair combinations within given list [ls] *)
let listIterPairs foo ls =
  let rec iter l1 l2 l2start  =
    match l1, l2, l2start with
      [], _, _
    | _, _, []  -> ()
    | _ :: t1 , [], _ :: t2 ->
        iter t1 t2 t2
    | h1 :: _, h2 :: t2, _ ->
        foo h1 h2;
        iter l1 t2 l2start
  in
  iter ls ls ls


let seqToString iter seq doElem sep =
  let firstElem = ref true in
  let buff = Buffer.create 10 in
  iter (fun elem ->
          if not (!firstElem) then Buffer.add_string buff sep
          else firstElem := false
          ;
          Buffer.add_string buff (doElem elem);
       ) seq;
  Buffer.contents buff


(***************************************************
 * File / resource functions
 ***************************************************)

(** Return the file extension from the given filename. *)
let get_extension name =
  let non_ext = Filename.chop_extension name in
  (* Skip the dot *)
  let non_ext_len_p1 = String.length non_ext + 1 in
  let orig_len = String.length name in
  String.sub name non_ext_len_p1 (orig_len - non_ext_len_p1)


let finaliseResource closeFunc = fun res ->
  L.logError "finaliseResource was called?!";
  closeFunc res

(** Open the resource (identified by [resourceName]), using 
    the given [openFunc], and call the given function [foo] 
    with the io_channel. Close the resource with given [closeFunc]
    on completion or failure *)
let open_for openFunc closeFunc resourceName foo =
  let chan = openFunc resourceName in
(*  let _ = Gc.finalise (finaliseResource closeFunc) chan in *)
  try
    let result = foo chan in
    let _ = closeFunc chan in
    result
  with e ->
    closeFunc chan;
    raise e


(** Some shortcuts for files *)

let open_in_bin_for foo = open_for open_in_bin close_in_noerr foo

let open_in_for foo = open_for open_in close_in_noerr foo

let open_out_bin_for foo = open_for open_out_bin close_out_noerr foo

let open_out_for foo = open_for open_out close_out_noerr foo


(** Some shortcuts for sockets *)

let open_connect addr = 
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in (* File opened! *)
  try
    Unix.connect sock addr;
    (sock, Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
  with e ->
    Unix.close sock (* at least close sock if connect fails! *);
    raise e
      
let close_connect (sock, sock_in, sock_out) =
  (try Unix.shutdown sock Unix.SHUTDOWN_ALL
   with e -> ()
  );
  try Unix.close sock
  with e -> ()

let open_conn_for foo = open_for open_connect close_connect foo

(* open for accept? *)


(************************************************************
    Read/write simple data structures into simple text files
 ***********************************************************)

(** Write a hashtable of strings to a text file *)
let tableToFile ?(sep=":") (tab  : (string, string) Hashtbl.t) (fname:string) =
  let doWrite oc =
    Hashtbl.iter
      (fun k v -> output_string oc (k ^ sep ^ v ^ "\n")) tab
  in
  open_out_for fname doWrite

(** Read a text file representing a table into a new table *)
let fileToTable ?(sep=":") (fname:string) : (string, string) Hashtbl.t =
  let result = Hashtbl.create 11 in
  let splitter = Str.regexp sep in
  let doRead ic =
    try while true do
      let line = input_line ic in
      match Str.split splitter line with
        [k; v] -> Hashtbl.add result k v
      | _ -> L.logError ("fileToTable: corrupt entry - " ^ line)
    done; with End_of_file ->
      ()
  in
  open_in_for fname doRead;
  result

(* User must coordinate to make sure the "sep"arator char is the same *)


(************************* Hashtbl utils ***************************)

let string_of_hashstats statsFun hashtbl caption : string =
  let tlen, entries, sum_buck_lens, min_buck, med_buck, max_buck =
    statsFun hashtbl in
  Printf.sprintf "%s hash stats (len, ents, sum/min/median/max bucket)\n\t%d\t%d\t%d\t%d\t%d\t%d\t" caption tlen entries sum_buck_lens min_buck med_buck max_buck
