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

(** Module to configure the garbage collector and print out the
    garbage collection statistics *)

open Gc
open Logging

let bytes_to_mbs bytes =
  Int64.div bytes (Int64.of_int (1024 * 1024))

(** Print time/memory usage statistics *)
let printStatistics () =
  let bytes_per_word = Sys.word_size / 8 in
  logStatusF "Elapsed time (secs): %f\n" (Sys.time ());
  let gcStat = quick_stat () in
  logStatusF "Top heap size (MB): %Ld\n"
    (bytes_to_mbs 
       (Int64.mul
          (Int64.of_int gcStat.top_heap_words) 
          (Int64.of_int bytes_per_word)));
  logStatusF "Current heap size (MB): %Ld\n"
    (bytes_to_mbs 
       (Int64.mul
          (Int64.of_int gcStat.heap_words)
          (Int64.of_int bytes_per_word)));
  logStatusF "Lifetime allocated bytes (MB): %Ld\n" 
    (bytes_to_mbs (Int64.of_float (allocated_bytes ())));
  logStatusF "Collections -- minor: %d major: %d compactions: %d\n"
    gcStat.minor_collections gcStat.major_collections gcStat.compactions;
  logStatusF "Bytes per word: %d\n" bytes_per_word
    
(** Change GC settings: heap sizes, and such *)
let setGCConfig () =
  set { (get ()) with minor_heap_size = 524288 }

(*

(** Count the number of gc heap dumps *)
let gc_dumps = ref 0

(** dump the heap to a file for inspection *)
let dump_heap () =
  logError ("dumping heap: " ^ (string_of_int !gc_dumps));
  incr gc_dumps;
  Gc.dump_heap ()

*)
