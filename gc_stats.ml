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
module L = Logging

(** Print time/memory usage statistics *)
let printStatistics () =
  L.logStatus ("Elapsed time (secs): " ^ 
                 (string_of_float (Sys.time ())));
  let gcStat = quick_stat () in
  L.logStatus ("Top heap size (words): " ^
                 (string_of_int gcStat.top_heap_words));
  L.logStatus ("Current heap size (words): " ^
                 (string_of_int gcStat.heap_words));
  L.logStatus ("Lifetime allocated bytes: " ^
                 (string_of_float (allocated_bytes ())));
  L.logStatus ("Collections -- minor: " ^ 
                 (string_of_int gcStat.minor_collections) ^ 
                 " major: " ^ (string_of_int gcStat.major_collections) ^ 
                 " compactions: " ^ (string_of_int gcStat.compactions))
    
(** Change GC settings: heap sizes, and such *)
let setGCConfig () =
  set { (get ()) with minor_heap_size = 524288 }

(*

(** Count the number of gc heap dumps *)
let gc_dumps = ref 0

(** dump the heap to a file for inspection *)
let dump_heap () =
  L.logError ("dumping heap: " ^ (string_of_int !gc_dumps));
  incr gc_dumps;
  Gc.dump_heap ()

*)
