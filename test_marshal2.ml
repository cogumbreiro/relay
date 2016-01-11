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
class type tab = object
  method add : int -> unit
  method get : int -> bool
end

class c = object (self)
    val h = Hashtbl.create 10
    method print = ()
    method add (i:int) = Hashtbl.add h i true
    method get (i:int) = Hashtbl.find h i
    method serialize fname = 
      let oc = open_out_bin fname in
      Marshal.to_channel oc self [Marshal.Closures];
      close_out oc
end;;

let loadC fname : #tab = 
   let ic = open_in_bin fname in
   Marshal.from_channel ic
;;

let ac = new c;;

let file = "/tmp/tempclass";;

ac#add 213;;
ac#add 111;;
ac#serialize file;;

let otherc = loadC file;;

(try 
   let _ = otherc#get 213 in
   let _ = otherc#get 111 in

   print_string "Successfully got stuff\n";
   flush stdout;
 with Not_found ->
   print_string "Mysteriously didn't get stuff\n";
);;

(try
   let _ = otherc#get 1 in
   print_string "Mysteriously got stuff\n";
 with Not_found ->
   print_string "Successfully didn't get stuff\n";
   flush stdout;
);;


