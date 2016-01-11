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


class virtual integer = object
  
  method virtual value : int

  method virtual add : integer -> unit

  method virtual print : unit

end

class positiveI i = object
  inherit integer

  val mutable repr = 
    if (i > 0) then i else 
      raise (Invalid_argument "must be positive")

  method value = repr

  method add other = repr <- repr + other#value

  method print = print_int repr

end

let bar () =
  prerr_string "barred!\n"

let testSerialOut () =
  let i1 = new positiveI 10 in
  i1#print;
  let oc = open_out_bin "temp_obj.txt" in
  Marshal.to_channel oc i1 [];
  close_out oc

let testSerialIn () =
  let ic = open_in_bin "temp_obj.txt" in
  let i1 = Marshal.from_channel ic in
  close_in ic;
  i1#print

let main () =
  if (Sys.argv.(1) = "o") then
    (print_string "Writing out\n";
     testSerialOut ())
  else
    (print_string "Reading in\n";
     testSerialIn ())
  ;
  bar ();
;;

main ();;
