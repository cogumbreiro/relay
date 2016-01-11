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

(** Test some random Ocaml stuff *)

class a x = object
  val mutable i = x

  method getI (_:unit) = 
    i

  method add (y:int) =
    i <- i + y
      
end;;


let o1 = new a 1;;

let o2 = new a (o1#getI());;

o2#add 7;;
o1#getI ();;
o2#getI ();;


(** Test if changing a mutable struct while it is in a Map or Set
 *  updates the object that is in the Map / Set
 *)

type thing = {
  mutable i : int;
  mutable s: string;
};;


module OrderedThing = 
struct
  type t = thing
  let compare t1 t2 = Pervasives.compare t1.i t2.i
end;;


module OrderedInt = 
struct
  type t = int
  let compare = Pervasives.compare
end;;



module TM = Map.Make(OrderedInt);;

module TS = Set.Make(OrderedThing);;

type tMap = thing TM.t;;

type tSet = TS.t;;


let thing1 = {i = 1;
              s = "thing1";}

let thing2 = {i = 2;
              s = "thing2";}

let aMap = TM.add 1 thing1 TM.empty;;


let aSet = TS.add thing2 TS.empty;;


let examine (m:tMap) (s:tSet) =
  TM.iter (fun k v -> print_string ("Map key: " ^ (string_of_int k) ^ 
                                      " value: " ^ (string_of_int v.i) ^ ";" 
                                    ^ v.s ^ "\n")) m;
  TS.iter (fun v -> 
             print_string ("Set value: " ^ (string_of_int v.i) ^ ";" 
                           ^ v.s ^ "\n")) s;;


let up (i:int) (m:tMap) (s:tSet) =
  let v1 = TM.find 1 m in
  v1.s <- ("mod" ^ (string_of_int i)) ;
  thing2.s <- ("mod" ^ (string_of_int i));
  examine m s;;




up 2 aMap aSet;;

thing2.i <- 3;;
  
up 3 aMap aSet;;

thing2.i <- 4;;

examine aMap aSet;;

TS.mem {i = 4; s = "mod3";} aSet;;

(** Conclusion: yes it does update the Map/Set *)


(** Testing module types *)

module type COMPAREABLE = 
sig 
  type t
  class c : t -> 
  object ('a)
    val repr : t
    method value : t
    method compare : 'a -> int
  end
end;;

module Int : COMPAREABLE = 
struct
  type t = int
  class c (x : int) =
  object (self : 'a)
    val repr = x
    method value = repr
    method compare (other:'a) = (repr - other#value)
  end
end;;


module Float : COMPAREABLE = 
struct
  type t = float
  class c (x : float) =
  object (self : 'a) 
    val repr = x
    method value = repr
    method compare (other:'a) = int_of_float (repr -. other#value)
  end
end;;

(* Doesn't work... *)

let i1 = new Int.c 10;;
let i2 = new Int.c 20;;

let f1 = new Float.c 1.111;;
let f2 = new Float.c 2.222;;


i1#compare i2;;
i2#compare i1;;

f1#compare f2;;
f2#compare f2;;

i1#compare f1;;


