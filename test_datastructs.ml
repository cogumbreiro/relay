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
(** Create instances of data structures and see how much
    mem / time is spent on basic operations *)

open Gc_stats
open Mapset
open Stdutil

module L = Logging

(***************************************************)
(* Commandline handling                            *)

let numItems = ref 10

let numItems2 = ref 10

let setNumItems (i:int) = 
  numItems := i

let setNumItems2 (i:int) = 
  numItems2 := i


let numSets = ref 10

let setNumSets (i:int) = 
  numSets := i

let testDupes = ref false

let testMapset = ref true

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-d", Arg.Set testDupes, "test efficiency when items are duplicated"); 
   ("-i", Arg.Int setNumItems, "number of items in the first set (if two)");
   ("-i2", Arg.Int setNumItems2, "number of items in the second set (if two)");
   ("-s", Arg.Int setNumSets, "number of sets to duplicate");
   ("-noms", Arg.Clear testMapset, "don't test correctness of mapset")]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "[-noms] [-d] [-i <INT>] [-s <INT>]\n"



(***************************************************)
(* Test Structures                                 *)


module OrderedInt = struct
  type t = int
  let compare = Pervasives.compare
end

module IntSet = Set.Make(OrderedInt)

type twoSets = {
  firstSet : IntSet.t;
  secSet : IntSet.t;
}

module IntMap = Map.Make(OrderedInt)

let longString = "The quick brown fox jumped over the lazy dog"

(* Test mapset by making a map from ints to ints *)

type intAttrib = {

  x: int;
  y: int;
}

module IntMS = Mapset.Make(OrderedInt)

let combineIntAttribs {x = x1; y = y1;} {x = x2; y = y2;} = 
  { x = if (x1 > x2) then x1 else x2;
    y = if (y1 > y2) then y1 else y2;
  }


let compIntAttribs {x = x1; y = y1;} {x = x2; y = y2;} =
  if (x1 < x2 && y1 < y2) then
    Some (-1)
  else if (x1 == x2 && y1 == y2) then
    Some (0)
  else if (x1 > x2 && y1 > y2) then
    Some (1)
  else
    None
    

(***************************************************)
(* Test  Functions                                 *)

let printIntMS curMS =
  L.logStatus "{ ";
  IntMS.iter 
    (fun key {x = vx; y = vy;} ->
       L.logStatus ("(" ^ (string_of_int key) ^ ", x=" ^
                       (string_of_int vx) ^ ", y=" ^
                       (string_of_int vy) ^ "), ")
    ) curMS;
  L.logStatus "}\n\n"


let makeIntMS1 (numMappings:int) =
  let rec makeIMSHelper1 (curMS) = function
      0 -> curMS
    | i -> makeIMSHelper1
        (IntMS.add i 
           { x = (numMappings - i);
             y = i; } curMS)
          (i - 1) 
  in
  makeIMSHelper1 IntMS.empty numMappings


let makeIntMS2 (numMappings:int) =
  let rec makeIMSHelper2 (curMS) = function
      0 -> curMS
    | i -> makeIMSHelper2
        (IntMS.add i 
           { x = i;
             y = (numMappings - i); } curMS)
          (i - 1) 
  in
  makeIMSHelper2 IntMS.empty numMappings


let rec makeASet (itemsLeft:int) (curSet) =
  if (itemsLeft <= 0) then
    curSet
  else
    makeASet (itemsLeft - 1) (IntSet.add itemsLeft curSet)


let rec makeListOfSets (setToAdd) (setsLeft:int) (curList) =
  if (setsLeft <= 0) then
    curList
  else
    makeListOfSets (setToAdd) (setsLeft - 1) (setToAdd :: curList)

let rec makeListOfGrowingSets (curSet) (setsLeft:int) (curList) =
  if (setsLeft <= 0) then
    curList
  else
    makeListOfGrowingSets (makeASet !numItems curSet) 
      (setsLeft - 1) (curSet :: curList)



let rec makeMapOfSets (setToAdd) (setsLeft:int) (curMap) =
  if (setsLeft <= 0) then
    curMap
  else
    makeMapOfSets (setToAdd) (setsLeft - 1) 
      (IntMap.add setsLeft setToAdd curMap)

let rec makeMapOfGrowingSets (curSet) (setsLeft:int) (curMap) =
  if (setsLeft <= 0) then
    curMap
  else
    makeMapOfGrowingSets (makeASet !numItems curSet) 
      (setsLeft - 1) (IntMap.add setsLeft curSet curMap)


let rec makeHashOfGrowingSets (curSet) (setsLeft:int) (curHash) =
  if (setsLeft <= 0) then
    curHash
  else
    let _ = Hashtbl.add curHash setsLeft curSet in
    makeHashOfGrowingSets (makeASet !numItems curSet) 
      (setsLeft - 1) curHash



let rec makeListOf (item) (itemsLeft:int) (curList) =
  if (itemsLeft <= 0) then
    curList
  else
    makeListOf (item) (itemsLeft - 1) (item :: curList)


let rec makeHashTblOf (item) (itemsLeft:int) (curHash) =
  if (itemsLeft <= 0) then
    curHash
  else
    let _ = Hashtbl.add curHash itemsLeft item in
    makeHashTblOf (item) (itemsLeft - 1) curHash


let rec makeMapOf (item) (itemsLeft:int) (curMap) =
  if (itemsLeft <= 0) then
    curMap
  else
    makeMapOf item (itemsLeft - 1) 
      (IntMap.add itemsLeft item curMap)

let doDupes () =
  (* ----------- sets of ints ------------ *)
  
  (* Making a map of identical sets will only use extra mem for the Map's
     ptrs, etc.
  *)
  let aSet = makeASet !numItems IntSet.empty in
  let _ = makeMapOf aSet !numSets IntMap.empty in
  ()
  
  (* Same thing if the item is a ref *)
  (*    let aRef = ref aSet in
        let _ = makeMapOf aRef !numSets IntMap.empty in 
  *)
  
  (* List also increases mem usage, but less than Maps *)
  (*    let _ = makeListOf aSet !numSets [] in 
  *)
  
  (* Increases mem usage. Less than maps if the target size is known, 
     more than lists *)
  (*    let _ = makeHashTblOf aSet !numSets (Hashtbl.create 100) in
  *)
  
  (* --------- strings ------------ *)
  
  (* let _ = makeMapOf longString !numSets IntMap.empty in
  *)
  
  (* --------- sets built on previous sets ----------- *)
  (*    let _ = makeListOfGrowingSets IntSet.empty !numSets [] in
  *)
  (*    let _ = makeHashOfGrowingSets IntSet.empty !numSets (Hashtbl.create 100) in
  *)
  
  
let doMapset () = 
  let ms1 = makeIntMS1 !numItems in
  let ms2 = makeIntMS2 !numItems2 in

  let size1 = IntMS.cardinal ms1 in
  let size2 = IntMS.cardinal ms2 in

  L.logStatus ("Size of ms1: " ^ (string_of_int size1) ^ "\n");
  L.logStatus ("Size of ms2: " ^ (string_of_int size2) ^ "\n");

  let u = IntMS.union combineIntAttribs ms1 ms2 in
  let i = IntMS.inter combineIntAttribs ms1 ms2 in
  let d = IntMS.diff ms1 ms2 in
  
  let changed1 = IntMS.addComb combineIntAttribs 10 {x = 20; y = 20;} ms1 in

  let isSubsetUI = IntMS.subset compIntAttribs u i in
  let isSubsetIU = IntMS.subset compIntAttribs i u in

  let isSubsetUC = IntMS.subset compIntAttribs u changed1 in
  let isSubsetCU = IntMS.subset compIntAttribs changed1 u in

  printIntMS ms1;
  printIntMS ms2;
  printIntMS u;
  printIntMS i;
  printIntMS d;
  printIntMS changed1;

  L.logStatus ("U subset I: " ^ (string_of_bool isSubsetUI) ^ "\n");
  L.logStatus ("I subset U: " ^ (string_of_bool isSubsetIU) ^ "\n");
  
  L.logStatus ("U subset C: " ^ (string_of_bool isSubsetUC) ^ "\n");
  L.logStatus ("C subset U: " ^ (string_of_bool isSubsetCU) ^ "\n")


(***************************************************)
(* Execution / Program Entry Point                 *)

let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    if (!testDupes) then
      doDupes()
    ;
    
    if (!testMapset) then
      doMapset()
    ;

    L.logStatus ("Checking mem usage for: (" ^ 
                    (string_of_int !numItems) ^ ", " ^ 
                    (string_of_int !numSets) ^ ")\n");
    L.logStatus "-----\n";


    printStatistics ();
    exit 0;
  with e -> Printf.printf "Exc. in Test Datastructs: %s\n"
    (Printexc.to_string e) ; 
    printStatistics ();
    raise e
;;
main () ;;
