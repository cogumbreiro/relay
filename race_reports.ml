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

(** Module describing the structure of warning reports for 
    data race violations *)

open Cil
open Fstructs
open Stdutil
open Warn_reports


module LS = Lockset
module GA = Guarded_access
module Du = Cildump
module L = Logging

module A = Alias
module Lv = Lvals
module CLv = Cil_lvals
module D = Cildump

let maxWarningsPerLoc = 5

let maxDiffString = (string_of_int (maxWarningsPerLoc -1))

(*************** types, etc. ********)

type threadRootLoc = Cil.location * string * fKey

type raceInfo = {
  access : GA.accessInfo;
  lval : Lv.aLval;
  imprec : Lv.imprecision;
  locks : LS.fullLS;
  emptied : Cil.location;
  threadRoot : threadRootLoc;
}

type raceRep = (raceInfo * raceInfo) list

module LocHash = Hashtbl.Make(
  struct
    type t = (Cil.location * Cil.location)
    let equal (l1,l2) (l3,l4) = 
      (Cil.compareLoc l1 l3 == 0 &&
         Cil.compareLoc l2 l4 == 0)  ||
        (Cil.compareLoc l1 l4 == 0 && 
           Cil.compareLoc l2 l3 == 0)
(*
    let hash (a,b) = (Hashtbl.hash a) lxor (Hashtbl.hash b)
*)
    let hash = Hashtbl.hash
  end
)

(******** Hash-consing / Distillation *****)

(** Hash-cons the key for a report *)
let hashRepKey (access1, access2) =
  CLv.distillLoc access1, CLv.distillLoc access2


let hashLS ls = 
  LS.LS.unique ls


let hashRaceInfo ri =
  { ri with 
      access = GA.hcAccesses ri.access;
      lval = Lv.mergeLv ri.lval;
      locks = hashLS ri.locks;
      emptied = CLv.distillLoc ri.emptied;
      threadRoot = 
      let l, fn, fk = ri.threadRoot in
      (CLv.distillLoc l, Strutil.addString fn, fk);
  }


let listsEq eq l1 l2 =
  try List.for_all2 (fun e1 e2 -> eq e1 e2) l1 l2
  with Invalid_argument _ -> false

let locksEq =
  LS.LS.equal

let rootsEq (l1, fn1, fk1) (l2, fn2, fk2) =
  fk1 == fk2 && compareLoc l1 l2 == 0

let eqRaceInfo ri1 ri2 =
  ri1.imprec = ri2.imprec &&
  rootsEq ri1.threadRoot ri2.threadRoot &&
  Lv.compare_lval ri1.lval ri2.lval == 0 &&
  Cil.compareLoc ri1.emptied ri2.emptied == 0 &&
  locksEq ri1.locks ri2.locks &&
  GA.eqAccesses ri1.access ri2.access


let eqOneReport (ri1, ri2) (ri1', ri2') =
  eqRaceInfo ri1 ri1' &&
    eqRaceInfo ri2 ri2'


let hashWarnData (ri1, ri2) =
  hashRaceInfo ri1, hashRaceInfo ri2


(** Hash-cons the data from a report *)
let hashReport rep =
  List.map (fun d -> hashWarnData d) rep

   
(************** Add warnings to report table *************)


(** Combine the list of race infos *)
let joinAReport rl1 rl2 =
  let rec fromL2 l2 left =
    if (left <= 0) then
      []
    else match l2 with
      [] ->
        l2
    | newR as hd :: tl ->
        if (List.exists (fun oldR -> eqOneReport newR oldR) rl1) then
          fromL2 tl left
        else
          hd :: fromL2 tl (left - 1)
  in
  rl1 @ (fromL2 rl2 (List.length rl1 - maxWarningsPerLoc))

    
(** Combine the two report tables (into the first table) *)
let joinReports reps1 reps2 =
  LocHash.iter 
    (fun k rep ->
       try
         let oldRep = LocHash.find reps1 k  in
         let newRep = joinAReport oldRep rep in
         LocHash.replace reps1 k newRep
       with Not_found ->
         let newK = hashRepKey k in
         let newRep = hashReport rep in
         LocHash.add reps1 newK newRep
    ) reps2


    
(*************** printing ***********)


let printLocks ls =
  let locked = LS.LS.getPlus ls in
  if (LS.LS.S.is_empty locked) then
    L.logStatus "L+ = empty;"
  else (
    L.logStatus "L+ = {";
    LS.LS.S.iter (fun lv l -> 
                    L.logStatus ((LS.string_of_lock lv l) ^ ", ")
                 ) locked;
    L.logStatus ("} (" ^ (string_of_int 
                            (LS.LS.S.cardinal locked))  ^  ")");
  )


let isLsEmpty ls =
  match ls with
    [] -> true
  | _ -> false


let printThreadRoot (tr, tf, tk) i =
  L.logStatus ("\tTh. " ^ (string_of_int i) ^ " spawned: " ^ 
                 (Du.string_of_loc tr) ^ 
                 " w/ func: " ^ tf)


let printDiffs (fri1, fri2) (ri1, ri2) =
  let ls1Eq = locksEq fri1.locks ri1.locks in
  if (not ls1Eq) then
    (L.logStatus ("LS for 1st access:");
     printLocks ri1.locks);
  if (Cil.compareLoc fri1.emptied ri1.emptied != 0) then 
    L.logStatus ("\tmade empty at: " ^ 
                   (Du.string_of_loc ri1.emptied))
  ;
  if (Lv.compare_lval fri1.lval ri1.lval != 0) then 
    L.logStatus ("\tlval 1: " ^ (Lv.string_of_lval ri1.lval))
  ;
  let ls2Eq = locksEq fri2.locks ri2.locks in
  if (not ls2Eq) then
    (L.logStatus ("LS for 2nd access:");
     printLocks ri2.locks);
  if (Cil.compareLoc fri2.emptied ri2.emptied != 0) then 
    L.logStatus ("\tmade empty at: " ^ 
                   (Du.string_of_loc ri2.emptied))
  ;
  if (Lv.compare_lval fri2.lval ri2.lval != 0) then 
    L.logStatus ("\tlval 2: " ^ (Lv.string_of_lval ri2.lval))
  ;
  (* TODO check if different too *)
  printThreadRoot ri1.threadRoot 1;
  printThreadRoot ri2.threadRoot 2


let printImprecision imp = 
  L.logStatus ("\tConfidence: " ^ 
                 (match imp with
                    Lv.Syntactic -> "no PTA nodes"
                  | Lv.PtsToSame (a, b) -> "used PTA nodes (" ^ 
                      (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")"
                  | Lv.SameType -> "based on types"))


(** Print out possible races... sorted *)
let printRaces races =
  let flattened = LocHash.fold 
    (fun locs rp cur -> (locs, rp) :: cur) races [] in
  let sorted = List.sort 
    (fun ((loc1, loc1'), rp1) ((loc2, loc2'), rp2) ->
       match (rp1, rp2) with
         (ri1, ri1') :: _ , (ri2, ri2') :: _ ->
           let c = Lv.compare_imprec ri1.imprec ri2.imprec in
           if (c <> 0) then c            
           else let c = Lv.compare_imprec ri1'.imprec ri2'.imprec in
           if (c <> 0) then c
           else let c = Lv.compare_lval ri1.lval ri2.lval in
           if (c <> 0) then c 
           else let c = Lv.compare_lval ri1'.lval ri2'.lval in
           if (c <> 0) then c
           else let c = Cil.compareLoc loc1 loc2 in
           if (c <> 0) then c
           else Cil.compareLoc loc1' loc2'
       | _ -> 
           let c = Cil.compareLoc loc1 loc2 in
           if (c <> 0) then c
           else Cil.compareLoc loc1' loc2') flattened in
  
  let numWarnings = List.fold_left
    (fun wcount ((loc1, loc2), rep) ->
       match rep with
          (ri1, ri2) as first :: tl ->
            L.logStatus ("****\nPossible race between access to:\n" ^
                           (Lv.string_of_lval_decl ri1.lval) ^ " and\n" ^
                           (Lv.string_of_lval_decl ri2.lval));
            L.logStatus ("\tAccessed at locs:\n\t" ^ 
                           (GA.string_of_accesses ri1.access) ^ " and\n\t" ^
                           (GA.string_of_accesses ri2.access) ^ "\n");
            printImprecision ri1.imprec; (* first is same as second *)
            L.logStatus "";
            L.logStatus ("LS for 1st access:");
            printLocks ri1.locks;
            L.logStatus ("\tmade empty at: " ^ 
                           (Du.string_of_loc ri1.emptied));
            L.logStatus ("LS for 2nd access:");
            printLocks ri2.locks;
            L.logStatus ("\tmade empty at: " ^ 
                           (Du.string_of_loc ri2.emptied));
            printThreadRoot ri1.threadRoot 1;
            printThreadRoot ri2.threadRoot 2;
            L.logStatus "";
            L.logStatus ("Different possible paths & LS (first " 
                         ^ maxDiffString ^ "):\n");
            let _ = List.fold_left
              (fun i next ->
                 L.logStatus ("(" ^ (string_of_int i) ^ ")");
                 printDiffs first next;
                 i + 1
              ) 0 tl in
            wcount + 1
       | _ ->
           wcount
    ) 0 sorted in
  L.logStatus ("\n\n$$$$$$\nTotal Warnings: " ^ 
                 (string_of_int numWarnings))


    
(*************** Print to XML *****************)

open Pretty

class racesXMLPrinter = object (self) 
  inherit LS.locksXMLPrinter

  (* TODO not very pretty right now (alignment, indentation)... make pretty *)

  method pCallpath emp (tr, tf, tk) (edges: (int * int) list) =
    indent 1 
      (text ("<cp root=\"" ^ (string_of_int tk) ^ "\">") ++ line ++
         indent 1 (text "<spawned_at>" ++ 
                     self#pLoc () (tr, None) ++ 
                     text "</spawned_at>" ++ line) ++
         (if (CLv.locIsUnknown emp) then nil
          else (indent 1 
                  (text "<empty_at>" ++ 
                     (self#pLoc () (emp, None))) ++ 
                  text "</empty_at>" ++ line)
         ) ++
         indent 1 
         (List.fold_left
            (fun doc (f1, f2) -> 
               doc ++ 
                 text ("<call_edge caller=\"" ^ (string_of_int f1) ^ 
                       "\" callee=\"" ^ (string_of_int f2) ^ "\"></call_edge>")
            ) nil edges) ++
         text "</cp>") ++ line
      
  method pAccess num acc =
    let blob = match acc.imprec with 
        Lv.PtsToSame (a, b) -> if (num == 1) then Some(a) else Some(b)
      | Lv.Syntactic -> None
      | Lv.SameType -> Some (66666) in (* TODO use a tag *)
    let accTag = "acc" ^ (string_of_int num) in
    text ("<" ^ accTag ^ ">") ++ line ++
      (self#pALval (acc.lval, blob)) ++
      (GA.Locs.fold 
         (fun (k, loc) doc -> 
            doc ++ self#pLoc () (loc, Some(k))) acc.access nil) ++
      (self#pLocks () acc.locks) ++
      (self#pCallpath acc.emptied acc.threadRoot []) ++
      text ("</" ^ accTag ^ ">") ++ line

  method pRaces () races =
    let doc = text "<?xml version=\"1.0\"?>" ++ line ++ text "<run>" ++ line in
    let doc = LocHash.fold
      (fun (loc1, loc2) rep doc ->
         doc ++ text "<cluster>" ++ line ++
           List.fold_left
           (fun doc (ri1, ri2) ->
              (doc ++ text "<race>" ++ line) ++
                (self#pAccess 1 ri1 ++ self#pAccess 2 ri2) ++
                (text "</race>" ++ line)
           ) nil rep ++
           text "</cluster>" ++ line
      ) races doc
    in
    doc ++ text "</run>" ++ line

end




(************************ Statistics **********************)

module LVH = Lv.LvalHash


let getFreq select getCount setCount foldFreqs races =
  LocHash.iter
    (fun (ac1, ac2) lvL ->
       let uniq = select (ac1, ac2) lvL in
       List.iter (fun u ->
                    let oldCount = try getCount u with Not_found -> 0 in
                    setCount u (oldCount + 1)
                 ) uniq
    ) races ;
  let list = foldFreqs (fun x f l -> (x, f) :: l) in
  List.sort (fun (_, f1) (_, f2) -> f2 - f1) list



let printLvalFreq races =
  let lvFreq = LVH.create 107 in
  let lSorted = getFreq 
    (fun _ reps ->
       List.fold_left 
         (fun (uniqLvs) (ri1, ri2) ->
            let ulv1 = addOnceP 
              (fun lv1 lv2 -> Lv.compare_lval lv1 lv2 == 0) 
              uniqLvs ri1.lval in
            addOnceP 
              (fun lv1 lv2 -> Lv.compare_lval lv1 lv2 == 0) ulv1 ri2.lval
         ) [] reps ) 
    (fun lv -> LVH.find lvFreq lv)
    (fun lv newCount -> LVH.replace lvFreq lv newCount )
    (fun f -> LVH.fold f lvFreq [] ) 
    races in
  L.logStatus "\n\nLval frequency\n===========================";
  List.iter (fun (lv, f) ->
               L.logStatus ((Lv.string_of_lval lv) ^ " :: " ^
                              (string_of_int f))) lSorted


let string_of_basev lv =
  try
    let v = Lv.findBaseVarinfoLval lv in
    (v.Cil.vname) ^ " @ " ^ (Du.string_of_loc v.Cil.vdecl)
  with CLv.BaseVINotFound ->
    ""


let printBaseVarFreq races =
  let lvFreq = Hashtbl.create 107 in
  let lSorted = getFreq 
    (fun _ reps ->
       List.fold_left 
         (fun (uniqVars) (ri1, ri2) ->
              let u1 = addOnceP (=) uniqVars (string_of_basev ri1.lval) in
              addOnceP (=) u1 (string_of_basev ri2.lval)
         ) [] reps )
    (fun lv -> Hashtbl.find lvFreq lv)
    (fun lv newCount -> Hashtbl.replace lvFreq lv newCount )
    (fun f -> Hashtbl.fold f lvFreq [] ) 
    races in
  L.logStatus "\n\nBase var frequency\n===========================";
  List.iter (fun (v, f) ->
               if (v <> "") then
                 L.logStatus (v ^ " :: " ^ (string_of_int f))) lSorted
    

let printAccessLocFreq races =
  let locFreq = Hashtbl.create 107 in
  let lSorted = getFreq
    (fun (l1, l2) _ ->
       addOnceP 
         (fun l1 l2 -> Cil.compareLoc l1 l2 == 0) [l1] l2)
    (fun loc -> Hashtbl.find locFreq loc)
    (fun loc newCount -> Hashtbl.replace locFreq loc newCount)
    (fun f -> Hashtbl.fold f locFreq [])
    races in
  L.logStatus "\n\nAccess location frequency\n===========================";
  List.iter (fun (l, f) -> 
               L.logStatus ((D.string_of_loc l) ^ " :: " ^
                              (string_of_int f))) lSorted


let printRootFreq races =
  let rootFreq = Hashtbl.create 107 in
  let rSorted = getFreq
    (fun (_, _) rep ->
       List.fold_left 
         (fun uniqRoots (ri1, ri2) ->
              let ur1 = addOnceP (=) uniqRoots ri1.threadRoot in
              addOnceP (=) ur1 ri2.threadRoot
         ) [] rep)
    (fun root -> Hashtbl.find rootFreq root)
    (fun root newCount -> Hashtbl.replace rootFreq root newCount)
    (fun f -> Hashtbl.fold f rootFreq []) 
    races in
  L.logStatus "\n\nThread root frequency\n===========================";
  List.iter (fun ((trLoc, trFun, _), f) -> 
               L.logStatus ((D.string_of_loc trLoc) ^ ", " ^ 
                              trFun ^ " :: " ^
                              (string_of_int f))) rSorted




(************************************************************
         Interface
 ************************************************************)

type raceTable = raceRep LocHash.t


class raceReports ?(initial = LocHash.create 137) () = object (self : 'a)

  val mutable curRaces : raceTable = initial

  method data = curRaces

  method joinReports (other : 'a) = 
    let rep2 = other#data in
    joinReports curRaces rep2

  method printWarnings =
    printRaces curRaces

  val xmlp = new racesXMLPrinter

  method serialize (fname:string) =
    let writeSelf oc =
      Marshal.to_channel oc curRaces [Marshal.Closures]
    in
    open_out_bin_for fname writeSelf

  method deserialize (fname:string) =
    let doLoad ic =
      Marshal.from_channel ic
    in
    curRaces <- open_in_bin_for fname doLoad


  method saveToXML (fname:string) = 
    let doWrite oc =
      let doc = xmlp#pRaces () curRaces in
      fprint oc 80 doc;
    in
    open_out_for fname doWrite


  (** Add a report to the set of curRaces *)
  method addRace ({ access = access1; }, { access = access2;} as newR) =
    let l1, l2 = GA.firstLocation access1, GA.firstLocation access2 in
    try
      let oldReport = LocHash.find curRaces (l1, l2) in
      if (List.exists (fun oldR -> eqOneReport newR oldR) oldReport) then
        ()
      else if (List.length oldReport < maxWarningsPerLoc) then
        let newR = hashWarnData newR in
        LocHash.replace curRaces (l1, l2) (newR :: oldReport)
    with Not_found ->
      L.logStatus "New race added to race reports";
      L.flushStatus ();
      let newR = hashWarnData newR in
      LocHash.add curRaces (l1, l2) [ newR ]


  (** frequency of lval appearance, thread root appearance, etc. *)
  method printStats =
    printLvalFreq curRaces;
    printBaseVarFreq curRaces;
    printAccessLocFreq curRaces;
    printRootFreq curRaces
    



end


let loadRaces fname : raceReports = 
  let result = new raceReports () in
  result#deserialize fname;
  result
