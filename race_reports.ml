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
    data race violations. TODO: refactor so that pseudo_access info 
    is separate / use the warn_reports functor *)

open Cil
open Cildump
open Logging
open Fstructs
open Warn_reports

module LS = Lockset
module Accs = Access_info

module A = Alias
module Lv = Lvals
module CLv = Cil_lvals

let maxWarningsPerLoc = 5

let maxDiffString = (string_of_int (maxWarningsPerLoc - 1))

(*************** types, etc. ********)

(** Where a thread was created, and which function was run *)
type threadRootLoc = location * string * Callg.funID

(** One access that is part of a race, and how it was reached *)
type raceInfo = {
  access : Accs.accessInfo;
  lval : Lv.aLval;
  imprec : Lv.imprecision;
  locks : LS.fullLS;
  emptied : location;
  threadRoot : threadRootLoc;
  threadEsc : bool;
}

(** Id for the cluster (unique for a given run) *)
type cluster_id = int

let cur_cluster_id = ref 0

let newClusterId () =
  let id = !cur_cluster_id in
  incr cur_cluster_id;
  id

(** Races that are similar *)
type raceRep = cluster_id * ((raceInfo * raceInfo) list)

let eqLocation l1 l2 =
  compareLoc l1 l2 == 0

(** Hashtable that clusters races by access location *)
module LocHash = Hashtbl.Make(
  struct
    type t = (location * location)
    let equal (l1,l2) (l3,l4) = 
      (eqLocation l1 l3 && eqLocation l2 l4)  ||
        (eqLocation l1 l4 && eqLocation l2 l3)
    let hash (a,b) = (Hashtbl.hash a) lxor (Hashtbl.hash b)
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
      access = Accs.hcAccesses ri.access;
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

let locksEq ls1 ls2 =
  LS.LS.equal ls1 ls2

let rootsEq (l1, fn1, fk1) (l2, fn2, fk2) =
  (* Don't care much about which context the root comes from ?
     TODO: OIC doesn't understand thread creation for now,
     so they should be equivalent to the "0th" context *)
  let fkey1 = Callg.fid_to_fkey fk1 in
  let fkey2 = Callg.fid_to_fkey fk2 in
  (fkey1 = fkey2) && eqLocation l1 l2

let eqRaceInfo ri1 ri2 =
  ri1.imprec = ri2.imprec &&
  rootsEq ri1.threadRoot ri2.threadRoot &&
  Lv.compare_lval ri1.lval ri2.lval == 0 &&
  eqLocation ri1.emptied ri2.emptied &&
  locksEq ri1.locks ri2.locks &&
  Accs.eqAccesses ri1.access ri2.access &&
  ri1.threadEsc = ri2.threadEsc 


let eqOneReport (ri1, ri2) (ri1', ri2') =
  (eqRaceInfo ri1 ri1' && eqRaceInfo ri2 ri2') ||
    (eqRaceInfo ri1 ri2' && eqRaceInfo ri2 ri1')

let hashWarnData (ri1, ri2) =
  hashRaceInfo ri1, hashRaceInfo ri2


(** Hash-cons the data from a report *)
let hashReport rep =
  List.map (fun d -> hashWarnData d) rep

   
(************** Add warnings to report table *************)


(** Combine the list of race infos *)
let joinAReport rl1 rl2 =
  let rec fromL2 l2 left =
    if (left <= 0) then []
    else match l2 with
      [] -> l2
    | newR as hd :: tl ->
        if (List.exists (fun oldR -> eqOneReport newR oldR) rl1) then
          fromL2 tl left
        else
          hd :: fromL2 tl (left - 1)
  in
  rl1 @ (fromL2 rl2 (maxWarningsPerLoc - List.length rl1))

    
(** Combine the two report tables (into the first table, so use IDs that
    are unique to the first table) *)
let joinReports reps1 reps2 =
  LocHash.iter 
    (fun k (_, rep) ->
       try
         let id1, oldRep = LocHash.find reps1 k  in
         let newRep = joinAReport oldRep rep in
         LocHash.replace reps1 k (id1, newRep)
       with Not_found ->
         let newK = hashRepKey k in
         let newRep = hashReport rep in
         let newID = newClusterId () in
         LocHash.add reps1 newK (newID, newRep)
    ) reps2

    
(*************** printing ***********)


let printLocks ls =
  let locked = LS.LS.getPlus ls in
  let buff = Buffer.create 16 in
  if (LS.LS.S.is_empty locked) then
    Buffer.add_string buff "L+ = empty;\n"
  else (
    Buffer.add_string buff "L+ = ";
    LS.set_to_buf buff locked;
  );
  if LS.hasSummaryLock ls then Buffer.add_string buff "\t(rep. lock)\n";
  logStatusB buff;
  Buffer.clear buff

let printThreadRoot (tr, tf, tk) i =
  logStatusF "\tTh. %d spawned: %s %d w/ func: %s\n" 
    i (string_of_loc tr) tr.byte tf

let printEmptiedLS loc = 
  if eqLocation loc locUnknown then ()
  else logStatusF "\tmade empty at: %s\n" (string_of_loc loc)


(** Print any differences between the first pair or report infos
    [(fri1, fri2)] and subsequent pairs of report infos *)
let printDiffs (fri1, fri2) (ri1, ri2) =
  let ls1Eq = locksEq fri1.locks ri1.locks in
  if (not ls1Eq) then
    (logStatus ("LS for 1st access:");
     printLocks ri1.locks);
  if not (eqLocation fri1.emptied ri1.emptied) then 
    printEmptiedLS ri1.emptied
  ;
  if (Lv.compare_lval fri1.lval ri1.lval != 0) then 
    logStatus ("\tlval 1: " ^ (Lv.string_of_lval ri1.lval))
  ;
  let ls2Eq = locksEq fri2.locks ri2.locks in
  if (not ls2Eq) then
    (logStatus ("LS for 2nd access:");
     printLocks ri2.locks);
  if not (eqLocation fri2.emptied ri2.emptied) then 
    printEmptiedLS ri2.emptied
  ;
  if (Lv.compare_lval fri2.lval ri2.lval != 0) then 
    logStatus ("\tlval 2: " ^ (Lv.string_of_lval ri2.lval))
  ;
  (* TODO check if different too *)
  printThreadRoot ri1.threadRoot 1;
  printThreadRoot ri2.threadRoot 2


let printImprecision imp = 
  logStatus ("\tConfidence: " ^ (Lv.string_of_imp imp))

let compare_esc esc1 esc2 =
  match esc1, esc2 with
    true, true -> 0
  | true, _ -> -1
  | _ -> 1

let compare_locks ls1 ls2 =
  if LS.hasSummaryLock ls1 then
    if LS.hasSummaryLock ls2 then 0 else -1
  else if LS.hasSummaryLock ls2 then 1 else 0

(** Compare race report entries for sorting  *)
let compare_reports ((loc1, loc1'), id1, rp1) ((loc2, loc2'), id2, rp2) = 
  match (rp1, rp2) with
    (ri1, ri1') :: _ , (ri2, ri2') :: _ ->
      let c = compare_esc ri1.threadEsc ri2.threadEsc in
      if (c <> 0) then c            
      else let c = compare_esc ri1'.threadEsc ri2'.threadEsc in
      if (c <> 0) then c
      else let c = Lv.compare_imprec ri1.imprec ri2.imprec in
      if (c <> 0) then c            
      else let c = Lv.compare_imprec ri1'.imprec ri2'.imprec in
      if (c <> 0) then c
      else let c = compare_locks ri1.locks ri2.locks in
      if (c <> 0) then c
      else let c = Lv.compare_lval ri1.lval ri2.lval in
      if (c <> 0) then c 
      else let c = Lv.compare_lval ri1'.lval ri2'.lval in
      if (c <> 0) then c
      else let c = compareLoc loc1 loc2 in
      if (c <> 0) then c
      else compareLoc loc1' loc2'
  | _ -> 
      let c = compareLoc loc1 loc2 in
      if (c <> 0) then c
      else compareLoc loc1' loc2'


let sortRaces races =
  let flattened = LocHash.fold 
    (fun locs (id, rp) cur -> (locs, id, rp) :: cur) races [] in
  List.sort compare_reports flattened

let printRaceRep rep id =
  match rep with
    (ri1, ri2) as first :: tl ->
      logStatusF "%s and\n%s\n"
        (Lv.string_of_lval_decl ri1.lval) (Lv.string_of_lval_decl ri2.lval);
      logStatusF "\tCluster ID: %d\n" id;
      logStatusF "\tEscapes? %b / %b \n" ri1.threadEsc ri2.threadEsc;
      logStatus ("\tAccessed at locs:\n\t" ^ 
                   (Accs.string_of_accesses ri1.access) ^ " and\n\t" ^
                   (Accs.string_of_accesses ri2.access) ^ "\n");
      printImprecision ri1.imprec; (* first is same as second *)
      logStatus "";
      logStatus ("LS for 1st access:");
      printLocks ri1.locks;
      printEmptiedLS ri1.emptied;
      logStatus ("LS for 2nd access:");
      printLocks ri2.locks;
      printEmptiedLS ri2.emptied;
      printThreadRoot ri1.threadRoot 1;
      printThreadRoot ri2.threadRoot 2;
      logStatus "";
      logStatus ("Different possible paths & LS (first " 
                 ^ maxDiffString ^ "):\n");
      let _ = List.fold_left
        (fun i next ->
           logStatus ("(" ^ (string_of_int i) ^ ")");
           printDiffs first next;
           i + 1
        ) 0 tl in
      ()
  | [] -> ()

(** Print out possible races... sorted *)
let printRaces races =
  logStatus "Sorting races\n";
  flushStatus ();
  let races = sortRaces races in
  logStatus "Printing races\n";
  let numWarnings = List.fold_left
    (fun wcount ((loc1, loc2), id, rep) ->
       logStatus "****\nPossible race between access to:\n";
       printRaceRep rep id ;
       wcount + 1
    ) 0 races in
  logStatus ("\n\n$$$$$$\nTotal Warnings: " ^ 
                 (string_of_int numWarnings))


    
(*************** Print to XML *****************)

open Pretty

class racesXMLPrinter = object (self) 
  inherit LS.locksXMLPrinter

  (* TODO not very pretty right now (alignment, indentation)... make pretty *)

  method pCallpath emp (tr, tf, tk) (edges: (int * int) list) =
    indent 1 
      (text ("<cp root=\"" ^ (Callg.fid_to_string tk) ^ "\">") ++ line ++
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
   
  method grabImpSize num (a, b) =
    if (num == 1) then Some(a) else Some(b)

  method pAccess num acc =
    let blob = match acc.imprec with 
        Lv.PtsToSame (a, b) 
      | Lv.RepsSame (a, b)
      | Lv.PtsToHostL (a, b)
      | Lv.PtsToHostR (a, b) -> self#grabImpSize num (a, b)
      | Lv.Syntactic -> None
      | Lv.SameType -> Some (66666) (* TODO separate cases  *)
    in 
    let accTag = "acc" ^ (string_of_int num) in
    text ("<" ^ accTag ^ ">") ++ line ++
      (self#pALval (acc.lval, blob)) ++
      (Accs.Locs.fold 
         (fun (k, loc) doc -> 
            doc ++ self#pLoc () (loc, Some(k))) acc.access nil) ++
      (self#pLocks () acc.locks) ++
      (self#pCallpath acc.emptied acc.threadRoot []) ++
      text ("</" ^ accTag ^ ">") ++ line

  method pRaceInfo (ri1, ri2) =
    (text "<race>" ++ line) ++ 
      (self#pAccess 1 ri1 ++ self#pAccess 2 ri2) ++
      (text "</race>" ++ line)
      

  method pRaces oc races =
    fprint oc 80 
      (text "<?xml version=\"1.0\"?>" ++ line ++ text "<run>" ++ line);
    LocHash.iter
      (fun (loc1, loc2) (id, rep) ->
         let doc = 
           dprintf "<cluster id=\"%d\">" id ++ line ++
             indent 1 (List.fold_left
                         (fun doc riPair ->
                            doc ++ self#pRaceInfo riPair
                         ) nil rep) ++
             text "</cluster>" ++ line in
         fprint oc 80 doc
      ) races;
    fprint oc 80 (text "</run>" ++ line ++ text "</xml>")

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
    (fun _ (id, reps) ->
       List.fold_left 
         (fun (uniqLvs) (ri1, ri2) ->
            let ulv1 = List_utils.addOnceP 
              (fun lv1 lv2 -> Lv.compare_lval lv1 lv2 == 0) 
              uniqLvs ri1.lval in
            List_utils.addOnceP 
              (fun lv1 lv2 -> Lv.compare_lval lv1 lv2 == 0) ulv1 ri2.lval
         ) [] reps ) 
    (fun lv -> LVH.find lvFreq lv)
    (fun lv newCount -> LVH.replace lvFreq lv newCount )
    (fun f -> LVH.fold f lvFreq [] ) 
    races in
  logStatus "\n\nLval frequency\n===========================";
  List.iter (fun (lv, f) ->
               logStatus ((Lv.string_of_lval lv) ^ " :: " ^
                              (string_of_int f))) lSorted


let string_of_basev lv =
  try
    let v = Lv.findBaseVarinfoLval lv in
    (v.vname) ^ " @ " ^ (string_of_loc v.vdecl)
  with CLv.BaseVINotFound ->
    ""


let printBaseVarFreq races =
  let lvFreq = Hashtbl.create 107 in
  let lSorted = getFreq 
    (fun _ (id, reps) ->
       List.fold_left 
         (fun (uniqVars) (ri1, ri2) ->
              let u1 = 
                List_utils.addOnceP (=) uniqVars (string_of_basev ri1.lval) in
              List_utils.addOnceP (=) u1 (string_of_basev ri2.lval)
         ) [] reps )
    (fun lv -> Hashtbl.find lvFreq lv)
    (fun lv newCount -> Hashtbl.replace lvFreq lv newCount )
    (fun f -> Hashtbl.fold f lvFreq [] ) 
    races in
  logStatus "\n\nBase var frequency\n===========================";
  List.iter (fun (v, f) ->
               if (v <> "") then
                 logStatus (v ^ " :: " ^ (string_of_int f))) lSorted
    

let printAccessLocFreq races =
  let locFreq = Hashtbl.create 107 in
  let lSorted = getFreq
    (fun (l1, l2) _ ->
       List_utils.addOnceP eqLocation [l1] l2)
    (fun loc -> Hashtbl.find locFreq loc)
    (fun loc newCount -> Hashtbl.replace locFreq loc newCount)
    (fun f -> Hashtbl.fold f locFreq [])
    races in
  logStatus "\n\nAccess location frequency\n===========================";
  List.iter (fun (l, f) -> 
               logStatus ((string_of_loc l) ^ " :: " ^
                              (string_of_int f))) lSorted


let printRootFreq races =
  let rootFreq = Hashtbl.create 107 in
  let rSorted = getFreq
    (fun (_, _) (id, rep) ->
       List.fold_left 
         (fun uniqRoots (ri1, ri2) ->
              let ur1 = List_utils.addOnceP (=) uniqRoots ri1.threadRoot in
              List_utils.addOnceP (=) ur1 ri2.threadRoot
         ) [] rep)
    (fun root -> Hashtbl.find rootFreq root)
    (fun root newCount -> Hashtbl.replace rootFreq root newCount)
    (fun f -> Hashtbl.fold f rootFreq []) 
    races in
  logStatus "\n\nThread root frequency\n===========================";
  List.iter (fun ((trLoc, trFun, _), f) -> 
               logStatus ((string_of_loc trLoc) ^ ", " ^ 
                              trFun ^ " :: " ^
                              (string_of_int f))) rSorted


(************************************************************)

(* TODO: refactor this stuff... *)

module AliasAssumptions = struct

  (** If there are a set of possible queries, randomly pick one? *)
  let randomize = ref true

  exception NoLvals
  exception UnknownPtrExp
  exception NoDeref

  let rec ptrOfExp exp =
    match exp with
      Lv.CLval lv -> lv
    | Lv.CBinOp (op, e1, e2, _) ->
        (match op with
           PlusPI
         | IndexPI
         | MinusPI -> ptrOfExp e1
         | _ -> raise UnknownPtrExp )
    | Lv.CCastE (_, e1) -> ptrOfExp e1
    | _ -> raise UnknownPtrExp


  let varPrefix var =
    let scopePrefix = match Scope.decipherScope var with
      Scope.SGlobal 
    | Scope.SFunc -> (* TODO: make the distinction in a centralized place *)
        if Trans_alloc.isAllocVar var.vname then "l:" 
        else "g:" 
    | Scope.SFormal _
    | Scope.STBD -> "l:" in
    scopePrefix ^ (string_of_int var.vid) ^ ":"


  let pickLval lvs =
    match List_utils.pickFromList lvs !randomize with
      None -> raise NoLvals
    | Some x -> x


  (* TODO: when AA becomes field sensitive, may want queries to take into
     account the offset *)
  let rec convLval (curLval:Lv.aLval) curStr : (varinfo * string) = 
    match curLval with
      Lv.AbsHost n, off ->
        (* Get first label from it *)
        let lvs = A.Abs.represents n in
        let lv = pickLval lvs in
        convLval (Lv.abs_of_lval lv) curStr
    | Lv.CVar vi, off ->
        let vi = Lv.var_of_abs vi in
        (vi, (curStr ^ vi.vname))
    | Lv.CMem exp, off ->
        let lv = ptrOfExp exp in (* ... offset may determine ptr, hmm *)
        convLval lv (curStr ^ "*")

  let lvToAliasStr (lval:Lv.aLval) =
    let vi, str = convLval lval "" in
    varPrefix vi ^ str

  let stripDeref lval =
    match lval with
      Lv.CMem exp, off ->
        ptrOfExp exp
    | _ -> 
        raise NoDeref

  let printAliasInfo id (ri1, ri2) =
    let worstCasePrint (s1, s2) =
      (* Start w/ nc to indicate that we can't check it *)
      logStatusF "nc\t%d\t%s/%s\t%d\t%d\n"
        id
        (Lv.string_of_lvscope ri1.lval)
        (Lv.string_of_lvscope ri2.lval)
        s1 s2
    in

    let whyAlias (s1, s2) =
      try
        let lv1 = (lvToAliasStr ri1.lval) in
        let lv2 = (lvToAliasStr ri2.lval) in
        logStatusF "wa\t%d\t%s/%s\t%d\t%d\n"
             id lv1 lv2 s1 s2
      with
        NoLvals
      | UnknownPtrExp ->
          worstCasePrint (s1, s2)
    in

    let whyPts id ptr targ (s1, s2) =
      try
        let lv1 = (lvToAliasStr (stripDeref ptr)) in
        let lv2 = (lvToAliasStr targ) in
        logStatusF "wp\t%d\t%s/%s\t%d\t%d\n"
             id lv1 lv2 s1 s2
      with
        NoLvals
      | UnknownPtrExp
      | NoDeref ->
          worstCasePrint (s1, s2)
    in

    match ri1.imprec with
      Lv.Syntactic -> ()
    | Lv.PtsToSame (s1, s2) ->
        whyAlias (s1, s2)
    | Lv.PtsToHostR (s1, s2) ->
        whyPts id ri2.lval ri1.lval (s2, s1)
    | Lv.PtsToHostL (s1, s2) ->
        whyPts id ri1.lval ri2.lval (s1, s2)
    | Lv.RepsSame (s1, s2) ->
        worstCasePrint (s1, s2) (* Missing the original pointers... *)
    | Lv.SameType -> ()

  let printAssumptions races =
    let races = sortRaces races in
    List.iter 
      (fun ((loc1, loc2), id, rep) ->
         match rep with
           ((ri1, ri2) :: t) ->
             (* Just print the first one *)
             printAliasInfo id (ri1, ri2)
         | [] -> failwith "empty race report?"
      ) races

end
      


(***************************************
 Object version of all the races
****************************************)

module RaceWarnInput = struct
  
  type key = Accs.accessInfo * Accs.accessInfo
  type data = raceInfo * raceInfo

  (* Maybe check the accessInfos completely to make clustering more even
     across the different analysis parameters? *)
  let equalKey (a1, b1) (a2, b2) = 
(*
    let a1 = Accs.firstLocation a1 in
    let a2 = Accs.firstLocation a2 in
    let b1 = Accs.firstLocation b1 in
    let b2 = Accs.firstLocation b2 in
    (eqLocation a1 a2 && eqLocation b1 b2)
    || (eqLocation a1 b2 && eqLocation b1 a2)
*)
    (Accs.eqAccesses a1 a2 && Accs.eqAccesses b1 b2)
    || (Accs.eqAccesses a1 b2 && Accs.eqAccesses b1 a2)


  let hashKey (a1, b1) =
    let a1 = Accs.firstLocation a1 in
    let b1 = Accs.firstLocation b1 in
    Hashtbl.hash a1 lxor Hashtbl.hash b1

  let hashConsKey (a1, b1) = 
    Accs.hcAccesses a1, Accs.hcAccesses b1

  let debugEqualData (ri1, ri1') (ri2, ri2') =
    let lv1 = Lv.string_of_lval ri1.lval in
    let lv2 = Lv.string_of_lval ri2.lval in
    let loc1 = Accs.firstLocation ri1.access in
    let loc2 = Accs.firstLocation ri2.access in
    lv1 = "loglist[0].filename" && lv1 = lv2 && 
        loc1.line = 26664 && loc2.line = 26664

  let equalData d1 d2 =
    let eq = eqOneReport d1 d2 in
    if not eq && debugEqualData d1 d2 then begin
      let ri1, _ = d1 in
      let ri2, _ = d2 in
      let lv1 = Lv.string_of_lval ri1.lval in
      let lv2 = Lv.string_of_lval ri2.lval in
      logStatusF "GOTCHA (1) %s\n" lv1;
      logStatusF "GOTCHA (2) %s\n" lv2;
    end;
    eq

  let hashConsData d =
    hashWarnData d    

  let xmlp = new racesXMLPrinter

  let pDataXML d =
    xmlp#pRaceInfo d

end


module RaceWarnOut = Warn_reports.MakeRep(RaceWarnInput)


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

  method printAliasAssumptions () =
    AliasAssumptions.printAssumptions curRaces

  val xmlp = new racesXMLPrinter

  method serialize (fname:string) =
    let writeSelf oc =
      Marshal.to_channel oc curRaces [Marshal.Closures]
    in
    Stdutil.open_out_bin_for fname writeSelf

  method deserialize (fname:string) =
    let doLoad ic =
      Marshal.from_channel ic
    in
    curRaces <- Stdutil.open_in_bin_for fname doLoad


  method saveToXML (fname:string) = 
    let doWrite oc =
      xmlp#pRaces oc curRaces
    in
    Stdutil.open_out_for fname doWrite



  (** Add a report to the set of curRaces *)
  method addWarning ({ access = access1; }, { access = access2;} as newR) 
    : cluster_id =

    let (l1, l2) = Accs.firstLocation access1, Accs.firstLocation access2 in
    
    let tryAddRace (id, oldReport) =
      if (List.length oldReport < maxWarningsPerLoc) then
        if (List.exists (fun oldR -> eqOneReport newR oldR) oldReport) then ()
        else 
          let newR = hashWarnData newR in
          LocHash.replace curRaces (l1, l2) (id, (newR :: oldReport))
    in
    try
      let (id, oldReport1) = LocHash.find curRaces (l1, l2) in
      tryAddRace (id, oldReport1);
      id
    with Not_found ->
      logStatus "New race added to race reports";
      flushStatus ();
      let newID = newClusterId () in
      let newR = hashWarnData newR in
      LocHash.add curRaces (l1, l2) (newID, [ newR ]);
      newID

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
