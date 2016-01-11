
(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Ravi Chugh
  
  All rights reserved.
  
  rEdistribution and use in source and binary forms, with or without 
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


open Cil
open Callg
open Fstructs
open Logging
open Cildump
open Summary_keys
open Pretty

module RS = Racesummary
module BS = Backed_summary
module GA = Guarded_access_base
module Lv = Lvals
module LP = Lockset_partitioner
module LSHash = LP.LSHash
module LvalHash = Lv.LvalHash


let markPseudoLocation loc =
  { loc with byte = -1 * loc.byte }

type racy = Racy | NotRacy

(* use of racy option -
     None : hasn't been checked yet
     Some Racy : initialize to this if you want to just
                 assume it's racy *)

type paKey = GA.pseudoAttrib

type paKeyHead = funID * GA.origLvId
type paKeyTail = GA.targetLvId

let smashPaKey (x, y) z : paKey =
  (x, y, z)

let getPaKeyHead (x, y, z) : paKeyHead =
  (x, y)

let string_of_pakeyhead (x, y) =
  Printf.sprintf "(%s,%d)" (fid_to_string x) y

let string_of_pakey (x, y, z) =
  Printf.sprintf "(%s,%d,%d)" (fid_to_string x) y z

module PaKeyHash = Hashtbl.Make(
  struct
    type t = paKey
    let hash = Hashtbl.hash
    let equal a b = a = b
  end
)

module PaKeySet = Set.Make(
  struct
    type t = paKey
    let compare = Pervasives.compare
  end
)

module PaKeyHeadHash = Hashtbl.Make(
  struct
    type t = paKeyHead
    let hash = Hashtbl.hash
    let equal a b = a = b
  end
)

(* TODO Right now the paRegions type supports multiple targets for an lval,
   but this isn't being used entirely properly.  For the case where an lval
   locally has multiple targets, these targets get added to the binding for
   the original lval.  But for the case where an lval can refer to targets
   resulting from callers passing in arguments, these targets cannot be
   determined locally, so only the original lval itself gets added as a
   target.  To support this, summary application would need to keep track of
   when a pseudo access lval gets renamed to something, and then modify the
   PAR summary of the callee with this additional target.  This new target
   would get a unique pa id, rather than just propagating the original one. *)

type paRegions =
  (location * ((paKeyHead * ((paKeyTail * racy option)
     LvalHash.t)) (* 3rd: lval -> pakeytail, racy option *)
     LvalHash.t)) (* 2nd: lval -> pakeyhead, ht          *)
     LSHash.t     (* 1st:   ls -> loc, ht                *)


(* this could be split into an intermediate int identifier to
   group locksets, like in lockset_partitioner *)
type paKeyHead2LsTable = RS.lockState PaKeyHeadHash.t

let computePaKeyHead2LsTable (par : paRegions) =
  let ht = PaKeyHeadHash.create 0 in
  LSHash.iter (fun ls (_, paBindings) ->
    LvalHash.iter (fun _ (pakey, _) ->
        PaKeyHeadHash.add ht pakey ls
    ) paBindings
  ) par;
  ht


type paKey2LvalsTable = (Lv.aLval * Lv.aLval) PaKeyHash.t

let computePaKey2LvalsTable (par : paRegions) =
  let ht = PaKeyHash.create 0 in
  LSHash.iter (fun ls (_, paBindings) ->
    LvalHash.iter (fun origLv (pakeyhead, targetStatus) ->
      LvalHash.iter (fun targLv (pakeytail, _) ->
         let pakey = smashPaKey pakeyhead pakeytail in
         PaKeyHash.add ht pakey (origLv, targLv)
      ) targetStatus
    ) paBindings       
  ) par;
  ht


let printPaRegions (par : paRegions) key =
  let str_of_target targetLv (i, stat) =
    Lvals.string_of_lval targetLv ^ "#["
      ^ string_of_int i ^ "] "
      ^ match stat with
          None -> "None"
        | Some Racy -> "Racy"
        | Some NotRacy -> "NotRacy" in

  let str_of_PaBinding origLv (pakey, targetStatus) =
    let doc = 
      text "{ " ++ 
        map_to_doc (text ", ") 
        LvalHash.iter
        (fun t s -> text (str_of_target t s)) targetStatus nil ++ text " }" in
    Printf.sprintf "%s#[%d] -> %s"
      (Lvals.string_of_lval origLv) (snd pakey) (sprint 80 doc)
  in
  
  let print_locksetPA ls (loc, paBindings) =
    logStatus ("Lockset has reprloc " ^ string_of_loc loc);
    RS.printLockset ls;
    let buff = Buffer.create 32 in
    LvalHash.iter
      (fun a b ->
         Buffer.add_string buff (str_of_PaBinding a b ^ "; ")
      ) paBindings;
    Buffer.add_string buff "\n";
    logStatusB buff
  in
  
  logStatus ("PAR summary for key " ^ fid_to_string key);
  LSHash.iter
    (fun ls (loc, paBindings) ->
       print_locksetPA ls (loc, paBindings)
    ) par
    

let emptySumm () : paRegions * paKeyHead2LsTable * paKey2LvalsTable =
  LSHash.create 0, PaKeyHeadHash.create 0, PaKeyHash.create 0

let printSumm (par, _) key =
  printPaRegions par key

let wrapSummary par =
  par, computePaKeyHead2LsTable par, computePaKey2LvalsTable par

module PARsum = struct

  type t = paRegions * paKeyHead2LsTable * paKey2LvalsTable
  type simpleSum = t
  let simplify s = s
  let desimplify s = s
  let initVal = emptySumm ()
  let unknownSummary = emptySumm ()

end

module PARsummary = Safer_sum.Make (PARsum)

let sums = new PARsummary.data (Backed_summary.makeSumType "par")

(* Take out for now... for newer radar passes 
let _ = Backed_summary.registerType sums
*)


let areTargetsSafe targetStatus =
  let result = 
  LvalHash.fold
    (fun _ (_, status) b ->
       match status with
         None
       | Some NotRacy -> b && true
       | Some Racy    -> b && false
    ) targetStatus true 
  in
(*
    logStatus ("areTargetsSafe " ^ (string_of_bool result));;
*)
    result

let markedRacy = Hashtbl.create 17

let markRacyAccess pardb ((sumKey, _ , _) as pakey) =
  if Hashtbl.mem markedRacy (pakey, pardb#sumTyp) then ()
  else begin
    Hashtbl.replace markedRacy (pakey, pardb#sumTyp) ();
    let tryMark () =
      let par, pakey2ls, pakey2lvals = pardb#find sumKey in
      let ls = PaKeyHeadHash.find pakey2ls (getPaKeyHead pakey) in
      let oLv, tLv = PaKeyHash.find pakey2lvals pakey in
      let reprloc, paBindings = LSHash.find par ls in
      let pakh, targetStatus = LvalHash.find paBindings oLv in
      let pakt, _ = LvalHash.find targetStatus tLv in
      LvalHash.replace targetStatus tLv (pakt, Some Racy);
      LvalHash.replace paBindings oLv (pakh, targetStatus);
      LSHash.replace par ls (reprloc, paBindings);
      pardb#addReplace sumKey (wrapSummary par);
      pardb#flushOne sumKey;
      logStatus ("markRacyAccess " ^ string_of_pakey pakey);
    in
    try tryMark ()
    with Not_found ->
      (* Just in case the sums weren't prepared *)
      Manage_sums.prepareSumms [sumKey] (BS.getDescriptors [pardb#sumTyp]);
      try tryMark () 
      with Not_found ->
        logError ~prior:1 ("markRacyAccess: Not_found " ^ 
                               string_of_pakey pakey ^ " " ^ 
                               BS.string_of_sumType pardb#sumTyp)
  end

  
(* TODO parametrize pardb *)
let bulkSetPA (fkAndPk: PaKeySet.t FMap.t) status =
  let pakeyNotFounds = ref 0 in
  let pakeysTouched = ref PaKeySet.empty in
  FMap.iter 
    (fun sumKey pks ->
       let par, pakey2ls, pakey2lvals = sums#find sumKey in
       PaKeySet.iter
         (fun pakey ->
            try
              let ls = PaKeyHeadHash.find pakey2ls (getPaKeyHead pakey) in
              let oLv, tLv = PaKeyHash.find pakey2lvals pakey in
              let reprloc, paBindings = LSHash.find par ls in
              let pakh, targetStatus = LvalHash.find paBindings oLv in
              let pakt, _ = LvalHash.find targetStatus tLv in
              LvalHash.replace targetStatus tLv (pakt, status);
              LvalHash.replace paBindings oLv (pakh, targetStatus);
              LSHash.replace par ls (reprloc, paBindings);
              pakeysTouched := PaKeySet.add pakey !pakeysTouched;
            with Not_found -> logError ~prior:1 
              ("bulkSetPA: pakey " ^ (string_of_pakey pakey) ^ " Not_found");
              incr pakeyNotFounds
         ) pks;
       sums#addReplace sumKey (wrapSummary par);
       sums#flushOne sumKey
    ) fkAndPk;
  (!pakeysTouched, !pakeyNotFounds)
  (* TODO: reuse some code? *)


exception FilterPAError

(** Read in the cluster_ids of races that are filtered, 
    and change their status to NotRacy *)
let overruleFiltered removedFile cluster_to_pakey =
  logStatusF "Reading filtered races from %s\n" removedFile;

  let parseError line =
    logError ("Error parsing: " ^ line);
    raise FilterPAError
  in

  let filtered_ids = Hashtbl.create 11 in
  let splitter = Str.split_delim (Str.regexp "\t") in
  let doRead ic =
    try while true do
      let line = input_line ic in
      match splitter line with
        [cluster_id; db_id] ->
          let cid = int_of_string (cluster_id) in
          Hashtbl.add filtered_ids cid ()
      | _ -> parseError line
    done
    with End_of_file ->
      logStatus "Done reading filtered races"
  in
  Stdutil.open_in_for removedFile doRead;

(* Debug *)
  let filtered_clusters = ref 0 in
  let cid_no_pakey = ref 0 in
  let pakey_not_found = ref 0 in
  let pakeysTouched = ref PaKeySet.empty in
(*/Debug *)

  Hashtbl.iter 
    (fun cid () -> 
       incr filtered_clusters;
       try
         (* Not merging the fkey maps first, but probably doesn't matter *)
         let fkAndPakeys = Inthash.find cluster_to_pakey cid in
         let morePakeys, moreNotFounds = bulkSetPA fkAndPakeys (Some NotRacy) in
         pakeysTouched := PaKeySet.union !pakeysTouched morePakeys;
         pakey_not_found := !pakey_not_found + moreNotFounds
       with Not_found ->
         logError ("Can't find pakeys for: " ^ (string_of_int cid));
         incr cid_no_pakey
    ) filtered_ids;
  logStatusF "Race clusters filtered: %d\n" !filtered_clusters;
  logStatusF "Cluster ids w/ no pakeys: %d\n" !cid_no_pakey;
  logStatusF "Pakeys touched: %d\n" (PaKeySet.cardinal !pakeysTouched);
  logStatusF "Cluster id has pakeys, but not the requested: %d\n\n" 
    !pakey_not_found
    

(********** Debugging Utilities **********)

(* TODO parametrize pardb *)
let overruleAllRacyAccesses fkey =
  let par, _, _ = sums#find fkey in
  LSHash.iter (fun ls (reprloc, paBindings) ->
    LvalHash.iter (fun origLv (pakeyhead, targetStatus) ->
      LvalHash.iter (fun targLv (pakeytail, _) ->
        LvalHash.replace targetStatus targLv (pakeytail, Some NotRacy)
      ) targetStatus;
      LvalHash.replace paBindings origLv (pakeyhead, targetStatus)
    ) paBindings;
    LSHash.replace par ls (reprloc, paBindings)
  ) par;
  sums#addReplace fkey (wrapSummary par);
  sums#flushOne fkey
