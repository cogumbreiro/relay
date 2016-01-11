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


(** Worker-logic for using data race analysis summaries to 
    generate warnings. Assumes server process is live. 
    Specialized for Radar. 

    TODO: duplicate less code... *)

open Cil
open Callg
open Fstructs
open Cilinfos
open Warn_reports
open Pretty
open Cildump
open Logging

module RP = Race_reports

module LS = Lockset
module Race = Racestate
module RS = Race.RS
module SPTA = Race.SPTA

module Stat = Mystats

module Lv = Lvals
module Acc = Access_info

module PaKeyHeadHash = Pseudo_access.PaKeyHeadHash
module PaKeySet = Guarded_access_base.PAS

(*************** State **************)

let localRaces = new RP.raceReports ()

let makeLockList ls =
  LS.LS.ditchMinus ls

(*************** Hardcoded stuff ***************)

let r2pakey_file = "race_to_pakey.dat"

(*let pardb = ref Pseudo_access.sums*)
(* TODO set these for the radar_race pass! *)
let parL = ref Pseudo_access.sums
let parNL = ref Pseudo_access.sums

(*let useLocks = ref true*)

(******************** Mapping pseudo accesses to races ********************)

type paRaceEntry = {
  lval : Lv.aLval;
  access : Acc.accessInfo;
  locks : LS.fullLS;
  threadRoot : RP.threadRootLoc;
}

module PaRaceSet = Set.Make
  (struct
     type t = paRaceEntry

     let compare a b =
       let c1 = Lv.compare_lval a.lval b.lval in
       if c1 <> 0 then c1
       else
         let c2 = Acc.Locs.compare a.access b.access in
         if c2 <> 0 then c2
         else
           let c3 = LS.LS.compare a.locks b.locks in
           if c3 <> 0 then c3
           else
             let loc1, s1, fk1 = a.threadRoot in
             let loc2, s2, fk2 = b.threadRoot in
             let c4 = Cil.compareLoc loc1 loc2 in
             if c4 <> 0 then c4
             else
               let c5 = compare s1 s2 in
               if c5 <> 0 then c5
               else compareFunID fk1 fk2
   end)

let paRaces : PaRaceSet.t PaKeyHeadHash.t =
  PaKeyHeadHash.create 300

let addRaceToPAs entry paIds =
  PaKeySet.iter
    (fun pak ->
       let pakh = (Pseudo_access.getPaKeyHead pak) in
       if PaKeyHeadHash.mem paRaces pakh then
         let entries = PaKeyHeadHash.find paRaces pakh in
         PaKeyHeadHash.replace paRaces pakh (PaRaceSet.add entry entries)
       else
         PaKeyHeadHash.add paRaces pakh (PaRaceSet.singleton entry)
    ) paIds

let printGammaReport () =
  let docs = ref [] in
  let paCount = ref 0 in

  PaKeyHeadHash.iter (fun pakh races ->

    let doc = ref nil in
    let cat x = doc := !doc ++ (text (x ^ "\n")) in

    let count = PaRaceSet.cardinal races in
    cat (" PA " ^ Pseudo_access.string_of_pakeyhead pakh
           ^ " has " ^ string_of_int count ^ " race entries\n");
    incr paCount;

  PaRaceSet.iter (fun race ->

    cat ("     lval: " ^ Lv.string_of_lval race.lval ^ "\n" ^
         "     locs: " ^ Acc.string_of_accesses race.access ^ "\n" ^
         "    locks:");
    doc := !doc ++ (LS.d_fullLS () race.locks);

    let loc, fn, fk = race.threadRoot in
    cat ("     root: thread spawned at " ^ string_of_loc loc
           ^ " w/ func " ^ fn ^ "\n");

  ) races;

     docs := !doc :: !docs
  ) paRaces;

  logStatusF "\nGamma Report -- %d racy PAs\n\n" !paCount;
  List.iter (fun doc -> logStatusD (indent 2 doc)) (List.rev !docs);
  flushStatus ()


(**************** Map races to pseudo accesses ***************)

(* Could probably just use the mapping from the other direction, but whatever *)

let racesToPAs : (PaKeySet.t FMap.t) Inthash.t =
  Inthash.create 300 (* assume integer key *)
    
let getPAs fkey fk2PAs =
  try FMap.find fkey fk2PAs with Not_found -> PaKeySet.empty

let bindRaceToPA cluster_id fkey pakey =
  let newFk2PA = 
    try
      let oldFk2PA = Inthash.find racesToPAs cluster_id in
      let newPAs = 
        let oldPAs = getPAs fkey oldFk2PA in
        PaKeySet.add pakey oldPAs
      in
      FMap.add fkey newPAs oldFk2PA
    with Not_found ->
      (* if cluster_id isn't found, fmap / pakey bindings to begin with *)
      FMap.add fkey (PaKeySet.singleton pakey) FMap.empty
  in
  Inthash.replace racesToPAs cluster_id newFk2PA

let bindRaceToPAs cluster_id paIds =
  PaKeySet.iter (fun ((fkey, _, _) as pakey) -> 
                   bindRaceToPA cluster_id fkey pakey) paIds

let saveR2PA filename =
  let doWrite oc =
    Marshal.to_channel oc racesToPAs [Marshal.Closures]
  in
  Stdutil.open_out_bin_for filename doWrite 

let loadR2PA filename =
  let doRead ic =
    Marshal.from_channel ic
  in
  Stdutil.open_in_bin_for filename doRead


(**************** CHECK *****************)
            
(* TODO: make this not hardcoded *)
let hardCodedSumTypes () =
  (*!Backed_summary.allTypes*)
  Backed_summary.getDescriptors [RS.sum#sumTyp;
                                 SPTA.SS.sum#sumTyp;
                                ]

type pairCounts = {
  notBlob : Int64.t;
  blob : Int64.t;
}

class warningChecker cg cgDir = object (self)
  inherit [Roots.state] Roots.unordRootChecker 
  inherit Roots.ordAccessChecker cg cgDir as superAcc
  inherit Roots.netChecker
    
  val mutable curTR1 = ({line = 0;
                         file = "";
                         byte = 0;}, "", dummyFID)
    
  val mutable curTR2 = ({line = 0;
                         file = "";
                         byte = 0;}, "", dummyFID)

  val totalP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val diffLvalP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val protectedP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val bothEmptyP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val oneEmptyP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val disjointP = ref { notBlob = Int64.zero; blob = Int64.zero; }

  val pa_totalP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val pa_diffLvalP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val pa_protectedP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val pa_bothEmptyP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val pa_oneEmptyP = ref { notBlob = Int64.zero; blob = Int64.zero; }
  val pa_disjointP = ref { notBlob = Int64.zero; blob = Int64.zero; }

  method inc c blob =
    if blob then begin
      c := { !c with blob = Int64.succ !c.blob };
      totalP := { !totalP with blob = Int64.succ !totalP.blob }
    end
    else begin
      c := { !c with notBlob = Int64.succ !c.notBlob };     
      totalP := { !totalP with notBlob = Int64.succ !totalP.notBlob }
    end

  method inc_pa c hasPseudo blob =
    if hasPseudo then begin
      if blob then begin
        c := { !c with blob = Int64.succ !c.blob };
        pa_totalP := { !pa_totalP with blob = Int64.succ !pa_totalP.blob }
      end
      else begin
        c := { !c with notBlob = Int64.succ !c.notBlob };
        pa_totalP := { !pa_totalP with notBlob = Int64.succ !pa_totalP.notBlob }
      end
    end

  method printPair caption count =
    let nb = !count.notBlob in
    let b = !count.blob in
    logStatusF "%s:\t%Ld\t+ %Ld \t= %Ld\n" caption 
      nb b (Int64.add nb b)

  method printAllPairCounts () =
    logStatus "All CheckPair Counts";
    self#printPair "totalP" totalP;
    self#printPair "diffLvalP" diffLvalP;
    self#printPair "protectedP" protectedP;
    self#printPair "bothEmptyP" bothEmptyP;
    self#printPair "oneEmptyP" oneEmptyP;
    self#printPair "disjointP" disjointP;
    logStatus "\nPA CheckPair Counts";
    self#printPair "pa_totalP: " pa_totalP;
    self#printPair "pa_diffLvalP" pa_diffLvalP;
    self#printPair "pa_protectedP" pa_protectedP;
    self#printPair "pa_bothEmptyP" pa_bothEmptyP;
    self#printPair "pa_oneEmptyP" pa_oneEmptyP;
    self#printPair "pa_disjointP" pa_disjointP


  method notifyDone =
    logStatus "completed all thread pairs";
    flushStatus ();
    localRaces#printWarnings;  (* for local debugging *)
    try
      Request.notifyRace localRaces#data
    with e ->
      logError ("Warnings notifyDone: " ^ (Printexc.to_string e));
      raise e
      
        
  (** Override accessChecker *)
  method startState (loc1, fn1, fk1) (loc2, fn2, fk2) =
    superAcc#startState (loc1, fn1, fk1) (loc2, fn2, fk2);
    curTR1 <- (loc1, fn1, fk1);
    curTR2 <- (loc2, fn2, fk2)

  (** Update race reports w/ a new race *)
  method addRace ?(paIds) ?(accessTypes) imp 
    lv1 access1 locks1 locEmpty1 esc1 
    lv2 access2 locks2 locEmpty2 esc2 =
    let lockList1 = makeLockList locks1 in
    let lockList2 = makeLockList locks2 in
    let raceWarning = 
      ( { RP.access = access1;
          RP.threadRoot = curTR1;
          RP.imprec = imp;
          RP.emptied = locEmpty1;
          RP.lval = lv1;
          RP.locks = lockList1;
          RP.threadEsc = esc1; },
        { RP.access = access2;
          RP.threadRoot = curTR2;
          RP.imprec = imp;
          RP.emptied = locEmpty2;
          RP.lval = lv2;
          RP.locks = lockList2;
          RP.threadEsc = esc2; }) in
    let cluster_id = localRaces#addWarning raceWarning in
    
    (* Extra pseudo-access info *)
    let entry1 =
      { lval = lv1;
        access = access1;
        locks = lockList1;
        threadRoot = curTR1; } in
    let entry2 =
      { lval = lv2;
        access = access2;
        locks = lockList2;
        threadRoot = curTR2; } in

    (match paIds, accessTypes with
       Some ids, Some Roots.WW -> 
         (addRaceToPAs entry1 ids;
          addRaceToPAs entry2 ids);
     | Some ids, Some Roots.WR ->
         addRaceToPAs entry1 ids;
     | Some ids, Some Roots.RW ->
         addRaceToPAs entry2 ids;
     | _ ->
         ()
    );
    (match paIds with
       Some ids -> bindRaceToPAs cluster_id ids 
     | None -> ()
    ) 

  method checkAccess accessTypes hasPseudo imprec
    lv1 access1 locks1 locEmpty1 paIds1 esc1
    lv2 access2 locks2 locEmpty2 paIds2 esc2 =
    let blob = match imprec with Lv.Syntactic -> false | _ -> true in
    let paIds = PaKeySet.union paIds1 paIds2 in

    let processRace useLocks =
      let pardb = match useLocks with
        | true  -> !parL
        | false -> !parNL
      in
      let markRacy pakey =
        (*
        logStatus
          (Lv.string_of_lval lv1 ^ "/" ^ Lv.string_of_lval lv2);
        *)
        Pseudo_access.markRacyAccess pardb pakey;
      in
      PaKeySet.iter markRacy paIds;
      match useLocks with
      | false -> ()
      | true ->
          self#addRace ~paIds:paIds ~accessTypes:accessTypes imprec
            lv1 access1 locks1 locEmpty1 esc1
            lv2 access2 locks2 locEmpty2 esc2 
    in begin
      (match locks1, locks2 with
         
         s1, s2 when Lockset.LS.emptyPlus s1 && Lockset.LS.emptyPlus s2 ->
           begin
             self#inc bothEmptyP blob;
             self#inc_pa pa_bothEmptyP hasPseudo blob;
             processRace false;
             processRace true
           end
             
       | s1, s2 when Lockset.LS.emptyPlus s1 || Lockset.LS.emptyPlus s2 ->
           begin
             self#inc oneEmptyP blob;
             self#inc_pa pa_oneEmptyP hasPseudo blob;
             processRace false;
             processRace true
             end
             
       | s1, s2 -> begin
           
           (* answer with just an alias query *)
           processRace false;
           
           (* answer with locksets also *)
           if LS.inter_isEmpty s1 s2 then begin
             self#inc disjointP blob;
             self#inc_pa pa_disjointP hasPseudo blob;
             processRace true
           end
           else begin
             self#inc protectedP blob;
             self#inc_pa pa_protectedP hasPseudo blob
           end
         end    
      )
    end

  (* HACK for main for now... we should actually have a config file
     or something for functions that are only spawned once or
     pairs of functions known not to happen in parallel *)
  method private funsRunInParallel () =
    let _, fn1, _ = curTR1 in
    let _, fn2, _ = curTR2 in
    fn1 = "main" && fn2 = "main"

  method checkPair accessTypes (lv1, corr1) (lv2, corr2) =
    if self#funsRunInParallel () then ()
    else 
      let lvCheck = Stat.time "sameLval" (Lv.sameLval lv1) lv2 in
      let hasPseudo = RS.GA.hasPseudo corr1 || RS.GA.hasPseudo corr2 in
      match lvCheck with
        None -> begin
          self#inc diffLvalP false;
          self#inc_pa pa_diffLvalP hasPseudo false
        end

      | Some (imprec) ->
          let esc1 = Shared.escapeableAbs lv1 in
          let esc2 = Shared.escapeableAbs lv2 in
          if not (esc1 && esc2) then begin
            let blob = match imprec with Lv.Syntactic -> false | _ -> true in
            self#inc diffLvalP blob;
            self#inc_pa pa_diffLvalP hasPseudo blob;
          end
          else 
            RS.GA.iterGuardedAccs
              (fun access1 locks1 locEmpty1 pas1 ->
                 RS.GA.iterGuardedAccs
                   (fun access2 locks2 locEmpty2 pas2 ->
                      self#checkAccess accessTypes hasPseudo imprec
                        lv1 access1 locks1 locEmpty1 pas1 esc1
                        lv2 access2 locks2 locEmpty2 pas2 esc2
                   ) corr2
              ) corr1
            
            
            
  (** Helper function to clear some memory between each checked pair *)
  method evictSumms =
    List.iter (fun s -> s#evictSummaries) (hardCodedSumTypes ())


end

(** Check for candidate pairs of accesses that may result in a data race 
    Assumes: function summaries computed *)
let flagRacesFromSumms cg cgDir =
  let checker = new warningChecker cg cgDir in
  checker#run;
  checker#printAllPairCounts ();
  Pseudo_access.sums#serializeAndFlush;
  let race2pakeyFile = (Filename.concat cgDir r2pakey_file) in
  logStatusF "Writing race cluster_id to pakey bindings to %s\n" 
    race2pakeyFile;
  saveR2PA race2pakeyFile;
  
  (* For now, have the client write out the warning data (separate file).
     TODO: give the server the race2pakey data so that the server
     writes out the warnings w/ IDs that match *)
  localRaces#saveToXML (Filename.concat cgDir "warnings2.xml")
  

