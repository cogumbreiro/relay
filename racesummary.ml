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

(** Definition and operations on data race analysis summaries *)


open Fstructs
open Cil
open Pretty
open Callg
open Scope
open Lockset

module GA = Guarded_access
module D = Cildump
module BS = Backed_summary
module L = Logging
module Stat = Mystats
module Lv = Lvals
module CLv = Cil_lvals

module Req = Request


(*******************************************************************)
(* Definition of state and function summaries                      *)


(* Per program-point state *)
type lockState = fullLS
    
(* Per function state *)
type corrState = {
  writeCorrs : GA.straintMap;
  readCorrs : GA.straintMap;
}

(* Per function state *)
type state = {
  lState : lockState;
  cState : corrState;
}

(* Represention of a summary. No double-locking error checks. *)
type summary = {
  sum_in : state;
  sum_out : state;
  (* lHat *)
}


(*******************************************************************)
(* Instances of state / summaries                                  *)


let dummyCorr = 
  { GA.corrLocks = LS.empty;
    GA.corrScope = SFormal (-1);
    GA.corrAccess = GA.Locs.empty;
    GA.corrLEmpty = Cil.locUnknown;
  }

(* Singleton representing \bottom *)
let bottomLval = (Lv.mkVar false "$RSBOT" (TVoid []), NoOffset)
  
let bottomLS = LS.doPlus LS.empty bottomLval ()
  
let bottomCS =
  {
    writeCorrs = GA.CMap.add bottomLval dummyCorr GA.CMap.empty;
    readCorrs = GA.CMap.empty;
  }

let bottom = 
  {
    lState = bottomLS;
    cState = bottomCS;
  }

(* State w/ empty sets *)
let emptyLS = LS.empty

let emptyCS = {
  writeCorrs = GA.CMap.empty;
  readCorrs = GA.CMap.empty;
}

let emptyState = 
  { 
    lState = emptyLS;
    cState = emptyCS;
  }
    
(* Summary for those w/ no real summary *)
let bottomSummary = 
  { 
    sum_in = bottom;
    sum_out = bottom;
  }

let emptySummary =
  { 
    sum_in = emptyState;
    sum_out = emptyState;
  }
   
let isBottomLS ls =
  ls == bottomLS ||
    LS.equal bottomLS ls

let isBottomCS cs =
  cs == bottomCS ||
    ((GA.cmAppEQ bottomCS.writeCorrs cs.writeCorrs) &&
       (GA.cmAppEQ bottomCS.readCorrs cs.readCorrs))

let isBottomState s =
  s == bottom ||
    (isBottomLS s.lState &&
       isBottomCS s.cState)

let isBottomSummary sum =
  sum == bottomSummary || 
    (isBottomState sum.sum_out &&
       isBottomState sum.sum_in)

let isEmptyLS ls =
  ls == emptyLS ||
    LS.equal emptyLS ls

let isEmptyCS cs =
  cs == emptyCS ||
    ((GA.cmAppEQ emptyCS.writeCorrs cs.writeCorrs) &&
       (GA.cmAppEQ emptyCS.readCorrs cs.readCorrs))

let isEmptyState s =
  s == emptyState ||
    (isEmptyLS s.lState &&
       isEmptyCS s.cState)

let isEmptySummary sum =
  sum == emptySummary ||
    (isEmptyState sum.sum_out &&
       isEmptyState sum.sum_in)

(********************************************************)
 
(** Returns the output state of the summary *)
let summOutstate (summ:summary) : state =
  summ.sum_out


(** Returns a new summary where the output state has been replaced *)
let makeSumm (inSt:state) (out:state) : summary =
  { 
    sum_in = inSt;
    sum_out = out;
  }

let makeCState writes reads =
  {
    writeCorrs = writes;
    readCorrs = reads;
  }

let makeState (ls:lockState) (corrs:corrState) =
  {
    lState = ls;
    cState = corrs;
  }


(*******************************************************************)
(* State Operations                                                *)

(** True if the first state is a subset of the second *)
let lStateSubset (a:lockState) (b:lockState) : bool =
  if (isBottomLS(a)) then true
  else if (isBottomLS(b)) then false
  else if (a == b) then true
  else
    LS.subset a b


let cStateSubset (a:corrState) (b:corrState) : bool =
  if (isBottomCS(a)) then true
  else if (isBottomCS(b)) then false
  else if (a == b) then true
  else
    GA.cmSubs a.writeCorrs b.writeCorrs &&
      GA.cmSubs a.readCorrs b.readCorrs


let statesSubset (a:state) (b:state) : bool =
  if (isBottomState(a)) then true
  else if (isBottomState(b)) then false
  else if (a == b) then true
  else
    lStateSubset a.lState b.lState &&
      cStateSubset a.cState b.cState


(** Combine the lock state portion of the state *)
let combineLStates (a:lockState) (b:lockState) =
  if (isBottomLS(a)) then
    b
  else if (isBottomLS(b)) then
    a
  else 
    GA.combineLS a b

(** Combine the read/write access portion of the state *)
let combineCStates (a:corrState) (b:corrState) =
  if (isBottomCS(a)) then
    b
  else if (isBottomCS(b)) then
    a
  else
    {
      writeCorrs = GA.combineCM a.writeCorrs b.writeCorrs;
      readCorrs = GA.combineCM a.readCorrs b.readCorrs;
    }

(** Combine two states *)
let combineStates (a:state) (b:state) : state =
  (* intersection w/ bottom just causes it to just return the other state *)
  if(isBottomState(a)) then
    b
  else if (isBottomState(b)) then
    a
  else if (a == b) then
    a
  else
    {
      lState = combineLStates a.lState b.lState;
      cState = combineCStates a.cState b.cState;
    }


(** Make / Add a write correlation to the given state. 
    Scope is not yet resolved *)
let addWriteCorr (curLocks:lockState) (s:corrState) lv
    (loc:Cil.location) fk (sc:scope) : corrState =
  {s with writeCorrs = GA.addCorr lv curLocks loc fk sc s.writeCorrs}

(** Make / Add a read correlation to the given state. 
    Scope is not yet resolved *)
let addReadCorr (curLocks:lockState) (s:corrState) lv
    (loc:Cil.location) fk (sc:scope) : corrState =
  {s with readCorrs = GA.addCorr lv curLocks loc fk sc s.readCorrs}


(** Fill scope fields of S w/ appropriate values (e.g., formal of F w/ 
    index i vs a global). Formal takes precedence over global (local scope) *)
let resolveScope (s:state) (f:Cil.fundec) : state =

  let scopeLocks = scopeLockset f in
  
  let scopeCorr = GA.scopeStraintMap f scopeLocks in
  
  let newLS = LS.unique (scopeLocks s.lState) in
  let newWriteCorr = GA.cacheCM (scopeCorr s.cState.writeCorrs) in
  let newReadCorr = GA.cacheCM (scopeCorr s.cState.readCorrs) in
  makeState newLS (makeCState newWriteCorr newReadCorr)


(* Take a summary and list the (lval, lockset) pairs in the function's 
   end state *)
let iterWrites f corrs =
  GA.CMap.iter f corrs.writeCorrs

let iterReads f corrs =
  GA.CMap.iter f corrs.readCorrs

let listWriteCorr corrs =
  GA.CMap.fold 
    (fun lval corr curList ->
       (lval, corr) :: curList)
    corrs.writeCorrs []
    

let listReadCorr corrs =
  GA.CMap.fold 
    (fun lval corr curList ->
       (lval, corr) :: curList)
    corrs.readCorrs []
  

let listMods (st:state) =
  GA.CMap.fold
    (fun lval corr curList ->
       (lval, corr.GA.corrScope) :: curList)
    st.cState.writeCorrs []


(******************************************************
 * Printing summaries to stdout for debugging
 ******************************************************)

let string_of_lvscope lv scope =
  (Lv.string_of_lval lv) ^ (string_of_scope scope)

        
let printLockset ls =
  let buff = Buffer.create 24 in
  let printASet s = 
    if (LS.S.is_empty s) then
      Buffer.add_string buff "empty"
    else begin
      Buffer.add_string buff "{";
      LS.S.iter 
        (fun lv l -> 
           Buffer.add_string buff ((string_of_lock lv l) ^ ", ")
        ) s;
      Buffer.add_string buff ("} (" ^ (string_of_int 
                                         (LS.S.cardinal s))  ^  ")");
    end
  in
  if (isBottomLS(ls)) then
    L.logStatus "LS is $BOTTOM"
  else begin
    let aSet = LS.getPlus ls in
    Buffer.add_string buff "L+ = ";
    printASet aSet;
    L.logStatus (Buffer.contents buff);
    Buffer.clear buff;
    let aSet = LS.getMinus ls in
    Buffer.add_string buff "L- = ";
    printASet aSet;
    L.logStatus (Buffer.contents buff);
    Buffer.clear buff;
  end
    
let printCorr clval c =
  L.logStatus ((string_of_lvscope clval c.GA.corrScope) ^ ": " ^ 
                 (GA.string_of_accesses c.GA.corrAccess) ^ "~");
  printLockset c.GA.corrLocks;
  L.logStatus ""


let printCorrMap cm =
  GA.CMap.iter printCorr cm


let printCorrState cs =
  if(isBottomCS(cs)) then
    L.logStatus "CS is $BOTTOM"
  else (
    L.logStatus ("Write Correlations (" ^
                   (string_of_int (GA.CMap.cardinal cs.writeCorrs)) ^ 
                    "):");
    L.logStatus "-----";
    printCorrMap cs.writeCorrs;
    
    L.logStatus ("Read Correlations (" ^ 
                   (string_of_int (GA.CMap.cardinal cs.readCorrs)) ^ 
                   "):");
    L.logStatus "-----";
    printCorrMap cs.readCorrs
  )
   
(** Print the state (lock set and the guarded accesses) *) 
let printState s = 
  if(isBottomState(s)) then
    L.logStatus "State is $BOTTOM"
  else (
    L.logStatus "Lock Diff:";
    L.logStatus "-----";
    printLockset s.lState;
    printCorrState s.cState;
  )

(** Print only the summary sizes for the given function key k,
    and the summary s *)
let printSizes k s =
  let prefix = ("SUMS (fk, lo, un, wr, re):\t" ^ 
                  (string_of_int k) ^ "\t") in
  if(isBottomState(s)) then
    L.logStatus (prefix ^ "$BOTTOM")
  else (
    let locked = LS.getPlus s.lState in
    let unlocked = LS.getMinus s.lState in
    let writes = s.cState.writeCorrs in
    let reads = s.cState.readCorrs in
    L.logStatus (prefix ^ 
                   (string_of_int (LS.S.cardinal locked)) ^ "\t" ^
                   (string_of_int (LS.S.cardinal unlocked)) ^ "\t" ^
                   (string_of_int (GA.CMap.cardinal writes)) ^ "\t" ^ 
                   (string_of_int (GA.CMap.cardinal reads)) ^ "\t\n")
  )


 
    
(*****************************************************************
 * Implementation of summary class: tracking, serialization, etc.
 *****************************************************************)


(* Simplify the summary structure before serialization, but be
   careful to preserve sharing (leave lvals and locksets alone) *)
    
type simplerSummary = summary
type simplerState = state      


let simplifySummary (sum:summary) : simplerSummary =
  sum
    
let desimplifySummary (simp:simplerSummary) : summary =
  
  let deSimplifyLoc (loc:location) : location =
    CLv.distillLoc loc    
  in
  let deSimplifyState (st:simplerState) : state =
    let ls = LS.unique st.lState in
    let wCorrs = st.cState.writeCorrs in
    GA.CMap.iter (fun lv corr ->
                    corr.GA.corrLocks <- LS.unique corr.GA.corrLocks;
                    corr.GA.corrLEmpty <- deSimplifyLoc corr.GA.corrLEmpty;
                    corr.GA.corrAccess <- 
                      GA.Locs.fold 
                      (fun (k, l) s -> GA.Locs.add (k, deSimplifyLoc l) s) 
                      corr.GA.corrAccess GA.Locs.empty;
                 ) wCorrs;
    let rCorrs = st.cState.readCorrs in
    GA.CMap.iter (fun lv corr ->
                    corr.GA.corrLocks <- LS.unique corr.GA.corrLocks;
                    corr.GA.corrLEmpty <- deSimplifyLoc corr.GA.corrLEmpty;
                    corr.GA.corrAccess <- 
                      GA.Locs.fold
                      (fun (k, l) s -> GA.Locs.add (k, deSimplifyLoc l) s) 
                      corr.GA.corrAccess GA.Locs.empty;
                 ) rCorrs;
    makeState ls (makeCState (GA.cacheCM wCorrs) (GA.cacheCM rCorrs))
  in
  {
    sum_in = deSimplifyState simp.sum_in;
    sum_out = deSimplifyState simp.sum_out;
  }
    
let deserializeFromPath fkey path : summary * BS.dbToken=
  try 
    let sum, token = BS.deserializeFromPath fkey "rs" path in
    (desimplifySummary sum, token) 
    with e ->
      L.logError ~prior:0 ("RS: deserialization failed for : " ^ 
                    (string_of_fkey fkey));
      raise e
        
        
let deserializeFromFile fname : summary =
  try
    let s = BS.deserializeFromFile fname "rs" in
    desimplifySummary s
  with e ->
    L.logError ~prior:0 ("RS: deserialization failed for : " ^ 
                    (fname));
    raise e


(************************************************************
    Actual instance of the summary manager
 ************************************************************)

module RaceSumType = struct

  let id = BS.makeSumType "rs"

  type t = summary

  type simpleSum = simplerSummary

  let simplify = simplifySummary

  let desimplify = desimplifySummary

  let initVal = bottomSummary

end

module RaceSum = Safer_sum.Make (RaceSumType)

class raceSummary = object (self)
  inherit RaceSum.data

  method getMods fkey = 
    let sum = self#find fkey in
    if isBottomSummary sum then
      raise Modsummary.BottomSummary
    else
      let outSt = sum.sum_out in
      listMods outSt

end

let sum = new raceSummary

let _ = BS.registerType (sum :> RaceSum.data)

(*************************************************
 * Initialization, parsing of the config file 
 *************************************************)

let ws = "[ \r\n\t]*"

(* Top level split, between the function name, type, lockset, correlations *)
let topSplitter = Str.split_delim (Str.regexp (ws ^ "[$]" ^ ws))

(* Split between different lock annotations in the locksets and
 * the correlations in the correlation sets *)
let setSplitter = Str.split_delim (Str.regexp (ws ^ "[,]" ^ ws))

(* Split between a predicate and the lock result *)
let predLockSplit = Str.split_delim (Str.regexp (ws ^ "[?]" ^ ws))

(* Split between the true & false lock result after a predicate *)
let trueFalseSplit = Str.split_delim (Str.regexp (ws ^ "[:]" ^ ws))

(* Split between the name of the lock and its lock type (r/w) *)
let lockTypeSplit = Str.split_delim (Str.regexp (ws ^ "[/]" ^ ws))

(* Split between the lval and its correlated lockset *)
let corrSplitter = Str.split_delim (Str.regexp (ws ^ "[~]" ^ ws))

(** Parse the given string and create a lockset. This is useful if you
    want to bootstrap the function summary for the base lock functions. *)
let lockset_from_string (descrip:string) =
  let informError (_:unit) = 
    L.logError "Corrupt lock description in func summary";
  in
  let curLockset = ref LS.empty in

  (* parse one lock description *)
  let handleLock (lockdescrip:string) =
    match (lockTypeSplit lockdescrip) with
      [op; name; prot; scp] -> 
        let protType = if (prot.[0] == 'w') then
          LPWrite
        else if(prot.[0] == 'r') then
          LPRead
        else begin 
          informError();
          LPWrite
        end
        in
        let scope = try 
          if (scp.[0] == 'f') then
            SFormal (int_of_string (Str.string_after scp 1))
          else if (scp.[0] == 'g') then
            SGlobal
          else begin
            informError();
            SGlobal
          end
        with Failure f ->
          informError();
          SGlobal
        in
        let lv, lock = makeSimpleLock name scope protType in 
        if (op.[0] == '+') then
          curLockset := LS.doPlus !curLockset lv lock
        else if (op.[0] == '-') then
          curLockset := LS.doMinus !curLockset lv lock
        else 
          informError ()
    | _ -> informError ()
  in


  (* Loop through each field in lockset, and handle predicates *)
  let rec processLocks (descrips:string list) = 
    match descrips with
      [] -> ()
    | hd :: tl ->
        begin
          match (predLockSplit hd) with
            [pred; true_false] -> 
              begin
                match (trueFalseSplit true_false) with 
                  [lexp_t; lexp_f] -> 
                    handleLock lexp_t;  (* TODO do something w/ pred *)
                    handleLock lexp_f
                | [lexp] ->
                    handleLock lexp
                | _ -> informError ()      
              end
          | [lexp] -> handleLock lexp
          | _ -> informError ()
        end;
        processLocks tl
  in
  processLocks (setSplitter descrip);
  LS.unique (!curLockset)

    

(** Parse the given string and create a set of guarded acceses.
    Used for bootstrapping summaries for missing (as in, we don't have
    the source) or base functions. TODO: implement (didn't need it yet) *)
let straints_from_string (descrip:string) = 
  GA.CMap.empty


let initSummaries settings cg = begin
  (* TODO: just build a reverse map and use that *)
  let fKey_of_fNT (fn,ft) =
    FMap.fold
      (fun fid fnode curResults ->
         if (compareNT (fn,ft) (fnode.name, fnode.typ) == 0) then
           fid :: curResults
         else
           curResults
        ) cg []
  in
  (* Initialize summaries based on config file *)
  let lockSettings = Config.getGroup settings "LOCK_FUNCS" in
  Config.iter
    (fun funcName details ->
       let fields = topSplitter details in
       match fields with
         typString :: locks :: corrSet :: [] ->
           (* Make a new summary *)
           let ls = lockset_from_string locks in
           let straints = straints_from_string corrSet in
           let fids = fKey_of_fNT (funcName, typString) in
           List.iter 
             (fun fkey ->
                (* Don't analyze functions w/ pre-specified summaries *)
                BS.setFinal fkey RaceSumType.id;
                (* Add the pre-specified summary *)
                let newSumm = try
                  (* If there's an existing entry *)
                  let exSumm = sum#find fkey in
                  { exSumm with 
                      sum_out = (combineStates exSumm.sum_out 
                                   (makeState ls 
                                      (makeCState straints GA.CMap.empty)));
                  }
                with Not_found ->
                  (* otherwise, make a new entry *)
                  { sum_in = emptyState;
                    sum_out = makeState ls 
                      (makeCState straints GA.CMap.empty);
                  }
                in
                sum#addReplace fkey newSumm;
             ) fids;
         | _ -> L.logError ~prior:0
             ("corrupt lock summary (from config) " ^ details);
    ) lockSettings;
  (* Initialize the rest of the summaries for functions w/ no bodies *)
  FMap.iter 
    (fun k n ->
       if (n.hasBody) then
           () (* leave a missing entry *)
       else begin
         (* no def/body for the func, so just set to empty/done *)
         if not (BS.isFinal k RaceSumType.id) then begin
           sum#addReplace k emptySummary;
           BS.setFinal k RaceSumType.id;
         end
       end
    ) cg
end



let printSummary k =
  let summ = sum#find k in
  if (isBottomSummary(summ)) then
    L.logStatus ("$BOTTOM")
  else begin
    (* Only print the sizes when running on linux (reduce log space) *)
    (*      printState summ.sum_out; *)
    printSizes k summ.sum_out;
    ()
  end

