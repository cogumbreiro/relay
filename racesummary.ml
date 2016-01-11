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
open Lockset
open Logging

open Guarded_access_base

(** Choose guarded access implementation *)

(* module GA = Guarded_access_sep *)
(* module GA = Guarded_access *)
module GA = Guarded_access_clust
               
open Lvals

(*** Debug stuff ***)

let verboseSum = ref false


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

(* Singleton representing \bottom *)
let bottomLval = dummyLval
  
let bottomLS = LS.doPlus LS.empty bottomLval ()
  
let bottomCS = {
  writeCorrs = CMap.add bottomLval GA.dummyCorr CMap.empty;
  readCorrs = CMap.empty;
}

let bottom = {
  lState = bottomLS;
  cState = bottomCS;
}

(* State w/ empty sets *)
let emptyLS = LS.empty

let emptyCS = {
  writeCorrs = CMap.empty;
  readCorrs = CMap.empty;
}

let emptyState = { 
  lState = emptyLS;
  cState = emptyCS;
}
    
(* Summary for those w/ no real summary *)
let bottomSummary = { 
  sum_in = bottom;
  sum_out = bottom;
}

let emptySummary = { 
  sum_in = emptyState;
  sum_out = emptyState;
}
   
let isBottomLS ls =
  ls == bottomLS || LS.equal bottomLS ls

let isBottomCS cs =
  cs == bottomCS ||
    ((GA.cmEQ bottomCS.writeCorrs cs.writeCorrs) &&
       (GA.cmEQ bottomCS.readCorrs cs.readCorrs))

let isBottomState s =
  s == bottom || (isBottomLS s.lState && isBottomCS s.cState)

let isBottomSummary sum =
  sum == bottomSummary || 
    (isBottomState sum.sum_out &&
       isBottomState sum.sum_in)

let isEmptyLS ls =
  ls == emptyLS ||
    LS.equal emptyLS ls

let isEmptyCS cs =
  cs == emptyCS ||
    ((GA.cmEQ emptyCS.writeCorrs cs.writeCorrs) &&
       (GA.cmEQ emptyCS.readCorrs cs.readCorrs))

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

let sumSubset a b =
  statesSubset a.sum_in b.sum_in && (* TODO: get rid of in *)
    statesSubset a.sum_out b.sum_out



(** Combine the lock state portion of the state *)
let combineLStates (a:lockState) (b:lockState) =
  if a == b then a
  else if (isBottomLS(a)) then b
  else if (isBottomLS(b)) then a
  else 
    combineLS a b

(** Combine the read/write access portion of the state *)
let combineCStates (a:corrState) (b:corrState) =
  if a == b then a
  else if (isBottomCS(a)) then b
  else if (isBottomCS(b)) then a
  else
    {
      writeCorrs = GA.combineCM a.writeCorrs b.writeCorrs;
      readCorrs = GA.combineCM a.readCorrs b.readCorrs;
    }

(** Combine two states *)
let combineStates (a:state) (b:state) : state =
  (* intersection w/ bottom just causes it to just return the other state *)
  if (a == b) then a
  else if(isBottomState(a)) then b
  else if (isBottomState(b)) then a
  else
    {
      lState = combineLStates a.lState b.lState;
      cState = combineCStates a.cState b.cState;
    }

let combineSummary a b =
  if a == b then a
  else {
    sum_in = combineStates a.sum_in b.sum_in;
    sum_out = combineStates a.sum_out b.sum_out;
  }

(** Make / Add a write correlation to the given state. 
    Scope is not yet resolved *)
let addWriteCorr (curLocks:lockState) (s:corrState) lv
    (loc:Cil.location) fk : corrState =
  {s with writeCorrs = GA.addCorr lv curLocks loc fk s.writeCorrs}


(** Make / Add a read correlation to the given state. 
    Scope is not yet resolved *)
let addReadCorr (curLocks:lockState) (s:corrState) lv
    (loc:Cil.location) fk : corrState =
  {s with readCorrs = GA.addCorr lv curLocks loc fk s.readCorrs}


(** Make a pseudo access *)
let addPseudoCorr curLocks state lv loc fk pseudoattrib =
  {state with readCorrs = 
      GA.addPseudo lv curLocks loc fk pseudoattrib state.readCorrs}


(** Fill in the scope fields for just the guarded access states *)
let resolveCorrScope (curFunc:Cil.fundec) (corrState:corrState) = 
  let scopeLocks = scopeLockset curFunc in
  let scopeCorr = GA.scopeStraintMap curFunc scopeLocks in
  let newWriteCorr = GA.uniqueCM (scopeCorr corrState.writeCorrs) in
  let newReadCorr = GA.uniqueCM (scopeCorr corrState.readCorrs) in
  makeCState newWriteCorr newReadCorr


(** Fill scope fields of S w/ appropriate values (e.g., formal of F w/ 
    index i vs a global). Formal takes precedence over global (local scope) *)
let resolveScope (f:Cil.fundec) (s:state) : state =
  let newLS = LS.unique (scopeLockset f s.lState) in
  let newCorrs = resolveCorrScope f s.cState in
  makeState newLS newCorrs


let scopeSummary curFunc sum : summary =
  {
    sum_in = resolveScope curFunc sum.sum_in;
    sum_out = resolveScope curFunc sum.sum_out;
  }


(* Take a guarded access set and list the lval/access info...
   TODO: list out the essential parts instead of requiring user to
   access the fields themselves *)
let iterWrites f corrs =
  GA.iterCorrs f corrs.writeCorrs

let iterReads f corrs =
  GA.iterCorrs f corrs.readCorrs

let listWriteCorr corrs =
  GA.enumAccesses corrs.writeCorrs

let listReadCorr corrs =
  GA.enumAccesses corrs.readCorrs

(******************************************************
 * Printing summaries to stdout for debugging
 ******************************************************)

let printLockset ls =
  let buff = Buffer.create 24 in
  if (isBottomLS(ls)) then
    logStatus "LS is $BOTTOM"
  else begin
    let aSet = LS.getPlus ls in
    Buffer.add_string buff "L+ = ";
    set_to_buf buff aSet;
    logStatusB buff;
    Buffer.clear buff;
    let aSet = LS.getMinus ls in
    Buffer.add_string buff "L- = ";
    set_to_buf buff aSet;
    logStatusB buff;
    Buffer.clear buff;
  end


let printCorrState cs =
  if(isBottomCS(cs)) then
    logStatus "CS is $BOTTOM"
  else (
    logStatus ("Write Correlations (" ^
                   (string_of_int (CMap.cardinal cs.writeCorrs)) ^ 
                    "):");
    logStatus "-----";
    GA.printCorrMap cs.writeCorrs printLockset;
    
    logStatus ("Read Correlations (" ^ 
                   (string_of_int (CMap.cardinal cs.readCorrs)) ^ 
                   "):");
    logStatus "-----";
    GA.printCorrMap cs.readCorrs printLockset;
  )
   
(** Print the state (lock set and the guarded accesses) *) 
let printState s = 
  if(isBottomState(s)) then
    logStatus "State is $BOTTOM"
  else (
    logStatus "Lock Diff:";
    logStatus "-----";
    printLockset s.lState;
    printCorrState s.cState;
  )

(** Print only the summary sizes for the given function key k,
    and the summary s *)
let printSizes (funname:string) k s =
  let prefix = ("SUMS (nm, lo, un, wr, re):\t" ^ funname ^ "\t") in
  if(isBottomState(s)) then
    logStatus (prefix ^ "$BOTTOM")
  else (
    let locked = LS.getPlus s.lState in
    let unlocked = LS.getMinus s.lState in
    let writes = s.cState.writeCorrs in
    let reads = s.cState.readCorrs in
    logStatus (prefix ^ 
                   (string_of_int (LS.S.cardinal locked)) ^ "\t" ^
                   (string_of_int (LS.S.cardinal unlocked)) ^ "\t" ^
                   (string_of_int (CMap.cardinal writes)) ^ "\t" ^ 
                   (string_of_int (CMap.cardinal reads)) ^ "\t\n")
  )


 
    
(*****************************************************************
 * Implementation of summary class: tracking, serialization, etc.
 *****************************************************************)


(* Simplify the summary structure before serialization, but be
   careful to preserve sharing (leave lvals and locksets alone) *)
    
type simplerSummary = summary
type simplerState = state      

let hcLockstate = LS.unique

let hcGAState s = 
  { 
    writeCorrs = GA.uniqueCM s.writeCorrs;
    readCorrs = GA.uniqueCM s.readCorrs;
  }

let hcState s =
  { lState = hcLockstate s.lState;
    cState = hcGAState s.cState; }

let hcSummary s =
  { sum_in = hcState s.sum_in;
    sum_out = hcState s.sum_out; }

let simplifySummary (sum:summary) : simplerSummary =
  hcSummary sum
    
let desimplifySummary (simp:simplerSummary) : summary =
  hcSummary simp

(************************************************************)

(* Deserialize summary from filename for debugging *)
let racesumID = "rs"

let deserializeFromPath key path : summary * Backed_summary.dbToken =
  try 
    let sum, token = Backed_summary.deserializeFromPath key racesumID path in
    (desimplifySummary sum, token) 
    with e ->
      logError ~prior:0 ("RS: deserialization failed for : " ^ 
                           (Summary_keys.string_of_sumKey key));
      raise e
        
        
let deserializeFromFile fname : summary =
  try
    let s = Backed_summary.deserializeFromFile fname racesumID in
    desimplifySummary s
  with e ->
    logError ~prior:0 ("RS: deserialization failed for : " ^ (fname));
    raise e


(************************************************************
    Actual instance of the summary manager
 ************************************************************)

module RaceSumType = struct

  type t = summary

  type simpleSum = simplerSummary

  let simplify = simplifySummary

  let desimplify = desimplifySummary

  let initVal = bottomSummary

  let unknownSummary = emptySummary

end

(* Use Cache_sum instead
module RaceSum = Safer_sum.Make (RaceSumType)
*)

let cacheSize = 256
module RaceSum = Cache_sum.Make (RaceSumType)

(** Extension to the default summary class *)

(****************** Use as mod summaries ************************)

let listMods (st:state) =
  CMap.fold
    (fun lval corr curList ->
       try
         let scope = getScope lval in
         (lval, scope) :: curList
       with Scope.BadScope as e ->
         logError "listMods: no scope annotation?";
         raise e
    ) st.cState.writeCorrs []

let iterMods foo st =
  CMap.iter 
    (fun lval corr ->
       let scope = getScope lval in
       foo (lval, scope)
    ) st.cState.writeCorrs


let foldMods foo acc st =
  CMap.fold 
    (fun lval corr acc ->
       let scope = getScope lval in
       foo (lval, scope) acc
    ) st.cState.writeCorrs acc

class virtual corrAsMods = object (self)

  method virtual find : Summary_keys.sumKey -> summary
  method virtual findLocal : Summary_keys.sumKey -> summary
  method virtual findGlobal : Summary_keys.sumKey -> summary

  method private findTestSum key =
    let sum = self#find key in
    if isBottomSummary sum then raise Modsummaryi.BottomSummary
    else sum.sum_out

  method private findTestGlobals key =
    let sum = self#findGlobal key in
    if isBottomSummary sum then raise Modsummaryi.BottomSummary
    else sum.sum_out

  method private findTestLocals key =
    let sum = self#findLocal key in
    if isBottomSummary sum then raise Modsummaryi.BottomSummary
    else sum.sum_out

  method getMods key = 
    let outSt = self#findTestSum key in
    listMods outSt

  method getGlobalMods key = 
    try
      let outSt = self#findTestGlobals key in
      CMap.fold (fun lv _ cur -> LvalSet.add lv cur) 
        outSt.cState.writeCorrs LvalSet.empty
        (* cache??? instead of building the set each time / unioning *)
    with Not_found ->
      LvalSet.empty (* external func *)

  method getLocalMods key = 
    let outSt = self#findTestLocals key in
    listMods outSt

  method iterMods key foo =
    let outSt = self#findTestSum key in
    iterMods foo outSt

  method foldMods : 'a .  Summary_keys.sumKey -> (aLval * Scope.scope -> 'a -> 'a) -> 'a -> 'a =
    fun key foo acc ->
      let outSt = self#findTestSum key in
      foldMods foo acc outSt

end



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
    logError "Corrupt lock description in func summary";
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
            Scope.SFormal (int_of_string (Str.string_after scp 1))
          else if (scp.[0] == 'g') then
            Scope.SGlobal
          else begin
            informError();
            Scope.SGlobal
          end
        with Failure f ->
          informError();
          Scope.SGlobal
        in
        (* TODO: find the formal variable if it exists, instead of
           making up a fresh var w/ formal scope ... *)
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
  makeCState CMap.empty CMap.empty

let fKey_of_fNT cg (fn, ft) =
  FMap.fold
    (fun fid fnode curResults ->
       (* if (compareNT (fn,ft) (fnode.name, fnode.typ) == 0) then *)
       if fn = fnode.name then fid :: curResults
       else curResults
    ) cg []

let printSummary (funname:string) sumKey summ =
  if (isBottomSummary(summ)) then
    logStatus ("$BOTTOM")
  else begin
    (* Only print the sizes when running on linux (reduce log space) *)
    if !verboseSum
    then (printSizes funname sumKey summ.sum_out; printState summ.sum_out)
    else printSizes funname sumKey summ.sum_out
  end

let initializeSumFromSettings settings cg sum =
  let lockSettings = Config.getGroup settings "LOCK_FUNCS" in
  Config.iter
    (fun funcName details ->
       let fields = topSplitter details in
       match fields with
         typString :: locks :: corrSet :: [] ->
           (* Make a new summary *)
           let ls = lockset_from_string locks in
           let straints = straints_from_string corrSet in
           let fids = fKey_of_fNT cg (funcName, typString) in
           List.iter 
             (fun sumKey ->
                (* Don't analyze functions w/ pre-specified summaries *)
                Backed_summary.setFinal sumKey (sum#sumTyp);
                (* Add the pre-specified summary *)
                let newSumm = try
                  (* If there's an existing entry *)
                  let exSumm = sum#find sumKey in
                  logErrorF "Multiple lock summaries for %s:%s\n" 
                    funcName (Summary_keys.string_of_sumKey sumKey);
                  let newSum = { exSumm with 
                                   sum_out = (combineStates exSumm.sum_out 
                                                (makeState ls straints));
                               } in
                  newSum
                with Not_found ->
                  (* otherwise, make a new entry *)
                  { sum_in = emptyState;
                    sum_out = makeState ls straints;
                  }
                in
                sum#addReplace sumKey newSumm;
             ) fids;
       | _ -> 
           failwith ("corrupt lock summary (from config) " ^ details);
    ) lockSettings
