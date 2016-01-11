
(** A pass to find all the unlocks() that can occur in executing a function *)

open Cil
open Fstructs
module L = Logging
module RS = Racesummary
module LS = Lockset


(* AllUnlocks summary info *)

let emptyAUsummary () =
  Lockset.LS.S.empty

module AUsum = struct
  type t = unit Lockset.LS.S.t
  type simpleSum = t
  let simplify s = s
  let desimplify s = Lockset.LS.uniquePart s
  let initVal = emptyAUsummary ()
  let unknownSummary = emptyAUsummary ()

end

module AUsummary = Safer_sum.Make (AUsum)

let sums = new AUsummary.data (Backed_summary.makeSumType "au")

let _ = Backed_summary.registerType sums


(* AllUnlocks computation *)

module AllUnlocksTransF = struct

  let fiState : (unit LS.LS.S.t) ref = ref LS.LS.S.empty

  let initState () =
    fiState := LS.LS.S.empty

  let addUnlocks lminus =
    fiState := LS.LS.S.union (fun () () -> ()) !fiState lminus

  class allUnlocksVisitor getLocks = object(self)
    inherit Pp_visitor.ppVisitor

    method vinst i =
      self#setInstrPP i;
      let pp = getCurrentPP () in
      let ls = getLocks pp in
      (* TODO: check if LS is bottom? *)
      addUnlocks (LS.LS.getMinus ls);
      self#bumpInstr 1;
      DoChildren

    method vstmt s =
      self#setStmtPP s;
      let pp = getCurrentPP () in
      let ls = getLocks pp in
      (* TODO: check if LS is bottom? *)
      addUnlocks (LS.LS.getMinus ls);
      DoChildren

  end

  let compute cfg getLocks =
    initState ();
    let vis = new allUnlocksVisitor getLocks in
    ignore (visitCilFunction (vis :> cilVisitor) cfg)
    

end


(** Package up the All Unlocks analysis *)
class auAnalysis
  (lockAnalysisSkips : fKey -> bool) 
  (getLocks : prog_point ->  RS.lockState) = object (self)

  method setInspect (yesno:bool) =
    ()

  (* Skip the same functions as the Lockset analysis skips *)
  method isFinal fkey =
    lockAnalysisSkips fkey
      
  method compute cfg =
    L.logStatus "doing all-unlocks";
    L.flushStatus ();
    AllUnlocksTransF.compute cfg getLocks

  method summarize fkey (_:fundec) =
    if self#isFinal fkey then begin
      L.logStatus ("Picking unlock-bits from LS summary for AU summary");
      let curSumOut = RS.summOutstate (RS.sum#find fkey) in
      let locks = curSumOut.RS.lState in
      sums#addReplace fkey (LS.LS.getMinus locks);
    end
    else
      sums#addReplace fkey !AllUnlocksTransF.fiState
    ;
    false

  method flushSummaries () =
    sums#serializeAndFlush

end

let lockMapper = new Relative_df.lockTransfer

let getWeakerLS curls i =
  let pp = getCurrentPP () in

  let applySum actuals ls fkey =
    let au = sums#find fkey in
    Lockset.LS.unique 
      (Lockset.LS.S.fold 
         (fun lockLv lockInfo cur ->
            lockMapper#applyUnlocked pp actuals lockLv lockInfo cur
         ) au ls)
  in

  match i with
    Call (_, Lval (Var finfo, NoOffset), actuals, _) ->
      let fkey, fname = finfo.vid, finfo.vname in
      applySum actuals curls fkey

  | Call (_, Lval (Mem exp, NoOffset), actuals, _) ->
      let aliasedFuns = Alias.deref_funptr exp in
      List.fold_left (fun wls fk -> applySum actuals wls fk) 
        curls aliasedFuns

  | _ ->
      L.logError ~prior:1
        "getWeakerLS: bad instr type";
      curls



