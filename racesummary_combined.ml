open Logging
open Callg
open Guarded_access_base
include Racesummary


(** Combine guarded accesses of locals and globals into one summary *)

(** Summary manager for locksets and local guarded accesses,
    given access to the global scc sums / fun -> scc ID mapper *)
class raceSummary sumID = object (self)
  inherit RaceSum.data cacheSize sumID as super
  inherit corrAsMods

  (** Override *)
  method initSummaries settings cg sccCG =
    (* Initialize summaries based on config file *)
    initialized <- true;
    initializeSumFromSettings settings cg self;
    (* Initialize the rest of the summaries for functions w/ no bodies *)
    super#initSummaries settings cg sccCG

  method private split sumPart =
    let globalR, localR = 
      GA.splitGlobalsFormals sumPart.cState.readCorrs in
    let globalW, localW = 
      GA.splitGlobalsFormals sumPart.cState.writeCorrs in
    let local = { sumPart with 
                    cState = { readCorrs = localR; writeCorrs = localW; } 
                } in
    let global = { lState = emptyLS; 
                   cState = { readCorrs = globalR; writeCorrs = globalW; }
                 } in
    global, local

  (** Specialized find that only gets the Local vars of the summary *)
  method findLocal sumKey = 
    let combo = self#find sumKey in
    let _, inL = self#split combo.sum_in in
    let _, outL = self#split combo.sum_out in
    { sum_in = inL; sum_out = outL; }

  (** Specialized find that only gets the Global vars of the summary.
      @raise Not_found if the function is external *)
  method findGlobal sumKey = 
    let combo = self#find sumKey in
    let inG, _ = self#split combo.sum_in in
    let outG, _ = self#split combo.sum_out in
    { sum_in = inG; sum_out = outG; }

  method findLockSum sumKey = 
    self#find sumKey (* it's in there... *)
    
end

let sum = new raceSummary (Backed_summary.makeSumType "rs")

let _ = Backed_summary.registerType (sum :> RaceSum.data)

(************************************************************)

let findPrintSumm funname sumKey =
  let summ = sum#find sumKey in
  printSummary funname sumKey summ

let getSumTypes () =
  [sum#sumTyp]
