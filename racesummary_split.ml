open Logging
open Callg
open Fstructs
open Guarded_access_base
include Racesummary

(** Split guarded access summaries into function local and SCC global *)

let funIDtoSCCID = Hashtbl.create 17
let initFunIDtoSCCID cg sccCG =
  IntMap.iter 
    (fun sccID scc ->
       FSet.iter 
         (fun funID -> 
            Hashtbl.replace funIDtoSCCID funID sccID
         ) scc.Scc_cg.scc_nodes
    ) sccCG


let funToSCCSumkey funID = 
  Summary_keys.inputFreeSumKey (Hashtbl.find funIDtoSCCID funID)

(************************************************************)


class raceSccSummaries sumID = object (self)
  inherit RaceSum.data cacheSize sumID as super

  method initSummaries settings cg sccCG =
    initialized <- true;
    initFunIDtoSCCID cg sccCG;
    (* Don't initialize the rest of the summaries for functions w/ no bodies...
       because this is not based on functions... *)
    ()

end 

    

(** Summary manager for SCC global guarded accesses *)
let sccsums = new raceSccSummaries (Backed_summary.makeSumType "rs_scc")
let _ = Backed_summary.registerType (sccsums :> RaceSum.data)


(** Summary manager for locksets and local guarded accesses,
    given access to the global scc sums / fun -> scc ID mapper *)
class raceSummary sccSums sumID = object (self)
  inherit RaceSum.data cacheSize sumID as super
  inherit corrAsMods

  (** Override *)
  method initSummaries settings cg sccCG =
    (* Initialize summaries based on config file *)
    initialized <- true;
    sccSums#initSummaries settings cg sccCG;
    initializeSumFromSettings settings cg self;
    (* Initialize the rest of the summaries for functions w/ no bodies *)
    super#initSummaries settings cg sccCG


  (** Specialized find that only gets the Local vars of the summary *)
  method findLocal sumKey = 
    super#find sumKey

  (** Specialized find that only gets the Global vars of the summary.
      @raise Not_found if the function is external *)
  method findGlobal sumKey = 
    let sccID = funToSCCSumkey sumKey in
    sccSums#find sccID
      
  (** Get only the lock summary *)
  method findLockSum sumKey =
    super#find sumKey (* stored with the local stuff *)

  (** Override to combine local GAs w/ globals *)
  method find sumKey =
    let localSum = self#findLocal sumKey in
    try
      let globalSum = self#findGlobal sumKey in
      let combine localPart globalPart = 
        let combo = 
          Mystats.time "LS combineCS (s)" 
            (combineCStates localPart.cState) globalPart.cState in
        { localPart with cState = combo; }
      in
      { sum_in = combine localSum.sum_in globalSum.sum_in;
        sum_out = combine localSum.sum_out globalSum.sum_out; }
    with Not_found ->
(*      logErrorF "noSCC ID (a) for: %s\n" (fid_to_string sumKey); *)
      localSum

  method private split part = 
    let globalR, localR = 
      GA.splitGlobalsFormals part.cState.readCorrs in
    let globalW, localW = 
      GA.splitGlobalsFormals part.cState.writeCorrs in
    let local = { part with 
                    cState = { readCorrs = localR; writeCorrs = localW; } 
                } in
    let global = { lState = emptyLS; 
                   cState = { readCorrs = globalR; writeCorrs = globalW; }
                 } in
    global, local

  (** Override to split local GAs from globals *)
  method addReplace sumKey sum =
    try
      let sccID = funToSCCSumkey sumKey in
      let inGlobal, inLocal = self#split sum.sum_in in
      let outGlobal, outLocal = self#split sum.sum_out in
      let localSum = { sum_in = inLocal; sum_out = outLocal; } in
      let globalSum = { sum_in = inGlobal; sum_out = outGlobal; } in
      super#addReplace sumKey localSum;
      sccSums#addReplace sccID globalSum
    with Not_found ->
(*      logErrorF "noSCC ID (b) for: %s\n" (fid_to_string sumKey); *)
      super#addReplace sumKey sum

  (** Override to flush scc sum *)
  method serializeAndFlush : unit =
    sccSums#serializeAndFlush;
    super#serializeAndFlush

  (** Override to flush scc sum *)
  method flushOne sumKey : unit =
    super#flushOne sumKey;
    try
      let sccID = funToSCCSumkey sumKey in
      sccSums#flushOne sccID
    with Not_found -> () (* hmm why would client flush an extern func? *)

  (** Override to get scc keys too *)
  method getDependentKeys sumKeys =
    let sccIDs = Hashtbl.create 17 in
    List.iter 
      (fun fkey ->
         try Hashtbl.add sccIDs (funToSCCSumkey fkey) ()
         with Not_found -> () (* hmm why would client request an extern func? *)
      ) sumKeys;
    let sccDeps = Hashtbl.fold
      (fun sccID () cur -> 
         (sccID, (sccSums :> Backed_summary.dbManagement)) :: cur
      ) sccIDs [] in
    sccDeps @ super#getDependentKeys sumKeys
      
end

let sum = new raceSummary sccsums (Backed_summary.makeSumType racesumID)

let _ = Backed_summary.registerType (sum :> RaceSum.data)

(************************************************************)

let findPrintSumm funname sumKey =
  let summ = sum#find sumKey in
  printSummary funname sumKey summ
    
let getSumTypes () =
  [sum#sumTyp;]

