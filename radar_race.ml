
(** Runs the race detection pass w/ pseudo-accesses.
    Pseudo-accesses should have already been added by the
    sequential pass of radar. 
    This pass propagates those accesses up to the thread roots. *)

open Cil
open Callg
open Fstructs
open Manage_sums
open Stdutil
open Logging

module Warn = Race_warnings2 (* incorporate into this file? *)
module Intra = IntraDataflow
module IDF = InterDataflow
module BS = Backed_summary
module Stat = Mystats
module Race = Racestate
module RS = Race.RS
module SPTA = Race.SPTA
module LP = Lockset_partitioner


module type PARDB  = sig
  val parL : Pseudo_access.PARsummary.data 
  val parNL : Pseudo_access.PARsummary.data 
end

module type PASS3DRIVER = sig val main : unit -> unit end

module MakePseudoRacePass (P : PARDB) : PASS3DRIVER = struct

  (**********************************)
  (* rkc: Taken from race_warnings2 *)
  (**********************************)

  (** Check for candidate pairs of accesses that may result in a data race 
      Assumes: function summaries computed *)
  let thisFlagRacesFromSumms cg cgDir useLocks = begin
    (*Warn.useLocks := useLocks;*)
    (*Warn.pardb := pardb; (* TODO hacky! *)*)
    Warn.parL := P.parL;
    Warn.parNL := P.parNL;
    logError ("rkcdebug: set Warn.parL to "
      ^ BS.string_of_sumType !Warn.parL#sumTyp);
    logError ("rkcdebug: set Warn.parNL to "
      ^ BS.string_of_sumType !Warn.parNL#sumTyp);
    let checker = new Warn.warningChecker cg cgDir in
    checker#run;
    checker#printAllPairCounts ();
    (*pardb#serializeAndFlush;*)
    P.parL#serializeAndFlush;
    P.parNL#serializeAndFlush;
    let race2pakeyFile = (Filename.concat cgDir Warn.r2pakey_file) in
    logStatusF "Writing race cluster_id to pakey bindings to %s\n"
      race2pakeyFile;
    Warn.saveR2PA race2pakeyFile;

    (* For now, have the client write out the warning data (separate file).
       TODO: give the server the race2pakey data so that the server
       writes out the warnings w/ IDs that match *)
    Warn.localRaces#saveToXML (Filename.concat cgDir "warnings2.xml")
  end

  (***********************************)
  (* rkc: Taken from racestate_temp2 *)
  (***********************************)

  (***************************************************)
  (* Intra-proc Analysis                             *)

  let debug = false
  let inspect = ref false

  let setInspect yesno =
    Race.setInspect yesno;
    inspect := yesno


  module RaceDF = Race.RaceDF
  module FITransF = Race.FITransF
  module RaceForwardDF = Race.RaceForwardDF
  let getLocksBefore = Race.getLocksBefore


  (***************************************************)
  (* Inter-proc Analysis                             *)
  
  
  (** Info needed by the inter-proc driver *)
  module RaceBUTransfer = struct
  

    (**** Statistics kept ****)
    
    let sccsDone = ref 0

    let sccsTotal = ref 0

    let curCG = ref emptyCG

    let curSCCCG = ref Scc_cg.emptySCCCG

    let getFunName fid : string =
      try (FMap.find fid !curCG).name
      with Not_found -> ""

    let initStats cg sccCG = 
      (curCG := cg;
       curSCCCG := sccCG;
       (* TODO: clean this up... ugly as hell *)
       Intra.curCG := cg;
       Intra.curSCCCG := sccCG;
       Race.curCG := cg;
       Race.curSCCCG := sccCG;

       (* Progress *)
       sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
       sccsDone := 0;
      )

      
    let updateStats lastSCC = begin
      (* Progress *)
      incr sccsDone;
      logStatusF ">>> PROGRESS %d/%d SCCs DONE!\n\n" !sccsDone !sccsTotal;
      flushStatus ()
    end


    (**** State management / calculation ****)

    type state = RS.state

    let seedRS cfg fid = begin
      (* Seed the guarded access pass w/ the summary from previous pass *)
      let curSummary = RS.sum#find fid in
      let curSumOut = RS.summOutstate curSummary in
      let curGAs = curSumOut.RS.cState in
      FITransF.initState fid cfg curGAs;
    end

    (**** List of analyses that should be run (listed in order) *****)

    class ['st] writeOnlyAnalyzer = object (self)
      inherit ['st] Race.readWriteAnalyzer

      (** override addRefs to leave out the reads *)
      method addRefs curLS st loc exp =
        st

    end

    class raceAnalysis rwChecker = object (self)
      inherit Race.raceAnalysis rwChecker

      (** Override compute to seed state first *)
      method compute fid cfg =
        (* TODO: get rid of input make the FID setting cleaner *)
        let input = RS.emptyState in
        RaceDF.initState fid cfg input.RS.lState;
        RaceForwardDF.clearPPData ();
        seedRS cfg fid;
        rwChecker#setFunID fid;
        Stat.time "Race/Lockset DF: " 
          (fun () ->
             (* Compute locksets *)
             logStatus "doing lockset";
             flushStatus ();
             RaceForwardDF.compute cfg.sallstmts;
             
             (* Update read/write correlation info *)
             logStatus "doing guarded access";
             flushStatus ();
             let gaVisitor = new FITransF.guardedAccessSearcher 
               rwChecker getLocksBefore in
             ignore (visitCilFunction (gaVisitor :> cilVisitor) cfg);
          ) ()

    end

    let ssAna = new SPTA.symexAnalysis
    let rsAna = new raceAnalysis (new writeOnlyAnalyzer)

    let needsFixpoint = [ ssAna; rsAna ]

    let nonFixpoint = []

    let flushSummaries () = begin
      BS.printSizeOfAll "Summarize (pre-flush)";
      RS.sum#serializeAndFlush;
      SPTA.SS.sum#serializeAndFlush;
      (*pardb#serializeAndFlush; *)
      LP.sums#evictSummaries;
      BS.printSizeOfAll "Summarize (post-flush)";
    end
        
    let doFunc ?(input:state = RS.emptyState) fid node : state IDF.interResult =
      let fn, defFile = node.name, node.defFile in
      logStatusF "Summarizing function: %s(%s):%s\n" 
        fn (fid_to_string fid) defFile;
      logStatus "-----";
      flushStatus ();
      match Cilinfos.getFunc (fid_to_fkey fid) defFile with
        Some cfg ->
          if Intra.runFixpoint needsFixpoint fid cfg then
            IDF.NewOutput (input, input) (* TODO: change return type *)
          else
            IDF.NoChange
          
      | None ->
          (* Don't have function definition *)
          logErrorF "doFunc can't get CFG for: %s:%s\n" fn defFile;
          IDF.NoChange 


    (** TRUE if the function should be put on the worklist *)
    let filterFunc f : bool =
      true

    let hardCodedSumTypes ()  =
      BS.getDescriptors [RS.sum#sumTyp;
                         SPTA.SS.sum#sumTyp;
                         (*pardb#sumTyp;*)
                         P.parL#sumTyp;
                         P.parNL#sumTyp;
                         LP.sums#sumTyp]

    (** Prepare to start an scc, acquiring the required summaries *)
    let sccStart scc  = begin
      logStatus "Acquiring needed summaries";
      flushStatus ();
      prepareSCCCalleeSums !curSCCCG scc (hardCodedSumTypes ());

      logStatus "Acquiring RS/SS summaries for current SCC";
      prepareSCCSums scc (hardCodedSumTypes ());
    end

    (** Scc is summarized. Do the rest *) 
    let sccDone scc (byThisGuy:bool) =
      let summPaths = if (byThisGuy) then
        let sumKeys = sumKeysOfScc scc [] in
        (* Now that all locksets have been computed, do remaining passes *)
        let prevFKey = !Race.curFunID in
        Intra.runNonFixpoint nonFixpoint needsFixpoint prevFKey scc;
  
        (* Debugging *)
        List.iter 
          (fun fkey -> ();
             let name = getFunName fkey in
             logStatusF "Summary for function: %s:%s\n" 
               name (fid_to_string fkey);
             logStatus "=======\n";
             RS.findPrintSumm name fkey;
             SPTA.SS.printSummary SPTA.SS.sum fkey;
          ) sumKeys;

        (* Serialize and record where each fun was placed *)
        flushSummaries ();
        
        (* Find out where the summaries were stored *)
        (* TODO: force them to pick the same directory (which they do
           unless chosen disk partition runs out of space) *)
        let tokenMap = RS.sum#locate sumKeys in
         
        (* Notify others that the functions are now summarized *)
        List.fold_left
          (fun paths (fkey, tok) ->
             let path = BS.pathFromToken tok in
             (fkey, path) :: paths
          ) [] tokenMap
      else [] in
      updateStats scc; (* and possibly delete obsolete summaries *)
      summPaths

  end (* end RaceBUTransfer module *)

  module BUDataflow = IDF.BottomUpDataflow (RaceBUTransfer)


  (***********************************)
  (* rkc: Taken from race_temp2_anal *)
  (***********************************)

  (***************************************************)
  (* Commandline handling                            *)

  let cgDir = ref ""

  let configFile = ref "client.cfg"

  let logDir = ref ""

  let restart = ref false

  let no_warn = ref false

  let userName = ref ""

  let statusFile = ref ""

  (* Command-line argument parsing *)

  let argSpecs = 
    [("-cg", Arg.Set_string cgDir, "name of call graph directory");
     ("-su", Arg.Set_string configFile,
        "name of config/summary bootstrap file");
     ("-i", Arg.String Inspect.inspector#addInspect,
        "inspect state of function (given name)");
     ("-nw", Arg.Set no_warn, "do not generate warnings");
     ("-r", Arg.Set restart, "causes analyzer to clear state and restart");
     ("-u", Arg.Set_string userName, "username to use");
     ("-l", Arg.Set_string logDir, "log status and errors to given dir");
     ("-st", Arg.Set_string statusFile,
      "file storing work status (current scc/pass/analysis)");
     ("-vs", Arg.Set RS.verboseSum, 
      "print verbose function summaries");
     ("-time", Arg.Set Stat.doTime, "Time operations");
     ("-mem", Arg.Set Osize.checkSizes, "Check memory usage");
    ]

  let anonArgFun (arg:string) : unit =
    ()

  let usageMsg = getUsageString "-cg fname -u username [options]\n"


  (***************************************************)
  (* Run                                             *)


  (** Initialize function summaries, and watchlist of special 
      functions (e.g., pthread_create) *)
  let initSettings () =
    let settings = Config.initSettings !configFile in
    Request.init settings;
    Checkpoint.init !statusFile;
    Default_cache.makeLCaches !cgDir;
    Threads.initSettings settings;
    Cilinfos.reloadRanges !cgDir;
    Alias.initSettings settings !cgDir;
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = Callg.readCalls cgFile in
    let sccCG = Scc_cg.getSCCGraph cg in 
    let () = BS.init settings !cgDir cg sccCG in
    SPTA.init settings cg (RS.sum :> Modsummaryi.modSum);
    Distributed.init settings !cgDir;
    Request.setUser !userName;
    Entry_points.initSettings settings;
    let _ = File_serv.init settings in (* ignore thread created *)
    let gen_num = Request.initServer () in
    if ( !restart ) then begin
      logStatus "trying to clear old summaries / local srcs, etc.";
      flushStatus ();
      BS.clearState gen_num;
      Request.clearState gen_num;
    end;
    (cg, sccCG)

  (** Initiate analysis *)
  let doRaceAnal () : unit = begin
    let cg, sccCG = initSettings () in

    (* Then do a bottom-up analysis *)
    RaceBUTransfer.initStats cg sccCG ;

    logStatus "Starting bottomup analysis";
    logStatus "-----";
    flushStatus ();

    BUDataflow.compute cg sccCG;

    logStatus "Bottomup analysis complete";
    logStatus "-----";
    flushStatus ();

    if (not !no_warn) then begin
      logStatus "\n\n\nBeginning Thread Analysis:";
      logStatus "-----";
      flushStatus ();  
      thisFlagRacesFromSumms cg !cgDir true;
    end;

    Warn.printGammaReport ();
  end


  let doCleanup () =
    Stat.print stdout "STATS:\n";
    Gc_stats.printStatistics ();
    flushStatus ()


  (** Entry point *)
  let main () = 
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" || !configFile = "" || !userName = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        if (!logDir <> "") then
          (setStatLog !logDir;
           setErrLog !logDir;
          )
        ;
        Logging.combineLogs ();
        Pervasives.at_exit doCleanup;
        Cil.initCIL ();
        Gc_stats.setGCConfig ();

        logStatus "Checking for data races";
        logStatus "-----";
        doRaceAnal ();

        Alias.finalizeAll ();
        exit 0;
      end
        
end
