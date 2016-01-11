
(** Generate a "driver" to run any radar clients *)

open Cil
open Gc_stats
open Pretty
open Fstructs
open Logging

type radarMode = RadSeq | RadAdj | RadAdjNL | RadPess | RadUnknown



module type RADAR_ANAL = sig
  
  val main : unit -> unit

end

module MakeRadarAnal 
  (AnaSeq : Radar.ANA)
  (AnaAdjL : Radar.ANA)
  (AnaAdjNL : Radar.ANA)
  (AnaPess : Radar.ANA) : RADAR_ANAL = struct

  module RS = Racestate.RS
  module SPTA = Racestate.SPTA
  module Th = Threads
  module BS = Backed_summary  
  module DC = Default_cache
  module Dis = Distributed
  module Stat = Mystats


  (***************************************************)
  (* Commandline handling                            *)

  let cgDir = ref ""

  let configFile = ref "client.cfg"

  let logDir = ref ""

  let restart = ref false

  let no_warn = ref true

  let userName = ref ""

  let statusFile = ref ""

  let mode = ref RadUnknown

  let setMode str = 
    mode := 
      (match str with
         "seq" -> RadSeq
       | "adj" -> RadAdj
       | "adjnl" -> RadAdjNL
       | "pess" -> RadPess
       | _ -> RadUnknown)


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
     ("-time", Arg.Set Stat.doTime, "Time operations");
     ("-mem", Arg.Set Osize.checkSizes, "Check memory usage");
     ("-mode", Arg.String setMode, "Change mode: {seq, adj, adjnl, pess}");
    ]

  let anonArgFun (arg:string) : unit = 
    ()

  let usageMsg = Stdutil.getUsageString "-cg fname -u username [options]\n"


  (***************************************************)
  (* Run                                             *)


  (** Initialize function summaries, and watchlist of special 
      functions (e.g., pthread_create) *)
  let initSettings () =
    let settings = Config.initSettings !configFile in
    Request.init settings;
    Checkpoint.init !statusFile;
    DC.makeLCaches (!cgDir);
    Cilinfos.reloadRanges !cgDir;
    Alias.initSettings settings !cgDir;
    Th.initSettings settings;
    (* Get Callgraph structures *)
    let cgFile = Dumpcalls.getCallsFile !cgDir in
    let cg = Callg.readCalls cgFile in
    let sccCG = Scc_cg.getSCCGraph cg in 
    let () = BS.init settings !cgDir cg sccCG in
    Dis.init settings !cgDir;
    Request.setUser !userName;
    SPTA.init settings cg (RS.sum :> Modsummaryi.modSum);
    Entry_points.initSettings settings;
    let _ = File_serv.init settings in (* ignore thread created *)
    let gen_num = Request.initServer () in
    if ( !restart ) then begin
      logStatus "trying to clear old summaries / local srcs, etc.";
      flushStatus ();
      BS.clearState gen_num;
      Request.clearState gen_num;
    end;
    (settings, cg, sccCG)


  let doAnal ana : unit = begin
    let settings, cg, sccCG = initSettings () in
    Guarded_access.clearCache ();
    
    ana#initStats cg sccCG ;

    logStatus "Before Dataflow";
    logStatus "-----";
    flushStatus ();

    ana#beforeDataflow settings cg sccCG;

    logStatus "Starting bottomup analysis";
    logStatus "-----";
    flushStatus ();

    ana#computeAll cg sccCG;
    
    logStatus "Bottomup analysis complete";
    logStatus "-----";
    flushStatus ();

    logStatus "After Dataflow";
    logStatus "-----";
    flushStatus ();

    ana#afterDataflow cg !cgDir;
  end

  let pickAnalysis () =
    match !mode with
      RadSeq -> new AnaSeq.anaClass
    | RadPess -> new AnaPess.anaClass
    | RadAdj -> new AnaAdjL.anaClass
    | RadAdjNL -> new AnaAdjNL.anaClass
    | RadUnknown -> failwith "pickAnalysis given RadUnknown"

  let printStatistics () = begin
    Stat.print stdout "STATS:\n";
    printStatistics ();
    flushStatus ()
  end

  let main () =
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "" || !configFile = "" || !userName = ""
        || !mode = RadUnknown ) then begin
      Arg.usage argSpecs usageMsg;
      exit 1
    end else begin
      if (!logDir <> "") then begin
        setStatLog !logDir;
        setErrLog !logDir;
      end;
      combineLogs (); (* Make sure errors show up in the right place *)
      Stdutil.printCmdline ();
      Pervasives.at_exit printStatistics;
      Cil.initCIL ();
      setGCConfig ();

      logStatus "About to do analysis";
      logStatus "-----";
      doAnal (pickAnalysis ());

      Alias.finalizeAll ();
      exit 0;
    end
    

end (* end MakeRadarAnal functor *)


