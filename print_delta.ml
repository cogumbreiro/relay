open Cil
open Fstructs
open Stdutil

module L = Logging
module NW = Null_warnings

(***************************************************)
(* Commandline handling                            *)

let seq_file = ref ""
let adj_file = ref ""
let cgDir = ref ""
let configFile = ref "client.cfg"
let verbose = ref false

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-seq", Arg.Set_string seq_file, "sequential null warning data");
   ("-adj", Arg.Set_string adj_file, "adjusted null warning data");
   ("-v", Arg.Set verbose, "verbose");
   ("-cg", Arg.Set_string cgDir, "callgraph directory");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString
  "-seq filename -adj filename -cg callgraph\n"
  

(***************************************************)
(* Execution / Program Entry Point                 *)

let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require files, so check manually *)
    if (!cgDir = "" || !seq_file = "" || !adj_file = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        let null_seq_data = !seq_file in
        let null_adj_data = !adj_file in
        L.logStatus "Printing null deltas between: ";
        L.logStatusF "\t%s\n" null_seq_data;
        L.logStatusF "\t%s\n\n" null_adj_data;

        let settings = Config.initSettings !configFile in
        (* omitting a lot that the Alias module usually needs *)
        Default_cache.makeLCaches !cgDir;
        Cilinfos.reloadRanges !cgDir;
        Alias.initSettings settings !cgDir;

        if !verbose then NW.printDiffs null_seq_data null_adj_data;

        NW.printDeltaReport null_seq_data null_adj_data;
        exit 0;
      end
  with e -> 
    L.logError ("Exc. in : " ^ (Printexc.to_string e));
    raise e
;;
main () ;;
