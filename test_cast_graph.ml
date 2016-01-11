
(** Check the casting that occurs in a program *)

open Gc_stats
open Stdutil 
open Cil

module Cast = Cast_hierarchy

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

(** Where to store the generated dot files *)
let outDir = ref ""

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-o", Arg.Set_string outDir, "name of output directory");]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg dir -o outfile [options]\n"

let doDot g =
  L.logStatus "Writing do graph";
  if !outDir = "" then
    let oc = stdout in
    Cast.TypeDot.dotGraphChannel g oc 
  else begin
    let outfile = Filename.concat !outDir "cast_graph.dot" in
    let oc = open_out outfile in
    Cast.TypeDot.dotGraphChannel g oc;
    close_out oc;
    L.logStatusF "Wrote dot graph to %s\n" outfile;
  end


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;

    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgDir = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Cil.initCIL ();
        L.logStatus "Preparing cast graph";
        L.logStatus "-----";
        L.flushStatus ();
        let castG = Cast.getCastInfo !cgDir in
        doDot castG;
        printStatistics ();
        exit 0;
      end
  with e -> 
    L.logStatus ("Exc. in cg_to_dot: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    L.flushStatus ();
    raise e
;;
main () ;;
