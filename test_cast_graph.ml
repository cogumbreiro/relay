
(** Check the casting that occurs in a program *)

open Gc_stats
open Stdutil 
open Cil
open Logging

module DC = Default_cache
module Cast = Cast_hierarchy

(***************************************************)
(* Commandline handling                            *)

let cgDir = ref ""

(** Where to store the generated dot files *)
let outDir = ref ""

let restart = ref false

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "name of call graph directory");
   ("-o", Arg.Set_string outDir, "name of output directory");
   ("-r", Arg.Set restart, "restart (remove old graph)");
  ]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg dir -o outfile [options]\n"

let doDot g =
  logStatus "Writing do graph";
  if !outDir = "" then
    let oc = stdout in
    Cast.CilCastG.TypeDot.dotGraphChannel g oc 
  else begin
    let outfile = Filename.concat !outDir "cast_graph.dot" in
    let oc = open_out outfile in
    Cast.CilCastG.TypeDot.dotGraphChannel g oc;
    close_out oc;
    logStatusF "Wrote dot graph to %s\n" outfile;
  end

let testStructReach g =
  let makePtrToStruct ci1 =
    TPtr (TComp (ci1, []), [])
  in
  let checkReach matches ci1 t1 ci2 =
    if ci1.ckey = ci2.ckey then ()
    else if Cast.CilCastG.isSubT g t1 (makePtrToStruct ci2) then begin
      if !matches = 0 then 
        logStatusF "Struct %s ~>\n" ci1.cname;
      incr matches;
      logStatusF "   %s\n" ci2.cname
    end
  in
  logStatus "Non-trivial possible subType relationships based on casts";
  Cilinfos.iterCompinfos 
    (fun ci1 -> 
       let numMatches = ref 0 in
       let t1 = makePtrToStruct ci1 in
       Cilinfos.iterCompinfos (checkReach numMatches ci1 t1);
       if !numMatches > 0 then
         logStatusF "Total matches: %d\n" !numMatches
    )


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
        Cilinfos.reloadRanges !cgDir;
        DC.makeLCaches (!cgDir);
        logStatus "Preparing cast graph";
        logStatus "-----";
        flushStatus ();
        let castG = Cast.getCastInfo !cgDir !restart in
        doDot castG;
        testStructReach castG;
        printStatistics ();
        exit 0;
      end
  with e -> 
    logStatus ("Exc. in cg_to_dot: " ^
                    (Printexc.to_string e)) ;
    printStatistics ();
    flushStatus ();
    raise e
;;
main () ;;
