(** Test extent of the cfields linking bug *)

open Cil
open Gc_stats
open Printf
open Stdutil

(******** ARG MANAGEMENT ********)
let cgFile = ref ""

let cgDir = ref ""

let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "call graph file");
  ]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname [options]\n"

(********************************)

let printEmptyCfields () =
  let count = ref 0 in
  printf "Structs with empty cfields:\n";
  Cilinfos.iterCompinfos 
    (fun compinfo ->
       if (!Cil.getCfields compinfo = []) then
         (incr count;
          printf "%s\n" compinfo.cname)
    );
  printf "total: %d\n" !count

let initSettings () = begin
  Cilinfos.reloadRanges !cgDir;
  Default_cache.makeLCaches (!cgDir);
end

let finalPrint () =
  printStatistics ()
  
(** Entry point *)
let main () = 
  Arg.parse argSpecs anonArgFun usageMsg;
  
  (* Didn't know how to require the -cg file, etc., so check manually *)
  if (!cgFile = "") then
    begin
      Arg.usage argSpecs usageMsg;
      exit 1
    end
  else
    begin
      Pervasives.at_exit finalPrint;
      Cil.initCIL ();
      setGCConfig ();
      
      cgDir := Filename.dirname !cgFile;
      let () = initSettings () in

      printEmptyCfields ();
      exit 0;
    end

;;

main () ;;

