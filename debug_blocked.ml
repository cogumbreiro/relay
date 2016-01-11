(** Given a list of functions that blocked (didn't generate a summary).
    Return the list of functions that are furthest down in the 
    context-insensitive SCC callgraph. *)

open Stdutil 
open Fstructs
open Scc_cg
open Callg
open Logging


(***************************************************)
(* Commandline handling                            *)

let cgFile = ref ""
let cgDir = ref ""
let configFile = ref "client.cfg"
let inFile = ref ""

(* Command-line argument parsing *)
  
let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "name of call graph file");
   ("-i", Arg.Set_string inFile, "name of input file");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-cg file [options]\n"

(************************************************************)

let readBlockedFuncs () =
  let ic = open_in !inFile in
  let cur = ref FSet.empty in
  (try
     while true do
       let next = input_line ic in
       cur := FSet.add (fid_of_string next) !cur;
     done
   with End_of_file -> ());
  !cur

type visNum = 
    { mutable preNum : int;
      mutable postNum : int; }
      
let getRoots scccg = 
  IntMap.fold 
    (fun sccNum scc cur ->
       if Scc_cg.isRoot scc then (sccNum, scc) :: cur else cur
    ) scccg []

let findLowest cg scccg funcs = 
  let visNums = Hashtbl.create 17 in
  let importantNums = Hashtbl.create 17 in
  let curNum = ref 0 in
  let nextNum () = 
    let x = !curNum in
    incr curNum;
    x
  in
  let rec visit (sccNum, scc) =
    if Hashtbl.mem visNums sccNum then ()
    else 
      let nums = { preNum = nextNum (); postNum = -1; } in
      Hashtbl.replace visNums sccNum nums;
      let impFuns = FSet.inter scc.scc_nodes funcs in
      if FSet.is_empty impFuns then ()
      else begin
        FSet.iter 
          (fun fkey ->
             Hashtbl.replace importantNums fkey nums
          ) impFuns
      end;
      (* visit them children *)
      IntSet.iter 
        (fun sccNum2 ->
           let scc2 = IntMap.find sccNum2 scccg in
           visit (sccNum2, scc2)
        ) scc.scc_callees;
      nums.postNum <- nextNum ();
  in
  List.iter visit (getRoots scccg);
  (* Post process... *)
  Printf.printf "Lowest-level blocked funs:\n";
  let funList = Hashtbl.fold 
    (fun fk nums cur ->
       let hasChild = 
         Hashtbl.fold 
           (fun fk2 nums2 hasChild ->
              hasChild || (nums.preNum < nums2.preNum &&
                             nums.postNum > nums2.postNum)
           ) importantNums false in
       if not hasChild then
         try
           let node = FMap.find fk cg in
           Printf.sprintf "%s\t%s" node.name (fid_to_string fk) :: cur
         with Not_found ->
           Printf.sprintf "???\t%s" (fid_to_string fk) :: cur
       else cur
    ) importantNums [] in
  List.iter (logStatus) (List.sort (Pervasives.compare) funList)
    

let doDebug () =
  cgDir := Filename.dirname !cgFile;
  Default_cache.makeLCaches !cgDir;
  let cg = readCalls !cgFile in
  let sccGraph = Scc_cg.getSCCGraph cg in
  (* Read in the list of blocking fkeys, etc ... *)
  let blockedFuncs = readBlockedFuncs () in
  findLowest cg sccGraph blockedFuncs
    
let printStats () = 
  Gc_stats.printStatistics ()

(** Entry point *)
let main () = 
  Arg.parse argSpecs anonArgFun usageMsg;
  Pervasives.at_exit printStats;
  (* Didn't know how to require the -cg file, etc., so check manually *)
  if (!cgFile = "" || !inFile = "") then
    begin
      Arg.usage argSpecs usageMsg;
      exit 1
    end
  else
    begin
      Cil.initCIL ();
      doDebug ();
      exit 0;
    end
;;
main () ;;
