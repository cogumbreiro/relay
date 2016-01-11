
(** Check structure of types / funptrs to see if they appear to have
    an OO-like structure. *)

open Cil
open Cildump
open Cil_lvals
open Cilinfos
open Type_reach

(************************************************************
  Check prevalence of receiver-object-taking function ptrs 
************************************************************)

(* 
   - Partition structs into as having many FP fields and few FP fields
   (FPS, NFPS)
   - for each FPS, find structs that can transitively reach an FPS
   - check if the signature of the functions in an FPS take one
     of those structs that can transitively reach an FPS
   TODO: limit the levels of indirection to reach ops to 1 or 2?
*)


module FM = Map.Make 
  (struct 
     type t = float
     let compare a b = Pervasives.compare a b
   end)
              
let getStructsWithFP pctMin pctLevels =
  FM.fold (fun pct ss acc -> 
             if pct >= pctMin 
             then List.rev_append acc ss
             else acc) pctLevels []

let getCompDeclLoc ci =
  let locWrap = List.fold_left 
    (fun l fi -> 
       match l with
         None -> 
           if compareLoc locUnknown fi.floc != 0 
           then Some fi.floc else None
       | Some _ -> l) None (!Cil.getCfields ci) in
  match locWrap with None -> locUnknown | Some l -> l


let printFPLevels pctLevels =
  FM.iter (fun pct ss ->
             Printf.printf "> %f frac:\n" pct;
             List.iter (fun ci -> Printf.printf "  %s : %s\n" 
                          ci.cname (string_of_loc (getCompDeclLoc ci))) ss;
             Printf.printf "(%d)\n\n" (List.length ss)
          ) pctLevels

let roundPct pct =
  if 0.75 <= pct then 0.75
  else if 0.50 <= pct && pct < 0.75 then 0.5
  else if 0.25 <= pct && pct < 0.5 then 0.25
  else if 0.0001 <= pct && pct < 0.25 then 0.0001
  else 0.0

let addPctStructBinding pct s m =
  let newPct = roundPct pct in
  let old = try FM.find newPct m with Not_found -> [] in
  FM.add newPct (s :: old) m
    
    
let partitionFpStructs () =
  (* Map from (pct of fields are FPs in struct) to (list of such structs) *)
  let pctFpToStruct = ref FM.empty in
  Cilinfos.iterCompinfos
    (fun compinfo ->
       let fpFields, nonFPFields =
         List.fold_left 
           (fun (fp, nonfp) finfo ->
              let unrolledType = unrollTypeNoAttrs finfo.ftype in
              if hitsFunptr unrolledType then
                (fp + 1, nonfp) else (fp, nonfp + 1)
           ) (0, 0) (!Cil.getCfields compinfo) in
       let pct = (float_of_int fpFields /.
                     float_of_int (fpFields + nonFPFields)) in
       pctFpToStruct := addPctStructBinding pct compinfo !pctFpToStruct
    );
  !pctFpToStruct

(************************************************************)

let checkReachOps fpStructs =
  Printf.printf "Finding other structs that reach ops\n";
  flush stdout;
  let fpStructCkeyToReacher = Hashtbl.create 17 in
  let addReacher targetCi sourceCi =
    let old = 
      try Hashtbl.find fpStructCkeyToReacher targetCi.ckey 
      with Not_found -> [] in
    Hashtbl.replace fpStructCkeyToReacher targetCi.ckey (sourceCi :: old)
  in
  Cilinfos.iterCompinfos
    (fun srcCi -> 
       List.iter 
         (fun fpCi -> 
            if fwdReachesStruct fpCi srcCi then addReacher fpCi srcCi
         ) fpStructs
    );
  fpStructCkeyToReacher


let printReachers reachers =
  Printf.printf "Structs that can reach an op struct\n";
  Hashtbl.iter 
    (fun targCkey srcList ->
       let targ = getCinfo targCkey in
       Printf.printf "%s <-\n" targ.cname;
       List.iter (fun src -> Printf.printf "  %s\n" src.cname) srcList;
       Printf.printf "(%d)\n\n" (List.length srcList)
    ) reachers

let passesSelf opStructs =
  let reachers = checkReachOps opStructs in
  printReachers reachers;
  let checkedOps = ref 0 in
  let totalOps = List.length opStructs in
  let updateStatus () =
    incr checkedOps; 
    if (!checkedOps mod 100 == 0) 
    then Printf.printf "Checked ops for OO: %d/%d\n" !checkedOps totalOps
  in
  List.fold_left
    (fun cur ci ->
       updateStatus ();
       if ci.cname = "consw" then 
         Printf.printf "gotcha\n";
       try
         let reachesCi = Hashtbl.find reachers ci.ckey in
         let numYes, numNo = List.fold_left
           (fun (y, n) fi ->
              match getFPArgs fi with
                None -> (y, n) (* don't count against it if field isn't FP *)
              | Some args ->
                  if List.exists (argReachesStructs reachesCi) args
                  then (y+1, n) else (y, n+1)
           ) (0, 0) (!Cil.getCfields ci) in
         let pct = (float_of_int numYes /.
                      float_of_int (numYes + numNo)) in
         addPctStructBinding pct ci cur
       with Not_found ->
         Printf.printf "No other struct reaches: %s\n" ci.cname;
         cur
    ) FM.empty opStructs

let pctTol = 0.5

let checkOOness root =
  let fpPctLevels = partitionFpStructs () in
  Printf.printf "Structs w/ n pct fields being funptrs:\n";
  printFPLevels fpPctLevels;
  flush stdout;
  Printf.printf "Filtering out structs w/ < %f FPs\n" pctTol;
  flush stdout;
  let possibleOpStructs = getStructsWithFP pctTol fpPctLevels in
  Printf.printf "Now checking if ops funcs take self-like arg\n";
  flush stdout;
  let passesSelf = passesSelf possibleOpStructs in
  Printf.printf "Structs w/ n pct funptr fields receiving as param a 'self':\n";
  printFPLevels passesSelf;
  flush stdout



(************************************************************)

let getFPStructs () =
  let pctToStruct = partitionFpStructs () in
  FM.fold (fun pct ss acc -> 
             if pct > 0.0 
             then List.rev_append acc ss
             else acc) pctToStruct []
    
