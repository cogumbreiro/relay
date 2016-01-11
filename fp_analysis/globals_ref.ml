
(** Pre-pass to store all the globals referenced directly by a function 
    Also, visit all initializers and set up the initial global store. *)

open Cil
open Fstructs
open Fp_types
open Fp_lattice_ops
open Fp_slice
open Logging
open Cildump


(* Misc data structures *)

module FH = Hashtbl.Make
  (struct 
     type t = fKey
     let equal a b = a = b
     let hash x = Hashtbl.hash x
   end)

module IS = Sparsebitv

type greader = Summary_keys.sumKey * prog_point

type readerOpt = 
    GReader of greader * raw_offset * structInfo
  | GNone 

module RS = Set.Make
  (struct
     type t  = greader
     let compare (sk1, pp1) (sk2, pp2) =
       let c = Pervasives.compare sk1 sk2 in
       if c == 0 then 
         Pervasives.compare pp1 pp2
       else c
   end)

(************************************************************)

(* Organize global summaries as clusters of globals. 
   Globals may include original program variables as well as global heap nodes.
   How to choose clusterID? 
    - globals connected by pointers have same clusterID ? Too much work.
    - hash of declaring file? 
    - also keep in mind we need to find matching heap nodes cheaply?
*)


type globalInfo =
    { gReaders : RS.t OffsetMap.t; 
      (* Trigger re-reading by offset not just var (not using width info) *)
      gVal : fvalue; }
      
type globalCluster = globalInfo VarMap.t

let hc_globalCluster gc =
  (* TODO *)
  gc


module FPGlobalSum = struct

  type t = globalCluster

  type simpleSum = t
  let simplify sum = hc_globalCluster sum
  let desimplify simp = hc_globalCluster simp
  let initVal = VarMap.empty
  let unknownSummary = VarMap.empty

end

(* Not using the "safer" version where it tries to request a newer
   copy if the current copy is corrupted *)
module FPGS = Backed_summary.Make (FPGlobalSum)

let globalSum = new FPGS.data (Backed_summary.makeSumType "fpgs")
let () = Backed_summary.registerType globalSum
  

let flushGlobals () : unit =
  globalSum#serializeAndFlush

let varToClusterID gvar =
  let id = (match gvar with
              FVar vid -> 
                let vi = varinfoOfVid vid in 
                Hashtbl.hash vi.vdecl
            | FInput _ | FHeap _ | FRet _ -> hashVar gvar) in
  id lsr 2 (* To get some more clustering *)

let getVarSumKey var =
  let clusterID = varToClusterID var in
  Summary_keys.inputFreeSumKey clusterID



(*** Cached-lookup / store ***)

(* TODO: just use Cache_sum module ... *)

module GCache = Cache.Make 
  (struct

     type t = Summary_keys.sumKey
     let equal a b = a = b
     let hash a = Hashtbl.hash a
     let evictHook k v =
       globalSum#addReplace k v;
       globalSum#flushOne k

   end)
  
let gcache = GCache.create 100

let getCluster sumKey = 
  try GCache.find gcache sumKey
  with Not_found ->
    let v = globalSum#find sumKey in
    ignore (GCache.add gcache sumKey v);
    v

let setCluster sumKey clust =
  GCache.replace sumKey clust



(*** Cluster operations ***)

let freshClusterInfo var off sinfo =
  { gReaders = OffsetMap.empty;
    gVal = defaultValTypeInfo sinfo; }


let singletonStore var clus =
  try 
    let clustInfo = VarMap.find var clus in
    VarMap.add var clustInfo.gVal VarMap.empty
  with Not_found -> 
    let sinfo = getStructInfo (typeOfFVar var) in
    let fresh = freshClusterInfo var noOff sinfo in
    VarMap.add var fresh.gVal VarMap.empty

let addReader sumKey cluster var off sinfo reader =
  let oldClustI =     
    try VarMap.find var cluster 
    with Not_found -> freshClusterInfo var off sinfo in
  let oldRS =
    try OffsetMap.find off oldClustI.gReaders
    with Not_found -> RS.empty in
  let newRS = RS.add reader oldRS in
  let newClustI = 
    { oldClustI with gReaders = OffsetMap.add off newRS oldClustI.gReaders; } in
  let newCluster = VarMap.add var newClustI cluster in
  globalSum#addReplace sumKey newCluster;
  newCluster




(** Look up the value from a global. *)
let getGlobalBinding var readerOpt =
  let var = Fp_unify_globals.gRep var in
  let sumKey = getVarSumKey var in
  let cluster = globalSum#find sumKey in
  let cluster = 
    (match readerOpt with
       GReader (reader, off, sinfo) -> 
         addReader sumKey cluster var off sinfo reader
     | GNone -> cluster ) in
  try 
    let oldClustI = VarMap.find var cluster in
    oldClustI.gVal
  with Not_found ->
    logStatusF "getGlobalBinding: no binding for global %s\n"
      (string_of_var var);
    let sinfo = getStructInfo (typeOfFVar var) in
    let fresh = freshClusterInfo var noOff sinfo in
    let newCluster = VarMap.add var fresh cluster in
    globalSum#addReplace sumKey newCluster;
    fresh.gVal  

(* If value isn't found, go ahead and return the default. All we
   need is to note this instruction as a reader to re-execute 
   when the value changes. *)


let hasGlobalBinding var =
  let sumKey = getVarSumKey var in
  let cluster = globalSum#find sumKey in
  VarMap.mem var cluster

  
let updateBinding newBinding var off sinfo cluster =
  let newVal = VarMap.find var newBinding in
  let oldClustI =     
    try VarMap.find var cluster 
    with Not_found -> freshClusterInfo var off sinfo in
  let newClustI = { oldClustI with gVal = newVal; } in
  newClustI, VarMap.add var newClustI cluster


(** Only use before any reads -- does not notify readers / merge w/ old value *)
let strongUpdateGlobal var off rhs sinfo =
  let sumKey = getVarSumKey var in
  let cluster = globalSum#find sumKey in
  let oldVarBinding = singletonStore var cluster in
  let newVarBinding = strongUpdate oldVarBinding var off rhs sinfo in
  (* Manually update the store *)
  let _, newCluster = updateBinding newVarBinding var off sinfo cluster in
  globalSum#addReplace sumKey newCluster


let getTheCluster var =
  let sumKey = getVarSumKey var in
  let cluster = globalSum#find sumKey in
  let clusterInfo =     
    try VarMap.find var cluster
    with Not_found -> 
      let sinfo = structInfoFVar var in
      freshClusterInfo var noOff sinfo
  in
  (sumKey, cluster, clusterInfo)

let defaultReaders off = RS.empty

let combineReaders r1 r2 =
  OffsetMap.unionC RS.union defaultReaders r1 r2
  
let rec unifyGlobalTargets gv =
  match gv with
    Refs locs ->
      let heapVars = FLocSet.fold 
        (fun (var, o) cur ->  
           match var with 
           FHeap _ | FInput _ -> VarSet.add var cur 
         | FVar _ | FRet _ -> cur) locs VarSet.empty in
      let byType = partitionByType heapVars in
      List.iter 
        (fun sameT ->
           match sameT with
             [_] | [] -> ()
           | h :: t -> 
               logStatusF "merged globals: %s <- {%s}\n" 
                 (string_of_var h) (string_of_var_list t);
               List.iter (doUnify h) t
        ) byType

  | Records recs ->
      List.iter 
        (fun (fp, nonfp, m) ->
           OffsetMap.iter (fun off v -> unifyGlobalTargets v) nonfp) recs
  | FIRecs fir ->
      List.iter (fun (fp, nonfp) -> unifyGlobalTargets nonfp) fir
  | FInt _ | FpRef _ | FNFP _ -> ()

and doUnify v1 v2 =
  let v1 = Fp_unify_globals.gRep v1 in
  let v2 = Fp_unify_globals.gRep v2 in
  if eqFVar v1 v2 then ()
  else begin
    Fp_unify_globals.unifyGlobal v1 v2;
    (* Combine vals and readers if they had that info before *)
    let sumKey1, cluster1, clusterI1 = getTheCluster v1 in
    let sumKey2, cluster2, clusterI2 = getTheCluster v2 in
    let newVal = 
      combineVals (valUseReps clusterI1.gVal) (valUseReps clusterI2.gVal) in
    let newClusterI = 
      { gReaders = combineReaders clusterI1.gReaders clusterI2.gReaders;
        gVal = newVal; } in
    let newCluster = VarMap.add v1 newClusterI cluster1 in
    globalSum#addReplace sumKey1 newCluster;
    unifyGlobalTargets newVal (* Trigger more *)
  end  


(** The more common update -- returns a set of readers to notify if new *)
let updateGlobal var off rhs sinfo =
  let var = Fp_unify_globals.gRep var in
  let sumKey, cluster, _ = getTheCluster var in
  let oldStore = singletonStore var cluster in
  let rhs = valUseReps rhs in
  let newStore = weakUpdate oldStore var off rhs sinfo in
  if eqStores oldStore newStore then RS.empty
  else begin
    let comboVal = VarMap.find var newStore in
    logStatusF "New global @ %s: %s\n" (string_of_loc !currentLoc)
      (string_of_var_val var comboVal);
    let newCI, newCluster = updateBinding newStore var off sinfo cluster in
    globalSum#addReplace sumKey newCluster;
    unifyGlobalTargets comboVal;

    (* Unification may have changed the cluster... get it again! *)
    let var = Fp_unify_globals.gRep var in
    let _, _, clusterI = getTheCluster var in
    try OffsetMap.find off clusterI.gReaders
    with Not_found -> RS.empty
  end


(** Do an update on the variable as a whole (vs var + off). Also, do
    not merge values if there was no binding previously *)
let updateGlobalBinding var rhs =
  let var = Fp_unify_globals.gRep var in
  let sumKey, cluster, _ = getTheCluster var in
  let oldStore = singletonStore var cluster in
  let doUpdate finalVal oldStore = 
      let newStore = VarMap.add var finalVal oldStore in
      logStatusF "New global @ %s: %s\n -> %s\n" 
        (string_of_loc !currentLoc) 
        (string_of_var var) (string_of_val finalVal);
      let sinfo = structInfoFVar var in
      let _, newCluster = updateBinding newStore var noOff sinfo cluster in
      globalSum#addReplace sumKey newCluster;

      unifyGlobalTargets finalVal;
      (* Unification may have changed the cluster... get it again! *)
      let var = Fp_unify_globals.gRep var in
      let _, _, newCI = getTheCluster var in

      (* Notify ALL readers of all offsets since the whole binding changed *)
      OffsetMap.fold (fun _ rs cur -> RS.union rs cur) newCI.gReaders RS.empty
  in
  try
    let rhs = valUseReps rhs in
    let oldVal = VarMap.find var oldStore in
    let oldVal = valUseReps oldVal in
    let comboVal = combineVals oldVal rhs in
    if eqVals oldVal comboVal then (RS.empty, rhs)
    else begin
      let comboVal = valUseReps comboVal in
      (doUpdate comboVal oldStore, comboVal)
    end
  with Not_found ->
    (* No old value... just add it *)
    let rhs = valUseReps rhs in
    (doUpdate rhs oldStore, rhs)

      
let printGlobalStats () =
  logStatus "Printing init global bindings:\n";
  let numBindings = globalSum#fold
    (fun key cluster cur ->
       VarMap.iter 
         (fun var cinfo ->
            printVar var; printVal var cinfo.gVal
         ) cluster;
       cur + VarMap.cardinal cluster
    ) 0 in
  logStatusF "Have initial values for %d globals\n" numBindings  


