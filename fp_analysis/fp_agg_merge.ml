
(** Aggressively merge states / collapse targets *)

open Cil
open Type_utils
open Fp_types
open Fp_lattice_ops
open Scope
open Pretty
open Logging
open Cildump
open Summary_keys


(*
  if merge(val1, val2) where val1 = st1(var) and val2 = st2(var) results in
  ptr { var1, var2 } unify by setting up mapping m (var2) = var1

  (1) first, only apply the mapping to m(st(var1)) = val1, 
      and m(st(var2)) = val2, do not apply mapping elsewhere yet
      as the mapping may be "incomplete" 

  (2) let x, m' = the recursive merge of the values (val1, val2) and a new map

  (3) finally, return m'(x) (do we really need to?)

  (4) when do we update the bindings? 
      - continuously replace them, then prune by:
      - keep "original" bindings (bindings for vars that had not been mapped)
      - and bindings for representatives
*)

type mergedIntos = fvar VarH.t

class type unifier = object

  method aggressiveMerge : fpState -> fvar list -> fpState

  method aggressiveMergeAll : fpState -> fpState

  (* Ugh... must expose these child functions *)
  method setFunc : sumKey -> unit
  method setVerbose : string -> unit
  method getMergedIntos : sumKey -> mergedIntos

end

class nopMerger : unifier = object (self) 

  method setFunc _ = ()
  method setVerbose _ = ()

  method aggressiveMerge st roots = st

  method aggressiveMergeAll st = st


  val emptySet = VarH.create 0

  method getMergedIntos sk =
    emptySet
    
end


(************************************************************)

class virtual absVarMerger = object (self)

  method virtual notifyMerge : fvar list -> unit

  method virtual filter : fvar list list -> fvar list list

  method virtual eligibleVar : fvar -> fpState -> bool

  method virtual getMergedIntos : sumKey -> mergedIntos

  val mutable mappings = makeMappings 0

  method getMappings = mappings

  val mutable seekedFurther = VarSet.empty
  val mutable funID = (-1, "")

  (* Debug *)
  val warnMergeTypes = Hashtbl.create 7


  method setFunc fid =
    funID <- fid

  method private init () =
    mappings <- makeMappings 7;
    seekedFurther <- VarSet.empty;
    Hashtbl.clear warnMergeTypes
      
  method private cleanup () =
    ()

  (* DEBUG *)
  method private checkMergeTypes locs =
    let ignoreT var t =
      Type_reach.isPoly t || isUnknownMallocVar var
    in
    match FLocSet.fold 
      (fun (var, o) (t1, t2) ->
         match (t1, t2) with
           None, None -> 
             (try
                let typ = typeOfLoc (var, o) in
                if ignoreT var typ then (t1, t2)
                else (Some (canonType (typeModArray typ)), t2)
              with UnknownType -> (t1, t2))
         | Some typ1, None ->
             (try
                let typ2 = typeOfLoc (var, o) in
                if ignoreT var typ2 || 
                  let typ2 = canonType (typeModArray typ2) in
                  Ciltools.compare_type typ1 typ2 == 0 then (t1, t2)
                else (t1, Some typ2)
              with UnknownType ->
                t1, t2)
         | None, Some _ -> failwith "impossible"
         | Some _, Some _ -> t1, t2) locs (None, None)
    with
      None, None
    | Some _, None
    | None, Some _ -> ()
    | Some t1, Some t2 ->
        let warnMsg = Printf.sprintf "Mess: diff t %s %s @ %s" 
          (string_of_type t1) (string_of_type t2) (string_of_loc !currentLoc)
        in
        if Hashtbl.mem warnMergeTypes warnMsg then ()
        else (Hashtbl.add warnMergeTypes warnMsg ();
              logError warnMsg) 
    
  method seekMerge locs curSt =
    (* Look for guys in this same points-to-set *)
    self#checkMergeTypes locs;
    let toMerge = FLocSet.fold 
      (fun (var, o) cur ->
         if self#eligibleVar var curSt 
         then VarSet.add var cur 
         else cur
      ) locs VarSet.empty in
    let toMergeList = partitionByType toMerge in
    let toMergeList = self#matchAlphas toMergeList in    

    (* Seek similar recursive-typed vars and merge those too...
       may be funky if we've already munged types though... *)
    let furtherMerge = Stat.time "seekFurther" 
      (FLocSet.fold (self#seekMergeFurther curSt) locs) VarSet.empty in
    let toMergeList = VarSet.fold collectTypeParts furtherMerge toMergeList in
    self#filter toMergeList


  (** Check if ptr points to T and 'a. In that case, make the 'a a T *)
  method matchAlphas parts =
    let alphas, nonAlphas = 
      List.partition (fun part -> List.exists isUnknownMallocVar part) parts in
    match alphas, nonAlphas with
      _, [] | [], _ -> parts
    | [alphVars], [nonAlph] ->
        [nonAlph @ alphVars]
    | _, _ -> 
        parts


  method seekMergeFurther curSt (var, o) curVis =
    let var = findMappingNonOpt mappings var in           
    if VarSet.mem var seekedFurther || VarSet.mem var curVis then curVis
    else if not (self#eligibleVar var curSt) then curVis
    else begin
      let curVis = (* Don't add if 'a *)
        if isUnknownMallocVar var then curVis 
        else VarSet.add var curVis in 
      seekedFurther <- VarSet.add var seekedFurther;
      try
        let v = getBinding var curSt in
        foldTargetsVal (self#seekMergeFurther curSt) v curVis
      with Not_found ->
        logErrorF "WTF - seekMergeFurther no binding %s\n" (string_of_var var);
        curVis
    end

  (************************************************************)

  method aggressiveMerge st roots =
    self#init ();
    let rec aMergeVal v curSt =
      match v with
        FNFP _ | FpRef _ | FInt _ -> v, curSt
      | Refs locs ->
          if FLocSet.is_singleton locs then (v, curSt)
          else 
            let toMergeList = self#seekMerge locs curSt in
            let newSt, did = List.fold_left 
              processMerge (curSt, false) toMergeList in
            (* Apply the mapping to get the new value *)
            if did then
              let newVal = updatePointersValNonOpt mappings v in
              (newVal, newSt)
            else 
              (v, newSt)
                
      | Records recs ->
          let newrecs, newSt = List.fold_left
            (fun (curRecs, curSt) (fp, nonfp, moff) ->
               let newNonfp, newSt = 
                 OffsetMap.fold 
                   (fun o v (om, st) ->
                      let newV, newSt = aMergeVal v st in
                      if v == newV then (om, newSt)
                      else (OffsetMap.add o newV om, newSt)
                   ) nonfp (nonfp, curSt) in 
               ((fp, newNonfp, moff) :: curRecs, newSt)
            ) ([], curSt) recs in
          Records (tryReduceRecordSet newrecs), newSt

      | FIRecs fir ->
          let changed, newFIR, newSt = List.fold_left 
            (fun (ch, newFIR, newSt) (fp, nonfp) ->
               let newNonfp, newSt = aMergeVal nonfp newSt in
               if nonfp == newNonfp then (ch, (fp, nonfp) :: newFIR, newSt)
               else (true, (fp, newNonfp) :: newFIR, newSt)
            ) (false, [], curSt) fir in
          if changed then (FIRecs newFIR, newSt) else (v, newSt)


    and processMerge (curSt, changed) toMerge = 
      let numToMerge = List.length toMerge in
      if numToMerge <= 1 
      then (curSt, changed)
      else begin
        self#notifyMerge toMerge;
        doMergeVar toMerge curSt, true
      end
            
    (* Pre-condition curSt[repVar] = v *)
    and aMergeValStore repVar v curSt =
      let newV, newSt = aMergeVal v curSt in
      if v == newV then newSt
      else 
        let repVar = findMappingNonOpt mappings repVar in
        addBinding newSt repVar newV
          
    and failedLookup var st =
      logErrorF "aggMerge failed lookup: %s\n" (string_of_var var);
      printState st;
      failwith "aggMerge lookup fail"
        
    and doMergeVar toMerge curSt =
      match toMerge with 
        repVar :: rest ->
          List.fold_left 
            (fun curSt nextVar ->
               (* In case they have changed across the iterations... *)
               let repVar = findMappingNonOpt mappings repVar in
               let nextVar = findMappingNonOpt mappings nextVar in
               
               updateMap mappings repVar nextVar;
               let repVal = try
                 updatePointersValNonOpt mappings (getBinding repVar curSt) 
               with Not_found ->
                 failedLookup repVar curSt
               in
               let nextVal = try
                 updatePointersValNonOpt mappings (getBinding nextVar curSt)
               with Not_found ->
                 failedLookup nextVar curSt
               in
               let comboVal = combineVals repVal nextVal in
               (* Add the binding to satisfy pre-condition of aMerge...*)
               let curSt = addBinding curSt repVar comboVal in
               aMergeValStore repVar comboVal curSt
                 
            ) curSt rest
      | [] -> failwith "doMergeVar given empty list" 
    in
    let calcMappings st roots =
      let tryMerge var curSt =
        let var = findMappingNonOpt mappings var in
        if not (isGlobalDebug "aggMerge2" true curSt var) then
          let v = getBinding var curSt in
          aMergeValStore var v curSt
        else curSt
      in
      List.fold_left (fun curSt var -> tryMerge var curSt) st roots
    in
    let applyMappings st =
      (* One more pass to clean up mappings? -- Maybe we should have 
         just used do-while-changing kind of algorithm? *)
      let newSt = updateStateMappings mappings st in
      { bindings = newSt.bindings;
        hAttrs = newSt.hAttrs; } 
    in
    let st = stUseReps st in
    let newSt = Stat.time "calcMap" (calcMappings st) roots in
    let newSt = applyMappings newSt in
    self#cleanup ();
    newSt

  method aggressiveMergeAll st =
    let roots = VarMap.fold (fun var _ cur -> var :: cur) st.bindings [] in
    Stat.time "aggMergeAll" (self#aggressiveMerge st) roots

end

(************************************************************)

(* Keep track of what has been merged for each function *)

module MergeTrack = struct

  type t = (sumKey * fvar VarH.t) list
  type simpleSum = t

  let simplify vars = vars
      
  let desimplify vars = vars

  let initVal = []
  let unknownSummary = []

end

module MergeTrackSum = Cache_sum.Make (MergeTrack)

let trackMerge verboseHdr tracked vars =
  (* See what input vars have been merged w/ fresh variables *)
  match vars with
      [_] | [] -> ()
  | rep :: (next :: _ as others) ->
      if verboseHdr <> "" then
        let doc = dprintf "aggMerge %s: %s <- {%s}\n" verboseHdr 
          (string_of_var rep) (string_of_var_list others) in
        logStatusD doc
      else ();
      if VarH.mem tracked rep then ()
      else VarH.add tracked rep next

let mergeKey funID =
  let fkey = fkey_of_sumKey funID in
  inputFreeSumKey fkey

let getMergedIntos funID merges =
  let sk = mergeKey funID in
  let tables = merges#find sk in
  try
    let _, t = List.find (fun (fid, t) -> funID = fid) tables in t
  with Not_found -> VarH.create 7

let replaceMergedIntos funID t merges =
  let sk = mergeKey funID in
  let oldList = merges#find sk in
  let _, newList = try
    List_utils.listFindReplace
      (fun (fidOld, _) (fidNew, _) -> fidOld = fidNew)
      (fun (fidOld, t1) (fidNew, t2) -> (fidNew, t2))
      (funID, t) oldList
  with Not_found -> ((funID, t), [(funID, t)]) in
  merges#addReplace funID newList
    (* Ugh... if this is new, it's like a new summary and the 
       callers may need to be reanalyzed...
       TODO: make this part of the flow-facts and summary.
       This will also allow us to unify different input variables  *)



(*************)
class trackedMerger cg sccCg settings = object (self) 
  inherit absVarMerger as super

  val mutable tracked = VarH.create 0
  val mutable verboseHdr = ""
  val merges = new MergeTrackSum.data 64 (Backed_summary.makeSumType "merg")
      
  initializer begin
    Backed_summary.registerType merges;
    (* Bleh... to force initialization of summaries for body-less funcs *)
    merges#initSummaries settings cg sccCg;
    logStatus "Doing aggressive merge and taking names";
  end

  method getMergedIntos funID =
    getMergedIntos funID merges
        
  method private init () =
    super#init ();
    tracked <- self#getMergedIntos funID
      
  method private cleanup () =
    replaceMergedIntos funID tracked merges

  method notifyMerge toMerge =
    trackMerge verboseHdr tracked toMerge


  method setVerbose verb =
    verboseHdr <- verb

  method filter parts =
    let rec filterAPart curInNode newPart aPart =
      match aPart with
        [] -> (match curInNode with 
                 None -> List.rev newPart 
               | Some x -> x :: List.rev newPart)
      | h :: t ->
          if isInputNode h then
            (match curInNode with 
               None -> filterAPart (Some h) newPart t
             | Some x -> filterAPart curInNode newPart t)
          else filterAPart curInNode (h :: newPart) t
    in
    List.map (filterAPart None []) parts


  method eligibleVar var curSt =
    match var with
      FHeap _ | FInput _ ->
        (* Not merging globals here because that would modify
           the separate global summary *)
        if isGlobalDebug "aggMerge" true curSt var then false
      else true
    | _ -> false

end


let myMerger = ref (new nopMerger :> unifier)
