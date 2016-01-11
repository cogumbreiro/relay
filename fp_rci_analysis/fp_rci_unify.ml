
(** Aggressively merge states / collapse targets *)

open Cil
open Type_utils
open Fp_rci_types
open Fp_rci_unify_structs
open Fp_rci_lattice_ops
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


class type unifier = object

  method aggressiveMerge : fpState -> fvar list -> fpState

  method aggressiveMergeAll : fpState -> fpState

  (* Ugh... must expose these child functions *)
  method setFunc : sumKey -> unit
  method setVerbose : string -> unit

end

class nopMerger : unifier = object (self) 

  method setFunc _ = ()
  method setVerbose _ = ()

  method aggressiveMerge st roots = st

  method aggressiveMergeAll st = st

end


(************************************************************)


class virtual absVarMerger = object (self)

  method virtual notifyMerge : flocation list -> unit

  method virtual filter : flocation list list -> flocation list list

  method virtual eligibleLoc : flocation -> fpState -> bool
  method virtual eligibleTransitiveLoc : flocation -> fpState -> bool

  val mutable mappings = UnifLocs.makeMappings 0

  method getMappings = mappings

  val mutable seekedFurther = VarSet.empty
  val mutable funID = (-1, "")

  method setFunc fid =
    funID <- fid

  method private init () =
    mappings <- UnifLocs.makeMappings 7;
    seekedFurther <- VarSet.empty
      
  method private cleanup () =
    ()

  method private tryInstrumentMerge toMergeL curSt =
(*
    let pattern1 = " * <.[32]< * <.[288]< * <.[352]< * <node:l:181.[0]" in
    let pattern2 = " * <.[32]< * <.[352]< * <node:l:181.[0]" in
    List.iter
      (fun aPart ->
         let f1, f2 = 
           List.fold_left 
             (fun (found1, found2) (var, o) ->
                let str = string_of_pointer (var, o) in
                if str = pattern1 then (true, found2)
                else if str = pattern2 then (found1, true)
                else (found1, found2)
             ) (false, false) aPart in
         if f1 && f2 then begin
           logStatusF "GOTCHA: %s\n" (string_of_loc_list aPart);
           printState curSt;
         end;
      ) toMergeL
*) ()
      
  method seekMerge locs curSt =
    (* Look for guys in this same points-to-set *)
    let toMerge = FLocSet.fold 
      (fun (var, o) cur ->
         if self#eligibleLoc (var, o) curSt
         then addToPtrSet (var, o) cur 
         else cur
      ) locs FLocSet.empty in
(*
    let toMergeL = partitionByType curSt toMerge in
    let toMergeL = self#matchAlphas curSt toMergeL in    
*)
    let toMergeL = partitionByOffset toMerge in
(*
    let toMergeL = [FLocSet.elements toMerge] in
    let toMergeL = if toMergeL = [[]] then [] else toMergeL in
*)

    (* Seek similar recursive-typed vars and merge those too...
       may be funky if we've already munged types though... *)
    let furtherMerge = 
      FLocSet.fold (self#seekMergeFurther curSt) locs FLocSet.empty in
    let toMergeL = 
      FLocSet.fold (collectTypeParts curSt.vAttrs) furtherMerge toMergeL in

    (* Keep input vars separate from fresh heap vars *)
    let toMergeL = self#partitionFreshIn toMergeL in 
    self#filter toMergeL


  (** Check if ptr points to T and 'a. In that case, make the 'a a T *)
  method matchAlphas curSt parts =
    let alphas, nonAlphas = 
      List.partition (fun part -> 
                        List.exists (isUnknownMallocVar curSt) part) parts in
    match alphas, nonAlphas with
      _, [] | [], _ -> parts
    | [alphVars], [nonAlph] ->
        [nonAlph @ alphVars]
    | _, _ -> 
        parts

  (** Keep fresh heap vars separate from input vars, because there's no
      convenient way to mark inputs as modded if the heap vars were modded
      before this merge happened *)
  method private partitionFreshIn toMergeL =
    List_utils.mapCross 
      (fun aPart ->
         let p1, p2 = List.partition (fun (var, o) -> isInputNode var) aPart in
         match p1, p2 with
           [], [] -> []
         | h :: t, [] -> [p1]
         | [], h :: t -> [p2]
         | _, _ -> [p1; p2]
      ) toMergeL


  method seekMergeFurther curSt (var, o) curVis =
    (* Just check bases of vars here, so ignore delta *)
    let newvar, delta = UnifLocs.findMappingNonOpt mappings var in
    let newloc = newvar, (noOff, false) in
    if VarSet.mem newvar seekedFurther || FLocSet.mem newloc curVis then curVis
    else if not (self#eligibleTransitiveLoc newloc curSt) then curVis
    else begin
      let curVis = (* Don't add if 'a *)
        if isUnknownMallocVar curSt.vAttrs newvar then curVis 
        else addToPtrSet newloc curVis in 
      seekedFurther <- VarSet.add newvar seekedFurther;
      try
        let v = getBinding newvar curSt in
        foldTargetsVal (self#seekMergeFurther curSt) v curVis
      with Not_found ->
        (match var with
           FInput _ | FVar _ | FRet _ -> curVis
         | FHeap _ ->
             logErrorF "seekMergeFurther no binding %s\n" 
               (string_of_var newvar);
             curVis)
    end
      
  (************************************************************)

  method aggressiveMerge st roots =
    self#init ();
    let rec mergeVal v curSt =
      match v with
        FNFP _ | FpRef _ | FInt _ -> v, curSt
      | Refs locs ->
          if FLocSet.is_singleton locs then (v, curSt)
          else 
            let toMergeList = 
              Stat.time "seekMerge" (self#seekMerge locs) curSt in
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
            (fun (curRecs, curSt) (fm, moff) ->
               let newFM, newSt = mergeRecord fm curSt in               
               ((newFM, moff) :: curRecs, newSt)
            ) ([], curSt) recs in
          makeNormalizedRec newrecs, newSt

      | NCRecord (fm, m) ->
          let newFM, newSt = mergeRecord fm curSt in
          NCRecord (newFM, m), newSt

      | FIRecs fir ->
          let newFIR, newSt = mergeVal fir curSt in
          if newFIR == fir then (v, newSt) else (FIRecs newFIR, newSt) 

    and mergeRecord fm curSt =
      OffsetMap.fold 
        (fun o v (om, st) ->
           let newV, newSt = mergeVal v st in
           if v == newV then (om, newSt)
           else (OffsetMap.add o newV om, newSt)
        ) fm (fm, curSt)

    and processMerge (curSt, changed) toMerge = 
      let numToMerge = List.length toMerge in
      if numToMerge <= 1 
      then (curSt, changed)
      else begin
        self#notifyMerge toMerge;
        doMergeVar toMerge curSt, true
      end
            
    (* Pre-condition curSt[repVar] = v *)
    and mergeValStore repVar v curSt =
      let newV, newSt = mergeVal v curSt in
      if v == newV then newSt
      else 
        (* After mergeVal, representative may have changed again *)
        let newRepVar, delta = UnifLocs.findMappingNonOpt mappings repVar in
        let newRepVal = alignRecordOff (invertOff delta) newV in
        addBinding newSt newRepVar newRepVal
                  
    and doMergeVar toMerge curSt =
      let (repVar, repOff), rest = pickShortestRepLoc toMerge in
(*
      logStatusF "One round of merge: %s <- %s\n"
        (string_of_pointer (repVar, repOff)) (string_of_loc_list rest);
*)
      List.fold_left 
        (fun curSt (nextVar, nextOff) ->
           (* In case they have changed across the iterations... *)
           let repVar2, dRep = UnifLocs.findMappingNonOpt mappings repVar in
           let nextVar2, dNext = UnifLocs.findMappingNonOpt mappings nextVar in
           if eqFVar repVar2 nextVar2 then curSt (* Don't even try *)
           else begin
             (* 
                know:
                &repVar2 = &repVar1 + dRep
                &nextVar2 = &nextVar1 + dNext

                want:
                &repVar1 + repOff = &nextVar1 + nextOff

                set up transitive equality with:
                &repVar2 = &repVar1 + dRep
                = ((&nextVar1 + nextOff) - repOff) + dRep
                = ((&nextVar2 - dNext) + nextOff - repOff + dRep)
             *)
             let delta = concatOffset (int_of_off nextOff)
                            (subtractOff 
                               (subtractOff dRep (int_of_off repOff)) dNext) in
(*
             logStatusF "unify %s <-(%d)- %s\n" 
               (string_of_var repVar2) delta (string_of_var nextVar2);
*)
             UnifLocs.updateMap mappings repVar2 (nextVar2, delta);

             let curSt = { curSt with vAttrs = 
                 markSummary repVar2 curSt.vAttrs; } in

             let repBind = getRepBinding repVar2 curSt in
             let nextBind = getRepBinding nextVar2 curSt in
             (match repBind, nextBind with
                None, None -> 
                  (* Haven't generated the bindings for either of the vars *)
                  curSt 
              | Some repVal, Some nextVal ->
                  combineAndRecurse repVar2 delta repVal nextVal curSt
              | None, Some nextVal ->
                  let baseAP = getBaseAPOpt curSt repVar2 in
                  let t = typeOfFVar curSt.vAttrs repVar2 in
                  let repVal, curSt = getInitialVar baseAP curSt repVar2 t in
                  combineAndRecurse repVar2 delta repVal nextVal curSt

              | Some repVal, None -> 
                  let baseAP = getBaseAPOpt curSt nextVar2 in
                  let t = typeOfFVar curSt.vAttrs nextVar2 in
                  let nextVal, curSt = getInitialVar baseAP curSt nextVar2 t in
                  combineAndRecurse repVar2 delta repVal nextVal curSt
             )
           end
        ) curSt rest

    and getRepBinding var curSt =
      try Some (updatePointersValNonOpt mappings (getBinding var curSt))
      with Not_found -> 
        match var with
          FHeap _ ->
            logErrorF "aggMerge failed lookup: %s\n" (string_of_var var);
            printState st;
            failwith "aggMerge lookup fail"
        | FInput _ -> None
        | FVar _ | FRet _ -> Some (defaultValNonInput var)
            
    and combineAndRecurse repVar2 delta repVal nextVal curSt =
      let shiftedNextVal = alignRecordOff (invertOff delta) nextVal in      
      (* Need this to generate defaults if heap var is merged w/ input *)
      let comboVal, curSt =
        combineVarValsSt curSt repVar2 repVal shiftedNextVal in
      (* Do rewrite one more time in case combo had to create initialVals? *)
      let comboVal = updatePointersValNonOpt mappings comboVal in
      (* Add the binding to satisfy pre-condition of aMerge...*) 
      let curSt = addBinding curSt repVar2 comboVal in
      mergeValStore repVar2 comboVal curSt
    in
      
    let calcMappings st roots =
      let tryMerge var curSt =
        let var2, _ = UnifLocs.findMappingNonOpt mappings var in
        if not (isGlobalDebug "aggMerge2" true curSt.vAttrs var2) then
          let v = 
            try getBinding var2 curSt
            with Not_found ->
              
              logErrorF "aggMerge2 missing %s vs %s\n" 
                (string_of_var var) (string_of_var var2);
              printState curSt;
              failwith ("aggMerge2 binding NF: " ^ string_of_var var2) in
          mergeValStore var2 v curSt
        else curSt
      in
      List.fold_left (fun curSt var -> tryMerge var curSt) st roots
    in
    let applyMappings st =
      (* One more pass to clean up mappings? -- Maybe we should have 
         just used do-while-changing kind of algorithm? *)
      updateStateMappings mappings st
    in
    let newSt = Stat.time "calcMap" (calcMappings st) roots in
    let newSt = applyMappings newSt in
    self#cleanup ();
    newSt

  method aggressiveMergeAll st =
    let roots = VarMap.fold (fun var _ cur -> var :: cur) st.bindings [] in
    Stat.time "aggMergeAll" (self#aggressiveMerge st) roots

end

(************************************************************)


(* Not actually tracking what input vars have been merged w/ fresh variables *)
let trackMerge verboseHdr vars =
  match vars with
      [_] | [] -> ()
  | (rep, repoff) :: (next :: _ as others) ->
      if verboseHdr <> "" then
        let doc = dprintf "aggMerge %s: %s <- {%s}\n" verboseHdr 
          (string_of_pointer (rep, repoff)) (string_of_loc_list others) in
        logStatusD doc
      else ()

(*************)
class trackedMerger cg sccCg settings = object (self) 
  inherit absVarMerger as super

  val mutable verboseHdr = ""
        
  method private init () =
    super#init ()
      
  method private cleanup () =
    ()

  method notifyMerge toMerge =
    trackMerge verboseHdr toMerge

  method setVerbose verb =
    verboseHdr <- verb

  method filter parts =
    parts

  method eligibleLoc (var, o) curSt =
    match var with
      FHeap _ ->
        (* Not merging globals here because that would modify
           the separate global summary *)
        not (isGlobalDebug "aggMerge" true curSt.vAttrs var)
    | FInput _ -> 
        not (isGlobalDebug "aggMerge" true curSt.vAttrs var)
    | FVar _ | FRet _ -> false

  method eligibleTransitiveLoc (var, o) curSt =
    match var with
      FHeap _ ->
        (* Not merging globals here because that would modify
           the separate global summary *)
        not (isGlobalDebug "aggMerge" true curSt.vAttrs var)
    | FInput _ (* Disallow merging input vars that are reached transitively *)
    | FVar _ | FRet _ -> false

end

(************************************************************)


let myMerger = ref (new nopMerger :> unifier)

let unsetMerger () =
  let oldAggMerge = !myMerger in
  myMerger := new nopMerger;
  oldAggMerge
  
let resetMerger oldAggMerge =
  myMerger := oldAggMerge

let doWithoutUnify foo =
  let oldMerge = unsetMerger () in
  let x = foo () in
  resetMerger oldMerge;
  x
