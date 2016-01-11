

(** Data structures for rewriting equal locations *)

open Logging
open Fp_rci_types

(************************************************************
  Rewrite variables only... var1 -> var2
************************************************************)

(* Unification w/ preferences for what representative vars should be *)

(* Could abstract this even more... *)

module type UnifVarOrder = sig
  
  val orderVars : fvar -> fvar -> (fvar * fvar)
    
end

(** Stateful to hide side-effects of path compression from lookup, etc. *)
type unifMap = fvar VarH.t

let printUMap map =
  VarH.iter (fun srcVar repVar ->
               logStatusF "%s <- %s\n" 
                 (string_of_var repVar) 
                 (string_of_var srcVar);
            ) map

module OrderedUnif (O:UnifVarOrder) = struct
  include O

  let makeMappings size : unifMap =
    VarH.create size

  let doUpdateMap (mappings : unifMap ) oldVar newVar =
    VarH.replace mappings oldVar newVar

  (** Find the final new mapping for a heap ID if there is a new mapping *)
  let findMapping (mappings : unifMap) oldVar =
    let rec loopFind oldVar =
      try
        let newVar = VarH.find mappings oldVar in
        if eqFVar oldVar newVar
        then Some newVar
        else match loopFind newVar with
          Some x -> 
            doUpdateMap mappings oldVar x; 
            Some x
        | None -> Some newVar
      with Not_found -> None
    in
    loopFind oldVar


  let findMappingNonOpt (mappings : unifMap) var =
    match findMapping mappings var with 
      None -> var 
    | Some newVar -> newVar


  (** Indicate that newVar is now used for oldVar *)
  let updateMap (mappings : unifMap) newVar oldVar =
    match findMapping mappings newVar, findMapping mappings oldVar with
      None, Some (oRep) -> 
        let newRep, toMap = orderVars newVar oRep in
        (* oldVar already linked -- just link one of newVar and oRep *)
        doUpdateMap mappings toMap newRep

    | Some nRep, None ->
        (* newVar already linked w/ nRep, just link oldVar *)
        let newRep, toMap = orderVars nRep oldVar in
        doUpdateMap mappings toMap newRep

    | Some nRep, Some oRep ->
        (* They are all linked to something, just link the parents *)
        let newRep, toMap = orderVars nRep oRep in
        doUpdateMap mappings toMap newRep

    | None, None ->
        let newRep, toMap = orderVars newVar oldVar in
        doUpdateMap mappings toMap newRep
        
  let varChanged (mappings : unifMap) var =
    match findMapping mappings var with
      Some newVar -> 
        if eqFVar newVar var then None
        else Some newVar
    | None -> None


  (** Get closure of equalities under acc path constructors...
      e.g., if "deref path1 <- deref path2" and we have 
      "deref f1 deref path2" that should map to "deref f1 deref path1" *)
  let closeMerges unifMap allVars =

    let knownOrWorse origVar newVar =
      match findMapping unifMap origVar with
        None -> false
      | Some x -> 
          eqFVar x newVar || 
            (match x, newVar with
               FInput (knownAP, knownL), FInput (newAP, newL) ->
                 knownL <= newL
             | _ -> false)
    in
    
    let rec seekShorterMerge curHead curTail curLen = 
      match curTail with
        AccDeref as h :: t -> 
          (match findMapping unifMap (makeInVar (curTail, curLen)) with
             None -> seekShorterMerge (h :: curHead) t (curLen - 1)
           | Some other -> 
               (match other with
                  FInput (otherTail, otherLen) ->
                    if curLen > otherLen then begin
                      let origAP = List.rev_append curHead curTail in 
                      let origVar = makeInVar (origAP, List.length origAP)  in
                      let newAP = List.rev_append curHead otherTail in
                      let newVar = makeInVar (newAP, List.length newAP) in
                      if knownOrWorse origVar newVar then ()
                      else begin
                        updateMap unifMap newVar origVar;
                      end
                    end;
                    (* Even if something is already known? *)
                    seekShorterMerge (h :: curHead) t (curLen - 1)
                | _ -> ())
          )
      | h :: t -> seekShorterMerge (h :: curHead) t (curLen - 1)
      | [] -> ()
    in
    VarSet.iter 
      (fun var ->
         match var with
           FInput (origAP, origLen) -> seekShorterMerge [] origAP origLen
         | _ -> ()
      ) allVars

  let foldSrcVars foo mappings cur =
    VarH.fold (fun srcVar _ cur -> foo srcVar cur) mappings cur

end

(** Take the order of the given parameters to determine the "preferred"
    representative variable *)
module InorderU = OrderedUnif 
  (struct
     let orderVars a b = (a, b)
   end)

(** Take the variable with the shortest access path (if it is an input var),
    otherwise just take the given order *)
module ShortAPU = OrderedUnif 
  (struct
     let orderVars var1 var2 = 
       match var1, var2 with
         FInput (ap1, l1), FInput (ap2, l2) ->
           if l1 < l2 then var1, var2
           else if l2 < l1 then var2, var1
           else if compFVar var1 var2 < 0 then var1, var2
           else var2, var1
       | FInput _, _ -> 
           var1, var2 
       | _, FInput _ ->
           var2, var1
       | _, _ ->
           var1, var2
   end)


(************************************************************
 Rewrite arbitrary locations (var1, delta_offset1) -> var2
************************************************************)

type unifLocMap = (fvar * offset_val) VarH.t


module UnifLocs  = struct
  
  let makeMappings size : unifLocMap =
    VarH.create size
      
  let computeNewOff (oldOff, sum) delta =
    (subtractOff oldOff delta, sum)

  let doUpdateMap (mappings : unifLocMap ) oldVar (newVar, delta) =
    if not (isNoOffset delta) && eqFVar oldVar newVar then
      logErrorF "trying to add self-loop w/ non-0-delta %s, %s\n"
        (string_of_var oldVar) (string_of_offsetval delta)
    else VarH.replace mappings oldVar (newVar, delta)
      
  let findMapping (mappings : unifLocMap) oldVar =
    let rec loopFind oldVar =
      try
        let newVar, delta = VarH.find mappings oldVar in
        if eqFVar oldVar newVar then begin
          if not (isNoOffset delta) then 
            logErrorF "self-loop w/ non-0-delta? %s, %s\n" 
              (string_of_var oldVar) (string_of_offsetval delta);
          Some (newVar, noOff)
        end else match loopFind newVar with
          Some (newVar2, delta2) -> 
            (* newVar already linked 
               new1 == old + f1
               but we already know 
               new2 == new1 + f2
               new2 == (old + f1) + f2...
            *)
            let fullDelta = concatOffset delta delta2 in
            doUpdateMap mappings oldVar (newVar2, fullDelta);
            Some (newVar2, fullDelta)
        | None -> Some (newVar, delta)
      with Not_found -> None
    in
    loopFind oldVar

  let findMappingNonOpt (mappings : unifLocMap) var =
    match findMapping mappings var with 
      None -> var, noOff
    | Some (v, delta) -> (v, delta)

  (** Equate locations (newVar, 0) with (oldVar, delta) 
      e.g., if new == &old.f 

      then ptr &old + offsetof(f) --> ptr new
           ptr old --> ptr new - offsetof(f)   *)
  let updateMap (mappings : unifLocMap) newVar (oldVar, delta) =
    match findMapping mappings newVar, findMapping mappings oldVar with
      None, Some (oRep, oDelta) -> 
        (* oldVar already linked 
           new == old1 + f1
           but we already know 
           old2 == old1 + f2
           new == (old2 - f2) + f1 ... *)
        doUpdateMap mappings oRep (newVar, subtractOff delta oDelta)

    | Some (nRep, nDelta), None ->
        (* new1 == old + f1
           but we already know 
           new2 == new1 + f2
           new2 == (old + f1) + f2 ... *)
        doUpdateMap mappings oldVar (nRep, concatOffset delta nDelta)

    | Some (nRep, nDelta), Some (oRep, oDelta) ->
        (* They are all linked to something, just link the parents 
           new1 = old1 + f1
           but we already know
           old2 = old1 + f2
           new2 = new1 + f3
           new2 = (old1 + f1) + f3 = (old2 - f2 + f1 + f3)
        *)
        let newDelta = concatOffset (subtractOff delta oDelta) nDelta in
        doUpdateMap mappings oRep (nRep, newDelta)

    | None, None ->
        doUpdateMap mappings oldVar (newVar, delta)
        
  let varChanged (mappings : unifLocMap) var =
    match findMapping mappings var with
      Some (newVar, delta) -> 
        if eqFVar newVar var then None
        else Some (newVar, delta)
    | None -> None


  (** Get closure of equalities under acc path constructors...
      e.g., if "deref path1 <- deref path2" and we have 
      "deref f1 deref path2" that should map to "deref f1 deref path1" *)
  let closeMerges unifLocMap allVars =
    let knownOrWorse origVar newVar =
      match findMapping unifLocMap origVar with
        None -> false
      | Some (x, d) -> 
          eqFVar x newVar || 
            (match x, newVar with
               FInput (knownAP, knownL), FInput (newAP, newL) ->
                 knownL <= newL
             | _ -> false)
    in
    
    let rec seekShorterMerge curHead curTail curLen = 
      match curTail with
        AccDeref as h :: t -> 
          (match findMapping unifLocMap (makeInVar (curTail, curLen)) with
             None -> seekShorterMerge (h :: curHead) t (curLen - 1)
           | Some (other, delta) -> 
               (match other with
                  FInput (otherTail, otherLen) ->
                    if curLen > otherLen then begin
                      let origAP = List.rev_append curHead curTail in 
                      let origVar = makeInVar (origAP, List.length origAP) in
                      let newAP = List.rev_append curHead otherTail in
                      let newVar = makeInVar (newAP, List.length newAP) in

                      (* Not interpreting the fields wrt to the deltas? *)
                      if curHead <> [] && not (isNoOffset delta) then begin
                        logErrorF "TODO: interpret delta wrt to field: %s %s\n"
                          (string_of_offsetval delta)
                          (string_of_var (FInput (curHead, List.length curHead)))
                      end;

                      if knownOrWorse origVar newVar then ()
                      else begin
                        updateMap unifLocMap newVar (origVar, delta);
                      end
                    end;
                    (* Even if something is already known? *)
                    seekShorterMerge (h :: curHead) t (curLen - 1)
                | _ -> ())
          )

      (* Not interpreting the fields wrt to the deltas? *)
      | h :: t -> seekShorterMerge (h :: curHead) t (curLen - 1)
      | [] -> ()
    in
    VarSet.iter 
      (fun var ->
         match var with
           FInput (origAP, origLen) -> seekShorterMerge [] origAP origLen
         | _ -> ()
      ) allVars

  let foldSrcVars foo mappings cur =
    VarH.fold (fun srcVar _ cur -> foo srcVar cur) mappings cur

  (* Could try to cache this *)
  let oneStepEquivs mappings var = 
    VarH.fold
      (fun srcVar (targ, delta) cur ->
         if eqFVar targ var then
           (* Hmm... lost whether or not it's summary? *)
           addToPtrSet (srcVar, (delta, false)) cur
         else cur
      ) mappings FLocSet.empty


  (** Get all locations in the same equivalence class *)
  let getEqMembers mappings var =
    let rec loop (curVar, off) curLocs =
      if FLocSet.mem (curVar, off) curLocs then curLocs
      else
        let curLocs = addToPtrSet (curVar, off) curLocs in
        let moreLocs = oneStepEquivs mappings curVar in
        (* Example...
           Find that &x + 0 == &y + off
           Next, find that &y + 0 = &z + off2
           Therefore, &x + 0 = &z + off2 + off *)
        FLocSet.fold 
          (fun (nextVar, nextOff) curLocs -> 
             loop (nextVar, concatOffsetSum nextOff off) curLocs
          ) moreLocs curLocs
    in
    loop (var, (noOff, false)) FLocSet.empty


  let joinMaps map1 map2 =
    let newMap = makeMappings (VarH.length map1 + VarH.length map2) in
    let copyMapping src (targ, delta) =
      updateMap newMap targ (src, delta)
    in
    VarH.iter copyMapping map1;
    VarH.iter copyMapping map2;
    newMap

  let eqMaps map1 map2 = 
    let searchAMapForNEQ map1 map2 =
      VarH.fold 
        (fun src (_, _) cur ->
           if not cur then cur
           else 
             let targ1, delta1 = findMappingNonOpt map1 src in
             let targ2, delta2 = findMappingNonOpt map2 src in
             if delta1 <> delta2 || not (eqFVar targ1 targ2) then false
             else cur) map1 true 
    in
    let eq1 = searchAMapForNEQ map1 map2 in
    if eq1 then searchAMapForNEQ map2 map1 else eq1


  let printMap map =
    VarH.iter (fun srcVar (repVar, delta) ->
                 logStatusF "%s --(%s)--> %s\n" 
                   (string_of_var srcVar) (string_of_offsetval delta) 
                   (string_of_var repVar);
              ) map

  let copy map =
    VarH.copy map

  let copyNoOffMap map =
    let newMap = makeMappings (VarH.length map) in
    VarH.iter 
      (fun orig rep -> VarH.add newMap orig (rep, noOff) ) map;
    newMap
    
end

(************************************************************)

let compareRepLocs ((v1, o1) as l1) ((v2, o2) as l2) =
  if l1 == l2 then 0
  else 
    let c = compFVar v1 v2 in
    if c == 0 then closestToZeroOff (int_of_off o1) (int_of_off o2)
    else c
  
let pickShortestRepLoc toMerge =
  List_utils.removeMinShuffle compareRepLocs toMerge

let compareRepVars v1 v2 =
  (* Get shortest acc path / prefer input vars over heap-vars, 
     then break ties w/ offset closest to zero *)
  match v1, v2 with
    FInput _, FHeap _ -> -1
  | FHeap _, FInput _ -> 1
  | _, _ -> compFVar v1 v2
        
let pickShortestRepVar toMerge =
  List_utils.removeMinShuffle compareRepVars toMerge

