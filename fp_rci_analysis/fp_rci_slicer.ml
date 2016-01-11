
(** Slice by marking values as NON-FP *)

open Cil
open Cildump
open Type_utils
open Type_reach
open Fp_rci_types
open Fp_rci_lattice_ops
open Logging

(************************************************************)

let nfpOutRef atts nfpsrc var v =
  if !filterNFP && neverReachesFPVar atts var v then FNFP nfpsrc else v

let nfpOutRefTyp atts nfpsrc typ v =
  if !filterNFP && neverReachesFPType atts typ v then FNFP nfpsrc else v

let shouldNfpField atts valOpt ftype =
  !filterNFP && neverReachesFPField atts valOpt ftype


(************************************************************)


(** Turn all non-fp values into NFP (more like NONFP) *)
let rec nfpOutRecord atts nfpsrc (offmap, mO) ci baseType curOff =
  let self = nfpOutRecord atts nfpsrc in
  (* Go through all the known fields and change them! *)
  let nfp = FNFP nfpsrc in
  List.fold_left 
    (fun (curO, curM) fi ->
       let ftype = typeModArray fi.ftype in
       match ftype with
         TComp (ci2, _) ->
           if ci2.cstruct then
             self (curO, curM) ci2 baseType (Field (fi, curOff))
           else 
             (* Don't risk nfp'ing out a union for now *)
             curO, curM
       | _ ->
           let off, _ = cilOff2Offset false baseType (Field (fi, curOff)) in
           let valOpt =
             try Some (OffsetMap.find off curO) 
             with Not_found -> None in
           (match valOpt with
              Some (FNFP _) -> curO, curM
            | _ ->
                if shouldNfpField atts valOpt ftype then
                  OffsetMap.add off nfp curO, maxOff curM off
                else curO, curM
           )
    ) (offmap, mO) (!Cil.getCfields ci)

    
let nfpOutType atts nfpsrc typ v =
  match v with
    FpRef _ -> v
  | FNFP _ -> (* debugNfpOutVar var v *) v

  | FInt i ->
      if Int64.compare Int64.zero i == 0 then v 
      else FNFP nfpsrc
      (* Only treat 0 as null pointer, and real pointers as abstract.
         Therefore, any non-zero value should not be a pointer? *)

  | Refs _ -> nfpOutRefTyp atts nfpsrc typ v

  | Records recs -> 
      (match typeModArray typ with
         TComp (ci, _) ->
           if isImpreciseMalloc typ then v
           else 
             let newrecs = List.map 
               (fun (fm, m) ->
                  let newfm, newmax =
                    nfpOutRecord atts nfpsrc (fm, m) ci typ NoOffset in
                  (newfm, newmax)
               ) recs in
             Records (newrecs)
       | _ -> v)
  | NCRecord (fm, m) ->
      (match typeModArray typ with
         TComp (ci, _) ->
           if isImpreciseMalloc typ then v
           else 
             let newfm, newmax =
               nfpOutRecord atts nfpsrc (fm, m) ci typ NoOffset in
             NCRecord (newfm, newmax)
       | _ -> v)
        
  | FIRecs _ -> v (* Can't do anything about it... *)
          
          
let nfpOutVar atts nfpsrc var v = 
  match v with
    FpRef _ -> v
  | FNFP _ -> (* debugNfpOutVar var v *) v
  | FInt i ->
      if Int64.compare Int64.zero i == 0 then v 
      else FNFP nfpsrc
      (* Only treat 0 as null pointer, and real pointers as abstract.
         Therefore, any non-zero value should not be a pointer? *)

  | Refs _ -> nfpOutRef atts nfpsrc var v

  | Records _ | NCRecord _ ->
      let baseType = typeOfFVar atts var in
      nfpOutType atts nfpsrc baseType v

  | FIRecs _ -> v

let nfpOutState nfpsrc st =
  { st with 
      bindings = VarMap.mapChI (nfpOutVar st.vAttrs nfpsrc) st.bindings; }



(** Garbage collect heap variables after weakening drops pointers *)
let gcState formals st =
  let vis = VarH.create 7 in 
  let rec visitVal v =
    match v with
      FNFP _ | FInt _ -> ()
    | FpRef var -> VarH.replace vis var ()
    | Refs ls -> FLocSet.iter visitPtrTarget ls
    | Records recs ->
        List.iter visitRecord recs
    | NCRecord (fm, m) -> visitRecord (fm, m)
    | FIRecs fir -> visitVal fir

  and visitPtrTarget (var, o) =
    visitVar var
      
  and visitRecord (fm, m) =
    OffsetMap.iter (fun o v -> visitVal v) fm
      
  and visitVar var =
    if VarH.mem vis var then ()
    else 
      (VarH.add vis var ();
       try 
         let v = getBinding var st in
         visitVal v
       with Not_found -> ()) (* global var? *)
  in

  let pruneVar var _ cur =
    if VarH.mem vis var then cur else VarMap.remove var cur
  in

  let pruneAtt var att cur =
    match var with
      FHeap _ -> pruneVar var () cur
    | FInput ([x], _)-> pruneVar var () cur
    | FInput _ -> 
        if extendsFormal formals var then 
          if att.vKind = HGlobal then cur 
          else pruneVar var () cur
        else pruneVar var () cur
    | FRet _ | FVar _ -> pruneVar var () cur (* shouldn't even have atts... *)
  in
  
  let pruneUnreached st =
    { bindings = VarMap.fold pruneVar st.bindings st.bindings; 
      vAttrs = VarMap.fold pruneAtt st.vAttrs st.vAttrs; }
  in

  (* Hmm... maybe we DO want to make the formals thing optional if we
     ever use this in the middle of a function, and not just to 
     generate the summary... *)
  let inRoots var =
    (* Check reachability from formals (minus formals themselves), etc *)
    match var with
      FRet _ -> true
    | FVar vid ->
        let vi = varinfoOfVid vid in
        if vi.vglob then (* Should have dropGlobalAssume already though? *)
          logError "Didn't drop globals before slicing?";
        false
    | FHeap _ -> 
        (* If "local" never treat as a root. 
           If "global", assume this happens after dropGlobalAssume, 
           so it really shouldn't be here *)
        false
    | FInput ([x], _) -> 
        logErrorF "Slicer had base input var %s\n" (string_of_var var);
        false (* only include the 1-level deref stuff *)
    | FInput (ap, _) ->
        (* If "local" check if it extends a formal. 
           If "global", assume this happens after dropGlobalAssume, 
           so... it really shouldn't be here *)
        (* only include the 1-level deref stuff *)
        hasOnlyOneDeref ap && extendsFormal formals var 
  in
  VarMap.iter
    (fun var v -> 
       if inRoots var then visitVar var
    ) st.bindings;
  pruneUnreached st

    
let weakenState formals nfpsrc st =
  let st = nfpOutState nfpsrc st in
  gcState formals st
