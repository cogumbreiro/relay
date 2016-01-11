
(** Slice by marking values as NON-FP *)

open Cil
open Cildump
open Fp_types
open Fp_lattice_ops
open Logging

(************************************************************)

let nfpOutRef nfpsrc var v =
  let shouldNfpout = neverReachesFPVar var v in
  if shouldNfpout then FNFP nfpsrc else v


let shouldNfpField valOpt fi ftype =
  neverReachesFPField valOpt ftype


(************************************************************)


(** Turn all non-fp values into NFP (more like NONFP) *)
let rec nfpOutRecord nfpsrc (offmap, mO) ci baseType curOff =
  (* Go through all the known fields and change them! *)
  let nfp = FNFP nfpsrc in
  List.fold_left 
    (fun (curO, curM) fi ->
       let ftype = typeModArray fi.ftype in
       match ftype with
         TComp (ci2, _) ->
           if ci2.cstruct then
             nfpOutRecord nfpsrc (curO, curM) ci2 baseType (Field (fi, curOff))
           else 
             (* Don't risk nfp'ing out a union for now *)
             curO, curM
       | _ ->
           let off = cilOff2Offset baseType (Field (fi, curOff)) in
           let valOpt =
             try Some (OffsetMap.find off curO) 
             with Not_found -> None in
           (match valOpt with
              Some (FNFP _) -> curO, curM
            | _ ->
                if shouldNfpField valOpt fi ftype then
                  OffsetMap.add off nfp curO, maxOff curM off
                else curO, curM
           )
    ) (offmap, mO) (!Cil.getCfields ci)

let debugNfpOutVar var v =
  let t = typeOfFVar var in
  (match t with
     TInt _ -> v
   | _ -> 
       if (Type_reach.isPoly t) || (notFwdReach t) then ()
       else logError ("already NFP when it shouldn't be: " 
                      ^ string_of_val v ^ " " ^ string_of_var var ^ " @ " 
                      ^ string_of_loc !currentLoc);
       v)
    
let nfpOutType nfpsrc typ v =
  match v with
    FpRef _ -> v
  | FNFP _ ->
      (* debugNfpOutVar var v *)
      v

  | FInt i ->
      if Int64.compare Int64.zero i == 0 then v 
      else FNFP nfpsrc
      (* Only treat 0 as null pointer, and real pointers as abstract.
         Therefore, any non-zero value should not be a pointer? *)

  | Refs _ -> 
      if neverReachesFPType typ v then FNFP nfpsrc else v

  | Records recs -> 
      if Type_utils.isImpreciseMalloc typ then v
      else 
        (match typeModArray typ with
           TComp (ci, _) ->
             let newrecs = List.map 
               (fun (fp, nonfp, m) ->
                  let newnonfp, newmax =
                    nfpOutRecord nfpsrc (nonfp, m) ci typ NoOffset in
                  (fp, newnonfp, newmax)
               ) recs in
             Records (newrecs)
         | _ -> v)
  | FIRecs _ -> v (* Can't do anything about it... *)
          
          
let nfpOutVar nfpsrc var v = 
  match v with
    FpRef _ -> v
  | FNFP _ ->
      (* debugNfpOutVar var v *)
      v

  | FInt i ->
      if Int64.compare Int64.zero i == 0 then v 
      else FNFP nfpsrc
      (* Only treat 0 as null pointer, and real pointers as abstract.
         Therefore, any non-zero value should not be a pointer? *)

  | Refs _ -> nfpOutRef nfpsrc var v

  | Records recs ->
      let baseType = typeOfFVar var in
      nfpOutType nfpsrc baseType v

  | FIRecs _ -> v

let nfpOutStore nfpsrc store =
  (* Just do variables for now (since we have the type). For fields
     we would need to convert offsets to fields first *)
  VarMap.mapChI (nfpOutVar nfpsrc) store

(** Garbage collect heap variables after weakening drops pointers *)
let gcState localsOpt st =
  let vis = VarH.create 7 in 
  let rec visitVal v =
    match v with
      FNFP _ | FInt _ | FpRef _ -> ()
    | Refs ls -> FLocSet.iter visitPtrTarget ls
    | Records recs ->
        List.iter visitRecord recs
    | FIRecs fir ->
        List.iter visitFIR fir

  and visitPtrTarget (var, o) =
    visitVar var
      
  and visitRecord (fp, nonfp, m) =
    OffsetMap.iter (fun o v -> visitVal v) nonfp
      
  and visitFIR (fp, nonfp) =
    visitVal nonfp

  and visitVar var =
    if VarH.mem vis var then ()
    else 
      (VarH.add vis var ();
       try 
         let v = getBinding var st in
         visitVal v
       with Not_found -> ()) (* global var? *)
  in
  let shouldPruneVar var _ cur =
    if VarH.mem vis var then cur else VarMap.remove var cur
  in
  let shouldPruneAtt var att cur =
    match att with 
      HGlobal -> cur
    | HSingle | HSum -> shouldPruneVar var () cur
  in
  let pruneUnreached st =
    let newStore = VarMap.fold shouldPruneVar st.bindings st.bindings in
    let newAtts = VarMap.fold shouldPruneAtt st.hAttrs st.hAttrs in
    { bindings = newStore; hAttrs = newAtts; }
  in
  let inRoots var =
    (* Treat globals / program variables as roots unless specified *)
    if isGlobalDebug "gcState" true st var then true
    else match localsOpt with 
      None -> 
        (match var with
           FHeap _ | FInput _ | FRet _ -> false
         | FVar _ -> true)
    | Some locals ->
        List.exists (fun var2 -> compFVar var var2 == 0) locals
  in
  VarMap.iter 
    (fun var v -> 
       if inRoots var then visitVar var
    ) st.bindings;
  pruneUnreached st
    
let weakenState roots nfpsrc st =
  let st = { bindings = nfpOutStore nfpsrc st.bindings;
             hAttrs = st.hAttrs; } in
  gcState roots st
