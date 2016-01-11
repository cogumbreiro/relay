(** Hashconsing things to save space *)

open Fp_types
    

module WHST = Weak.Make (HashableState)

module WHV = Weak.Make
  (struct
     type t = fvar
     let equal a b = eqFVar a b
     let hash a = hashVar a
   end)

module WHVal = Weak.Make
  (struct
     type t = fvalue
     let equal a b = eqVals a b
     let hash a = hashVal a
   end)

module WHOM = Weak.Make
  (struct
     type t = fieldMap
     let equal a b = eqFM a b
     let hash a = hashFM a
   end)

(************************************************************)

let goldenState = WHST.create 29

let goldenOffM = WHOM.create 29

let goldenVars = WHV.create 131

let goldenVals = WHVal.create 131

let hc_var v =
  try WHV.find goldenVars v
  with Not_found ->
    WHV.merge goldenVars v

let rec distillVal v =
  match v with
    FInt _ | FNFP _ | FpRef _ -> v
  | Refs ls -> 
      let newLs, changed = 
        FLocSet.fold 
          (fun (var, o) (cur, ch) -> 
             let newv = hc_var var in
             let ch = if newv == var then ch else true in
             FLocSet.add (newv, o) cur, ch
          ) ls (FLocSet.empty, false) in
      if not changed then v
      else Refs newLs
  | Records recs ->
      (* Damn, pretty much forced to have a new list each time... *)
      let newrecs = List.map
        (fun (fp, nonfp, m) -> (fp, hc_offmap nonfp, m)) recs in
      Records newrecs
  | FIRecs fir ->
      let changed, newFIR = mapNonFPFIR hc_val (==) fir in
      if changed then FIRecs newFIR else v


and distillOM om =
  OffsetMap.mapCh hc_val om
    
and hc_offmap om =
  try WHOM.find goldenOffM om
  with Not_found ->
    let newOM = distillOM om in
    WHOM.merge goldenOffM newOM

and hc_val v =
  try WHVal.find goldenVals v
  with Not_found ->
    let newv = distillVal v in
    WHVal.merge goldenVals newv


let distillStore st =
  VarMap.fold 
    (fun var v cur -> 
       let newVar = hc_var var in
       let newVal = hc_val v in
       if var == newVar && v == newVal then cur
       else VarMap.add newVar newVal cur
    ) st st

let distillAtts atts =
  VarMap.fold
    (fun var att cur ->
       let newv = hc_var var in
       if newv == var then cur
       else VarMap.add newv att cur
    ) atts atts

let distillState st =
  { bindings = distillStore st.bindings;
    hAttrs = distillAtts st.hAttrs; }

let hc_state st =
  try WHST.find goldenState st 
  with Not_found ->
    let newSt = distillState st in
    WHST.merge goldenState newSt
