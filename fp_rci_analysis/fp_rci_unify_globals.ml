
open Cil
open Cildump
open Fp_rci_types
open Fp_rci_unify_structs
open Logging

let globalUnifTable = UnifLocs.makeMappings 17 

let unifyGlobal gv1 gv2 diff : unit =
  logStatusF "merged globals: %s <-(%s)- %s @ %s\n" 
    (string_of_var gv1) (string_of_offsetval diff)
    (string_of_var gv2) (string_of_loc !currentLoc);
  UnifLocs.updateMap globalUnifTable gv1 (gv2, diff)
    (* Hmm... what if it ended up using a different representative? *)


(** Should only call this if gv is currently considered a global !!! *)
let globRep gv : fvar * offset_sum = 
  match UnifLocs.varChanged globalUnifTable gv with
    None -> (gv, (noOff, false))
  | Some (newV, delta) ->
      (newV, UnifLocs.computeNewOff (noOff, false) delta)
        (* Hmm... always returns a non-summary offset, but I guess
           the base var is a global, so it is okay? *)

(** Project out the mappings for currently known globals (according to st)
    and are NOT input-vars for the current function *)
let globMappings curFormals st = 
  let mappings = UnifLocs.makeMappings 17 in
  VarMap.iter 
    (fun var att ->
       if att.vKind = HGlobal && not (extendsFormal curFormals var) then
         match UnifLocs.findMapping globalUnifTable var with
           Some (rep, delta) -> UnifLocs.updateMap mappings rep (var, delta)
         | None -> ()
    ) st.vAttrs;
  mappings


(** Make sure the closure of equalities is known for vars mentioned in value *)
let closeGlobalMerges v =
  let vars = foldTargetsVal
    (fun (var, o) cur -> VarSet.add var cur) v VarSet.empty in
  UnifLocs.closeMerges globalUnifTable vars
