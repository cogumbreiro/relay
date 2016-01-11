
(** Get the list functions whose summaries are needed by the end *)

open Callg
open Threads

let getNeededFuncs (cg) = begin
  (* Find which functions actually fork new threads *)
  let threadCreatorCallers = findTCCallers cg in
  
  (* Find the fork targets (i.e., thread roots) *)
  let threadRoots = getThreadRoots cg threadCreatorCallers in

  (* Also need the functions called by the threadCreator itself 
     (so that Symstate pass will work) *)
  let results = List.fold_left
    (fun cur tc ->
       try
         let fnode = FMap.find tc.tccID cg in
         List.fold_left 
           (fun cur fkey -> FSet.add fkey cur) cur (calleeKeys fnode)
       with Not_found ->
         Logging.logErrorF "getNeededFuncs: no thread creator in CG %s:%s!"
           tc.tccName (fid_to_string tc.tccID);
         cur
    ) threadRoots threadCreatorCallers in
  
  (* Finally, need entry-point functions *)
(*
  let results = List.fold_left
    (fun cur (fk, _) -> FSet.add fk cur) results (getRoots cg) in

*)
  FSet.elements results
    
end
