
open Cil
open Logging
open Type_utils
open Fp_types
open Fp_lattice_ops

(*********************************************************
 * Update state on Malloc -- 
 * - detect summary nodes and merge
 * - set offsets of new mem to NULL
 *********************************************************)

let topLevelID = (ppUnknown, -1)

let makeMalloc t (pp, fkey) =
  FHeap ({hTyp = addTyp (canonType t); hID = [(pp, fkey)];}), HSingle
(*
  FHeap ({hTyp = toTypID (canonType t); hID = [topLevelID];}), HGlobal
*)

(* Maybe should have used a queue instead... *)
let rec wrapAddHID callsite hid curFront =
  match hid with
    [] -> callsite :: List.rev curFront
  | [h; _] -> wrapAddHID callsite [] (h :: curFront) (* Drop the last dude *)
  | h :: t -> wrapAddHID callsite t (h :: curFront)


(** Mark that the given function was handed/returned the heap variable *)
let extendHeapID callsite hid =
  let cpp, cfk = callsite in
  let len, numSameFunc, identical = 
    List.fold_left 
      (fun (len, n, found) (pp, fk) ->
         if found then (len + 1, n, found) (* don't really need the len *)
         else if cfk = fk then 
           if cpp = pp then (len + 1, n + 1, true)
           else (len + 1, n + 1, found)
         else (len + 1, n, found)
      ) (0, 0, false) hid in
  if identical then hid
  else if numSameFunc < !heapRecMax && len < !heapMax then callsite :: hid
  else wrapAddHID callsite hid []


(** Return true if the heap variable was allocated by the given function *)
let funAllocedHID fkey hid =
  match hid with
    (pp, fk) :: _ -> fk = fkey
  | _ -> false
  
let funAllocedVar fkey var =
  match var with
    FHeap hi -> funAllocedHID fkey hi.hID
  | FVar _ | FRet _ | FInput _ -> false

let isTransMalloc (var, _) cur =
  match cur, var with
    Some _, _ -> cur
  | None, FVar vid ->
      let vi = varinfoOfVid vid in
      if Trans_alloc.isAllocVar vi.vname then Some (vi.vtype)
      else cur
  | _, _ -> cur

(** Check if the RHS value of an assignment instruction indicates
    that the instruction is actually a malloc *)
let assignedMalloc rhs =
  match rhs with
    Refs ls -> 
      FLocSet.fold isTransMalloc ls None
  | _ -> None


let isMallocFun var =
  isFunctionType var.vtype && Alloc.isAlloc var.vname


(************************************************************
 Find same heap var in existing state, and mark as summary
************************************************************)


(** Check if the given malloc var has an existing binding *)      
let mergeMallocs malVar malVal malAtt inSt =
  let tryFindInStore store =
    try 
      let oldVal = VarMap.find malVar store in
      let newVal = combineVals malVal oldVal in
      true, VarMap.add malVar newVal store
    with Not_found ->
      false, store
  in
  let found, newB = tryFindInStore inSt.bindings in
  if found then begin
    (*  let oldAtt = VarMap.find malVar inSt.hAttrs in
        assert (oldAtt <> HGlobal); *)
    let newAtt = combineAttrs malAtt HSum in
    { bindings = newB;
      hAttrs = VarMap.add malVar newAtt inSt.hAttrs; }
  end else begin
    let newAtts = VarMap.add malVar malAtt inSt.hAttrs in
    { bindings = VarMap.add malVar malVal inSt.bindings;
      hAttrs = newAtts; }
  end
