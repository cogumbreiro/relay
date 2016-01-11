
open Cil
open Logging
open Type_utils
open Fp_rci_types
open Fp_rci_lattice_ops

(*********************************************************
 * Update state on Malloc -- 
 * - detect summary nodes and merge
 * - set offsets of new mem to NULL
 *********************************************************)

let topLevelID = (ppUnknown, -1)

let makeMalloc t (pp, fkey) =
  (FHeap ( { hID = [(pp, fkey)]; } ), 
   { vKind = HSingle;
     vType = addTyp (canonType t); } )


(* Maybe should have used a queue instead... *)
let rec wrapAddHID callsite hid curFront =
  match hid with
    [] -> callsite :: List.rev curFront
  | [h; _] -> wrapAddHID callsite [] (h :: curFront) (* Drop the last dude *)
  | h :: t -> wrapAddHID callsite t (h :: curFront)
      (* Note that dropping part of the name makes it harder compare /
         check aliasing across procedures (easy within) *)


let hidCollapseCycles callsite hid =
  let sameAs h (other, _) = h = other in
  let rec loop curHead curTail seen =
    match curTail with
      [] -> curHead
    | h :: t ->
        try loop (snd (List.find (sameAs h) seen)) t seen
        with Not_found ->
          let newHead = h :: curHead in
          loop newHead t ((h, newHead) :: seen)
  in
  loop [] (List.rev (callsite :: hid)) []
    

(** Mark that the given function was handed/returned the heap variable *)
let extendHeapID callsite hid =
  let len = List.length hid in
  if len < !heapMax then hidCollapseCycles callsite hid
  else hid


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

(* TODO: move this, make it handle more functions *)
let isMemcpyFun var =
  isFunctionType var.vtype && var.vname = "memcpy"

(************************************************************
 Find same heap var in existing state, and mark as summary
************************************************************)


(** Check if the given malloc var has an existing binding *)      
let mergeMallocs malVar malVal malAtt inSt =
  let tryFindInStore store =
    try 
      let oldVal = VarMap.find malVar store in
      let newVal = combineVals valBaseNone malVal oldVal in
      true, VarMap.add malVar newVal store
    with Not_found ->
      false, store
  in
  let found, newB = tryFindInStore inSt.bindings in
  if found then begin
    let newAtt = combineAttr malAtt { malAtt with vKind = HSum; } in
    { bindings = newB;
      vAttrs = VarMap.add malVar newAtt inSt.vAttrs;  }
  end else begin
    { bindings = VarMap.add malVar malVal inSt.bindings;
      vAttrs = VarMap.add malVar malAtt inSt.vAttrs; }
  end
