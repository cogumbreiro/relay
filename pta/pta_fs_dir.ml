

(** Pointer analysis that is flow-insensitive, field-sensitive
    and uses directional constraints. *)

open Pta_types
open Cil
open Cilinfos

module Arr = GrowArray
module IH = Inthash
module PC = Pta_compile
module CLv = Cil_lvals

(************* Debug watchers **************)
  

let debugFunc fid =
  if fid = 5840 then 
    print_string "found func!\n"

let debugCheck id =
  if id = 45 then
    print_string "found id!\n"
      

(***** Choice of set representation ******)

module S = Iset

(** Choice of "set" *)
module LVS = S.Make (
  struct
    type t = ptaLv
    let compare a b = compareLv a b
  end
)

module IDS = S.Make (
  struct
    type t = int
    let compare a b = a - b
  end)

module StringSet = S.Make (String)

(***** Common state / ops *****)

let listAddOnce eq x curList =
  if (List.exists (fun y -> eq x y) curList) then curList
  else x :: curList
    
(***************** Ops ************************************)


let filterTypes = ref true

let setFilter what =
  filterTypes := what

(** Return true if the given type matches the varinfo type of 
    the given vid *)
let funTypesEq (typ:ctyp) (id:vid) =
  if !filterTypes then
    try
      let finfo = PC.getFunInfo id in
(*      compatibleTypes typ finfo.funType *)
      compatibleTypesNoUnroll typ finfo.funType
    with Not_found ->
      false
  else 
    true 


(** Add the var id to accum if the given lval matches the function *)
let lvCanBeFun typ lv accum =
  let host, _ = lv in (* If it's a function, it shouldn't have an offset *)
  match host.HC.node with
    PVar v ->
      (match v.HC.node with
         PGlobal (id, _) ->
           if funTypesEq typ id then IDS.add id accum
           else accum
       | PTemp tid -> failwith "Didn't handle temp var in resolve_fp"
       | _ -> accum  (* Mismatch for function *)
      )
  | PDeref _ -> failwith "Deref as part of labels: didn't simplify"


(** true if the set of lvals has an lval based on a global *)
let hasGlobal lvs =
  LVS.exists PC.isGlobalLv lvs

    
(** Function call stuff *)
      
let getFormal fid index = 
  debugFunc fid;
  let finfo = PC.getFunInfo fid in
  List.nth finfo.funFormals index
    
let host_of_formal fid paramIndex =
  let f = getFormal fid paramIndex in
  makeHost (PVar f)
    
let host_of_ret fid =
  makeHost (PVar (makeVar (PRet fid)))
    


(***** Choice of solver ******)


class virtual ['id] constraintIndexer = object (self)

  (***** Handle constraints that have been classified ******)
  
  (* Assumes arbitrary effects through mutation... *)

  method virtual addBaseAssign : ('id -> unit) -> ptaLv -> ptaLv -> location -> unit

  method virtual addSimpleAssign : ('id -> unit) -> ptaLv -> ptaLv -> location -> unit

  method virtual addComplexL : ('id -> unit) -> ptaLv -> ptaLv -> location -> unit

  method virtual addComplexR : ('id -> unit) -> ptaLv -> ptaLv -> location -> unit

  method virtual addFPCall : ptaLv -> ptaCall -> unit

  (****** Classify constraints *******)

  method handleBaseAssign (ifAdded : 'id -> unit) 
    ({lhs = lhs; rhs = rhs; aloc = loc;} as assign) =
    match rhs.HC.node with
      PAddrOf rhsLv ->
        let lhsBase, lhsOff = lhs in
        (match lhsBase.HC.node with
           PVar _ ->
             let rhsBase, rhsOff = rhsLv in
             (match rhsBase.HC.node with
                PVar _ ->
                  self#addBaseAssign ifAdded lhs rhsLv loc
              | PDeref _ ->
                  failwith "baseAssign: not simplified"
             )
         | PDeref _ -> failwith "baseAssign: not simplified")

    | PCast (_, r) ->
        self#handleBaseAssign ifAdded { assign with rhs = r; }

    | _ -> failwith "baseAssign: rhs not addrOf"
        

  method handleAssign (ifAdded: 'id -> unit) 
    ({ lhs = lhs; rhs = rhs; aloc = loc;} as assign) =
    match rhs.HC.node with
      PAddrOf _ -> 
        self#handleBaseAssign ifAdded assign
    | PCast (t, r) -> 
        self#handleAssign ifAdded { assign with rhs = r; }
    | PLv rhsLv ->
        let lHost, lOff = lhs in
        let rHost, rOff = rhsLv in
        (* TODO: handle field-sensitive store/load? *)
        (match lHost.HC.node, rHost.HC.node with
           PVar _, PVar _ -> (* Simple assign *)
             self#addSimpleAssign ifAdded lhs rhsLv loc

         | PDeref ptr, PVar _ -> (* Complex: *p <- v *)
             let ptrLv = getLval ptr in
             self#addComplexL ifAdded ptrLv rhsLv loc
               
         | PVar _, PDeref ptr -> (* Complex: v <- *p *)
             let ptrLv = getLval ptr in
             self#addComplexR ifAdded lhs ptrLv loc

         | PDeref _, PDeref _ -> (* Complex both ways *)
             failwith "handleAssign: didn't simplify!"
        )

  method addBaseAssigns baseAssigns =
    VarH.iter 
      (fun _ assigns -> List.iter (self#handleBaseAssign ignore) assigns) 
      (PC.rehashAssigns baseAssigns)
      
  method addAssignEdges simpleAndComplexAssign =
    VarH.iter
      (fun _ assigns ->
         List.iter (self#handleAssign ignore) assigns
      ) (PC.rehashAssigns simpleAndComplexAssign)

  method addArgEdges (ifAdded: 'id -> unit) loc fid (acts, paramIndex) =
    if (paramIndex <> retIndex ) then
      try 
        let formal = makeLv (host_of_formal fid paramIndex, noOffset) in
        List.iter (fun actual -> 
                     let assign = makeAssign formal actual loc in
                     self#handleAssign ifAdded assign) acts
      with 
        Not_found ->
          L.logError ("addArgEdges: can't find func info? " ^
                        (string_of_int fid))
      | Failure "nth" ->
          L.logError ("addArgEdges: can't handle varargs? " ^
                        (string_of_int fid))
    else
      let abstractRet = makeRv (PLv (makeLv (host_of_ret fid, noOffset))) in
      List.iter
        (fun retRv -> match retRv.HC.node with
           PLv lv -> 
             let assign = makeAssign lv abstractRet loc in
             self#handleAssign ifAdded assign
         | _ -> failwith "PTA: return not captured by lval"
        ) acts

  method doCall (cinfo, args) =
    let loc = cinfo.cloc in
    List.iter 
      (fun callLv ->
         let host, _ = callLv in
         match host.HC.node with
           PVar v -> (* Direct call *) 
             (match v.HC.node with PGlobal (fid, _) ->
                (* add edges once and for all *)
                List.iter (self#addArgEdges ignore loc fid) args

              | _ -> failwith "Non-global func, or non-func used as func"
             )
         | PDeref rv -> (* Indirect: remember funptr to add more edges later *)
             let lv = getLval rv in
             self#addFPCall lv (cinfo, args)
      ) cinfo.cexp

  method addCallEdges calls =
    VarH.iter
      (fun _ callList ->
         List.iter self#doCall callList
      ) (PC.rehashCalls calls)

end


(** Hardekopf and Lin's Lazy Cycle detection solver 
    (well, it will use those tricks when I get to it...) *)
module LCDSolver = struct

  let doCycleDetect = ref true

  let setCycleDetect yesno =
    doCycleDetect := yesno

  (** Node id *)
  type nodeID = int

  (* TODO Simplify the unification + cleanup stuff *)

  (** Set of nodes *)
  module NS = S.Make (struct 
                        type t = nodeID
                        let compare = Pervasives.compare
                      end)

  (** Set of fp calls *)
  module FPS = S.Make 
    (struct 
       type t = ptaCall
       let compare c1 c2 = compareCall c1 c2
     end)
    
  (** this.offset, other.offset sets *)
  module NOS = S.Make (struct
                         type t = ptaOff * nodeID * ptaOff
                         let compare = Pervasives.compare 
                           (* beware... not using the offset compare? *)
                       end)

  (** Set of simple constraints *)
  module SimpCS = Hashtbl.Make 
    (struct
       type t = (nodeID * nodeID)
       let equal x y = (x = y)
       let hash x = Hashtbl.hash x
     end)

  (* TODO: make one that works w/ offsets *)

  (* Debug info *)
  type recStep =
      DerefL of nodeID * nodeID (* though x ptsTo y, and added the assign *)
    | DerefR of nodeID * nodeID
    | FPCall of nodeID * nodeID

  type nodeInfo = {
    mutable ptTargets : NS.t;  (** set of nodes this guy points to *)
    mutable oldTargets : NS.t; (** set of nodes already processed for complex *)

    mutable subsCons : NS.t;   (** simple constraints (e.g., x <- this) *)

    mutable complexL : NS.t;  (** complex constraints w/ deref of this guy on 
                                   the LHS (e.g., *this <- y). *)
    mutable complexR : NS.t;  (** complex constraints w/ deref of this guy on
                                   the RHS (e.g., y <- *this) *)
    mutable fpCalls : FPS.t; (** Set of function pointer calls derefence this *)

    mutable labels : LVS.t;    (** lvals this node represents (can be more 
                                   than one if unified) *)

    (* temporary data for SCC search *)
    mutable sccIndex : int;
    mutable sccRoot : int;
  }


  (***************** The state ******************************)

  let freshNodeInfo () =
    { 
      ptTargets = NS.empty;
      oldTargets = NS.empty;

      subsCons = NS.empty;
      complexL = NS.empty;
      complexR = NS.empty;
      labels = LVS.empty;
      fpCalls = FPS.empty;
      
      (* scc crap *)
      sccIndex = -1;
      sccRoot = -1;
    }

  let dummyNI = 
    (let n = freshNodeInfo () in
     { n with 
         ptTargets = NS.add (-1) n.ptTargets;
     })

  (** Convert lvals to node ids *)
  let lvToID = LvalH.create 101

  (** ID to equiv. class representative node info *)
  let idToNodeECR = Arr.make 128 (Arr.Elem dummyNI)

  let dummyIndex = -1

  (** Forwarding indices... hints to update the NS.t 
      and remove duplicate entries *)
  let idToNewID = Arr.make 128 (Arr.Elem dummyIndex)

  (** Constraints on which the the cycle detection has already tested *)
  let cycleBlackList = SimpCS.create 1
 
  let curNodeID = ref 0

  (**********************************)

  let setForwarding oldID newID =
    Arr.setg idToNewID oldID newID;
    (* Kill info from old *)
    Arr.setg idToNodeECR oldID dummyNI

  let rec chaseForwarding curID =
    let forwardID = Arr.getg idToNewID curID in
    if forwardID != dummyIndex && curID != forwardID then begin
      let finalID = chaseForwarding forwardID in
      (* path compress *)
      setForwarding curID finalID;
      finalID
    end
    else curID
    
  let checkForwarding ifChanged idToCheck =
    let forwardID = chaseForwarding idToCheck in
    if idToCheck != forwardID then begin
      ifChanged idToCheck forwardID;
      forwardID
    end else idToCheck

  let updateNeighbors neighSetRef oldIndex newIndex =
    neighSetRef := NS.remove oldIndex !neighSetRef;
    neighSetRef := NS.add newIndex !neighSetRef

  let ignoreForwarding _ _ = ()

  let getNodeID lv =
    try 
      let id = LvalH.find lvToID lv in
      let newID = checkForwarding 
        (fun oldID newID -> LvalH.replace lvToID lv newID) id in
      newID
    with Not_found ->
      let newID = !curNodeID in
      incr curNodeID;
      LvalH.add lvToID lv newID;
      newID
      
  let makeNewNode (id : nodeID) =
    let ni = freshNodeInfo () in
    Arr.setg idToNodeECR id ni;
    ni
      
  let getNodeRef ifChanged (id : nodeID) : (nodeID * nodeInfo) =
    let newID = checkForwarding ifChanged id in
    let x = Arr.getg idToNodeECR newID in
    if x == dummyNI then begin
      assert (newID == id);
      let newNode = makeNewNode newID in
      (newID, newNode)
    end
    else (newID, x)
      
  let getNode lv : (nodeID * nodeInfo) =
    let id = getNodeID lv in
    let id, ni = getNodeRef ignoreForwarding id in
    (id, ni)

  (*************** workqueues *****************)

  module WQ = Queueset.Make (struct
                 type t = nodeID
                 let compare = Pervasives.compare
               end)

  let work = WQ.create ()

  let addToWork id =
    WQ.addOnce id work

  let initWork () =
    LvalH.iter (fun _ nodeID -> addToWork nodeID) lvToID

  (*************** Initialization **************)

  (** Reset constraint graph, indices, and points-to graph *)
  let reset () =
    LvalH.clear lvToID;
    Arr.clear idToNodeECR;
    SimpCS.clear cycleBlackList;
    curNodeID := 0


  (***** Handle constraints that have been classified ******)

  class myIndexer = object (self)
    inherit [nodeID] constraintIndexer

      
    method addBaseAssign ifAdded lhs rhsLv loc =
      let lhsID, lhsNi = getNode lhs in
      let rhsID, rhsNi = getNode rhsLv in
      if not (LVS.mem rhsLv rhsNi.labels) then begin
        rhsNi.labels <- LVS.add rhsLv rhsNi.labels;
        ifAdded rhsID
      end;
      if not (NS.mem rhsID lhsNi.ptTargets) then begin
        lhsNi.ptTargets <- NS.add rhsID lhsNi.ptTargets;
        ifAdded lhsID
      end

    method addSimpleAssignID ifAdded lhsID rhsID rhsInfo =
      let newLHS = checkForwarding ignoreForwarding lhsID in
      if not (NS.mem newLHS rhsInfo.subsCons) then begin
        rhsInfo.subsCons <- NS.add newLHS rhsInfo.subsCons;
        ifAdded rhsID;
      end

    method addSimpleAssign ifAdded lhs rhs loc =
      let lhsID, _ = getNode lhs in
      let rhsID, rhsInfo = getNode rhs in
      self#addSimpleAssignID ifAdded lhsID rhsID rhsInfo


    method addComplexLID ifAdded lhsID lNodeInfo rhsID =
      let newRHS = checkForwarding ignoreForwarding rhsID in
      if not (NS.mem newRHS lNodeInfo.complexL) then begin
        lNodeInfo.complexL <- NS.add newRHS lNodeInfo.complexL;
        ifAdded lhsID;
      end

    method addComplexL ifAdded lhs rhs loc =
      let lID, lNodeInfo = getNode lhs in 
      let rID, rNodeInfo = getNode rhs in
      self#addComplexLID ifAdded lID lNodeInfo rID
        
    method addComplexRID ifAdded lhsID rhsID rNodeInfo =
      let newLHS = checkForwarding ignoreForwarding lhsID in
      if not (NS.mem newLHS rNodeInfo.complexR) then begin
        rNodeInfo.complexR <- NS.add newLHS rNodeInfo.complexR;
        ifAdded rhsID;
      end
        
    method addComplexR ifAdded lhs rhs loc =
      let lID, _ = getNode lhs in
      let rID, rNodeInfo = getNode rhs in
      self#addComplexRID ifAdded lID rID rNodeInfo

    method addFPCall funPtr callinfo =
      let id, ni = getNode funPtr in
      Printf.printf "Delaying fp call %d\n" id;
      ni.fpCalls <- FPS.add callinfo ni.fpCalls

  end

  let indexer = new myIndexer


  (** Load the initial constraint and points-to graph from the root directory *)
  let loadConstraints root =
    (* Assume one-file mode. TODO: check *)
    let filename = PC.getConstraintFile root in
    let _, baseAss, complexAss, calls, pseudo = 
      PC.loadFor filename in
    indexer#addBaseAssigns baseAss;
    indexer#addAssignEdges complexAss;
    indexer#addCallEdges calls

  (** Make sure nodes representing functions have the function label *)
(*  let labelFunctionNodes () =
    Hashtbl.iter 
      (fun fid finfo ->
         let funVar = makeVar (PGlobal (fid, rehashType finfo.funType)) in
         let funLv = makeLv (makeHost (PVar funVar), noOffset) in
         let nid, ninfo = getNode funLv in
         ninfo.labels <- LVS.add funLv ninfo.labels
      ) PC.funTable
*)

  let init root =
    reset ();
    loadConstraints root
(*    labelFunctionNodes () *)


  (*********** Cycle detection optimization crap *********)

  (* Small SCC info bit savings...
     (but only handles half a billion or so nodes, and pain) *)

  let stackBit = 1 lsl 29

  let notStackBit = lnot stackBit

  let isOnStack nodeInfo =
    (nodeInfo.sccIndex land stackBit) != 0

  let setIndex info index =
    assert (index < stackBit);
    if info.sccIndex == (-1)
    then info.sccIndex <- index (* must happen before on stack *)
    else info.sccIndex <- index lor (info.sccIndex land stackBit)

  let getIndex info =
    info.sccIndex land notStackBit

  let pushStack stack (nodeID, nodeInfo) =
    nodeInfo.sccIndex <- nodeInfo.sccIndex lor stackBit;
    Stack.push (nodeID, nodeInfo) stack
    
  let popStack stack =
    let id, info = Stack.pop stack in
    info.sccIndex <- info.sccIndex land notStackBit;
    (id, info)
      
  let setRootIndex info index =
    info.sccRoot <- index

  let getRootIndex info =
    info.sccRoot

  let notVisted info =
    info.sccIndex < 0

  let isSccRoot info =
    (getRootIndex info) == (getIndex info)

  let nonTrivial scc =
    match scc with
      [] -> failwith "empty scc?"
    | [(id,_)] -> 
        false
    | _ -> true

  let clearScc scc =
    List.iter 
      (fun (nodeID, nodeInfo) -> 
         nodeInfo.sccIndex <- -1;
         nodeInfo.sccRoot <- -1;
      ) scc

  (** Return list of non-trivial sccs found from exploration
      starting at the startNode *)
  let detectCycles startID startNode : (nodeID * nodeInfo) list list =
    let curIndex = ref 0 in
    let sccStack = Stack.create () in
    let curSccs = ref [] in

    let rec popScc (rootID, rootNode) nodesInSCC : (nodeID * nodeInfo) list =
      let (otherID, otherNode) = popStack sccStack in
      let newScc = (otherID, otherNode) :: nodesInSCC in
      if otherNode == rootNode then newScc
      else popScc (rootID, rootNode) newScc
    in

    let rec visit (nodeID, node) =
      setIndex node !curIndex;
      setRootIndex node !curIndex;
      incr curIndex; (* not checking for overflow *)
      pushStack sccStack (nodeID, node);
      (* Might find forwarding that changes neighbor set *)
      let newNeighs = ref node.subsCons in
      NS.iter
        (fun neighID ->
           let neighID, neigh = getNodeRef 
             (updateNeighbors newNeighs) neighID in
           if notVisted neigh then begin
             visit (neighID, neigh);
             setRootIndex node 
               (min (getRootIndex node) (getRootIndex neigh))
           end else if isOnStack neigh then 
             setRootIndex node 
               (min (getRootIndex node) (getIndex neigh))
       ) node.subsCons;
      node.subsCons <- !newNeighs;
      if isSccRoot node then begin
        let newScc = popScc (nodeID, node) [] in
        curSccs := newScc :: !curSccs
      end
    in

    visit (startID, startNode);

    (* Clear visitation data for next round, and prune trivial sccs *)
    List.fold_left 
      (fun cur scc ->
         clearScc scc;
         if nonTrivial scc then scc :: cur
         else cur) [] !curSccs

  let mergeSetsWithout2 s1 s2 x y =
    NS.remove y (NS.remove x (NS.union s1 s2))
(*    NS.union s1 s2
*)


  let mergeSetsWithout s1 s2 x =
   (NS.remove x (NS.union s1 s2))
(*    NS.union s1 s2
*)
    
  (** Hmm... nodes are only pointer equivalent, not location equivalent,
      so only merge if they aren't locations.
      Do something else when we are able to figure out location equiv.? *)
  let notLocation (_, nodeInfo) =
    LVS.is_empty nodeInfo.labels


  (** merge info INTO the first structure *)
  let mergeInfo (i1ID, i1) (i2ID, i2) = begin
    (* Can point to self, but not doubly *)
    i1.ptTargets <- mergeSetsWithout i1.ptTargets i2.ptTargets i2ID;

    i1.oldTargets <- NS.inter i1.oldTargets i2.oldTargets;

    i1.complexL <- mergeSetsWithout i1.complexL i2.complexL i2ID;
    i1.complexR <- mergeSetsWithout i1.complexR i2.complexR i2ID;
    i1.fpCalls <- FPS.union i1.fpCalls i2.fpCalls;
    (* No labels to merge -- assert no labels *)

    (* No point in flowing to self... *)
    i1.subsCons <- mergeSetsWithout2 i1.subsCons i2.subsCons i1ID i2ID;
    (* ignore temp scc info fields *)
  end

  (** Extract the lowest number ID in the list (that isn't a location). 
      May shuffle items a bit *)
  let extractMinNode l =
    let rec loop curRest (curBest, newList) =
      match curRest with
        [] -> 
          (curBest, newList)
      | (otherID, otherNode) as x :: rest ->
          (match curBest with
             Some (curMin, curNode) ->
               if otherID < curMin
               then loop rest (Some x, (curMin, curNode) :: newList)
               else loop rest (curBest, x :: newList)
           | None ->
               loop rest (Some x, newList)
               
          )
    in
    loop l (None, [])
      
  let unifyScc (scc : (nodeID * nodeInfo) list) =
    (* Combine info, make the others use the lowest number ID, 
       and leave crumb indicating the change of address *)
    let nonLocations = List.filter notLocation scc in
    match extractMinNode nonLocations with
      Some (nodeID, node), rest ->
        if rest = [] then 
          ()
        else begin
          Printf.printf "Collapsing: %d " nodeID;
          let id, info = List.fold_left 
            (fun (curID, curInfo) (otherID, otherInfo) ->
               Printf.printf "%d " otherID;
               mergeInfo (curID, curInfo) (otherID, otherInfo);
               setForwarding otherID curID;
               (curID, curInfo)
          ) (nodeID, node) rest in
          print_newline ();
          assert (id == nodeID);
          addToWork id
        end
    | None, [] -> Printf.printf "No non-locations to collapse\n"
    | _ -> failwith "failed to pick non-location"

  let tryUnify ((lhsID, rhsID) as edge) s1 s2 =
    if !doCycleDetect then
      (* TODO: black list the actual nodeinfos or just the IDS? *)
      if SimpCS.mem cycleBlackList edge then ()
      else if NS.equal s1 s2 then begin
        SimpCS.add cycleBlackList edge ();
        (* Don't care if IDs changed (even if ID is used in blacklist?) *)
        let lhsID, lhsNode = getNodeRef ignoreForwarding lhsID in
        let nonTrivSccs = detectCycles lhsID lhsNode in
        List.iter unifyScc nonTrivSccs
      end
      else ()
    else ()
  
  (**************** Solver steps ********************)

  (* Function pointer calls *)

  let funcsOfTargets typ targIDs : IDS.t =
    NS.fold 
      (fun targID cur ->
         let targID, ni = getNodeRef ignoreForwarding targID in
         LVS.fold (lvCanBeFun typ) ni.labels cur
      ) targIDs IDS.empty
  
  let getFuncs typ targets : IDS.t =
    let results = funcsOfTargets typ targets in
(*
    Printf.printf "getFuncs returns: ";
    IDS.iter (fun vid ->  Printf.printf "%d, " vid) results;
    print_string "\n";
*)
    results

  (* Complex constraints *)

  let checkFPCall ni nodeID targetsToConsider =
    if not (FPS.is_empty ni.fpCalls) then
      Printf.printf "Doing fp call *%d()\n" nodeID
    ;
    FPS.iter 
      (fun (callinfo, args) ->
         let funs = getFuncs callinfo.ctype targetsToConsider in
         let loc = callinfo.cloc in
         IDS.iter
           (fun fid ->
              List.iter (indexer#addArgEdges addToWork loc fid) args
           ) funs;
      ) ni.fpCalls
      
  let checkComplexR ni targID targInfo =
    let newComplexR = ref ni.complexR in
    NS.iter 
      (fun lhsID ->
         let lhsID, _ = 
           getNodeRef (updateNeighbors newComplexR) lhsID in
         indexer#addSimpleAssignID addToWork lhsID targID targInfo
      ) ni.complexR;
    ni.complexR <- !newComplexR

  let checkComplexL ni targID =
    let newComplexL = ref ni.complexL in
    NS.iter
      (fun rhsID ->
         let rhsID, rhsInfo = 
           getNodeRef (updateNeighbors newComplexL) rhsID in
         indexer#addSimpleAssignID addToWork targID rhsID rhsInfo
      ) ni.complexL;
    ni.complexL <- !newComplexL
        
  let checkComplexEdges ni nodeID =
    let targetsToConsider = NS.diff ni.ptTargets ni.oldTargets in

    let prunedTargets = ref ni.ptTargets in

    let checkR = checkComplexR ni in
    let checkL = checkComplexL ni in

    checkFPCall ni nodeID targetsToConsider;

    NS.iter 
      (fun targID ->
         let targID, targInfo = 
           getNodeRef (updateNeighbors prunedTargets) targID in
         
         (* iter on complex assigns *)
         checkR targID targInfo;
         
         checkL targID;
      ) targetsToConsider;

    ni.ptTargets <- !prunedTargets;
    ni.oldTargets <- !prunedTargets (* processed them all *)

  (* Simple assignments *)

  let flowPtsTo ni curNodeID =
    let newSubs = ref ni.subsCons in
    NS.iter
      (fun destID ->
         let destID, destInfo = getNodeRef (updateNeighbors newSubs) destID in
         (* Don't flow to functions *)
         if not (LVS.is_empty destInfo.labels) && 
           LVS.for_all PC.isFuncLv destInfo.labels then
             ()
         else begin(* Check if ptsToSet will be new *)
           if not (NS.subset ni.ptTargets destInfo.ptTargets) then begin
             Printf.printf "Flowing pts from %d to %d\n" curNodeID destID;
             destInfo.ptTargets <- NS.union ni.ptTargets destInfo.ptTargets;
             addToWork destID;
           end else
             tryUnify (destID, curNodeID) ni.ptTargets destInfo.ptTargets
         end
      ) ni.subsCons;
    ni.subsCons <- !newSubs

  let solve root =
    PC.loadFuncInfo root; (* Just in case *)

    initWork ();
    let initialWork = WQ.length work in
    Printf.printf "Working with %d lval nodes\n" (LvalH.length lvToID);
    Printf.printf "Solving constraints -- workqueue: %d\n" initialWork;
    let steps = ref 0 in
    let firstSteps = WQ.length work in
    while not (WQ.is_empty work) do
      let curNodeID = WQ.pop work in
      (* TODO: maybe try seeing if newID is already on the queue? *)
      let curNodeID, ni = getNodeRef ignoreForwarding curNodeID in

      checkComplexEdges ni curNodeID;

      flowPtsTo ni curNodeID;

      incr steps;
      if (!steps > firstSteps && !steps mod 50 == 0) then begin
        Printf.printf "Iteration %d, work %d\n" !steps (WQ.length work);
        flush stdout;
      end
    done;
    Printf.printf "Took %d iterations, %d more than initial worklist\n"
      !steps (!steps - initialWork);
    flush stdout

end

module Solver = LCDSolver




(************** Final representation of PTA info *************)

let aaName = "anders"

let getResultsFilename (root : string) =
  Filename.concat root (".pta_results." ^ aaName)

let getTempResults root =
  Filename.concat root (".pta_temp." ^ aaName)


module Final = struct
  
  (**** Messy conversion of old nodes to serializable data ****)

  type oldNode = Solver.nodeInfo

  type nodeID = Solver.nodeID

  module NS = Solver.NS

  module OldNH = Hashtbl.Make (
    struct
      type t = oldNode
      let equal = (==)
      let hash x = Hashtbl.hash_param 16 32 x
    end )

  let oldNodeToID = OldNH.create 107 

  (* New node infos + their IDs *)

  type nodeInfo = {
    ptTargets : NS.t;  (** set of nodes this guy points to *)
    labels : LVS.t;    (** lvals this node represents (can be more 
                           than one if unified) *)
    mutable globReach : bool; (** reachable from global? *)
  }

  let dummyInfo = {
    ptTargets = NS.empty;
    labels = LVS.empty;
    globReach = true;
  }

  let emptyInfo = {
    ptTargets = NS.empty;
    labels = LVS.empty;
    globReach = false;
  }

  let combineInfo i1 i2 = 
    { ptTargets = NS.union i1.ptTargets i2.ptTargets;
      labels = LVS.union i1.labels i2.labels;
      globReach = i1.globReach || i2.globReach;
    }

  let lvToID = ref (LvalH.create 107)
  let idToNode = ref (Arr.make 128 (Arr.Elem dummyInfo))

  let getIDLv lval =
    LvalH.find !lvToID lval
    
  let setID id node =
    Arr.setg !idToNode id node

  let derefID id =
    try Arr.get !idToNode id 
    with (Invalid_argument "index out of bounds") as ex ->
      L.logError ("derefID: no node for id " ^ (string_of_int id));
      raise ex

  let rec convertNode oldID ni =
    try
      let prevID = OldNH.find oldNodeToID ni in
      prevID
    with Not_found ->
      (* old IDs were unique, so reuse *)
      OldNH.add oldNodeToID ni oldID;
      let newNode = 
        { ptTargets = convertTargets ni.Solver.ptTargets;
          labels = ni.Solver.labels;
          globReach = false; (* just recompute this from markGlobalReach *)
        } in
      setID oldID newNode;
      oldID

  and convertID id =
    let oldID, oldN = Solver.getNodeRef Solver.ignoreForwarding id in
    convertNode oldID oldN

  and convertTargets ids =
    NS.fold (fun id cur -> NS.add (convertID id) cur) ids NS.empty

  let convertTable () =
    let len = LvalH.length Solver.lvToID in
    Printf.printf "Converting pts to info to be serializable -- %d elems\n" len;
    flush stdout;
    OldNH.clear oldNodeToID;
    LvalH.iter
      (fun lv oldID ->
         if isTemp lv then () (* prune temp vars *)
         else begin 
           let newID = convertID oldID in
           LvalH.replace !lvToID lv newID
         end
      )
      Solver.lvToID;
    OldNH.clear oldNodeToID

  (**** Fill in reachability info ****)

  let markGlobalReach () =
    print_string "Marking global reachability\n";
    flush stdout;
    let visited = IH.create 17 in
    let rec visit g id = 
      (if (IH.mem visited id) then ()
       else begin
         IH.add visited id ();
         let node = derefID id in
         let newG = if (g) then g
         else hasGlobal node.labels in
         node.globReach <- node.globReach || newG;
         NS.iter (visit newG) node.ptTargets
       end
      );
    in
    LvalH.iter 
      (fun lv id ->
         IH.clear visited;
         visit (PC.isGlobalLv lv) id
      ) !lvToID;
    print_string "Done marking global reachability\n";
    flush stdout

  (**** Rehash to find equivalent nodes, after converting and
        calculating global reachability ****)
      
  (* This is super-similar to the above conversion from old -> new... *)
      
  let sameInfo n1 n2 =
    NS.equal n1.ptTargets n2.ptTargets &&
      LVS.equal n1.labels n2.labels &&
      n1.globReach == n2.globReach
      (* Hmm... still worried about location equivalence and 
         cleaning up points-to sets after merging *)
      
  module NewNH = Hashtbl.Make (
    struct
      type t = nodeInfo
      let equal x y = sameInfo x y
      let hash x = Hashtbl.hash_param 32 64 x
    end
  )
    
  (* Hmm... how to prevent non-aliased targetless nodes from being 
     aliased (both pointer and location aliasing)? *)
    
  let curNewIndex = ref 0 
  let oldNodeToID = ref (NewNH.create 37)
  let newLvToID = ref (LvalH.create 37)
  let newNodeToID = ref (NewNH.create 37)
  let newIDToNode = ref (Arr.make 128 (Arr.Elem dummyInfo))

  (** Filled by the "rehash" *)
  let nodeToID = ref (NewNH.create 37)

  let clearRehashTemp sizeHint =
    let sizeHint = if sizeHint <= 0 then 8 else sizeHint in
    (* not resetting curNewIndex ? *)
    newLvToID := LvalH.create (sizeHint / 8); 
    (* abitrarily expecting an 8 fold decrease *)
    oldNodeToID := NewNH.create (sizeHint / 8);
    newNodeToID := NewNH.create (sizeHint / 8);
    newIDToNode := Arr.make (sizeHint / 8) (Arr.Elem dummyInfo)
      
  let setNewNode info2ID id2Info newID newNode =
    Arr.setg id2Info newID newNode;
    NewNH.replace info2ID newNode newID

  let rec rehashID oldID =
    let oldNode = derefID oldID in
    try
      NewNH.find !oldNodeToID oldNode
    with Not_found ->
      let newID = !curNewIndex in
      incr curNewIndex;

      (* redirect old nodes to the new one that's about to be 
         created / mark the old guy visited so that cycles are no problem *)
      NewNH.add !oldNodeToID oldNode newID;

      (* new node has convert pts to targets... *)
      let newNode = 
        { oldNode with
            ptTargets = rehashIDSet oldNode.ptTargets;
        } in
      (* assert (LVS.cardinal newNode.labels <= 1);
  *)
      setNewNode !newNodeToID !newIDToNode newID newNode;
      newID

  and rehashIDSet idSet =
    NS.fold (fun oldID cur -> NS.add (rehashID oldID) cur) idSet NS.empty

  let rehashTable () =
    print_string "Rehashing data to find more equivalences\n";
    flush stdout;
    let oldNum = (Arr.max_init_index !idToNode) in
    clearRehashTemp oldNum;
    LvalH.iter
      (fun lv oldID ->
         let newID = rehashID oldID in
         LvalH.replace !newLvToID lv newID
      )
      !lvToID;
    Printf.printf "Old bindings: %d\tNew: %d\n\n" 
      oldNum (Arr.max_init_index !newIDToNode);
    lvToID := !newLvToID;
    idToNode := !newIDToNode;
    nodeToID := !newNodeToID;
    clearRehashTemp 0


  (**** Convert -> Serialize -> ... -> Deserialize ****)
      
  let saveState outName =
    let out_chan = open_out_bin outName in
    Marshal.to_channel out_chan 
      (!lvToID, !idToNode, !nodeToID) [Marshal.Closures] ;
    close_out out_chan

  let saveAll (root:string) =
    convertTable ();
    (* Don't need solver state anymore *)
    Solver.reset ();

    markGlobalReach ();

    rehashTable ();
    if (NewNH.length !nodeToID != 0) then
      L.logError "Warning: working w/ 0 pointer info\n"
        (* common on small test cases *)
    ;
    
    let outName = getResultsFilename root in
    saveState outName


  let loadAll (oldFile:string) =
    let in_chan = open_in_bin oldFile in
    let lvID, id2N, n2ID = Marshal.from_channel in_chan in
    lvToID := lvID;
    idToNode := id2N;
    nodeToID := n2ID;
    close_in in_chan;
    curNewIndex := (Arr.max_init_index !idToNode + 1)

  (** Get ID from node info *)
  let getIDInfo info =
    try
      (* TODO: try storing one reference copy of info in there too... *)
      NewNH.find !nodeToID info
    with Not_found -> 
      assert (!curNewIndex > Arr.max_init_index !idToNode);
      let newID = !curNewIndex in
      incr curNewIndex;
      setNewNode !nodeToID !idToNode newID info;
      newID

  (**** Ops on final node infos, node IDs, and lvals *)

  let globalReach node =
    node.globReach

  let labelsOfTargets targIDs : LVS.t =
    NS.fold
      (fun targID cur ->
         let ni = derefID targID in
         LVS.union cur ni.labels
      ) targIDs LVS.empty

  let funcsOfTargets typ targIDs : IDS.t =
    NS.fold 
      (fun targID cur ->
         let ni = derefID targID in
         LVS.fold (lvCanBeFun typ) ni.labels cur
      ) targIDs IDS.empty

  let targetsOfID ptrID =
    let ptrInfo = derefID ptrID in
    ptrInfo.ptTargets

  let rec targetNodes ptrLv : NS.t =
    try (* maybe the lval has target pts-to info already *)
      let id = getIDLv ptrLv in
      targetsOfID id
  
    with Not_found -> (* maybe it doesn't *)
      (* TODO: handle offsets *)
      let host, off = ptrLv in
      match host.HC.node with
        PVar _-> 
          L.logError ("nodeInfo not found for: " ^ string_of_ptaLv ptrLv);
          NS.empty
            
      | PDeref rv ->
          let lv = getLval rv in
          (* Find targets of self (a derefed pointer) by getting targets 
             of targets of derefed pointer *)
          let targets = targetNodes lv in
          NS.fold 
            (fun tid cur ->
               NS.union (targetsOfID tid) cur
            ) targets NS.empty

  let resolveFPs typ ptrLv =
    let targets = targetNodes ptrLv in
    let fids = funcsOfTargets typ targets in
    fids

  let foldNode foo ptrLv accum =
    try
      let id = getIDLv ptrLv in
      foo id accum
    with Not_found ->
      (* TODO: handle offsets *)
      let host, off = ptrLv in
      match host.HC.node with
        PVar _ ->
          L.logError ("nodeInfo not found for: " ^ string_of_ptaLv ptrLv);
          accum

      | PDeref rv ->
          let lv = getLval rv in
          let targets = targetNodes lv in
          NS.fold 
            (fun tid cur ->
               foo tid accum
            ) targets accum


  let labelsOfID nodeID =
    let info = derefID nodeID in
    info.labels

  let labelsOf ptrLv : LVS.t =
    foldNode (fun id accum -> LVS.union (labelsOfID id) accum) ptrLv LVS.empty

  let globalReachID nodeID =
    let info = derefID nodeID in
    info.globReach

  let globalReachLv ptrLv : bool =
    foldNode (fun id accum -> accum || globalReachID id) ptrLv false

  module NCache = Cache.Make (HashableLv)

  let nodeCache = NCache.create 1024

  let regenNodeinfo ptrLv =
    L.logError ("regenNodeinfo " ^ string_of_ptaLv ptrLv);
    (* try constructing pts to info, etc. *)
    let targets = targetNodes ptrLv in
    (* Hmm... don't store labels w/ generated nodes? 
       no need it to check location aliasing (later) *)
    let labels = labelsOf ptrLv in 
(*
    let labels = LVS.empty in
*)
    let globR = globalReachLv ptrLv in
    let newInfo = { ptTargets = targets;
                    labels = labels;
                    globReach = globR;
                  } in
    let newID = getIDInfo newInfo in
    newID, newInfo


  (** some-what costly way of re-constructing nodeInfo for a given lval *)
  let getNodeinfo ptrLv =
    try (* maybe the lval has pts to info *)
      let id = LvalH.find !lvToID ptrLv in
      let node = derefID id in
      (id, node)
    with Not_found ->
      try
        NCache.find nodeCache ptrLv
      with Not_found ->
        let nodeID, nodeInfo = regenNodeinfo ptrLv in
        NCache.add nodeCache ptrLv (nodeID, nodeInfo);
        (nodeID, nodeInfo)
 

  (** some-what costly graph reachability check -- 
      TODO: use pre-visit and post-visit numbers to check 
      hmm, but not good w/ back-edges? *)
  let canReach targLv srcLv =
    let srcs = targetNodes srcLv in
    let visited = IH.create 7 in
    let rec visit targLv srcID = 
      let node = derefID srcID in
      if LVS.mem targLv node.labels then true
      else
        (if (IH.mem visited srcID) then false
         else begin
           IH.add visited srcID ();
           NS.exists (visit targLv) node.ptTargets
         end)
    in
    NS.exists (visit targLv) srcs

  module RCache = Cache.Make(
    struct
      type t = (nodeID * nodeID)
      let equal x y = x = y
      let hash x = Hashtbl.hash x
    end)
    
  let reachableCache = RCache.create 2048

  (** Is targID pointed to by some srcID? 
      -- assumes targs are only things that have had their address taken... 
      e.g., given 
      
      a = &b;
      b = &c;

      we never ask "canReach *b a"?
  *)
  let canReachID targID srcID =
    try 
      RCache.find reachableCache (targID, srcID)
    with Not_found ->
      let visited = IH.create 7 in
      let rec visit srcID =
        if targID == srcID then true
        else if IH.mem visited srcID then false
        else begin
          IH.add visited srcID ();
          let node = derefID srcID in
          NS.exists visit node.ptTargets 
        end
      in
      let result = visit srcID in
      RCache.add reachableCache (targID, srcID) result;
      result
    

end


(************** API ************)

(** Choose temp results from last run, or choose original results *)
let chooseOldFile root =
  let origResults = getResultsFilename root in
  let tempResults = getTempResults root in
  if Sys.file_exists tempResults then
    Some tempResults
  else if Sys.file_exists origResults then
    Some origResults
  else None


(** Re-analyze or reload analysis state *)
let analyzeAll (root:string) (fresh:bool) =
  PC.loadFuncInfo root;

  let regen () =
    Solver.init root;
    Solver.solve root;
    Final.saveAll root;
  in

  if (not fresh) then begin
    match chooseOldFile root with
      Some oldFile ->
        (* load existing results *)
        Printf.printf "Using old PTA results: %s\n" oldFile;
        Final.loadAll oldFile;
        print_string "Old PTA results loaded\n";

    | None ->
        regen ()
  end else 
    regen ()


(** Write out analysis state that may have accumulated as analysis is used *)
let writeState rootPath =
  let fname = getTempResults rootPath in
  L.logStatusF "\n\nwriteState: writing new state for Andersen to %s\n\n" fname;
  L.flushStatus ();
  Final.saveState fname


(** Interface to pts-to data based on abstract "representatives" *)
module Abs = struct

  type nodeIdent = Final.nodeID

  type t = nodeIdent

  module NS = Final.NS

  let getNodeIdent (ptLv : ptaLv ) : nodeIdent =
    let id, info = Final.getNodeinfo ptLv in
    id

  (**** Thunk operations ****)

  let getNodeLvs (lvs : ptaLv list) : t option =
    match lvs with
      [] -> None
    | [ptaLv] -> 
        Some (getNodeIdent ptaLv)
    | _ -> 
        let finalInfo = List.fold_left 
          (fun curInfo lv ->
             let id, info = Final.getNodeinfo lv in
             Final.combineInfo info curInfo
          ) Final.emptyInfo lvs in
        Some (Final.getIDInfo finalInfo)
 
  let getNodeLval (lv : Cil.lval) : t option =
    let ptaLvs = PC.analyze_lval lv in
    getNodeLvs ptaLvs

  let getNodeExp (exp : Cil.exp) : t option =
    let ptRvs = PC.analyze_exp exp in
    let ptLvs = List.fold_left 
      (fun cur rv -> try (getLval rv) :: cur with Not_found -> cur) [] ptRvs in
    getNodeLvs ptLvs

  module DCache = Cache.Make (struct 
                                type t = int 
                                let equal x y = x == y
                                let hash x = Hashtbl.hash x
                              end)

  (** Deref cache *)
  let derefCache = DCache.create 4096

  let deref (abs : t) : t =
    try
      DCache.find derefCache abs
    with Not_found ->
      let targets = Final.targetsOfID abs in
      let finalInfo = 
        NS.fold (fun tid cur ->
                   let info = Final.derefID tid in
                   Final.combineInfo info cur
                ) targets Final.emptyInfo in
      let finalID = Final.getIDInfo finalInfo in
      DCache.add derefCache abs finalID;
      finalID

  let deref_lval lv = 
    let ptLvs = PC.analyze_lval lv in
    match getNodeLvs ptLvs with
      None -> (* raise UnknownLoc *) []
    | Some n -> [deref n]

  let deref_exp exp = 
    let ptRvs = PC.analyze_exp exp in
    let ptLvs = List.fold_left 
      (fun cur rv -> try (getLval rv) :: cur with Not_found -> cur) [] ptRvs in
    match getNodeLvs ptLvs with
      None -> (* raise UnknownLoc *) []
    | Some n -> [deref n]

  (* TODO: canonicize the abstract nodes some... what if we have
     a set of them, and the info is redundant? Problem is some of the
     info may only be "pointer-equivalent" and not "location-equivalent", etc.
  *)

  let lvals_of (abs : t) : Cil.lval list =
    let ptLvs = Final.labelsOfID abs in
    LVS.fold
      (fun lv results  ->
         try 
           lv_of_ptaLv lv :: results
         with Not_found ->
           results
      ) ptLvs []
      
  let may_alias (abs1 : t) (abs2 : t) =
    let targs1 = Final.targetsOfID abs1 in
    let targs2 = Final.targetsOfID abs2 in
    not (NS.inter_isEmpty targs1 targs2)
      
  let location_alias (abs1 : t) (abs2 : t) =
    (* If we check that ids are the same, maybe rehashing/collapsing 
       shouldn't happen *)
    let labels1 = Final.labelsOfID abs1 in
    let labels2 = Final.labelsOfID abs2 in
    not (LVS.inter_isEmpty labels1 labels2)

  let points_to (ptr : t) (targ :t) =
    let ptrTargs = Final.targetsOfID ptr in
    let inPts = NS.mem targ ptrTargs in
    if inPts then
      true
    else begin
      (* Extra check: What if target is one of the "invented" infos? *)
      let labelsOfTarg = Final.labelsOfID targ in
      NS.exists 
        (fun ptTargID ->
           let ptTargLabs = Final.labelsOfID ptTargID in
           not (LVS.inter_isEmpty labelsOfTarg ptTargLabs)
        ) ptrTargs
    end
      
  let compare (abs1 : t) (abs2 : t) =
    Pervasives.compare abs1 abs2

  let hash abs = 
    Hashtbl.hash abs

  let string_of abs = 
    "REP:[" ^ string_of_int abs ^ "]"

  let pts_size abs : int =
    let targs = Final.targetsOfID abs in
    NS.cardinal targs

  let label_size abs : int =
    let labs = Final.labelsOfID abs in
    LVS.cardinal labs

  let reachableFrom (target : t) (srcs : t list) : bool =
    List.exists 
      (fun src -> Final.canReachID target src) srcs
      
  let reachableFromG (target : t) : bool =
    Final.globalReachID target

end


let resolve_funptr (ptrExp:Cil.exp) : vid list =
  let e_typ = 
    try 
      CLv.typeAfterDeref ptrExp    (* if function pointer is given *)
    with CLv.TypeNotFound ->
      L.logError "Pta: resolve_funptr not given a pointer";
      CLv.typeOfUnsafe ptrExp in   (* if function pointer deref is given *)
  let e_typ = cil_type_to_ptype e_typ in
  let lv_exps = PC.analyze_exp ptrExp in
  let idSet = List.fold_left
    (fun results rv ->
       try 
         let ptrLv = getLval rv in
         let fids = Final.resolveFPs e_typ ptrLv in
         IDS.union results fids
       with Not_found ->
         results
    ) IDS.empty lv_exps in
  IDS.elements idSet


let lvals_to_vars lvals =
  List.fold_left (fun cur lv -> match lv with
                    Var vi, _ -> vi :: cur
                  | _ -> cur) [] lvals
    

let deref_exp (ptr : Cil.exp) : Cil.varinfo list =
  let abs = Abs.deref_exp ptr in
  let lvals =  List.fold_left 
    (fun cur abs ->
       let lvals = Abs.lvals_of abs in
       List.rev_append lvals cur) [] abs in
  lvals_to_vars lvals


(* may points to same... not refer to same location... *)
let may_alias (ptr1 : Cil.exp) (ptr2 : Cil.exp) : bool =
  match Abs.getNodeExp ptr1 , Abs.getNodeExp ptr2 with
    Some abs1, Some abs2 ->
      Abs.may_alias abs1 abs2 
  | _, _ -> 
      L.logError "Pta_fs_dir may_alias couldn't find node";
      false


let points_to (ptr : Cil.exp) (v : varinfo) : bool =
  failwith "TODO"



(**************************************************************** 
    Debug 
*****************************************************************)


(** Info required to print debugging info *)
module type PrintablePtsTo = sig

  type nodeID
  type nodeInfo

  val string_of_id : nodeID -> string

  val forEachTarget : (nodeID -> unit) -> nodeInfo -> unit
  val numTargets : nodeInfo -> int

  val forEachLabel : (ptaLv -> unit) -> nodeInfo -> unit
  val numLabels : nodeInfo -> int

  val forEachLval : (ptaLv -> nodeID -> unit) -> unit

  val globalReach : nodeInfo -> bool

  val derefID : nodeID -> nodeInfo

end

(** Print based on the solver state *)
module SolverDebug = struct

  type nodeID = Solver.nodeID
  type nodeInfo = Solver.nodeInfo

  let string_of_id id = string_of_int id

  let forEachTarget foo node =
    Solver.NS.iter foo node.Solver.ptTargets

  let numTargets node =
    Solver.NS.cardinal node.Solver.ptTargets

  let forEachLabel foo node =
    LVS.iter foo node.Solver.labels
    
  let numLabels node =
    LVS.cardinal node.Solver.labels

  let forEachLval foo =
    LvalH.iter foo Solver.lvToID

  let globalReach node = 
    true (* not actually tracked *)

  let derefID id =
    let _, nodeInfo = Solver.getNodeRef Solver.ignoreForwarding id in
    nodeInfo

end

(** Print based on the finalized state *)
module FinalDebug = struct

  type nodeID = Final.nodeID
  type nodeInfo = Final.nodeInfo

  let string_of_id id = string_of_int id

  let forEachTarget foo node =
    Final.NS.iter foo node.Final.ptTargets

  let numTargets node =
    Final.NS.cardinal node.Final.ptTargets

  let forEachLabel foo node =
    LVS.iter foo node.Final.labels
    
  let numLabels node =
    LVS.cardinal node.Final.labels

  let forEachLval foo =
    LvalH.iter foo !Final.lvToID
    
  let globalReach node = 
    Final.globalReach node

  let derefID id =
    Final.derefID id

end

(** Actual printer of the debugging *)
module DebugSets (PPT:PrintablePtsTo) = struct

  module Dist = Distributions
      
  let string_of_id id =
    "(id = " ^ (PPT.string_of_id id) ^ ")"
      
  let printNodeId id =
    print_string (PPT.string_of_id id);
    print_string ", "
    
  let printTargets nodeInfo =
    PPT.forEachTarget printNodeId nodeInfo

  let printLabel label =
    printPtaLv label;
    print_string ", "
            
  let printLabels nodeInfo =
    PPT.forEachLabel printLabel nodeInfo
      
  let printPtsToSets () =
    let lab_sizes = Dist.makeDistro () in
    let pt_sizes = Dist.makeDistro () in
    let maxID = ref None in
    
    let updateMaxID anID =
      match !maxID with
        None -> maxID := Some anID
      | Some cur -> if cur < anID 
        then maxID := Some anID else ()
    in

    PPT.forEachLval 
      (fun k nodeID ->
         updateMaxID nodeID;

         print_string "Node w/ lval key: ";
         printPtaLv k;
         print_string "\t";
         print_string ((string_of_id nodeID) ^ "\n");
         
         let node = PPT.derefID nodeID in
         let lab_size = PPT.numLabels node in
         let pt_size = PPT.numTargets node in 
         if lab_size == 0 && pt_size == 0 then
           print_string "is trivial\n"
         else begin
           if lab_size == 0 then
             print_string "Represents: Nothing\n"
           else begin
             print_string "Represents: {";
             printLabels node;
             Printf.printf "} (%d)\n" lab_size;
           end;
           
           if pt_size == 0 then
             print_string "Points to: Nothing\n"
           else begin 
             print_string "Points to: {";
             printTargets node;
             Printf.printf "} (%d)\n" pt_size;
           end;
           (* TODO: count labels in the pt_set for 
              the real count of points-to size *)
           
         end;
         Printf.printf "Global-reachable: %B\n" (PPT.globalReach node);
         Dist.updateDistro lab_sizes lab_size;
         Dist.updateDistro pt_sizes pt_size;

         print_string "\n"

      );

    (match !maxID with
       None -> ()
     | Some x ->
         Printf.printf "Max ID is: %s\n\n" (string_of_id x)
    );

    Dist.printDistroSortKey lab_sizes string_of_int "label sets";
    Dist.printDistroSortKey pt_sizes string_of_int "pts-to sets"

end

module DebugSolver = DebugSets (SolverDebug)

module DebugFinal = DebugSets (FinalDebug)

(***** Consistency checks *****)

(** Check that targets are "locations" *)
let nonLocationTarget targID cur =
  if cur == None then
    let node = Final.derefID targID in
    if LVS.is_empty node.Final.labels then Some targID else None
  else cur

let checkAllTargets () =
  print_string "\nTesting pointer targets\n------------------\n";
  LvalH.iter 
    (fun k nodeID ->
       let node = Final.derefID nodeID in
       match Final.NS.fold nonLocationTarget node.Final.ptTargets None with
         None -> ()
       | Some targID ->
           failwith ("ID: " ^ (string_of_int targID) ^ " is a non-location")
    ) !Final.lvToID;
  print_string "\tpassed\n"
  

(***** Debug fun ptrs *****)

class fpTest : Pta_fp_test.fpTestable = object (self)
  
  method init (f:Cil.file) = ()

  method resolve_fp = resolve_funptr

  method name = "Anders"

end


