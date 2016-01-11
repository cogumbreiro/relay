

module type NodeInfo = sig

  type id

  val eqID : id -> id -> bool

  val hashID : id -> int

  val maxStack : int

  val iterNeighs : (id -> unit) -> id -> unit

  val clearState : unit -> unit 

end


module CycleDetector (N:NodeInfo) = struct

  type nodeBookeeping = {
    (* temporary data for SCC search *)
    mutable sccIndex : int;
    mutable sccRoot : int;
  }

  module NH = Hashtbl.Make 
    (struct 
       type t = N.id
       let equal x y = N.eqID x y
       let hash x = N.hashID x
     end) 

  (* Maybe make this an array... clearing is "expensive" if search is sparse.

     (64 bits for the 2 ints * 100000 nodes == 25MB of crap 
     to crawl per round of clearing... takes too long / wouldn't want
     to reallocate that!)

     Or keep a list of accessed indexes and clear just those ? 
     
     Also... OfflineNode doesn't use int as ID...
  *)

  let bookkeeping = NH.create 17 
  let sccStack = Stack.create ()
  let stackHeight = ref 0
  let curIndex = ref 0

  let stackBit = 1 lsl 29
  let notStackBit = lnot stackBit

  let clear () = begin
    Stack.clear sccStack;
    stackHeight := 0;
    NH.clear bookkeeping;
    curIndex := 0;
    N.clearState ();
  end

  let getInfo id = 
    try NH.find bookkeeping id
    with Not_found ->
      let newInfo = { sccIndex = -1; sccRoot = -1; } in
      NH.add bookkeeping id newInfo;
      newInfo

  let isOnStack nodeInfo =
    (nodeInfo.sccIndex land stackBit) != 0

  let setIndex info index =
    if info.sccIndex == (-1)
    then info.sccIndex <- index (* must happen before on stack *)
    else info.sccIndex <- index lor (info.sccIndex land stackBit)

  let getIndex info =
    info.sccIndex land notStackBit

  let pushStack stack (id, nodeInfo) =
    nodeInfo.sccIndex <- nodeInfo.sccIndex lor stackBit;
    Stack.push (id, nodeInfo) stack;
    incr stackHeight (* hack *)
    
  let popStack stack =
    let id, info = Stack.pop stack in
    decr stackHeight;
    info.sccIndex <- info.sccIndex land notStackBit;
    (id, info)
      
  let setRootIndex info index =
    info.sccRoot <- index

  let getRootIndex info =
    info.sccRoot

  let notVisited info =
    info.sccIndex < 0

  let isSccRoot info =
    (getRootIndex info) == (getIndex info)

  let nonTrivial scc =
    match scc with
      [] -> failwith "empty scc?"
    | [_] -> false
    | _ -> true

  let earlyTerminate () =
    !stackHeight >= N.maxStack


  (** Return list of non-trivial sccs found from exploration
      starting at the startID*)
  let detectCycles shouldClear startID : (N.id) list list =
    if shouldClear then clear ();
    let startNode = getInfo startID in
    let curSccs = ref [] in
    
    let rec popScc (rootID, rootNode) nodesInSCC =
      let (otherID, otherNode) = popStack sccStack in
      let newScc = otherID :: nodesInSCC in
      if otherID == rootID then newScc
      else popScc (rootID, rootNode) newScc
    in

    let rec visit (id, node) =
      setIndex node !curIndex;
      setRootIndex node !curIndex;
      incr curIndex; (* not checking for overflow *)
      pushStack sccStack (id, node);
      if earlyTerminate () then ()
      else begin
        N.iterNeighs
          (fun neighID ->
             let neigh = getInfo neighID in
             if notVisited neigh then begin
               visit (neighID, neigh);
               setRootIndex node (min (getRootIndex node) (getRootIndex neigh))
             end else if isOnStack neigh then 
               setRootIndex node (min (getRootIndex node) (getIndex neigh))
          ) id;
      end;
      (* In loop or out of the if-else? *)
      if isSccRoot node then begin
        let newScc = popScc (id, node) [] in 
        curSccs := newScc :: !curSccs
      end
    in
    visit (startID, startNode);
    assert (Stack.is_empty sccStack);
    (* prune trivial sccs *)
    let result = (List.fold_left (fun cur scc -> 
                                    if nonTrivial scc then scc :: cur
                                    else cur) []) !curSccs in
    result
      
end
