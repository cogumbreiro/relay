(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following 
  conditions are met:
  
  1. Redistributions of source code must retain the above copyright 
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above 
  copyright notice, this list of conditions and the following disclaimer 
  in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the University of California, San Diego, nor 
  the names of its contributors may be used to endorse or promote 
  products derived from this software without specific prior 
  written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
*)
(** Link the pta information from each file and do a very simple
    analysis (field-insensitive with equality constraints) *)

(***
    TODO:
    - Too many aliases from field insensitivity for functions ?
       - Do a field-based thing on the side and intersect the two results?
    - Lots of constants
*)

open Cil
open Pta_types
open Fstructs
open Pretty
open Cilinfos
open Pta_shared
open Cildump

module IH = Inthash
module HC = Simplehc
module CLv = Cil_lvals
module PC = Pta_compile

(******************** UTILs **********************)

(** Choice of "set" *)
module LVS = Iset.Make (
  struct
    type t = ptaLv
    let compare a b = compareLv a b
  end
)


(**************** Debugging *****************)

(* What is considered a step in the algorithm *)
type algoStep = ptaAssign 
    (* TODO: know if it originates from function call / field assignment *)

let stepsProcessed = ref 0

let curStep = ref (makeAssign dummyLv dummyRv Cil.locUnknown)
  
module TrackedSteps = struct

  type step = algoStep
  let curStep = curStep

  let dotNode n = 
    ""
      
  let dotEdge (n1, n2) =
    ""
      
  (* TODO: allow different kinds of steps to have a dumper *)
  let dotSteps steps =
    (* Hmm... how to actually connect the lvs and rvs as a graph?
       need to interpret deref-stars and stuff?
       Just print for now... *)
    List.fold_left
      (fun curDoc (assign : step) -> 
         curDoc ++ text (string_of_assign assign) ++ line
      ) nil steps 

end

type mergeHistory =
    HNil
  | HMerge of mergeHistory * mergeHistory * algoStep * int
    (* old history, current step processed, and resulting label size *)

let mergeHistories h1 h2 step finalSize =
  HMerge (h1, h2, step, finalSize)

let rec d_history h =
  match h with
    HNil -> nil
  | HMerge (h1, h2, assign, size) ->
      let next = Pretty.dprintf "%s (%d)\n" 
        (string_of_assign assign) size in
      let h1d = d_history h1 in
      let h2d = d_history h2 in
      if h1d = nil then
        if h2d = nil then next
        else next ++ indent 2 (text "nil\n" ++ h2d)
      else
        if h2d = nil then next ++ indent 2 (h1d ++ text "nil\n")
        else next ++ indent 2 (h1d ++ h2d)

let hardCodedHistoryFile = "tempsteens.history"

(***************** Ops ************************************)

(* TODO: have finer levels of type filtering *)
let filterTypes = ref true

let setFilter what =
  filterTypes := what

(** Return true if the given type matches the varinfo type of 
    the given vid *)
let funTypesEq (typ:ctyp) (id:vid) =
  if !filterTypes then
    try
      let finfo = getFunInfo id in
(*      compatibleTypes typ finfo.funType
*)
      compatibleFunSig typ finfo.funType
        (*
          compatibleTypesNoUnroll typ finfo.funType
        *)
    with Not_found -> false
  else true


let matchFunPtr typ lv =
  let host, _ = lv in (* no field-based info for now *)
  match host.HC.node with
    PVar v ->
      (match v.HC.node with
         PGlobal (id, _) ->
           if (funTypesEq typ id) then Some id
           else None
       | _ -> None  (* Mismatch for function *)
      )
  | _ -> None (* Mismatch for function *)


(** true if the set of lvals has an lval based on a global *)
let hasGlobal lvs =
  LVS.exists isGlobalLv lvs


module IDS = IntSet



(*******************************************************)

module MakeSolver (U:Uf.UNIONFIND) = struct

  (* Too bad we can't extend the ptNodeInfo type. Now we need the
   * debugging info in the type all the time =( *)

  type ptNodeInfo = {
    mutable ptTarget : ptaVal;      (* node this guy points to *)
    mutable labels : LVS.t;         (* lvals this node represents *)
    mutable globalPointsTo : bool;  (* true if a global ptr points to this *)
    mutable history : mergeHistory; (* debugging *)
  }
      
  and ptaVal = 
      PTRef of ptNode         (* plus offset, if we go field-sens ? *)
    | PTBot
    | PTSelf (* self-loop *)
        
  and ptNode = ptNodeInfo U.uref


  (***************** The state ******************************)

  let lvToNode = LvalH.create 101

  let changed = ref false
    
  let rec onPath nodeInfo path =
    match path with
      [] -> 
        false
    | hd :: tl ->
        let hdInfo = U.deref hd in
        if hdInfo == nodeInfo then
          true
        else
          onPath nodeInfo tl

  let mergeNonRecursive n1 n2 =
    let ni1 = U.deref n1 in
    let ni2 = U.deref n2 in
    let chosenI = U.union (n1,n2) in
    chosenI.labels <- LVS.union ni1.labels ni2.labels;
    chosenI.globalPointsTo <- ni1.globalPointsTo or ni2.globalPointsTo;
    (* indicate that this is due to a cycle collapse? *)
    chosenI.history <- mergeHistories ni1.history ni2.history
      !curStep (LVS.cardinal chosenI.labels)

  let rec mergeRest startN curN =
    let ni = U.deref curN in
    match ni.ptTarget with
      PTBot | PTSelf -> ()
    | PTRef newTarg ->
        let startNI = U.deref startN in
        let tinfo = U.deref newTarg in
        if (tinfo == startNI) then ()
        else
          (mergeNonRecursive curN newTarg;
           mergeRest startN newTarg)
            
          
  let rec mergePath termNode path =
    let termNodeInfo = U.deref termNode in
    let finish () =
      mergeRest termNode termNode;  
      (* really shouldn't be any more, since it used the 
         last edge to make a self loop? *)
      termNodeInfo.ptTarget <- PTSelf;
    in
    match path with
      [] ->
        prerr_string "mergePath: empty path?\n";
        finish ();
        path
    | hd :: tl ->
        let hdInfo = U.deref hd in
        if hdInfo == termNodeInfo then begin
          finish ();
          path (* leave termNode on the path *)
        end
        else begin
          (* merge info from head into termNodeInfo *)
          mergeNonRecursive termNode hd;
          (* indicate self loop at the end *)
          mergePath termNode tl
        end
          

  (** Combine / unify nodes *)      
  let rec combineNodeInfo (curPath1, curPath2) (n1,n2) =
    let ni1 = U.deref n1 in
    let ni2 = U.deref n2 in
    let h1 = ni1.history in
    let h2 = ni2.history in
    let newTarg = 
      match ni1.ptTarget, ni2.ptTarget with
        PTBot, _ -> 
          ni2.ptTarget
      | _, PTBot -> 
          ni1.ptTarget
      | PTSelf, PTSelf ->
          PTSelf
      | PTRef n, PTSelf
      | PTSelf, PTRef n ->
          mergeRest n n;
          PTSelf
      | PTRef n1, PTRef n2 ->
          let t1 = U.deref n1 in
          let t2 = U.deref n2 in
          (* Watch out for cycles that were around before unifying more *)
          let newPath1 = 
            if (onPath t1 curPath1) then
              (* manually unify everything on the path from t1 onward *)
              mergePath n1 curPath1
            else
              n1 :: curPath1
          in
          let newPath2 = 
            if (onPath t2 curPath2) then
              (* manually unify everything on the path from t2 onward *)
              mergePath n2 curPath2
            else
              n2 :: curPath2
          in
          (* unify targets *)
          unifyNodes (newPath1, newPath2) n1 n2;
          let newNI = U.deref n1 in (* either n1/n2 works; they're unified *)
          newNI.ptTarget 
    in
    (* refresh info? *)
    let ni1 = U.deref n1 in 
    let ni2 = U.deref n2 in
    (* finish unification *)
    let newLabels = LVS.union ni1.labels ni2.labels in
    let newGPF = ni1.globalPointsTo or ni2.globalPointsTo in
    let newHistory = mergeHistories h1 h2 (* orig histories *) 
      !curStep (LVS.cardinal newLabels) in
    changed := true;
    { ptTarget = newTarg;  
      labels = newLabels;
      globalPointsTo = newGPF;
      history = newHistory;
    }

  and unifyNodes (curPath1,curPath2) n1 n2 =
    (* Assume unify checks if they are the same before trying to unify *)
    let result = U.unify (combineNodeInfo (curPath1,curPath2)) (n1,n2) in
    result

  (** Make a fresh node that represents a unique null *)
  let freshNull () : ptNode =
    U.uref { ptTarget = PTBot;
             labels = LVS.empty;
             globalPointsTo = false;
             history = HNil;
           }

  (** Get the new node for the lval x *)
  let rec getNode (lv:ptaLv) : ptNode =
    let host, _ = lv in
    match host.HC.node with
      PVar _ -> begin
        let lvNoOff = makeLv (host, noOffset) in
        try
          LvalH.find lvToNode lvNoOff
        with Not_found ->
          let newNode = freshNode lvNoOff in
          newNode
      end

    | PDeref (rv) -> begin
        (* current node should be the target of the pointer lv *)
        let ptrNode = getNodeRv rv in
        let pInfo = U.deref ptrNode in
        match pInfo.ptTarget with
          PTRef n -> n
        | PTSelf -> ptrNode
        | PTBot ->
            (* get the node for target, and make ptr point to it *)
            let targNoOff = makeLv (host, noOffset) in
            let targNode = 
              try LvalH.find lvToNode targNoOff 
              with Not_found -> freshNode targNoOff in
            pInfo.ptTarget <- PTRef targNode;
            let targInfo = U.deref targNode in
            targInfo.globalPointsTo <- 
              (targInfo.globalPointsTo) or (hasGlobal pInfo.labels);
            targNode
      end

  and getNodeRv (rv:ptaRv) : ptNode =
    match rv.HC.node with
      PLv lv -> getNode lv
    | PCast (_, e) -> getNodeRv e
    | PAddrOf (lv) ->
        let host, _ = lv in
        match host.HC.node with
          PVar _ -> (* make a pointer node that just points to this guy *)
            let target = getNode lv in
            let targetInfo = U.deref target in
            targetInfo.labels <- LVS.add lv targetInfo.labels;
            let newNode = freshNull () in
            let newNodeInfo = U.deref newNode in
            newNodeInfo.ptTarget <- PTRef target;
            newNode
        | PDeref ptr -> (* assume no offset for now *)
            getNodeRv ptr


  (** make a new node for the lval x as needed *)
  and freshNode (x:ptaLv) : ptNode =
    let newNode = 
      U.uref { ptTarget = PTBot;
               labels = LVS.empty;     (* label set by AddrOf assignments *)
               globalPointsTo = false;
               history = HNil;
             } in
    if (LvalH.mem lvToNode x) 
    then failwith "called freshNode on an lval w/ a node"
    else LvalH.replace lvToNode x newNode;
    newNode

  (***************** Assignment constraints ****************)

  let recordStep step =
    incr stepsProcessed;
    curStep := step

  (** handle constraints due to an assignment *)
  let rec doAssign ({lhs = lhs; rhs = rhs;} as assign) =
    recordStep assign;

    (* Make lhs point to rTarg (also) *)
    let assignToLeft rTarg =
      let lhsN = getNode lhs in
      let lhsNi = U.deref lhsN in
      match lhsNi.ptTarget with
        PTBot ->
          let newTarg = PTRef rTarg in
          let newHistory = mergeHistories lhsNi.history HNil !curStep
            (LVS.cardinal lhsNi.labels) in
          let newLeft = { lhsNi with
                            ptTarget = newTarg;
                            history = newHistory;
                        } in
          changed := true;
          U.update (lhsN, newLeft)
      | PTRef lTarg ->
          unifyNodes ([], []) lTarg rTarg
      | PTSelf ->
          unifyNodes ([], []) lhsN rTarg
    in
    match rhs.HC.node with
      PAddrOf (lv) -> begin
        let host, _ = lv in
        match host.HC.node with
          PVar _ ->
            (* get node of target lv and make a pointer to it *)
            let rTarg = getNode lv in
            let rInfo = U.deref rTarg in
            let newGP = rInfo.globalPointsTo or (isGlobalLv lhs) in
            let newLabels, newHist = 
              if not (LVS.mem lv rInfo.labels) then begin
                changed := true;
                let newLabels = LVS.add lv rInfo.labels in
                (newLabels,
                 mergeHistories rInfo.history HNil !curStep 
                   (LVS.cardinal newLabels))
              end else
                (rInfo.labels, rInfo.history)
            in
            let newInfo = 
              { rInfo with
                  labels = newLabels;
                  globalPointsTo = newGP;
                  history = newHist;
              } in
            U.update (rTarg, newInfo);
            (* then join with old lhs contents *)
            assignToLeft rTarg
            
        | PDeref r -> (* copy pointer... need to be more careful w/ offsets *)
            doAssign { assign with rhs = r; } 
      end

    | PLv (lv) -> begin
        (* copy contents of lv into lhs *)
        let rhsN = getNode lv in
        let rhsNi = U.deref rhsN in
        match rhsNi.ptTarget with
          PTBot ->
            let nullTarget = freshNull () in
            let newHistory = mergeHistories rhsNi.history HNil !curStep
              (LVS.cardinal rhsNi.labels) in
            let newRight = { rhsNi with
                               ptTarget = PTRef (nullTarget);
                               history = newHistory;
                           } in
            U.update (rhsN, newRight);
            assignToLeft nullTarget
        | PTRef rTarg ->
            assignToLeft rTarg
        | PTSelf ->
            assignToLeft rhsN
      end

    | PCast (_, r) -> 
        doAssign { assign with rhs = r; } 

  (***************** Function Call constraints ****************)

  let lvToIdFilter filter lvs =
    LVS.fold (fun lv cur -> 
                match filter lv with
                  Some id -> IDS.add id cur
                | None -> cur) lvs IDS.empty
      
  (** Generic way of derefencing a pointer, and getting target variable ids *)
  let derefPtrVarId ptrRv filter : IDS.t =
    let ptrNode = getNodeRv ptrRv in
    let ptrInfo = U.deref ptrNode in
    match ptrInfo.ptTarget with
      PTBot ->
        IDS.empty
    | PTRef targ ->
        let targInfo = U.deref targ in
        lvToIdFilter filter targInfo.labels
    | PTSelf ->
        lvToIdFilter filter ptrInfo.labels

  (** Return a list of target fun ids for the given ptr *)
  let resolveFPs typ ptrRv : IDS.t =
    derefPtrVarId ptrRv (matchFunPtr typ)
      

  (** Return a list of fun ids *)
  let resolveFuns typ callLv : vid list =
    let host, _ = callLv in
    match host.HC.node with
      PDeref ptrExp ->
        (* Indirect call *)
        IDS.elements (resolveFPs typ ptrExp)
          
    | PVar v -> 
        (match v.HC.node with
           PGlobal (id, _) -> (* Direct call *) [id]
         | _ -> (* ignore *)
             prerr_string "resolveFunHelp given non-global var?\n";
             []
        )

  let getFormal fid index = 
    let finfo = getFunInfo fid in
    List.nth finfo.funFormals index

  let host_of_formal fid paramIndex =
    let f = getFormal fid paramIndex in
    makeHost (PVar f)

  let host_of_ret fid =
    makeHost (PVar (makeVar (PRet fid)))

  let oldWarns = Hashtbl.create 101
  let warnOnce msg =
    if Hashtbl.mem oldWarns msg then ()
    else begin
      Hashtbl.add oldWarns msg ();
      Logging.logError msg;
    end

  (** Handle parameter passing for function with id [fid] *)
  let handleCallArg loc fid (actualExp, paramIndex) =
    if (paramIndex <> retIndex ) then
      try 
        let formal = makeLv (host_of_formal fid paramIndex, noOffset) in
        List.iter (fun actual -> 
                     let assign = makeAssign formal actual loc in
                     doAssign assign) actualExp
      with 
        Not_found -> 
          warnOnce ("PTA: doCall can't find func info? " ^ (string_of_int fid))
      | Failure _ ->
          warnOnce ("PTA: doCall can't handle varargs? " ^ (string_of_int fid))
    else
      let abstractRet = 
        makeRv (PLv (makeLv (host_of_ret fid, noOffset))) in
      List.iter
        (fun retRv -> match retRv.HC.node with
           PLv lv ->
             let assign = makeAssign lv abstractRet loc in
             doAssign assign
         | _ -> ()
        ) actualExp

  let doCallFun args loc fid =
    List.iter (handleCallArg loc fid) args

  let doCall (cinfo, args) =
    List.iter 
      (fun callLv ->
         let funs = resolveFuns cinfo.ctype callLv in
         List.iter (doCallFun args cinfo.cloc) funs
      ) cinfo.cexp

  let init root =
    LvalH.clear lvToNode

  let printStart base complex calls pseudo =
    Printf.printf "base assigns: %d\n" (VarH.length base);
    Printf.printf "complex assigns: %d\n" (VarH.length complex);
    Printf.printf "calls assigns: %d\n" (VarH.length calls)

  let printStatus () =
    Printf.printf "Iterating -- steps done: %d\n" !stepsProcessed;
    flush stdout

  let printDone () =
    Printf.printf "Finished -- steps done: %d\n" !stepsProcessed;
    flush stdout

  module NH = Hashtbl.Make (
    struct 
      type t = ptNodeInfo
      let equal = (==)
      let hash = Hashtbl.hash
    end )

  let printHistories () =
    print_string "Merge histories:\n====================\n\n";
    let visited = NH.create 0 in
    LvalH.iter 
      (fun lv node -> 
        let info = U.deref node in
        if NH.mem visited info then
          Printf.printf "%s rep. visited\n\n" (string_of_ptaLv lv)
        else begin
          NH.add visited info ();
          Printf.printf "%s:\n%s\n" 
            (string_of_ptaLv lv) (sprint 80 (d_history info.history));
       end
      ) lvToNode 

  let saveHistories fname =
    Printf.printf "saving merge histories to %s\n" fname;
    flush stdout;
    let mapping = LvalH.create (LvalH.length lvToNode) in
    LvalH.iter
      (fun lv node ->
        let info = U.deref node in
        LvalH.add mapping lv info.history
      ) lvToNode;
    let out_chan = open_out_bin fname in
    Marshal.to_channel out_chan mapping [Marshal.Closures];
    close_out out_chan

  let loadHistoriesBase fname =
    if not (Sys.file_exists fname) then 
      failwith "Merge histories not found.";
    let in_chan = open_in_bin fname in
    let mapping = Marshal.from_channel in_chan in
    close_in in_chan;
    mapping

  let loadHistories root =
    let filename = Filename.concat root hardCodedHistoryFile in
    loadHistoriesBase filename

  let solverStateFile root = 
    Filename.concat root "steens.solver.dat"

  let saveState root =
    (* Hmmm... can't save the Uf stuff because it uses refs ? *)
    let fname = solverStateFile root in
    let oc = open_out_bin fname in
    Marshal.to_channel oc lvToNode [Marshal.Closures];
    close_out oc

  let loadState root =
    let fname = solverStateFile root in
    if not (Sys.file_exists fname) then raise Not_found;
    let ic = open_in_bin fname in
    let loadedLvToNode = Marshal.from_channel ic in
    close_in ic;
    LvalH.clear lvToNode;
    LvalH.iter (fun k v -> LvalH.add lvToNode k v) loadedLvToNode


  let solve root =
    loadSharedState root; (* Just in case *)

    changed := true;
    (* Assume one-file mode... TODO: check *)
    let filename = PC.getConstraintFile root in
    let _, baseAss, complexAss, calls, pseudo = PC.loadFor filename in
    let baseAss = PC.rehashAssigns baseAss in
    let complexAss = PC.rehashAssigns complexAss in
    let calls = PC.rehashCalls calls in

    printStart baseAss complexAss calls pseudo;

    while !changed do
      printStatus ();
      changed := false;
      VarH.iter 
        (fun vinfo assList ->
           List.iter doAssign assList
        ) baseAss;
      
      VarH.iter
        (fun vinfo assList ->
           List.iter doAssign assList
        ) complexAss;
      
      VarH.iter
        (fun vinfo callList ->
           List.iter doCall callList
        ) calls;

(*      if VarH.length pseudo <> 0 then
        failwith "Steens: not to handle temp vars"
      ;
*)
    done;
    printDone ();
    saveHistories (Filename.concat root hardCodedHistoryFile)


end


(******* Finalize and save solutions **********)

let aaName = "steens"

let getResultsFilename (root : string) =
  Filename.concat root (".pta_results." ^ aaName)


module U = Uf.NotTracked
module Solver = MakeSolver (U)


(* TODO: parameterize finalizer w/ solver... kind of funky to do
   so right now because we are going to redefine types in Final,
   based on the types in Solver. Also, need to make sure
   that Final uses the same Uf module *)
module Final =
struct

  type nodeRef = int

  type nodeInfo = {
    finalTarget : finalVal;      (* node this guy points to *)
    finalLabels : LVS.t;         (* lvals this node represents *)
    mutable finalGReach : bool;     (* true if reachable from a global ptr *) 
  }

  and finalVal = 
      FRef of nodeRef
    | FBot
    | FSelf

  (* Hashing old nodeinfos *)

  let eqNode a b =
    (a.Solver.globalPointsTo == b.Solver.globalPointsTo) &&
      (match a.Solver.ptTarget, b.Solver.ptTarget with
         Solver.PTRef n1, Solver.PTRef n2 ->
           let ni1 = U.deref n1 in
           let ni2 = U.deref n2 in
           ni1 == ni2 (* UF should have made them the same? *)
       | Solver.PTSelf, Solver.PTSelf -> true
       | Solver.PTBot, Solver.PTBot -> true
       | _ -> false
      ) &&
      LVS.equal a.Solver.labels b.Solver.labels

  let hashNode n =
    Hashtbl.hash_param 16 32 n

  module NH = Hashtbl.Make (
    struct
      type t = Solver.ptNodeInfo
      let equal = (==) (* eqNode *)
      let hash = hashNode
    end )
    

  let oldNodeToID = ref (NH.create 107)

  let clearOldNodeToID sizeHint =
    oldNodeToID := NH.create sizeHint

  (* New node infos + their IDs *)

  type state = {
    mutable idCounter : nodeRef;
    idToNode : nodeInfo IH.t;
    lvToID : nodeRef LvalH.t;
    addrOfToID : nodeRef LvalH.t;
  }

  let myState = ref {
    idCounter = 0;
    idToNode = IH.create 107;
    lvToID = LvalH.create 107;
    addrOfToID = LvalH.create 107;
  }

  let nextID () =
    let res = !myState.idCounter in
    !myState.idCounter <- res + 1;
    res
    
  let addIDNode id node =
    IH.replace !myState.idToNode id node

  let getIDNode id =
    IH.find !myState.idToNode id

  let addLvalID lval id =
    LvalH.replace !myState.lvToID lval id

  let getLvalID lval =
    LvalH.find !myState.lvToID lval

  let addAddrOfID lval id =
    LvalH.replace !myState.addrOfToID lval id

  let getAddrOfID lval =
    LvalH.find !myState.addrOfToID lval

  (******************************)

  let rec convertNode n = 
    let ni = U.deref n in
    try
      NH.find !oldNodeToID ni
    with Not_found ->
      let id = nextID () in
      NH.add !oldNodeToID ni id;
      let newNode = 
        { finalLabels = ni.Solver.labels;
          finalGReach = false; (* just recompute this from markGlobalReach *)
          finalTarget = (match ni.Solver.ptTarget with
                           Solver.PTBot -> FBot
                         | Solver.PTSelf -> FSelf
                         | Solver.PTRef n' -> FRef (convertNode n')); } in
      addIDNode id newNode;
      id

  let convertTable () =
    print_string "Converting Steens results to be serializable\n";
    flush stdout;
    LvalH.iter 
      (fun lv oldN ->
         let newNodeID = convertNode oldN in
         addLvalID lv newNodeID
      ) Solver.lvToNode;
    print_string "Done converting\n";
    flush stdout


  let markGlobalReach () =
    print_string "Marking global reachability\n";
    flush stdout;
    let visitStack = IH.create 17 in
    let rec visit g id = 
      (if (IH.mem visitStack id) then
         ()
       else
         (try
            IH.add visitStack id ();
            let node = getIDNode id in
            let newG = 
              if (g) then g else hasGlobal node.finalLabels in
            node.finalGReach <- node.finalGReach || newG;
            match node.finalTarget with
              FBot 
            | FSelf ->
                ()
            | FRef x -> visit newG x
          with Not_found ->
            prerr_string ("No node for PTA ID? " ^ (string_of_int id) ^ "\n")
         )
      );
      IH.remove visitStack id
    in
    LvalH.iter 
      (fun lv id ->
         IH.clear visitStack;
         visit (isGlobalLv lv) id
      ) !myState.lvToID;
    print_string "Done marking global reachability\n";
    flush stdout


  (** Finalize table, then save table to file *)
  let saveAll (root : string) =
    let oldBindings = LvalH.length Solver.lvToNode in
    clearOldNodeToID oldBindings;

    convertTable ();
    markGlobalReach ();

    clearOldNodeToID 0;
    LvalH.clear Solver.lvToNode;

    let outName = getResultsFilename root in
    Printf.printf "Saving all state to %s\n" outName;
    flush stdout;
    let out_chan = open_out_bin outName in
    Marshal.to_channel out_chan !myState [Marshal.Closures] ;
    close_out out_chan


  let loadAll (oldFile : string) =
    let in_chan = open_in_bin oldFile in
    myState := Marshal.from_channel in_chan;
    close_in in_chan


  (** Use the final table to get nodes associated w/ an lval.
      May raise Not_found *)
  let rec getNode lv =
    let host, _ = lv in
    match host.HC.node with
      PVar _ -> 
        (try
           let lvNoOff = makeLv (host, noOffset) in
           getLvalID lvNoOff
         with Not_found ->
           (* make one up for the dude anyway (w/ 0 targets, etc.) *)
           let freshNode = { finalLabels = LVS.empty;
                             finalGReach = false;
                             finalTarget = FBot; } in
           let newID = nextID () in
           addIDNode newID freshNode;
           newID )
        
    | PDeref (rv) ->
        (* current node should be the target of the pointer lv *)
        let ptrNode = getNodeRv rv in
        let ptrInfo = getIDNode ptrNode in
        (match ptrInfo.finalTarget with
           FRef n -> n
         | FSelf -> ptrNode
         | FBot -> raise Not_found )

  and getNodeRv rv =
    match rv.HC.node with
      PLv lv -> getNode lv
    | PCast (_, e) -> getNodeRv e
    | PAddrOf (lv) ->
        try getAddrOfID lv
        with Not_found -> begin
          let host, _ = lv in
          let nodeID = 
            (match host.HC.node with
               PVar _ -> 
                 (* make a pointer node that just points to this guy... 
                    overlap w/ old uses? Find id w/ same info? *)
                 let target = getNode lv in
                 let targetInfo = getIDNode target in
                 let targetInfo = 
                   { targetInfo with
                       finalLabels = LVS.add lv targetInfo.finalLabels;} in
                 addIDNode target targetInfo;
                 let newNodeInfo = {
                   finalLabels = LVS.empty;
                   finalGReach = false; 
                   finalTarget = FRef target; } in
                 let newID = nextID () in
                 addIDNode newID newNodeInfo;
                 newID
             | PDeref ptr -> (* assume no offset for now *)
                 getNodeRv ptr
            )
          in
          addAddrOfID lv nodeID;
          nodeID
        end


  let accumulateTargets filter labels acc =
    LVS.fold 
      (fun funLv res ->
         match filter funLv with
           None -> res
         | Some id -> IDS.add id res
      ) labels acc


  (** Return a list of target fun ids for the given ptr *)
  let resolveFPs typ ptrRv : IDS.t =
    let accumulateTargets = accumulateTargets (matchFunPtr typ) in
    let ptrNode = getNodeRv ptrRv in
    let ptrInfo = getIDNode ptrNode in
    match ptrInfo.finalTarget with
      FBot -> IDS.empty
    | FRef targ ->
        let targInfo = getIDNode targ in
        accumulateTargets targInfo.finalLabels IDS.empty
    | FSelf ->
        accumulateTargets ptrInfo.finalLabels IDS.empty


  (** Given a PTA node and current set of var IDs, add IDs of 
      pointed-to targets to the set *)
  let targetIDs node curResults =
    let accumulateTargets = 
      accumulateTargets (fun (host, _) ->
                           match host.HC.node with
                             PVar v ->
                               (match v.HC.node with
                                  PGlobal (id, _) | PLocal (id, _) -> Some id
                                | _ -> None)
                           | _ -> None) in
    let ni = getIDNode node in
    match ni.finalTarget with
      FBot -> curResults
    | FRef targ ->
        let targInfo = getIDNode targ in
        accumulateTargets targInfo.finalLabels curResults
    | FSelf -> accumulateTargets ni.finalLabels curResults


  let nodePointsTo node targ =
    let ni = getIDNode node in
    match ni.finalTarget with
      FBot -> false
    | FSelf -> targ == node (* nodes are equivalence class ids *)
    | FRef targ2 -> targ == targ2 (* nodes are equivalence class ids *)
          
          
  let may_alias n1 n2 =
    let ni1 = getIDNode n1 in
    match ni1.finalTarget with 
      FBot -> false
    | FSelf -> nodePointsTo n2 n1
    | FRef n -> nodePointsTo n2 n

  let location_alias n1 n2 =
    n1 = n2

  let compare n1 n2 =
    n1 - n2

  let hash = Hashtbl.hash

  let labels node =
    let ni = getIDNode node in
    ni.finalLabels

  let deref node =
    let ni = getIDNode node in
    match ni.finalTarget with
      FRef n -> n
    | FBot -> raise Not_found
    | FSelf -> node

  (* reachable from global (reflexive/transitive) *)
  let reachableFromG node =
    let ni = getIDNode node in
    ni.finalGReach

  let string_of node =
    ("[REP: " ^ (string_of_int node) ^ "]")

  let label_size node =
    let ni = getIDNode node in
    let s = LVS.cardinal ni.finalLabels in
(*    if (s == 0 && ni.finalTarget == FSelf) then 66667 (* TODO use tag *)
    else  *)
      s

  let pts_size node =
    let ni = getIDNode node in
    match ni.finalTarget with
      FRef n -> label_size n
    | FSelf -> label_size node
    | FBot -> 0

  let reachableFrom n srcs =
    let rec reachable n s visited =
      if (n == s) then true
      else if (List.mem s visited) then false
      else let srcInfo = getIDNode s in
        match srcInfo.finalTarget with
          FRef t -> reachable n t (s :: visited)
        | FBot -> false
        | FSelf -> n == s
    in
    List.exists 
      (fun srcN -> 
         reachable n srcN []
      ) srcs

end



(********** Iterate through all files, solve & save **********)

module FPCS = Set.Make(
 struct
    type t = ptaCall
    let compare = compareCall
  end
)
  

(** Load all the assignments and calls and process *)
let analyzeAll (root : string) (fresh:bool) =

  (* Load info for each function *)
  loadSharedState root;

  (* Decide to process assignments or use old results *)
  let oldFile = getResultsFilename root in
  if ((not fresh) && (Sys.file_exists oldFile)) then begin
    (* load existing results *)
    print_string "Using old PTA results\n";
    Final.loadAll oldFile;
    print_string "Old PTA results loaded\n";
  end
  else begin
    (* Solve and get fresh results *)
    print_string "Processing constraints\n";
    flush stdout;
    Solver.solve root;
    Final.saveAll root;
    print_string "PTA Simple analysis done\n";
    flush stdout
  end
    

(***** High-level Query Interface. Assumes everything is finalized  ******)


(** Return the variable IDs for functions that the func ptr may 
    point to. May raise Not_found. *)
let resolve_funptr (e:Cil.exp) : vid list =
  let e_typ = 
    try 
      CLv.typeAfterDeref e    (* if function pointer is given *)
    with CLv.TypeNotFound ->
      Logging.logError "Pta_fi_eq: resolve_funptr not given a pointer";
      CLv.typeOfUnsafe e in   (* if function pointer deref is given *)
  let e_typ = cil_type_to_ptype e_typ in
  let lv_exps = PC.analyze_exp e in
  let idSet = List.fold_left
    (fun results rv ->
       let fids = Final.resolveFPs e_typ rv in
       IDS.union results fids
    ) IDS.empty lv_exps in
  if IDS.is_empty idSet then
    prerr_string ("empty FP targets " ^ string_of_exp e ^ "\n")
  ;
  IDS.elements idSet

module NsCache = Cache.Make
  (struct
     type t = Final.nodeRef list
     let equal a b = a = b
     let hash a = Hashtbl.hash a
   end)


let derefCache = NsCache.create 512

let do_deref_nodes nodes =
  let vids = List.fold_left 
    (fun results node -> Final.targetIDs node results) 
    IDS.empty nodes in
  IDS.fold
    (fun id results  ->
       try (getVarinfo id) :: results
       with Not_found -> results
    ) vids []


(** Given a list of PTA ptr lvals, get the pointed-to targets *)
let deref_nodes nodes =
  try NsCache.find derefCache nodes 
  with Not_found ->
    let result = do_deref_nodes nodes in
    ignore (NsCache.add derefCache nodes result);
    result
    

(** Return the base lvals that this expression can point to. 
    TODO return varinfo * offset pair. May raise Not_found *) 
let deref_lval (lv:Cil.lval) = 
  let lvs = PC.analyze_lval lv in
  let nodes = List.fold_left 
    (fun results lv -> 
       try List_utils.addOnce results (Final.getNode lv)
       with Not_found -> results) [] lvs in
  deref_nodes nodes
    
    
(** Return the base lvals that this expression can point to.
    May raise Not_found *) 
let deref_exp (e:Cil.exp) =
  let rvs = PC.analyze_exp e in
  let nodes = List.fold_left 
    (fun results rv -> 
       try List_utils.addOnce results (Final.getNodeRv rv) 
       with Not_found -> results) [] rvs in
  deref_nodes nodes


(** True if the two ptr expressions may point to the same memory cell. *)
let may_alias (e1:Cil.exp) (e2:Cil.exp) : bool =
  let lv_exps1 = PC.analyze_exp e1 in
  let lv_exps2 = PC.analyze_exp e2 in
  List.exists 
    (fun ptrRv1 ->
       let node1 = Final.getNodeRv ptrRv1 in
       List.exists 
         (fun ptrRv2 ->
            let node2 = Final.getNodeRv ptrRv2 in
            Final.may_alias node1 node2
         ) lv_exps2
    ) lv_exps1

let node_points_to = Final.nodePointsTo
  
let nodes_point_to nodes v : bool =
  let targ = makeLv (PC.host_of_var v, noOffset) in
  let targNode = Final.getNode targ in
  List.exists (fun node -> node_points_to node targNode) nodes


let lval_points_to lv v : bool =
  let lvs = PC.analyze_lval lv in
  let nodes = List.fold_left 
    (fun cur lv -> try (Final.getNode lv) :: cur with Not_found -> cur) 
    [] lvs in
  nodes_point_to nodes v


(** True if the ptr expressions may point the memory cell of the var *)
let points_to (e:Cil.exp) (v:Cil.varinfo) : bool =
  let rvs = PC.analyze_exp e in
  let nodes = List.fold_left 
    (fun cur rv -> try Final.getNodeRv rv :: cur with Not_found -> cur)
    [] rvs in
  nodes_point_to nodes v


(** True if lvalue is reachable from a global pointer *)
let reachableFromG (lval:Cil.lval) =
  let lvs = PC.analyze_lval lval in
  List.exists (fun lv ->
                 let node = Final.getNode lv in
                 Final.reachableFromG node
              ) lvs


(***************** Abstract Node Interface **************)

module Abs = struct
  
  type t = Final.nodeRef

  (* Compare / hash funcs for abstract nodes *)
  let compare = Final.compare

  let hash = Final.hash

  let string_of n =
    Final.string_of n

  (* resolve targets in terms of abstract nodes *)
  let deref_nodes nodes =
    List.fold_left
      (fun results node ->
         let targNode = Final.deref node in
         List_utils.addOnceP (fun a b -> compare a b == 0) 
           results targNode
      ) [] nodes

  let deref_lval (lv:Cil.lval) = 
    let lvs = PC.analyze_lval lv in
    let nodes = List.fold_left 
      (fun results lv -> 
         try (Final.getNode lv) :: results with Not_found -> results) 
      [] lvs in
    deref_nodes nodes
      
    
  (** Return the base lvals that this expression can point to.
      May raise Not_found *) 
  let deref_exp (e:Cil.exp) =
    let rvs = PC.analyze_exp e in
    let nodes = List.fold_left 
      (fun results rv -> 
         try (Final.getNodeRv rv) :: results with Not_found -> results) 
      [] rvs in
    deref_nodes nodes


  let pickNode (nodes : Final.nodeRef list) =
    match nodes with
      [] -> None
    | [x] -> Some x
    | x :: _ -> Some x (* just pick first *)


  (* Get abstract node that represents this Cil lval *)
  let getNodeLval lv =
    let lvs = PC.analyze_lval lv in
    let nodes = List.fold_left 
      (fun cur lv ->
         try Final.getNode lv :: cur with Not_found -> cur) [] lvs in
    pickNode nodes

  let getNodeExp exp =
    let exps = PC.analyze_exp exp in
    let nodes = List.fold_left 
      (fun cur rv -> 
         try Final.getNodeRv rv :: cur with Not_found -> cur) [] exps in
    pickNode nodes


  (* Deref abstract node and get the target abstract node *)
  let deref (abs:t) : t =
    Final.deref abs

  let may_alias abs1 abs2 = 
    Final.may_alias abs1 abs2  

  let location_alias abs1 abs2 =
    Final.location_alias abs1 abs2

  let points_to abs1 abs2 =
    Final.nodePointsTo abs1 abs2

  (** Get Cil lvals associated w/ a node *)
  let lvals_of abs =
    let lvs = Final.labels abs in
    LVS.fold
      (fun lv results  ->
         try 
           lv_of_ptaLv lv :: results
         with Not_found ->
           results
      ) lvs []


  (** Get the number of lvals represented by the node *)
  let label_size abs =
    Final.label_size abs

  let pts_size abs =
    Final.pts_size abs
    
  (** Return true if [n] is reachable from the [srcs] *)
  let reachableFrom (abs:t) (srcs:t list) =
    Final.reachableFrom abs srcs
 
  (** Return true if [n] is reachable from the globals *)
  let reachableFromG (abs:t) =
    Final.reachableFromG abs
     
end



(**************************************************************** 
                          Debug 
*****************************************************************)

let shouldInspect lv inspect =
  let lvS = string_of_ptaLv lv in
  List.exists (fun regexp -> Str.string_match regexp lvS 0) inspect

let pHistory lv hist =
  Printf.printf "%s:\n%s\n" 
    (string_of_ptaLv lv) (sprint 80 (d_history hist))
  (* TODO: make this nicer *) 

let printHistory root inspect =
  Printf.printf "Printing merge histories\n";
  let inspect = List.map (fun str -> Str.regexp str) inspect in
  let histories = Solver.loadHistories root in
  if (inspect <> []) then
    LvalH.iter 
      (fun lv hist ->
        if shouldInspect lv inspect then
          pHistory lv hist
      ) histories


let printLvs lvals =
  LVS.iter 
    (fun label ->
       printPtaLv label;
       print_string ", ";
    ) lvals

let string_of_id id =
  "(id = " ^ (string_of_int id) ^ ")"

let rec printPtrChain curNode visited =
  try
    let targ = Final.deref curNode in
    if (Hashtbl.mem visited targ) then
      print_string ("\tself-loop to " ^ (string_of_id targ) ^ "\n")
    else
      (Hashtbl.add visited targ true;
       print_string ("\t" ^ (string_of_id targ) ^ " ->\n");
       printPtrChain targ visited)
  with Not_found ->
    print_string "\tnull\n"

module Dist = Distributions

let printPtsToSets () =
  let visited = Hashtbl.create 10 in
  let printed = Hashtbl.create 10 in
  let size_dist = Dist.makeDistro () in
  LvalH.iter 
    (fun k node ->
       print_string "Node w/ lval key: ";
       printPtaLv k;
       print_string "\t";

       print_string ((string_of_id node) ^ "\n");
       print_string "Represents: {";
       let labels = Final.labels node in
       let size = LVS.cardinal labels in
       if (Hashtbl.mem printed node) then
         print_string " see old printout "
       else
         (printLvs labels;
          Hashtbl.add printed node true;
          Dist.updateDistro size_dist size;
         ) 
       ;
       print_string ("} (" 
                     ^ (string_of_int size) ^
                     ")\n");

       print_string "Points to:\n";
       Hashtbl.clear visited;
       Hashtbl.add visited node true;
       printPtrChain node visited;
       
       print_string ("Reachable from a global: " ^ 
                       (string_of_bool (Final.reachableFromG node)) ^ "\n\n");
    ) (!Final.myState).Final.lvToID;
  Dist.printDistroSortKey size_dist string_of_int "rep. node sizes"


(** FP Test *)

class fpTest : Pta_fp_test.fpTestable = object (self) 
    
  method init (f:Cil.file) = ()
    
  method resolve_fp = resolve_funptr

  method name = "Steens"

end
    
