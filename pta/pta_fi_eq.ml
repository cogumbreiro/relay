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
open Pta_compile
open Intrange
open Fstructs
open Simplehc
open Cilinfos
open Strutil

module D = Cildump
module U = Uf
module CilPTA = Myptranal
module FC = Filecache
module L = Logging
module Lv = Cil_lvals



(******************** UTILs **********************)


(** Table of fun id -> funInfo *)
let funTable = Hashtbl.create 101

let getFunInfo (id:vid) : funInfo =
  Hashtbl.find funTable id

let isFunc id =
  Hashtbl.mem funTable id

(***************** Ops ************************************)


(** Return true if the given type matches the varinfo type of 
    the given vid *)
let funTypesEq (typ:string) (id:vid) =
  try
    let finfo = getFunInfo id in
     typ = finfo.funType
    (* Ciltools.compare_type typ finfo.funType.node == 0 *)
  with Not_found ->
    false


(** true if the lval is based on a global but not a function... *)
let rec isGlobalLv (lv : ptaLv) : bool =
  let host, _ = lv.node in
  match host.node with
    PVar v ->
      (match v.node with
         PGlobal (id, _) -> not (isFunc id)
       | _ -> false
      )
  | PDeref ptrRv ->
      isGlobalRv ptrRv

and isGlobalRv (rv : ptaRv) : bool = 
  match rv.node with
    PLv lv -> 
      isGlobalLv lv
  | PAddrOf lv ->
      isGlobalLv lv
  | PCast (_, rv) ->
      isGlobalRv rv

(** true if the set of lvals has an lval based on a global *)
let hasGlobal lvs =
  LVS.exists isGlobalLv lvs

let rec getLval rv =
  match rv.node with
    PLv l -> l
  | PCast (_, r) -> getLval r
  | PAddrOf _ -> raise Not_found


module IDS = IntSet



(*******************************************************)
  
module Solver = struct

  type ptNodeInfo = {
    mutable ptTarget : ptaVal;      (* node this guy points to *)
    mutable labels : LVS.t;         (* lvals this node represents *)
    mutable globalPointsTo : bool;  (* true if a global ptr points to this *) 
  }
      
  and ptaVal = 
      PTRef of ptNode         (* plus offset, if we go field-sens ? *)
    | PTBot
    | PTSelf (* self-loop *)
        
  and ptNode = ptNodeInfo U.uref


  (***************** The state ******************************)

  let lvToNode = ref (LvalH.create 101)

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
    chosenI.globalPointsTo <- ni1.globalPointsTo or ni2.globalPointsTo
      

  let rec mergeRest startN curN =
    let ni = U.deref curN in
    match ni.ptTarget with
      PTBot -> 
        ()
    | PTSelf -> 
        ()
    | PTRef newTarg ->
        let startNI = U.deref startN in
        let tinfo = U.deref newTarg in
        if (tinfo == startNI) then
          ()
        else
          (mergeNonRecursive curN newTarg;
           mergeRest startN newTarg
          )
            
          
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
    changed := true;
    { ptTarget = newTarg;  
      labels = newLabels;
      globalPointsTo = newGPF;
    }

  and unifyNodes (curPath1,curPath2) n1 n2 =
    (* Assume unify checks if they are the same before trying to unify *)
    U.unify (combineNodeInfo (curPath1,curPath2)) (n1,n2)


  (** Make a fresh node that represents a unique null *)
  let freshNull () : ptNode =
    U.uref { ptTarget = PTBot;
             labels = LVS.empty;
             globalPointsTo = false;
           }

  (** Get the new node for the lval x *)
  let rec getNode (lv:ptaLv) : ptNode =
    let host, _ = lv.node in
    match host.node with
      PVar _ -> begin
        let lvNoOff = (lv_of_hostOff host NoOffset) in
        try
          LvalH.find !lvToNode lvNoOff
        with Not_found ->
          let newNode = freshNode lvNoOff in
          newNode
      end

    | PDeref (rv) -> begin
        (* current node should be the target of the pointer lv *)
        try
          let phost, _ = (getLval rv).node in (* TODO: use offset? *)
          let ptrNoOff = (lv_of_hostOff phost NoOffset) in
          let ptrNode = getNode ptrNoOff in
          let pInfo = U.deref ptrNode in
          match pInfo.ptTarget with
            PTRef n ->
              n
          | PTSelf ->
              ptrNode
          | PTBot ->
              (* make up a node for target, and make ptr point to it *)
              let targNoOff = (lv_of_hostOff host NoOffset) in
              let node = freshNode targNoOff in
              pInfo.ptTarget <- PTRef node;
              let nInfo = U.deref node in
              nInfo.globalPointsTo <- 
                (nInfo.globalPointsTo) or 
                (hasGlobal pInfo.labels);
              node
        with Not_found ->
          failwith "getNode given an AddrOf thing\n"
      end

  (** make a new node for the lval x as needed *)
  and freshNode (x:ptaLv) : ptNode =
    let newNode = 
      U.uref { ptTarget = PTBot;
               labels = LVS.empty;     (* label set by AddrOf assignments *)
               globalPointsTo = false;
             } in
    LvalH.add !lvToNode x newNode;
    newNode

  (***************** Assignment constraints ****************)

  (** handle constraints due to an assignment *)
  let rec doAssign (lhs : ptaLv) (rhs : ptaRv) =
    (* Make lhs point to rTarg (also) *)
    let assignToLeft rTarg =
      let lhsN = getNode lhs in
      let lhsNi = U.deref lhsN in
      match lhsNi.ptTarget with
        PTBot ->
          let newLeft = { lhsNi with
                            ptTarget = PTRef rTarg;
                        } in
          changed := true;
          U.update (lhsN, newLeft)
      | PTRef lTarg ->
          unifyNodes ([], []) lTarg rTarg
      | PTSelf ->
          unifyNodes ([], []) lhsN rTarg
    in
    match rhs.node with
      PAddrOf (lv) -> begin
        let host, _ = lv.node in
        match host.node with
          PVar _ ->
            (* get node of target lv and make a pointer to it *)
            let rTarg = getNode lv in
            let rInfo = U.deref rTarg in
            let newInfo = 
              { rInfo with
                  labels = LVS.add lv rInfo.labels;
                  globalPointsTo = rInfo.globalPointsTo or (isGlobalLv lhs);
              } in
            U.update (rTarg, newInfo);
            (* then join with old lhs contents *)
            assignToLeft rTarg
            
        | PDeref r -> (* copy pointer... need to be more careful w/ offsets *)
            doAssign lhs r
      end

    | PLv (lv) -> begin
        (* copy contents of lv into lhs *)
        let rhsN = getNode lv in
        let rhsNi = U.deref rhsN in
        match rhsNi.ptTarget with
          PTBot ->
            let nullTarget = freshNull () in
            let newRight = { rhsNi with
                               ptTarget = PTRef (nullTarget);
                           } in
            U.update (rhsN, newRight);
            assignToLeft nullTarget
        | PTRef rTarg ->
            assignToLeft rTarg
        | PTSelf ->
            assignToLeft rhsN
      end

    | PCast (_, r) -> 
        doAssign lhs r

  (***************** Function Call constraints ****************)


  (** Generic way of derefencing a pointer *)
  let derefPtr ptrLv filter : LVS.t =
    let ptrNode = getNode ptrLv in
    let ptrInfo = U.deref ptrNode in
    match ptrInfo.ptTarget with
      PTBot ->
        LVS.empty
    | PTRef targ ->
        let targInfo = U.deref targ in
        LVS.filter filter targInfo.labels
    | PTSelf ->
        LVS.filter filter ptrInfo.labels


  (** Return a list of target fun ids for the given ptr *)
  let resolveFPs typ ptrLv : IDS.t =
    let lvs = derefPtr ptrLv 
      (fun funLv -> let host, _ = funLv.node in (* no field info for now *)
       match host.node with
         PVar v ->
           (match v.node with
              PGlobal (id, _) ->
                funTypesEq typ id
            | _ -> false  (* Mismatch for function *)
           )
       | _ -> false (* Mismatch for function *)
      ) in
    (* Map LV set to ID set... *)
    LVS.fold (fun lv res -> let host, _ = lv.node in
              match host.node with
                PVar v ->
                  (match v.node with
                     PGlobal (id, _) -> IDS.add id res
                   | _ -> res)
              | _ -> res) lvs IDS.empty


  (** Return a list of fun ids *)
  let resolveFuns typ callLv : vid list =
    let host, _ = callLv.node in
    match host.node with
      PDeref ptrExp ->
        (* Indirect call *)
        (try
           let lv = getLval ptrExp in 
           IDS.elements (resolveFPs typ lv)
         with Not_found ->
           prerr_string "pta: resolveFuns can't find lval of ptrExp\n";
           []
        )
    | PVar v -> 
        (match v.node with
           PGlobal (id, _) -> (* Direct call *)
             [id]
         | _ -> (* ignore *)
             prerr_string "resolveFunHelp given non-global var?\n";
             []
        )

  let getFormal fid index = 
    let finfo = getFunInfo fid in
    List.nth finfo.funFormals index
      
  let doCall (cinfo, index, actualExp) =
    List.iter 
      (fun callLv ->
         let funs = resolveFuns cinfo.ctype callLv in
         List.iter 
           (fun fid ->
              if (index <> retIndex ) then
                try 
                  let formal = lv_of_hostOff 
                    (makeHost (PVar (getFormal fid index))) NoOffset  in
                  List.iter 
                    (fun actualLv -> doAssign formal actualLv) actualExp
                with 
                  Not_found
                | Failure _ ->
                    L.logError ("PTA: doCall can't find func info? " ^
                                  (string_of_int fid))
                      (* or vararg indexing unsoundness? *)
              else
                let abstractRet = 
                  makeRv 
                    (PLv (lv_of_hostOff 
                            (makeHost (PVar (makeVar (PRet fid))))
                               NoOffset)) in
                List.iter
                  (fun retRv ->
                     match retRv.node with
                       PLv lv ->
                         doAssign lv abstractRet
                     | _ ->
                         ()
                  ) actualExp 
           ) funs
      ) cinfo.cexp

end


(******* Finalize and save solutions **********)

let getResultsFilename (root : string) =
  Filename.concat root ".pta_results"


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
    Hashtbl.hash n

  module NH = Hashtbl.Make (
    struct
      type t = Solver.ptNodeInfo
      let equal = eqNode
      let hash = hashNode
    end )


  let oldNodeToID = NH.create 107 

  (* New node infos + their IDs *)

  let idCounter = ref 0

  let idToNode = ref (IH.create 107)
    
  let lvToID = ref (LvalH.create 107)
    
  let rec convertNode n = 
    let ni = U.deref n in
    try
      NH.find oldNodeToID ni
    with Not_found ->
      let id = !idCounter in
      incr idCounter;
      NH.add oldNodeToID ni id;
      let newNode = 
        { finalLabels = ni.Solver.labels;
          finalGReach = false; (* just recompute this from markGlobalReach *)
          finalTarget = (match ni.Solver.ptTarget with
                           Solver.PTBot -> FBot
                         | Solver.PTSelf -> FSelf
                         | Solver.PTRef n' -> FRef (convertNode n')); } in
      IH.add !idToNode id newNode;
      id

  let markGlobalReach () =
    let visitStack = IH.create 17 in
    let rec visit g id = 
      (if (IH.mem visitStack id) then
         ()
       else
         (try
            IH.add visitStack id ();
            let node = IH.find !idToNode id in
            let newG = 
              if (g) then g
              else hasGlobal node.finalLabels in
            node.finalGReach <- newG;
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
      ) !lvToID

  let convertTable () =
    LvalH.iter 
      (fun lv oldN ->
         LvalH.replace !lvToID lv (convertNode oldN))
      !Solver.lvToNode

  (** Finalize table, then save table to file *)
  let saveAll (root : string) =
    NH.clear oldNodeToID;
    convertTable ();
    markGlobalReach ();
    NH.clear oldNodeToID;
    LvalH.clear !Solver.lvToNode;

    let outName = getResultsFilename root in
    let out_chan = open_out_bin outName in
    Marshal.to_channel out_chan (!lvToID, !idToNode) [Marshal.Closures] ;
    close_out out_chan


  let loadAll (oldFile : string) =
    let in_chan = open_in_bin oldFile in
    let lvID, idN = Marshal.from_channel in_chan in
    lvToID := lvID;
    idToNode := idN;
    close_in in_chan

  let derefID id =
    IH.find !idToNode id

  (** Use the final table to get nodes associated w/ an lval.
      May raise Not_found *)
  let rec getNode lv =
    let host, _ = lv.node in
    match host.node with
      PVar _ -> 
        let lvNoOff = (lv_of_hostOff host NoOffset) in
        LvalH.find !lvToID lvNoOff
        
    | PDeref (rv) ->
        (* current node should be the target of the pointer lv *)
        let host, _ = (getLval rv).node in
        let lvNoOff = (lv_of_hostOff host NoOffset) in
        let ptrNode = getNode lvNoOff in
        let ptrInfo = derefID ptrNode in
        (match ptrInfo.finalTarget with
           FRef n ->
             n
         | FSelf ->
             ptrNode
         | FBot ->
             raise Not_found )


  (** Return a list of target fun ids for the given ptr *)
  let resolveFPs typ ptrLv : IDS.t =
    let accumulateTargets labels =
      LVS.fold 
        (fun funLv res ->
           let host, _ = funLv.node in (* no field-based info for now *)
           match host.node with
             PVar v ->
               (match v.node with
                  PGlobal (id, _) ->
                    if (funTypesEq typ id) then (* could use t also *) 
                      IDS.add id res
                    else
                      res
                | _ -> res  (* Mismatch for function *)
               )
           | _ -> res (* Mismatch for function *)
        ) labels IDS.empty in
    let ptrNode = getNode ptrLv in
    let ptrInfo = derefID ptrNode in
    match ptrInfo.finalTarget with
      FBot ->
        IDS.empty
    | FRef targ ->
        let targInfo = derefID targ in
        accumulateTargets targInfo.finalLabels
    | FSelf ->
        accumulateTargets ptrInfo.finalLabels


  (** Given a PTA node and current set of var IDs, add IDs of 
      pointed-to targets to the set *)
  let targetIDs node curResults =
    let accumulateTargets labels =
      LVS.fold 
        (fun funLv res ->
           let host, _ = funLv.node in
           match host.node with
             PVar v -> 
               (match v.node with
                  PGlobal (id, _) 
                | PLocal (id, _) ->
                    IDS.add id res
                | _ ->
                    res (* Mismatch for now *)
               )
           | _ ->
               res 
        ) labels curResults in
    let ni = derefID node in
    match ni.finalTarget with
      FBot ->
        curResults
    | FRef targ ->
        let targInfo = derefID targ in
        accumulateTargets targInfo.finalLabels
    | FSelf ->
        accumulateTargets ni.finalLabels


  let nodePointsTo node targ =
    let ni = derefID node in
    match ni.finalTarget with
      FBot -> false
    | FSelf ->         
        targ == node (* assumming nodes are equivalence class ids *)
    | FRef targ2 ->
        targ == targ2 (* assumming nodes are equivalence class ids *)
          
          
  let may_alias n1 n2 =
    let ni1 = derefID n1 in
    match ni1.finalTarget with 
      FBot -> 
        false
    | FSelf -> 
        nodePointsTo n2 n1
    | FRef n ->
        nodePointsTo n2 n

  let compare n1 n2 =
    n1 - n2

  let hash = Hashtbl.hash

  let labels node =
    let ni = derefID node in
    ni.finalLabels

  let deref node =
    let ni = derefID node in
    match ni.finalTarget with
      FRef n ->
        n
    | FBot ->
        raise Not_found
    | FSelf -> node

  (* TODO: convert to REACHABLE from global (reflexive/transitive) *)
  let reachableFromG node =
    let ni = derefID node in
    ni.finalGReach

  let string_of node =
    ("[REP: " ^ (string_of_int node) ^ "]")

  let size_of node =
    let ni = derefID node in
    let s = LVS.cardinal ni.finalLabels in
    if (s == 0 && ni.finalTarget == FSelf) then 66667 (* TODO use tag *)
    else s

  let reachableFrom n srcs =
    let rec reachable n s visited =
      if (n == s) then true
      else if (List.mem s visited) then false
      else let srcInfo = derefID s in
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
  
  
let isFPCall ({cexp = ce;}, _, _) =
  List.exists 
    (fun ptaLv ->
       let host, _ = ptaLv.node in
       match host.node with
         PDeref _ -> true
       | _ -> false
    ) ce
    

(** Load all the assignments and calls and process *)
let analyzeAll (root : string) (fresh:bool) =

  (* Load info for each function *)
  print_string "Loading function info\n";
  flush stdout;
  Filetools.walkDirSimple
    (fun filename ->
       if (Filename.check_suffix filename ".pta") then
         let funs, _, _, _ = loadState filename in
         List.iter 
           (fun finfo ->
              Hashtbl.replace funTable finfo.funId finfo
           ) (rehashFuns funs)
    ) root;

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
    print_string "Processing assignments\n";
    flush stdout;

    (* Then process assignment constraints *)
    Solver.changed := true;
    while !Solver.changed do
      print_string "Iterating\n";
      flush stdout;
      Solver.changed := false;
      Filetools.walkDirSimple
        (fun filename ->
           if (Filename.check_suffix filename ".pta") then
             let _, baseAss, complexAss, calls = loadState filename in
             VarH.iter 
               (fun vinfo assList ->
                  List.iter 
                    (fun (lhs, rhs) -> Solver.doAssign lhs rhs) assList
               ) (rehashAssigns baseAss);
             
             VarH.iter
               (fun vinfo assList ->
                  List.iter 
                    (fun (lhs, rhs) -> Solver.doAssign lhs rhs) assList
               ) (rehashAssigns complexAss);
             
             VarH.iter
               (fun vinfo callList ->
                  List.iter 
                    (fun call -> Solver.doCall call) callList
               ) (rehashCalls calls)
        ) root;
    done;

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
      Lv.typeAfterDeref e    (* if function pointer is given *)
    with Not_found ->
      Lv.typeOfUnsafe e in   (* if function pointer deref is given *)
  let lv_exps = analyze_exp e in
  let idSet = List.fold_left
    (fun results rv ->
       try 
         let ptrLv = getLval rv in 
         let fids = Final.resolveFPs (D.string_of_ftype e_typ) ptrLv in
         IDS.union results fids
       with Not_found ->
         results
    ) IDS.empty lv_exps in
  IDS.elements idSet



(** Given a list of PTA ptr lvals, get the pointed-to targets *)
let deref_lvs_main lvs =
  let vids = List.fold_left
    (fun results ptrLv ->
       try 
         let node = Final.getNode ptrLv in
         Final.targetIDs node results
       with Not_found ->
         results
    ) IDS.empty lvs in
  IDS.fold
    (fun id results  ->
       try 
         (getVarinfo id) :: results
       with Not_found ->
         results
    ) vids []

(** Return the base lvals that this expression can point to. 
    TODO return varinfo * offset pair. May raise Not_found *) 
let deref_lval (lv:Cil.lval) = 
  let lvs = analyze_lval lv in
  deref_lvs_main lvs


(** Return the base lvals that this expression can point to.
    May raise Not_found *) 
let deref_exp (e:Cil.exp) =
  let lv_exps = analyze_exp e in
  let lvs = List.fold_left 
    (fun results rv ->
       try 
         getLval rv :: results
       with Not_found ->
         results
    ) [] lv_exps in
  deref_lvs_main lvs


(** True if the two ptr expressions may point to the same memory cell. *)
let may_alias (e1:Cil.exp) (e2:Cil.exp) : bool =
  let lv_exps1 = analyze_exp e1 in
  let lv_exps2 = analyze_exp e2 in
  List.exists 
    (fun ptrRv1 ->
       try 
         let ptrLv1 = getLval ptrRv1 in
         let node1 = Final.getNode ptrLv1 in
         List.exists 
           (fun ptrRv2 ->
              try 
                let ptrLv2 = getLval ptrRv2 in
                let node2 = Final.getNode ptrLv2 in
                Final.may_alias node1 node2
              with Not_found ->
                false) lv_exps2
       with Not_found ->
         false
    ) lv_exps1

let node_points_to = Final.nodePointsTo

  
let lvals_point_to lvs v : bool =
  let targ = lv_of_hostOff (host_of_var v) NoOffset in
  let targNode = Final.getNode targ in
  List.exists 
    (fun ptrLv ->
       try
         let node = Final.getNode ptrLv in
         node_points_to node targNode
       with Not_found ->
         false
    ) lvs


let lval_points_to lv v : bool =
  let lvs = analyze_lval lv in
  lvals_point_to lvs v


(** True if the ptr expressions may point the memory cell of the var *)
let points_to (e:Cil.exp) (v:Cil.varinfo) : bool =
  let lv_exps = analyze_exp e in
  let lvs = List.fold_left 
    (fun l ptrRv -> try getLval ptrRv :: l with Not_found -> l) [] lv_exps in
  lvals_point_to lvs v


(** True if lvalue is reachable from a global pointer *)
let reachableFromG (lval:Cil.lval) =
  let lvs = analyze_lval lval in
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
  let deref_lvs_main lvs =
    List.fold_left
      (fun results ptrLv ->
         try
           let node = Final.deref (Final.getNode ptrLv) in
           if (List.exists (fun other -> compare node other == 0) results) then
             results
           else
             node :: results
         with Not_found ->
           results
      ) [] lvs

  let deref_exp e : t list =
    let lv_exps = analyze_exp e in
    let lvs = List.fold_left 
      (fun results rv ->
         try 
           getLval rv :: results
         with Not_found ->
           results
      ) [] lv_exps in
    deref_lvs_main lvs

  let deref_lval lv : t list =
    let lvs = analyze_lval lv in
    deref_lvs_main lvs

  (* Get abstract node that represents this Cil lval *)
  let getNodeLval lv =
    let lvs = analyze_lval lv in
    List.fold_left 
      (fun res lv -> match res with
         None -> (try Some (Final.getNode lv) with Not_found -> res)
       | Some x -> res  ) None lvs

  (* Deref abstract node and get the target abstract node *)
  let deref (abs:t) : t =
    Final.deref abs

  let may_alias abs1 abs2 = 
    Final.may_alias abs1 abs2  

  let points_to abs1 abs2 =
    Final.nodePointsTo abs1 abs2

  (* Get Cil lvals associated w/ a node *)
  let lvals_of abs =
    let rec lv_of_ptaLv ptaLv = 
      let ho, off = ptaLv.node in
      match ho.node with
        PVar vi ->
          (match vi.node with
             PGlobal (id, _) 
           | PLocal (id, _) ->
               (Var (getVarinfo id), off.node)
           | _ -> raise Not_found
          )
      | PDeref rv ->
          let lv' = getLval rv in
          (Mem (Lval (lv_of_ptaLv lv')), off.node)
    in
    let lvs = Final.labels abs in
    LVS.fold
      (fun lv results  ->
         try 
           lv_of_ptaLv lv :: results
         with Not_found ->
           results
      ) lvs []

  (** Get the number of lvals represented by the node *)
  let size_of abs =
    Final.size_of abs
    
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

let printAssignment (lhs, rhs) =
  printPtaLv lhs; print_string " = "; printPtaRv rhs; print_string "\n"

let printConstraints root =
  print_string "PTA constraints:\n";
  Filetools.walkDirSimple
    (fun filename ->
       if (Filename.check_suffix filename ".pta") then
         let _, baseAss, complexAss, calls = loadState filename in
         VarH.iter 
           (fun vinfo assList ->
              List.iter 
                (fun ass -> printAssignment ass) assList
           ) baseAss;
         
         VarH.iter
           (fun vinfo assList ->
              List.iter 
                (fun ass -> printAssignment ass) assList
           ) complexAss;
         
         VarH.iter
           (fun vinfo callList ->
              List.iter 
                (fun call -> printCallCons call) callList
           ) calls
    ) root;
  print_string "\n\n"
    
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

let printPtsToSets () =
  let visited = Hashtbl.create 10 in
  let printed = Hashtbl.create 10 in
  let size_dist = ref IntMap.empty in
  LvalH.iter 
    (fun k node ->
       print_string "Node w/ lval key: ";
       printPtaLv k;
       print_string "\n";

       print_string ("Assoc. w/ node " ^ (string_of_id node) ^ 
         " representing: {");
       let labels = Final.labels node in
       let size = LVS.cardinal labels in
       if (Hashtbl.mem printed node) then
         print_string " see old printout "
       else
         (printLvs labels;
          Hashtbl.add printed node true;
          try 
            let old_freq = IntMap.find size !size_dist in
            size_dist := IntMap.add size (old_freq + 1) !size_dist
          with Not_found -> 
            size_dist := IntMap.add size 1 !size_dist
         )
       ;
       print_string ("} (" 
                     ^ (string_of_int size) ^
                     ")\n");

       print_string "Points to:\n";
       Hashtbl.clear visited;
       Hashtbl.add visited node true;
       printPtrChain node visited;
       
       print_string ("Pointed to by a global: " ^ 
                       (string_of_bool (Final.reachableFromG node)) ^ "\n\n");
    ) !Final.lvToID;
  print_string "Distribution of sizes: \n";
  IntMap.iter 
    (fun size freq ->
       print_string ((string_of_int size) ^ ": " ^ (string_of_int freq) ^ "\n")
    ) !size_dist;
  print_string "\n"


let aliasCache = Hashtbl.create 10 (* alias file-info cache *)

let cilAASetFile f =
  (* TODO: cache PTA state?? *)
  if (not (Hashtbl.mem aliasCache f.fileName)) then
    begin
      CilPTA.reset_globals ();
      CilPTA.analyze_file f;
      CilPTA.compute_results false;
      Hashtbl.clear aliasCache;
      Hashtbl.add aliasCache f.fileName true;
    end


let cilFPAA fexp =
  (* Should also match ftypes, since the PTA may munge a bunch
     when it comes to structs, etc. *)
  let ftype = Cil.typeOf fexp in
  let ftype_str = D.string_of_ftype ftype in    
  let fdecs = 
    try
      CilPTA.resolve_funptr fexp
    with 
      Not_found
    | CilPTA.UnknownLocation ->
        []
  in
  List.fold_left 
    (fun curList fdec ->
       let ft = fdec.svar.vtype in
       let fts = D.string_of_ftype ft in
       if (fts = ftype_str) then
         fdec.svar.vid :: curList
       else
         curList
    ) [] fdecs


let printFSet header set =
  print_string header;
  StringSet.iter (fun fn -> print_string (fn ^ ", ")) set;
  print_string "\n"

class testVisitor = object 
  inherit nopCilVisitor

  method vinst (i:instr) : instr list visitAction =
    match i with
      Call(_, callexp, _, _) -> begin
        match callexp with
          Lval (Var(vi),NoOffset) ->
            SkipChildren

        | Lval (Mem(ptrExp), NoOffset) ->
            let this = ref StringSet.empty in
            let fids = resolve_funptr ptrExp in
            List.iter 
              (fun fid ->
                 let varinfo = getVarinfo fid in
                 this := StringSet.add varinfo.vname !this
              ) fids;
            let cil = ref StringSet.empty in
            let cilFids = cilFPAA ptrExp in
            List.iter 
              (fun fid ->
                 let varinfo = getVarinfo fid in
                 cil := StringSet.add varinfo.vname !cil
              ) (List.fast_sort (fun a b -> a - b) cilFids);
            let diff = StringSet.diff !this !cil in
            if (not (StringSet.is_empty diff)) then
              (print_string ("cexp: " ^ (D.string_of_exp callexp) ^ "\n");
               printFSet "PTA says: " !this;
               printFSet "Cil says: " !cil;
               printFSet "Diff: " diff;
               print_string "\n")
            ;
            SkipChildren
        | _ ->
            SkipChildren
      end
    | _ ->
        SkipChildren

end


let testFunPtrs root =
  print_string "PTA Simple testing function pointers\n";
  flush stdout;
  Filetools.walkDir 
    (fun ast filename ->
       cilAASetFile ast;
       let vis = new testVisitor in
       Cil.visitCilFileSameGlobals vis ast
    ) root
    

(*********** Print variable ids so that we actually understand the output *)

let tempVIDSet = Hashtbl.create 101

let string_of_loc l = 
  Pretty.sprint 80 (Cil.d_loc () l)

class vidCollector = object
  inherit nopCilVisitor 

  method vvdec (vi:varinfo) =
    Hashtbl.replace tempVIDSet vi.vid (vi.vname, vi.vdecl);
    SkipChildren
end

let collectVidsFile f =
  let collector = new vidCollector in
  visitCilFileSameGlobals collector f

let printVarIDs root =
  Filetools.walkDir
    (fun ast filename ->
       collectVidsFile ast
    ) root;
  Hashtbl.iter 
    (fun vid (vname, vdecl) ->
       print_string ("vid : " ^ (string_of_int vid) ^ " == " ^
                       vname ^ " @ ");
       print_string ((string_of_loc vdecl) ^ "\n");
    ) tempVIDSet;
  Hashtbl.clear tempVIDSet

