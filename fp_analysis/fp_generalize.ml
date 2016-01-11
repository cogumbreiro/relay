(** Generalize the input heap before picking a context for a function call.
    Also, manage the reverse mapping, etc. *)

open Cil
open Fp_types
open Fp_lattice_ops
open Fp_malloc
open Logging
open Cildump


(** Bi-directional map -- but not a bijection *)
module BidirMap (T:Map.OrderedType) = struct
  
  module M = Map.Make(T)
  type elt = T.t
  type fwdMap = elt M.t
  type revMap = elt list M.t
  type t = fwdMap 
      (* Generate reverse mapping on demand. Usually that is only 
         asked-for once  *)
      
  let empty : t = M.empty
    
  let add x y im1 : t  =
    M.add x y im1

  let findFwd x im1 : elt =
    M.find x im1

  let memFwd x im1 =
    M.mem x im1
        
  let findRev y im2 : elt list =
    M.find y im2

  let memRev y im2 =
    M.mem y im2

  let getFwd im1 = im1

  let getRev im1 = 
    let im2 = M.fold 
      (fun x y im2 -> 
         let old = try M.find y im2 with Not_found -> [] in
         M.add y (x :: old) im2
      ) im1 M.empty in
    im2

end 

module VIso = BidirMap(OrderedFVar) 
module TH = Hashtbl.Make 
  (struct
     type t = typ
     let hash x = Ciltools.hash_type x
     let equal a b = Ciltools.compare_type a b == 0
   end)


(************************************************************)


type remapOrNot =
    NoGlobalProgVar
  | NoGlobalHeapVar
  | YesWithAtt of heapKind

let eligibleVar var curSt =
  match var with
    FRet _ -> failwith "remap given ret var"
  | FVar vid ->
      let vi = varinfoOfVid vid in
      if vi.vglob then NoGlobalProgVar
      else YesWithAtt (HSingle)
  | FHeap _ | FInput _ ->
      (try
         let att = VarMap.find var curSt.hAttrs in
         match att with
           HGlobal -> NoGlobalHeapVar
         | HSingle | HSum -> YesWithAtt (att)
       with Not_found ->
         logErrorF "eligibleVar attrib NF: %s\n" (string_of_var var);
         NoGlobalHeapVar)

let eligibleVarBool var curSt =
  match eligibleVar var curSt with
    YesWithAtt _ -> true
  | NoGlobalHeapVar | NoGlobalProgVar -> false
      


(************************************************************)

(*

(** Structure-preserving version of generalizer *)
class structPreserveGen = object (self)

  val mutable origSt = emptyState
  val mutable curMap = VIso.empty
  val mutable curSt = emptyState
  val mutable nextIDs = TH.create 0
  val mutable curFkey = -2

  method private getNextVar origVar =
    let t = canonType (typeOfFVar origVar) in
    let prev = try TH.find nextIDs t with Not_found -> 0 in
    let x = prev + 1 in
    TH.replace nextIDs t x;
    makeInputVar t x curFkey
  
  method private addMapping origVar newVar att =
    (* Update the val binding *)
    try 
      let oldV = getBinding origVar origSt in
      let newV = self#remapVal oldV in
      let newV = 
        try 
          let existingV = getBinding newVar curSt in
          combineVals newV existingV
        with Not_found ->
          newV in
      curSt <- 
        { bindings = 
            VarMap.add newVar newV curSt.bindings;
          hAttrs = 
            VarMap.add newVar att curSt.hAttrs; }
    with Not_found ->
      failwith "addMapping NF"

(* TODO: seed curSt w/ origSt instead? *)
  method private notRemapped origVar = 
    if VarMap.mem origVar curSt.bindings then ()
    else
      let bindings = 
        try
          let oldV = getBinding origVar origSt in
          (* TODO: Should it really remap the targets of a var that is 
             not remapped? Well... why not? *)
          let newV = self#remapVal oldV in
          VarMap.add origVar newV curSt.bindings;
        with Not_found ->
          if isGlobalDebug "notRemapped" false origSt origVar then
            curSt.bindings 
          else begin 
            logErrorF "notRemapped binding %s @ %s\n" (string_of_var origVar)
              (string_of_loc !currentLoc);
            curSt.bindings
          end
      in
      let atts =
        match origVar with
          FHeap _ -> 
            (try        
               let att = VarMap.find origVar origSt.hAttrs in
               VarMap.add origVar att curSt.hAttrs 
             with Not_found ->
               logErrorF "notRemapped att %s @ %s\n" (string_of_var origVar)
                 (string_of_loc !currentLoc);
               curSt.hAttrs)
        | FVar _ | FRet _ -> 
            curSt.hAttrs
      in
      curSt <- { bindings = bindings; hAttrs = atts; }
        
  method private seekReachable (var, o) curVis =
    if (VIso.memFwd var (VIso.getFwd curMap)) || VarSet.mem var curVis 
    then curVis
    else 
      let curVis = VarSet.add var curVis in 
      try
        let v = getBinding var origSt in
        foldTargetsVal self#seekReachable v curVis
      with Not_found ->
        if not (isGlobalDebug "seekReachable" false origSt var) then
          logErrorF "seekReachable binding %s @ %s\n" (string_of_var var)
            (string_of_loc !currentLoc);
        curVis

  method private eligibleVar var = eligibleVar var origSt

  method private seekRecursiveType var =
    (* Don't add 'a... *)
    if isUnknownMallocVar var then VarSet.singleton var
    else     
      let reachables = self#seekReachable (var, noOff) VarSet.empty in
      let reachables = VarSet.add var reachables in
      let curType = canonType (typeOfFVar var) in 
      VarSet.filter 
        (fun other -> 
           let otherType = canonType (typeOfFVar other) in
           Ciltools.compare_type curType otherType == 0 &&
           (match self#eligibleVar other with YesWithAtt _ -> true | _ -> false)
        ) reachables
        
  method private remapVar var =
    (* Check if it's already been remapped *)
    try Some (VIso.findFwd var (VIso.getFwd curMap))
    with Not_found -> 
      match self#eligibleVar var with
        NoGlobalProgVar -> 
          curMap <- VIso.add var var curMap;
          None
      | NoGlobalHeapVar ->
          curMap <- VIso.add var var curMap;
          (* Well, it had an attribute... value MIGHT be there too *)
          self#notRemapped var;
          None
      | YesWithAtt att ->
          let newVar = self#getNextVar var in
          (* Seek further reachable vars of the same type and 
             make them map them to this same newVar. *)
          let reachableSameType = self#seekRecursiveType var in
          let att = 
            if VarSet.is_singleton reachableSameType then att 
            else HSum in
          (* Mark the mapping first *)
          VarSet.iter 
            (fun var -> curMap <- VIso.add var newVar curMap) 
            reachableSameType;
          (* Then update the values w/ this new mapping *)
          VarSet.iter 
            (fun var -> self#addMapping var newVar att) reachableSameType;
          Some (newVar)


  method private remapRec nonfp =
    OffsetMap.fold 
      (fun off v cur ->
         let newVal = self#remapVal v in
         if v == newVal then cur
         else OffsetMap.add off newVal cur
      ) nonfp nonfp

  method private remapVal v =
    match v with
      FNFP _ | FpRef _ | FInt _ -> v
    | Refs locs ->
        let newLocs, changed = FLocSet.fold
          (fun (var, off) (cur, ch) ->
             let var, ch = match self#remapVar var with
                 Some (newVar) -> newVar, true
               | None -> var, ch in
             FLocSet.add (var, off) cur, ch
          ) locs (FLocSet.empty, false) in
        if not changed then v
        else Refs newLocs
    | Record (fp, nonfp, moff) ->
        let newnonfp = self#remapRec nonfp in
        if newnonfp == nonfp then v
        else Record (fp, newnonfp, moff)
    | RecordSet fmsID ->
        let fms = findFMS fmsID in
        let newFMS = FMS.fold 
          (fun fp (nonfp, m) cur ->
             let newnonfp = self#remapRec nonfp in
             if newnonfp == nonfp then cur
             else FMS.add fp (newnonfp, m) cur
          ) fms fms in
        tryReduceRecordSet newFMS
    | FIRecs fir ->
        let changed, newFIR = mapNonFPFIR (self#remapVal) (==) fir in
        if changed then FIRecs (newFIR) else v

  method generalize st roots fkey =
    origSt <- st;
    curMap <- VIso.empty;
    curSt <- emptyState;
    nextIDs <- TH.create 10;
    curFkey <- fkey; 
    List.iter 
      (fun root ->
         try
           let origVal = getBinding root origSt in
           let newVal = self#remapVal origVal in
           curSt <- addBinding curSt root newVal
         with Not_found ->
           failwith ("Generalize no binding for root " ^ (string_of_var root))
      ) roots;
    (* Let the client store the mapping somewhere *)
    curSt, curMap

end

(************************************************************)

(** Merge nodes reachable from same access path (e.g., if x->f1->f2
    can refer to A, or B, then A and B get merged.

    Strategy is to first run the fp_agg_merge thing w/ special hooks to 
    get that effect, then do the renaming to generalize them. *)

class accessPathGenHelper = object (self)
  inherit Fp_agg_merge.absVarMerger as super

  method getMergedIntos sk =
    failwith "accessPathGenHelper doesn't track merges!"

  method notifyMerge toMerge =
    (* For this, don't track merges done *)
    ()
      
  method filter parts =
    parts
      
  method eligibleVar var curSt = 
    eligibleVarBool var curSt

    (*
  method seekMergeFurther curSt (var, o) curVis =
    (* already do the seek merge further elsewhere? *)
    curVis
    *)
     
  method private checkMergeTypes x = ()

  method wasMerged var =
    VarH.fold 
      (fun v1 v2 found ->
         if found then found 
         else (compFVar var v1 == 0 || compFVar var v2 == 0)
      ) (self#getMappings) false

end


(** Generalizer that merges nodes reachable from same access path *)
class accessPathMergingGen = object (self)
  inherit structPreserveGen as super

  val prePassMerger = new accessPathGenHelper
    
  method private eligibleVar var =
    let el = super#eligibleVar var in
    match el with
      YesWithAtt att ->
        (match att with 
           HSingle -> 
             if prePassMerger#wasMerged var 
             then YesWithAtt HSum else el
         | _ -> el)
    | _ -> el

  method private setupMappingsForMerged () =
    let mergings = prePassMerger#getMappings in
    VarH.iter 
      (fun var repVar ->
         try
           let newVar = VIso.findFwd repVar (VIso.getFwd curMap) in
           curMap <- VIso.add var newVar curMap;
         with Not_found -> 
           failwith ("no new var for merged rep: " ^ string_of_var repVar)
      ) mergings
      
  method generalize st roots fkey =
    origSt <- prePassMerger#aggressiveMergeAll st;
    curMap <- VIso.empty;
    curSt <- emptyState;
    nextIDs <- TH.create 10;
    curFkey <- fkey; 
    List.iter
      (fun root ->
         try
           let origVal = getBinding root origSt in
           let newVal = self#remapVal origVal in
           curSt <- addBinding curSt root newVal
         with Not_found ->
           failwith ("Generalize no binding for root " ^ (string_of_var root))
      ) roots;
    self#setupMappingsForMerged (); 
    (* Let the client store the mapping somewhere *)
    Fp_hashcons.hc_state curSt, curMap

end

*)

(************************************************************)

module AccPathGen = struct

  let rec remapRecord map offmap =
    (* Only replace the stuff that needs to be replaced *)
    OffsetMap.mapCh (remapVal map) offmap

  and remapVal map v = 
    match v with
      FInt _ | FpRef _ | FNFP _ -> v
    | Refs locs ->
        let fwd = VIso.getFwd map in
        Refs (FLocSet.fold
                (fun (var, o) cur ->
                   FLocSet.add (VIso.findFwd var fwd, o) cur
                ) locs FLocSet.empty)
    | Records recs ->
        let newrecs = List.map
          (fun (fp, nonfp, m) -> (fp, remapRecord map nonfp, m)) recs in
        Records newrecs
    | FIRecs fir ->
        FIRecs (List.map (fun (fp, nonfp) -> (fp, remapVal map nonfp)) fir)

  let mergeAccPathCycles accPath =
    let rec helper curHead curTail vis =
      match curTail with
        (AccVar _) as h :: t -> 
          (* Don't count first var as something to match against *)
          helper (h :: curHead) t vis
      | h :: t ->
          (try
             let _, oldAP = 
               List.find (fun (acc, _) -> comp_accElem h acc == 0) vis in
             helper oldAP t vis
           with Not_found ->
             let newHead = h :: curHead in
             helper newHead t ((h, newHead) :: vis))
      | [] -> curHead
    in
    helper [] (List.rev accPath) []

    
  (** Access path-based generalizer *)
  let generalize st roots fkey =
    let shortestAccPath = VarH.create 17 in
    let queue = Queue.create () in
    List.iter
      (fun var ->
         match var with
           FVar vid ->
             let curAccPath = [(AccVar vid)] in
             VarH.add shortestAccPath var curAccPath;
             Queue.add (var, curAccPath) queue;
         | _ -> failwith "generalize: non-prog var as root"
      ) roots;

    (* One pass to figure out shortest-access paths *)
    let rec visitAccPathVal v oldAP =
      match v with
        FInt _ | FNFP _ | FpRef _ -> ()
      | Refs locs ->
          visitAccPathRef locs oldAP
      | Records recs ->
          List.iter 
            (fun (_, nonfp, _) -> visitAccPathRec nonfp oldAP) recs
      | FIRecs fir ->
          List.iter 
            (fun (fp, nonfp) ->
               visitAccPathVal nonfp (AccField (topOff) :: oldAP)) fir

    and visitAccPathRef locs oldAP =
      FLocSet.iter 
        (fun (var, o) ->
           let typVar = typeIDOfFvar var in
           let newAccPath = AccDeref (typVar, o) :: oldAP in
           if VarH.mem shortestAccPath var then ()
           else begin
             VarH.add shortestAccPath var newAccPath;
             Queue.add(var, newAccPath) queue
           end 
        ) locs

    and visitAccPathRec offMap oldAP =
      OffsetMap.iter
        (fun o v ->
           let newAccPath = AccField o :: oldAP in
           visitAccPathVal v newAccPath
        ) offMap
    in

    while not (Queue.is_empty queue) do
      let (var, ap) = Queue.take queue in
      try
        let v = getBinding var st in
        visitAccPathVal v ap
      with Not_found ->
        if eligibleVarBool var st then
          logErrorF "Generalize no binding for %s\n" (string_of_var var)
    done;

    (* Finally, do the remapping *)
    let map = VarH.fold 
      (fun var accPath curMap ->
         match accPath with
           [AccVar _] -> (* Don't remap roots *)
             VIso.add var var curMap
         | _ ->
             if eligibleVarBool var st 
             then 
               let accPath = mergeAccPathCycles accPath in
               VIso.add var (FInput accPath) curMap
             else VIso.add var var curMap
      ) shortestAccPath VIso.empty in

    let newStore = VarMap.fold
      (fun var v curSt ->
         let newvar = VIso.findFwd var (VIso.getFwd map) in
         let newval = remapVal map v in
         try
           let oldval = VarMap.find newvar curSt in
           let combo = combineVals oldval newval in
           if combo == oldval then curSt
           else VarMap.add newvar combo curSt
         with Not_found ->
           VarMap.add newvar newval curSt
      ) st.bindings VarMap.empty in

    (* Iter over all known vars (from old bindings and old hAttrs)
       to cover all vars *)
    let knownVars = 
      VarMap.fold (fun var _ cur -> VarSet.add var cur) st.bindings VarSet.empty
    in
    let knownVars = 
      VarMap.fold (fun var _ cur -> VarSet.add var cur) st.hAttrs knownVars in
    
    let newAtts = VarSet.fold
      (fun var cur ->
         let newvar = VIso.findFwd var (VIso.getFwd map) in
         match newvar with 
           FInput _ -> (* was remapped *)
             (try
                let existing = VarMap.find newvar cur in
                VarMap.add newvar (combineAttrs existing HSum) cur
              with Not_found ->
                try VarMap.add newvar (VarMap.find var st.hAttrs) cur
                with Not_found -> VarMap.add newvar HSingle cur)

         | FVar _ | FRet _ -> (* not remapped *) cur
         | FHeap _ -> (* not remapped *)
             try VarMap.add newvar (VarMap.find var st.hAttrs) cur
             with Not_found -> 
               (failwith ("remap heap att? " ^ (string_of_var newvar) ^ 
                            " " ^ (string_of_var var)))
      ) knownVars VarMap.empty in

    let newSt = { bindings = newStore; hAttrs = newAtts; } in
    ( newSt, map )


end

(************************************************************)

class reverseMapper = object (self)

  val mutable origSt = emptyState
  val mutable calleeFK = 0
  val mutable calleeMergedIntos = VarH.create 0
  val mutable madeUpVars = VarH.create 0
  val mutable madeUpID = 0

  method private init sumKey st =
    calleeFK <- Summary_keys.fkey_of_sumKey sumKey;
    calleeMergedIntos <- !Fp_agg_merge.myMerger#getMergedIntos sumKey;
    origSt <- st;
    madeUpVars <- VarH.create 10;
    madeUpID <- 0

  method reverseMap sumKey st revMap =
    self#init sumKey st;
    let newStore = 
      VarMap.fold 
        (fun var v cur ->
           let vars = self#reverseMapVar revMap var in
           let v = self#reverseMapVal revMap v in
           List.fold_left (fun cur var -> VarMap.add var v cur) cur vars
        ) st.bindings VarMap.empty in
    let newAtts =
      VarMap.fold
        (fun var att cur ->
           let vars = self#reverseMapVar revMap var in
           List.fold_left (fun cur var -> VarMap.add var att cur) cur vars
        ) st.hAttrs VarMap.empty in
    let newStore = self#makeUpValues newStore in
    let newAtts = self#makeUpAttrs newAtts in
    { bindings = newStore; hAttrs = newAtts; }
      

  method private reverseMapVar revMap var =
    (* var may map to multiple... *)
    try 
      let oldVars = VIso.findRev var revMap in
      self#detectEscape var oldVars
    with Not_found ->
      match var with
        FRet _ 
      | FVar _ -> [var]
      | FHeap hi ->
          (try 
             if funAllocedHID calleeFK hi.hID || isGlobalHeapVar origSt var 
             then [var] (* no need to do anything crazy w/ those guys *)
             else self#makeUpVar var
           with Not_found -> self#makeUpVar var)
      | FInput _ ->
          (try
             if isGlobalFVar origSt var then [var] else self#makeUpVar var
           with Not_found -> self#makeUpVar var)


  method private detectEscape newVar oldVars =
    match newVar with
      FInput _ ->
        (try 
           (* if it is NOW global, but check if it wasn't before... *) 
           let tryUnify curNewVar oldVar =
             if not (eqFVar newVar oldVar) && 
               (match oldVar with
                  FVar _ | FRet _ -> false | FHeap _ | FInput _ -> true) 
             then begin
               Fp_unify_globals.unifyGlobal newVar oldVar;
               logStatusF "merged g (2): %s <- %s\n" 
                 (string_of_var newVar) (string_of_var oldVar);
               curNewVar
             end else
               oldVar :: curNewVar
           in
           if isGlobalFVar origSt newVar then begin
             List.fold_left tryUnify oldVars [newVar]
           end
           else oldVars
         with Not_found -> oldVars)
    | _ -> (* not remapped *)
        oldVars


  method private makeUpVar var =
    try [VarH.find madeUpVars var]
    with Not_found ->
      (* If no fresh nodes ever merged into it, then drop it *)
      (try 
         let newVar = VarH.find calleeMergedIntos var in
         logErrorF "revMap: using fresh %s @ %s\n" (string_of_var newVar)
           (string_of_loc !currentLoc);
         (* ugh... need to track and make up an attrib (and value?) too! *)
         VarH.add madeUpVars var newVar;
         [newVar]
       with Not_found -> [] )


  method private getNextMadeup var =
    let id = madeUpID in
    madeUpID <- madeUpID + 1;
    [( { pp_stmt = -1;
         pp_instr = id; }, 
       calleeFK) ]

  method private makeUpAttrs curAttrs =
    VarH.fold 
      (fun var newVar cur -> 
         if VarMap.mem newVar cur then cur 
         else VarMap.add newVar HSingle cur) madeUpVars curAttrs

  method private makeUpValues curStore =
    VarH.fold
      (fun var newVar cur ->
         if VarMap.mem newVar cur then cur
         else begin
           (* Shady because the fresh var may have been initialized,
              but we don't know that... *)
           logErrorF "revMap: fresh val for %s\n" (string_of_var newVar);
           VarMap.add newVar (defaultVarVal newVar) cur
         end
      ) madeUpVars curStore
         
  method private reverseMapRec revMap nonfp =
    OffsetMap.mapCh (self#reverseMapVal revMap) nonfp

  method private reverseMapVal revMap v =
    match v with
      FNFP _ | FpRef _ | FInt _ -> v
    | Refs locs ->
        let newLocs = FLocSet.fold
          (fun (var, off) cur ->
             let vars = self#reverseMapVar revMap var in
             List.fold_left 
               (fun cur var -> FLocSet.add (var, off) cur) cur vars
          ) locs FLocSet.empty in
        if FLocSet.is_empty newLocs then Refs (nullVar#addToSet newLocs)
        else Refs newLocs
    | Records recs ->
        let newrecs = List.map 
          (fun (fp, nonfp, m) ->
             (fp, self#reverseMapRec revMap nonfp, m)) recs in
        Records (newrecs)
    | FIRecs fir ->
        let newFIR =  List.map (self#reverseMapFIRec revMap) fir in
        FIRecs newFIR

  method private reverseMapFIRec revMap (fp, nonfp) =
    (fp, self#reverseMapVal revMap nonfp)

end

(************************************************************)

(* let generalizer = ref (new accessPathMergingGen) *)
let reverser = ref (new reverseMapper)


module GeneralizeMapTrack = struct

  type t = VIso.revMap
  type simpleSum = VIso.revMap

  let simplify x = x
  let desimplify x = x

  let initVal = VIso.getRev (VIso.empty)
  let unknownSummary = VIso.getRev (VIso.empty)
    
end
(* TODO: cluster these more? *)


module GeneralizeMapSum = Cache_sum.Make (GeneralizeMapTrack)
let gensums = new GeneralizeMapSum.data 64 
  (Backed_summary.makeSumType "gen_rev")
let () = Backed_summary.registerType gensums


let toKey inputs roots fkey =
  let inStr = rawstring_of_state inputs in
  let rootStr = List.fold_left (fun cur var -> cur ^ string_of_var var) 
    "" roots in
  Inout_summary.sumKeyOfKeyString fkey (inStr ^ rootStr)


let genStoreMapping inputs roots fkey =
  let key = toKey inputs roots fkey in
(*  let newIn, map = !generalizer#generalize inputs roots fkey in *)
  let newIn, map = AccPathGen.generalize inputs roots fkey in
  let revMap = VIso.getRev map in
  gensums#addReplace key revMap ;
  newIn

let reverseMapping inputs roots sumKey toRev =
  let fkey = Summary_keys.fkey_of_sumKey sumKey in
  let key = toKey inputs roots fkey in
  let map = gensums#find key in
  let result = !reverser#reverseMap sumKey toRev map in
  result
