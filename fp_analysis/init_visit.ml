
(** Sets up initial state (global and local). 
    Have functions to set up input states for all the "root" functions *)

open Cil
open Fstructs
open Callg
open Fp_types
open Fp_slice
open Fp_store
open Fp_summary
open Gen_defaults
open Globals_ref
open Fp_utils
open Logging

(************************************************************
 Set up globals and locals (i.e., formals)
************************************************************)

(************************************************************)

let isGlobalDataVar vi =
  vi.vglob && 
    not ((isFunctionType vi.vtype) || (Trans_alloc.isAllocVar vi.vname))
    (* Don't bring in the malloc crap *)

class initGlobals_getRefs = object(self)
  inherit nopCilVisitor

  val mutable curFunc = dummyFunDec

  method vfunc f = 
    curFunc <- f;
    DoChildren

  method vinit var off init : Cil.init Cil.visitAction =
    curFunc <- dummyFunDec;
    let rec checkInit curOff curInit =
      (match curInit with
         SingleInit exp -> self#processAssign var.vdecl (Var var, curOff) exp
       | CompoundInit (t, moreInitList) -> 
           List.iter (fun (moreOff, moreInit) ->
                        checkInit (addOffset moreOff curOff) moreInit) 
             moreInitList
      ) in
    checkInit off init;
    DoChildren

  method private processAssign loc lv rhs =
    (* Should only be constant expressions on RHS? *)
    let rhs, _ = eval emptyState rhs emptyMisc in
    let sinfo = getStructInfo (Cil_lvals.typeOfLvalUnsafe lv) in
    match lv with
      (Var (vi), off) ->
        let var, off = getLocation vi off in
        strongUpdateGlobal var off rhs sinfo

    | (Mem _, _) ->
        failwith "Initializer shouldn't require deref to store"

end


(************************************************************)

(* Set globals that are not initialized by initializers, and do not
   reach FPs to the NFP value *)

class initNonFPGlobals = object
  inherit nopCilVisitor

  method vglob glob : global list visitAction =
    (match glob with
       GVarDecl (vi, _)
     | GVar (vi, _, _) -> 
         if isGlobalDataVar vi then
           let var = FVar (vidOfVarinfo vi) in
           if nonPtrInt vi.vtype then
             if hasGlobalBinding var then ()
             else
               (logStatusF "Overriding global val to NFP: %s\n" vi.vname;
                let oldVal = getGlobalBinding var GNone in
                let newVal = nfpOutVar rootSource var oldVal in
                let sinfo = getStructInfo vi.vtype in
                strongUpdateGlobal var noOff newVal sinfo )
                 
     | _ -> ());
    DoChildren
      
end

(************************************************************)


(** Give globals that are totally undefined some value *)
class initTotallyUndefined = object (self)
  inherit nopCilVisitor

  val mutable hasWrite = IS.empty
  val mutable hasAddrTaken = IS.empty
  val mutable knownGlobals = IS.empty

  method private checkAddrTaken exp =
    match exp with
      Lval (lv) -> self#checkAddrTakenLv lv
    | AddrOf (lv)
    | StartOf (lv) ->
        (match lv with
           Var (vi), _ ->
             if isGlobalDataVar vi then
               hasAddrTaken <- IS.add vi.vid hasAddrTaken
         | Mem (e), _ ->
             self#checkAddrTaken e)
    | CastE (t, e) -> self#checkAddrTaken e
    | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ -> ()
    | AlignOfE _ | SizeOfE _ -> ()
    | UnOp (op, e, t) -> self#checkAddrTaken e
    | BinOp (op, e1, e2, t) -> self#checkAddrTaken e1; self#checkAddrTaken e2

  method private checkAddrTakenLv lv =
    match lv with
      Var _, _ -> ()
    | Mem e, _ -> self#checkAddrTaken e

  method private checkWrite lv =
    match lv with
      Var (vi), off -> 
        if isGlobalDataVar vi then
          hasWrite <- IS.add vi.vid hasWrite
    | Mem (e), off ->
        self#checkAddrTaken e

  (** Only check the globals that are written / have address taken to *)
  method vinst i =
    (match i with
       Set (lv, rhs, loc) ->
         self#checkWrite lv;
         self#checkAddrTaken rhs
     | Asm (_, _, _, _, _, _) -> ()
     | Call(lvopt, callexp, args, loc) ->
         (match lvopt with
            Some (lv) -> self#checkWrite lv
          | None -> ());
         List.iter self#checkAddrTaken args;
         self#checkAddrTaken callexp);
    DoChildren

  method vstmt (s:stmt) : stmt visitAction =
    (match s.skind with
       Instr _  | Goto _ | Break _ | Continue _ | Loop _ | Block _
     | TryFinally _ | TryExcept _  | Return (None, _) -> ()
     | Return (Some e, _) ->
         self#checkAddrTaken e
     | Switch (e, _, _, _)
     | If (e, _, _, _) -> 
         ()
    ); DoChildren

  method vglob glob : global list visitAction =
    (match glob with
       GVarDecl (vi, _)
     | GVar (vi, _, _) -> 
         if isGlobalDataVar vi then
           knownGlobals <- IS.add vi.vid knownGlobals
     | _ -> ());
    DoChildren
    

  method setUndefinedGlobals () =
    logStatus "Totally undefined globals:";
    IS.iter
      (fun vid ->
         if not (IS.mem vid hasAddrTaken) && not (IS.mem vid hasWrite) then
           let var = Cilinfos.getVarinfo vid in
           let fvar = FVar (vidOfVarinfo var) in
           if hasGlobalBinding fvar then flushGlobals ()
           else 
             (logStatusF "Setting unknown to NFP: %s\n" var.vname;
              let oldVal = getGlobalBinding fvar GNone in
(* Hmm... shouldn't it be checked for reachability? *)
              let newVal = nfpOutVar rootSource fvar oldVal in
              let sinfo = getStructInfo var.vtype in
              strongUpdateGlobal fvar noOff newVal sinfo )
      ) knownGlobals;
    logStatus "============================="
      
(* Hmm... if we make up default dummy nodes, we should add those
   dummy nodes to the GLOBAL state so that we have it in the summary
   instead of adding it into the local state... this means we need
   to create globals instead of heap cells (heap cells are considered local)
*)

end

let initGlobals root cg =
  let vis = new initGlobals_getRefs in
  logStatus "Setting initial values for globals w/ Initializers\n";
  Filetools.walkDir 
    (fun ast fname -> 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       flushGlobals ();
    ) root;

  let vis = new initNonFPGlobals in
  logStatus "Setting values of undefined nonFP globals to Nfp\n";
  flushStatus ();
  Filetools.walkDir 
    (fun ast fname -> 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       flushGlobals ();
    ) root;

  let vis = new initTotallyUndefined in
  logStatus "Finding totally uninitialized globals\n";
  flushStatus ();
  Filetools.walkDir 
    (fun ast fname -> 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       flushGlobals ();
    ) root;
  vis#setUndefinedGlobals ();
  flushGlobals ();
  printGlobalStats ()


(************************************************************)
(* Init formals of roots *)

let voidMap = ref None

let initVoidMap root =
  let vis = new Void_arg.voidMapper in
  logStatus "Mapping void * formals to a type if possible";
  Filetools.walkDir
    (fun ast _ -> visitCilFileSameGlobals (vis :> cilVisitor) ast) root;
  voidMap := Some (vis#getVoidMapping ())

let typeOfFormal voidMap formal =
  if Type_reach.isPoly formal.vtype then
    try 
      Inthash.find voidMap formal.vid 
    with Not_found ->
      formal.vtype
  else formal.vtype

let externalNfpSource = (-3)

let getStateFor cg voidMap fkey =
  let state, context = 
    match getFormals cg fkey with
      Some (fnode, formals) ->
        let fn, defFile =  fnode.name, fnode.defFile in
        logStatusF "Making initial state for %s : %s\n" fn defFile;
        let localSt = List.fold_left 
          (fun st formal -> 
             let t = typeOfFormal voidMap formal in
             let v, st = getDefaultValForTyp t st in
             let var, off = getLocation formal NoOffset in
             addBinding st var v
          ) emptyState formals in
        let roots = List.map (fun f -> FVar (vidOfVarinfo f)) formals in
        let result = weakenState (Some roots) rootSource localSt in
(*        printState result; *)
(*        let result = sliceState result roots in *)
(* TODO: make similar to the getInputsForCall instead? *)
(*        logStatusF "Initial state for func: %s\n" fn;
        printState result;
        *)
        result, getSumContext roots result

    | None ->
        (* Don't have function definition? *)
        logError ("Skipping func w/ no body: " ^ (string_of_fkey fkey)); 
        emptyState, emptyState
  in
  (makeSumKey (fkey, context), state)


(************************************************************)

let removeDirectCalled node fset =
  List.fold_left (fun cur (fk, indir) -> 
                    if not indir then FSet.remove fk cur else cur) 
    fset (calleeKeyBools node)

let conservativeRoots cg addrTk =
  let maybeRoot = FMap.fold 
    (fun fk _ res -> FSet.add fk res) cg FSet.empty in
  logStatusF "Before removing addrTk: %d\n" 
    (FSet.cardinal maybeRoot);
  logStatusF "Num addrTk: %d\n" (FSet.cardinal addrTk);
  let maybeRoot = FSet.diff maybeRoot addrTk in
  logStatusF "After removing addrTk: %d\n" (FSet.cardinal maybeRoot);
  let maybeRoot = FMap.fold 
    (fun fk node res -> removeDirectCalled node res) cg maybeRoot in
  logStatusF "After removing directly called: %d\n" 
    (FSet.cardinal maybeRoot);
  logStatusD (d_funcset cg maybeRoot);
  FSet.elements maybeRoot

let unsetMerger () =
  let oldAggMerge = !Fp_agg_merge.myMerger in
  Fp_agg_merge.myMerger := new Fp_agg_merge.nopMerger;
  oldAggMerge
  
let resetMerger oldAggMerge =
  Fp_agg_merge.myMerger := oldAggMerge
  

let getInitialStates root addrTk cg =
  let oldAggMerge = unsetMerger () in
  initGlobals root cg;
  let roots = conservativeRoots cg addrTk in
  match !voidMap with
    Some (vm) ->
      let roots = List.map fid_to_fkey roots in
      let ret = List.map (getStateFor cg vm) roots in
      resetMerger oldAggMerge;
      ret
  | _ -> failwith "did not initialize voidMap!"
      
(* Return all the functions we omitted before simply because they
   had their address taken. This can return more functions that needed
   because if foo1 and foo2 both had their address taken, but foo1
   is the one that calls foo2, then we really only needed to return foo1.
   It's okay though, because we will only end up exploring more contexts *)
let getMoreStates cg touchedFuncs =
  let oldAggMerge = unsetMerger () in
  let roots = conservativeRoots cg touchedFuncs in  
  match !voidMap with
    Some (vm) ->
      let roots = List.map fid_to_fkey roots in
      let ret = List.map (getStateFor cg vm) roots in
      resetMerger oldAggMerge;
      ret
  | _ -> failwith "did not initialize voidMap!"
      
