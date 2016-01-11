
(** Sets up initial state (global and local). 
    Have functions to set up input states for all the "root" functions *)

open Cil
open Fstructs
open Callg
open Type_utils
open Fp_rci_types
open Fp_rci_lattice_ops
open Fp_rci_slicer
open Fp_rci_store
open Fp_rci_summary
open Fp_rci_defaults
open Fp_rci_globals
open Fp_rci_utils
open Logging

module IS = Sparsebitv

(************************************************************
 Set up initial values for globals and formals
************************************************************)

(************************************************************)

let isGlobalDataVar vi =
  vi.vglob && 
    not ((isFunctionType vi.vtype) || (Trans_alloc.isAllocVar vi.vname))
    (* Don't bring in the malloc crap *)


(** Process global initializers (first pass) *)
class initGlobals = object(self)
  inherit nopCilVisitor

  val mutable curFunc = dummyFunDec

  method vfunc f = 
    curFunc <- f;
    DoChildren

  method vinit var off init : Cil.init Cil.visitAction =
    curFunc <- dummyFunDec;
    (* If it's an array offset, conflate + combine w/ the old as a post-pass *)
    let rec checkInit curOff curInit =
      (match curInit with
         SingleInit exp -> 
           self#processAssign var.vdecl (Var var, curOff) exp
       | CompoundInit (t, moreInitList) -> 
           List.iter (fun (moreOff, moreInit) ->
                        checkInit (addOffset moreOff curOff) moreInit) 
             moreInitList
      ) in
    checkInit off init;
    DoChildren

  method private processAssign loc lv rhs =
    (* Should only be constant expressions on RHS? *)
    let rhs, _ = eval emptyState rhs (emptyMisc curFunc) in
    let expectedT = typeModArray (Cil_lvals.typeOfLvalUnsafe lv) in
    match lv with
      (Var (vi), off) ->
        let var, (off, sum) = getLocation true vi off in
(*        strongUpdateGlobal var off rhs expectedT *)
        weakUpdateGlobal var off rhs expectedT
          
    | (Mem _, _) ->
        failwith "Initializer shouldn't require deref to store"

end


(************************************************************)

(** Make a pass to set globals that are not initialized by initializers 
    and/or set those that do not reach FPs to the NFP value *)
class initNonFPGlobals = object
  inherit nopCilVisitor

  method vglob glob : global list visitAction =
    (match glob with
       GVarDecl (vi, _)
     | GVar (vi, _, _) -> 
         if isGlobalDataVar vi then begin
           let var = FVar (vidOfVarinfo vi) in
           logStatusF "Looking to NFP-out global: %s\n" vi.vname;
           let oldVal, _, newVar, _ = getGlobalBinding var GNone in
           if not (eqFVar var newVar) then
             failwith (Printf.sprintf "initNonFPGlobals: new var %s vs %s\n"
                         (string_of_var newVar) (string_of_var var));
           let newVal = 
              nfpOutVar emptyState.vAttrs rootSource var oldVal in
           strongUpdateGlobalBinding var newVal (typeModArray vi.vtype)
         end
 
     | _ -> ());
    DoChildren
      
end


(************************************************************)

(** Make a pass to normalize the globals 
    (1) inline embedded structs 
    (2) collapse array elements  *)
class normalizeGlobals = object (self)
  inherit nopCilVisitor

  method vglob glob : global list visitAction =
    (match glob with
       GVarDecl (vi, _)
     | GVar (vi, _, _) -> 
         if isGlobalDataVar vi then begin
           let var = FVar (vidOfVarinfo vi) in
           let oldVal, _, newVar, _ = getGlobalBinding var GNone in

           if not (eqFVar var newVar) then
             failwith (Printf.sprintf "normalizeGlobals: new var %s vs %s\n"
                         (string_of_var newVar) (string_of_var var));

           let newVal = self#flattenVal oldVal in
           let newVal = self#collapseArrayOffs vi newVal in
           strongUpdateGlobalBinding var newVal (typeModArray vi.vtype)
         end
     | _ -> ());
    DoChildren


  method private flattenVal oldVal = 
    match oldVal with
      Records oldrecs ->
        let recs = List_utils.mapCross flattenRecord oldrecs in
        Records recs 
    | _ -> oldVal 


  method private collapseArrayOffs baseVar oldVal =
    let vtype = Cil.unrollType baseVar.vtype in
    match vtype with
      TArray (bt, _, _) ->
        let wrapAtSize = Cil.bitsSizeOf bt in
        (match oldVal with
           Records recs ->
             logStatusF "Collapsing array elements for %s:%d\n" 
               baseVar.vname baseVar.vid;
             let wrapped = wrapRecords wrapAtSize recs in
             makeNormalizedRec wrapped
         | _ -> 
             logStatusF "Not collapsing array %s -> %s\n" 
               baseVar.vname (string_of_val oldVal);
             oldVal
        )
    | _ -> oldVal
      
end



(************************************************************)


let initGlobals root cg =
  let vis = new initGlobals in
  logStatus "========================================";
  logStatus "Setting initial values for globals w/ Initializers\n";
  Filetools.walkDir 
    (fun ast fname -> 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       flushGlobals ();
    ) root;

  let vis = new initNonFPGlobals in
  logStatus "========================================";
  logStatus "Marking nonFP globals as NFP\n";
  flushStatus ();
  Filetools.walkDir 
    (fun ast fname -> 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       flushGlobals ();
    ) root;

  let vis = new normalizeGlobals in
  logStatus "========================================";
  logStatus "Normalizing globals\n";
  flushStatus ();
  Filetools.walkDir 
    (fun ast fname -> 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       flushGlobals ();
    ) root;
  flushGlobals ();
  printGlobalStats ()


(************************************************************)
(* Init formals of roots *)

let externalNfpSource = (-3)

let getStateFor cg fid =
  let fkey = fid_to_fkey fid in
  match getFormals cg fkey with
    Some (fnode, formals) ->
      let fn, defFile =  fnode.name, fnode.defFile in
      logStatusF "Leaving initial state unconstrained %s : %s\n" fn defFile;
      (try (* Try to reserve a summary slot for this "root" *)
         let readerOpt = None in
         let _ = findContext dummyFunDec readerOpt emptyState fkey fn [] [] in
         failwith "shouldn't be able to find context"
       with ContextNotReady (mContext, sum, sumKey) ->
         (sumKey, sum.fpIn)
      )
        
  | None ->
      (* Don't have function definition? *)
      logError ("Skipping func w/ no body: " ^ (string_of_fkey fkey)); 
      ((fkey, "0"), emptyState)
  

(************************************************************)

(* Instead return ALL the functions, but in bottom-up order *)
let getInitialStates root addrTk cg =
  let reallyDo () =
    initGlobals root cg;
    let visited = Hashtbl.create 10 in
    let worklist = ref [] in
    let rec dfs (fkey, callN) =
      if callN.hasBody then begin
        List.iter 
          (fun calleeK ->
             if Hashtbl.mem visited calleeK then ()
             else begin
               Hashtbl.add visited calleeK ();
               try 
                 let calleeN = FMap.find calleeK cg in
                 dfs (calleeK, calleeN)
               with Not_found -> ()
             end
          ) (calleeKeys callN);
        worklist := getStateFor cg fkey :: !worklist
      end else ()
    in
    List.iter dfs (getRoots cg);
    !worklist
  in
  Fp_rci_unify.doWithoutUnify reallyDo



(* Return all the functions we omitted before simply because they were
   not reachable from what we thought were roots *)
let getRestOfFuncs cg touchedFuncs =
  let maybeRoot = FMap.fold 
    (fun fk node res -> 
       if node.hasBody && not (FSet.mem fk touchedFuncs) 
       then FSet.add fk res
       else res
    ) cg FSet.empty in
  logStatusF "After removing already analyzed funcs: %d\n" 
    (FSet.cardinal maybeRoot);
  logStatusD (d_funcset cg maybeRoot);
  FSet.elements maybeRoot

      
let getMoreStates cg touchedFuncs =
  let reallyDo () =
    let leftovers = getRestOfFuncs cg touchedFuncs in  
    let ret = List.map (getStateFor cg) leftovers in
    ret
  in
  Fp_rci_unify.doWithoutUnify reallyDo


