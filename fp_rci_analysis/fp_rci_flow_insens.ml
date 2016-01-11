
(** Flow insensitive information *)

open Cil
open Pretty
open Logging
open Type_utils 
open Fp_rci_types
open Fp_rci_unify_structs



(************************************************************
  Relevant context information
 ************************************************************)

(* (1) alias assumptions 
   - caller determines and seeds "may equal" constraints
   - callee states "not equal" constraints that were assumed

   (2) input-based access paths of needed function pointers *)
  
type funPtrRequire = fvalue VarMap.t

type collectedFPReq = VarSet.t (* collection used to delay marking
                                  input-based FPs as required *)

(* Track the callee end of things in DF info.
   Caller end is communicated by seeding initial DF info *)
    
(** Set of access paths that may be considered non-aliased in the future and 
    have been modified thus far (just for globals) *)
type mods = VarSet.t

type modIn = OffsetSet.t VarMap.t

(* one per function (context) *)
type fpSideCond = {
  initialFP : funPtrRequire;
  nonAliasG : mods;
  modIn : modIn;
  svTypes : varTypes; 
  (* copy of varTypes that is LUB over progpoints where side cond is observed *)

  aliasedIn : unifLocMap; (* variables in same aliasing equiv. class *)
}

let emptySide () = 
  { initialFP = VarMap.empty;
    nonAliasG = VarSet.empty;
    modIn = VarMap.empty; 
    svTypes = VarMap.empty;
    aliasedIn = UnifLocs.makeMappings 0;
  }

type initialState = fpState * fpSideCond


(************************************************************)

let d_initialFPs fids =
  seq_to_doc (text ", ") 
    List.iter (fun vid -> 
                 let vi = varinfoOfVid vid in
                 text vi.vname) fids (text " -> ")
    
let printInitialFP initialFP =
  VarMap.iter (fun var v -> printVar var; printVal v None) initialFP

let doc_of_aliasg printer nonAliasG =
  (seq_to_doc (text ", ") VarSet.iter 
     (fun v -> printer#d_var v) nonAliasG nil ++ line)
    

let printNonAliasG nonAliasG =
  logStatusD (doc_of_aliasg defPrint nonAliasG)

let d_offsetset printer offs =
  seq_to_doc (text ", ") OffsetSet.iter
    (fun o -> printer#d_rawoff nil o) offs nil

let string_of_offsetset offs =
  sprint 80 (d_offsetset defPrint offs)

let d_modIn printer modIn =
  (map_to_doc line VarMap.iter 
     (fun var offs -> printer#d_var var ++ text " -> " 
        ++ d_offsetset printer offs)  modIn nil ++ line)

let printModIn modIn =
  logStatusD (d_modIn defPrint modIn)

let printAliasIn aliasIn =
  UnifLocs.printMap aliasIn

let printSideCond side =
  logStatusF "Initial FPs: (%d)\n" (Stdutil.mapSize side.initialFP VarMap.fold);
  printInitialFP side.initialFP;
  logStatusF "Global mods: (%d)\n" (VarSet.cardinal side.nonAliasG);
  printNonAliasG side.nonAliasG;
  logStatusF "Input mods: (%d)\n" (Stdutil.mapSize side.modIn VarMap.fold);
  printModIn side.modIn

let d_varTypes vtypes =
  (map_to_doc (text ", ") VarMap.iter 
     (fun var tid -> defPrint#d_var var ++ text " : " 
        ++ defPrint#d_typ tid ()) vtypes nil ++ line)

let printVarTypes vtypes =
  logStatusD (d_varTypes vtypes)


let printSideCondVerb side =
  printSideCond side;
  logStatusF "Side atts: (%d)\n" (Stdutil.mapSize side.svTypes VarMap.fold);
  printVarTypes side.svTypes

    
let printInitialState (st, side) =
  logStatus "-------";
  if (isBottomState st) then
    logStatus "State is $BOTTOM\n"
  else if (isTopState st) then
    logStatus "State is $TOP\n"
  else begin
    printStateCore st;
  end;
  printSideCond side;
  logStatus "-------"

(************************************************************)

let rawdoc_of_initialfp initialFP =
  rawdoc_of_store initialFP nil

let rawdoc_of_aliasg nonAliasG =
  doc_of_aliasg rawPrint nonAliasG

let rawdoc_of_modIn modIn =
  d_modIn rawPrint modIn
  
let rawdoc_of_aliasin unifMap = 
  failwith "TODO"

(************************************************************)

(** Return true if the value holds any function pointer values
    (values that affect control flow) *)
let rec hasControlFlowValue v =
  match v with 
    FNFP _ | FInt _ -> false
  | FpRef var -> not (isSpecialVar var)
  | Refs locs -> FLocSet.exists isFunc locs
  | Records recs -> List.exists hasControlFlowRecord recs
  | NCRecord r -> hasControlFlowRecord r
  | FIRecs v -> hasControlFlowValue v

and hasControlFlowRecord (fm, _) =
  OffsetMap.exists (fun o v -> hasControlFlowValue v) fm

let hasNoControlFlow sideCond =
  not (VarMap.exists (fun var v -> hasControlFlowValue v) sideCond.initialFP)
