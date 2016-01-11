
(***** Shared state, utils, and debug code *****)

open Pta_types
open Pta_compile
open Cil

(*************** Global function info ***********)

(** Table of fun id -> funInfo *)
let funTable = Hashtbl.create 101

let funcsUninit () =
  Hashtbl.length funTable == 0

let getFunInfo (id:vid) : funInfo =
  Hashtbl.find funTable id

let isFunc (id:vid) =
  Hashtbl.mem funTable id

let getFunType fid = 
  let finfo = Hashtbl.find funTable fid in
  finfo.funType

(** Reload func info into the global funTable *)
let loadFuncTypes root =
  let filename = getConstraintFile root in
  let funs, _, _, _, _ = loadFor filename in
  Hashtbl.iter 
    (fun fid finfo ->
       let finfo = { finfo with 
                       funType = rehashType finfo.funType;
                       funFormals = List.map rehashVar finfo.funFormals; } in
       let finfo = 
         try
           let old = Hashtbl.find funTable fid in
           combineFunInfo old finfo
         with Not_found -> finfo in
       Hashtbl.replace funTable fid finfo
    ) funs

let loadFuncInfo root =
  if funcsUninit () then begin
    print_string "Loading function info\n";
    flush stdout;
    loadFuncTypes root;
  end

let printFunTypes root =
  loadFuncInfo root; (* just in case *)
  print_string "Printing function info\n========================\n";
  Pta_types.printFunTypes funTable



(*** Mapping for temporary variables used to simplify constraints ***)

(** Table of fun id -> funInfo *)
let tempVars = Hashtbl.create 101

let tempsUninit () =
  Hashtbl.length tempVars == 0

let getLvOfTemp tempID =
  Hashtbl.find tempVars tempID

let loadTempVars root =
  if tempsUninit () then begin
    let tempIDOfVar var =
      match var.HC.node with
        PTemp tid -> tid
      | _ -> failwith "Nontempvar in tempvar table?"
    in
    print_string "Loading tempVar info\n";
    flush stdout;
    let filename = getConstraintFile root in
    let _, _, _, _, temps = loadFor filename in
    VarH.iter 
      (fun vi lv ->
         Hashtbl.replace tempVars (tempIDOfVar vi) (rehashLv lv)
      ) temps
  end

(*** Centralize load shared ***)

let loadSharedState root =
  loadFuncInfo root;
  loadTempVars root


(************************************************************)

let isAllocVar vid =
  let vi = Cilinfos.getVarinfo vid in
  Trans_alloc.isAllocVar vi.vname

(** true if the lval is based on a global but not a function... *)
let rec isGlobalLv (lv : ptaLv) : bool =
  let host, _ = lv in
  match host.HC.node with
    PVar v ->
      (match v.HC.node with
         PGlobal (id, _) -> 
           not (isFunc id) && not (isAllocVar id)
       | _ -> false
      )
  | PDeref ptrRv ->
      isGlobalRv ptrRv

and isGlobalRv (rv : ptaRv) : bool = 
  match rv.HC.node with
    PLv lv -> 
      isGlobalLv lv
  | PAddrOf lv ->
      isGlobalLv lv
  | PCast (_, rv) ->
      isGlobalRv rv

let isFuncLv (lv : ptaLv) : bool =
  let host, _ = lv in
  match host.HC.node with 
    PVar v ->
      (match v.HC.node with
         PGlobal (id, _) -> isFunc id
       | _ -> false)
  | PDeref _ -> false


(*************** Debug *************************)

(** Assume all constraints in one file *)
let printConstraints root =
  print_string "PTA constraints:\n";
  let numBase = ref 0 in
  let numComplex = ref 0 in
  let numCallCons = ref 0 in
  let numPseudo = ref 0 in

  (* Assume it ran in one-file mode... 
     TODO: check disk to see if that is the case *)
  
  let filename = getConstraintFile root in
  let _, baseAssign, complexAssign, calls, pseudo = loadFor filename in
  print_string "Base cons:\n";
  VarH.iter
    (fun vinfo aList ->
       List.iter 
         (fun a -> printAssignment a; incr numBase) 
         aList
    ) baseAssign;
  
  print_string "\nComplex cons:\n";
  VarH.iter
    (fun vinfo aList ->
       List.iter 
         (fun a -> printAssignment a; incr numComplex) aList
    ) complexAssign;

  print_string "\nCall cons:\n";
  VarH.iter
    (fun vinfo callList ->
       List.iter
         (fun call -> printCallCons call; incr numCallCons) callList
    ) calls;

  print_string "\nTemp var -> lvals:\n";
  VarH.iter
    (fun vinfo lv ->
       printVinfo vinfo;
       print_string " <- ";
       printPtaLv lv;
       print_string "\n";
       incr numPseudo;
    ) pseudo;
  Printf.printf "=============================\n Constraint STATS\n";
  Printf.printf "Base constraints:\t %d\n" !numBase;
  Printf.printf "Complex constraints:\t %d\n" !numComplex;
  Printf.printf "Call constraints:\t %d\n" !numCallCons;
  Printf.printf "Temp constraints:\t %d\n" !numPseudo;
  print_string "\n\n"


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


(********** See if lval and rval hit function pointers ***********)

let rec typeOfVar vinfo =
  match vinfo.HC.node with
    PGlobal (_, t)
  | PLocal (_, t) -> t
  | PRet funID ->
      let funInfo = getFunInfo funID in
      (match funInfo.funType.HC.node with
         PFun (retType, _, _)-> retType
       | _ -> failwith "Fun doesn't have fun type?")
  | PTemp tempID ->
      let targetLv = getLvOfTemp tempID in
      typeOfVar (baseVar targetLv)

let hitsFunptrDeepHelper curType =
  Type_reach.hitsFunptrDeep (ptype_to_cil_type curType)

let hitsFunptrDeepLv lv =
  hitsFunptrDeepHelper (typeOfVar (baseVar lv))

let hitsFunptrDeepRv rv =
  hitsFunptrDeepHelper (typeOfVar (baseVarRv rv))
