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


(** "Compile" each source file into basic assignments (argument passing
    for function calls are tagged as part of a function call, but they 
    can be treated as assignments). These basic assignments are 
    treated as constraints in the actual points-to analysis *)

open Cil
open Filetools
open Pta_types

module HC = Simplehc
module CLv = Cil_lvals
module D = Cildump

(* 
   TODO: Support field-based analysis:

   - for each type T and field f, have a variable representing
     t.f for all instances t of type T. 

   - how to cheaply track types and offsets 
     (instead of using original Cil objects?)
      - create type-representative vars during pre-pass and 
        generate assignments for those too? that way, the assignments
        don't need to keep type info?
      - more work to convert to field-sensitive by doing so though
      - also, lose any possible interaction between the f-b and 
        f-i solvers as the f-i solver will not have the same info

   - soundness?
      - unions
      - casting?
         - (struct a)x.f = blah
         - (struct b)x.f = blah  
           (where both struct a and b have f as first field)
         - do a separate cast analysis to conflate fields of types?
      - ptr arith?

   - how to handle assignment from/to complex lvals?
      - e.g., x->f1->f2 = y->f3->f4 ?
      - only look at type of *(x->f1) and *(y->f3)?
      - what extra info would you get by looking a whole access path?

   - how best to solve constraints if both field-based & field-insens.
     information are available? interaction w/ iteration for fun ptrs?
      - do base-case assignments, then basic calls for both before doing 
        complex & funptr calls? actually, the type-based ones should all
        be basic assignments?

*)


(*************** State management **************)

(** Table of base assignments involving each ptaLv *)
let baseAssigns = VarH.create 101

(** Table of complex assignments. Indexed by the base var of the RHS exp *)
let compAssigns = VarH.create 101

(** Table of function calls. Indexed by the base var of call exp/actual exp *)
let calls = VarH.create 101

(** List of functions in the file *)
let funsInFile = ref []


(** Flag: true if assignments should be simplified to have one deref *)
let simplify = ref true

(** Pseudo-var -> orig lval *)
let pseudoToOrig = VarH.create 101

(** Orig lval -> pseudo-var *)
let origToPseudo = LvalH.create 101

let nextTempID = ref 0


let initState () =
  funsInFile := [];
  VarH.clear baseAssigns;
  VarH.clear compAssigns;
  VarH.clear calls;
  nextTempID := 0;
  VarH.clear pseudoToOrig;
  LvalH.clear origToPseudo


let getPTAFile (forFile:string) = 
  changeExtension forFile ".pta"


(** Saves constraints from one file *)
let saveState (forFile:string) =
  let newFile = getPTAFile forFile in
  let out_chan = open_out_bin newFile in
  Marshal.to_channel out_chan 
    (!funsInFile, baseAssigns, compAssigns, calls, pseudoToOrig)
    [Marshal.Closures] ;
  close_out out_chan

    
(** Loads constraints of one file. User is responsible for rehashing *)
let loadState (f:string) = 
  let in_chan = open_in_bin f in
  let funs, baseAss, compAss, callsTab, pseudoTab = 
    (Marshal.from_channel in_chan : 
       funInfo list * 
       (ptaAssign list) VarH.t * 
       (ptaAssign list) VarH.t *
       (ptaCall list) VarH.t *
       ptaLv VarH.t
    ) in
  close_in in_chan;
  (funs, baseAss, compAss, callsTab, pseudoTab)


let rehashFuns funs =
  List.map 
    (fun ({funType = ft; funFormals = ff;} as fi) ->
       {fi with
          funType = rehashType ft;
          funFormals = List.map rehashVar ff;
       }
    ) funs
    
let rehashAssigns ass =
  let rehashedAss = VarH.create ((VarH.length ass) * 2) in
  VarH.iter 
    (fun var assignList ->
       let rehashedAssignList = List.map rehashAssign assignList in
       VarH.replace rehashedAss (rehashVar var) rehashedAssignList
    ) ass;
  rehashedAss

let rehashCalls calls = 
  let rehashedCalls = VarH.create ((VarH.length calls) * 2) in
  VarH.iter
    (fun var callList ->
       let rehashedCallList = List.map rehashCall callList in
       VarH.replace rehashedCalls (rehashVar var) rehashedCallList
    ) calls;
  rehashedCalls
  

let loadFor (forFile:string) =
  let newFile = getPTAFile forFile in
  loadState newFile


(*************** State for current function **********)


let dummyFun = { funId = -1337;
                 funType = dummyTyp;
                 funFormals = [];
               }
    
let curFun : (funInfo ref) = ref dummyFun


(*************** Analysis (compile phase) one file ***)

(* Based on Cil ptranal.ml *)

let isFunPtrType (t : typ) : bool =
  match t with
    TPtr (t, _) -> isFunctionType t
  | _ -> false
      
let isGlobal v =
  (* TODO: have this in one place *)
  (v.vglob) && (not (Trans_alloc.isAllocVar v.vname))

let host_of_var (v : varinfo ) : ptaHost =
  let h = if isGlobal v then 
    PVar (makeVar 
            (PGlobal (v.vid, 
                      cil_type_to_ptype v.vtype)))
  else 
    PVar (makeVar 
            (PLocal  (v.vid, 
                      cil_type_to_ptype v.vtype))) in
  makeHost h

let rec analyze_lval (lv : lval ) : ptaLv list = 
  match lv with
    Var v, off ->
      let baseT = v.vtype in
      [makeLv ((host_of_var v), (makeOff baseT off))]

  | Mem e, off ->
      let lvsInExp = analyze_exp e in
      let baseT = CLv.typeOfLvalUnsafe (Mem e, NoOffset) in
      let ptOff = makeOff baseT off in
      let lvs = List.fold_left 
        (fun curL lv ->
           let newLv = makeDeref lv ptOff in
           newLv :: curL
        ) [] lvsInExp in
      lvs

and analyze_exp (e : exp ) : ptaRv list =
  match e with
    Const (CStr s) ->
      []
  | Const _
  | SizeOf _
  | SizeOfStr _
  | AlignOf _
  | AlignOfE _
  | SizeOfE _ ->
      []

  | Lval l ->
      let lvs = analyze_lval l in
      List.map 
        (fun lv -> makeRv (PLv lv)) lvs

  | UnOp (op, e, t) ->
      analyze_exp e  (* really shouldn't do pointer stuff w/ unop,
                      but they could negate  twice and end up with
                      the pointer again... *)

  | BinOp (op, e, e', t) -> begin 
      match op with
        PlusA
      | MinusA -> 
          let left = analyze_exp e in
          let right = analyze_exp e' in
          left @ right (* lists shouldn't be too long *)
      | PlusPI
      | IndexPI
      | MinusPI -> (* maybe should add LAnd, LOr, Shiftlt, Shiftrt ? *)
          analyze_exp e
            (* ignore rhs (since it should be an int) *)
            
      | _ -> 
          (* assume other ops can't be used w/ the pointer part of ptr arith *)
          []
    end

  | CastE (t, e) -> 
      let rvs = analyze_exp e in
      List.map 
        (fun rv -> 
           makeRv (PCast 
                     ((* makeType t *)
                       dummyTyp, 
                       rv))) rvs

  | AddrOf l
  | StartOf l -> (* functions aren't special for now *)
      let lvsInLv = analyze_lval l in
      let rvs = List.fold_left 
        (fun curL lv ->
           let rval = makeAddr lv in
           rval :: curL
        ) [] lvsInLv
      in
      rvs


let addToCollectionList find replace addonce key value =
  try 
    let old = find key in
    replace key (addonce old value);
  with Not_found ->
    replace key [value]

let addToBase =
  addToCollectionList 
    (VarH.find baseAssigns)
    (VarH.replace baseAssigns)
    addOnceAssign

let addToComp =
  addToCollectionList
    (VarH.find compAssigns)
    (VarH.replace compAssigns)
    addOnceAssign

let addToCall =
  addToCollectionList
    (VarH.find calls)
    (VarH.replace calls)
    addOnceCall

(** Actually add an assignment constraint *)
let doAssign lhs rhs loc =
  let assign = makeAssign lhs rhs loc in
  if isAddrOf rhs then
    let v = baseVarRv rhs in
    addToBase v assign
  else
    let rVar = baseVarRv rhs in
    addToComp rVar assign


(** Make a new temp variable *)
let nextTemp () =
  let i = !nextTempID in
  incr nextTempID;
  makeVar (PTemp i)


(** Map the given lval to its pseudo-variable-lval *)
let getPseudo lval =
  try
    let existingTemp = LvalH.find origToPseudo lval in
    let tempLv = makeLv (makeHost (PVar existingTemp), noOffset) in
    tempLv
  with Not_found ->
    let tempVar = nextTemp () in
    LvalH.add origToPseudo lval tempVar;
    VarH.add pseudoToOrig tempVar lval;

    (* Make an assignment constraint in both directions, to express equality *)
    let tempLv = makeLv (makeHost (PVar tempVar), noOffset) in
    let rval = makeRv (PLv lval) in
    let () = doAssign tempLv rval Cil.locUnknown in
    let tempRv = makeRv (PLv tempLv) in
    let () = doAssign lval tempRv Cil.locUnknown in
    tempLv

let rec simplifyLval (lv : ptaLv) (derefsLeft : int) : ptaLv =
  let host, off = lv in
  match host.HC.node with
    PVar _ -> lv
  | PDeref (rv) ->
      let newRv = simplifyRval rv (derefsLeft - 1) in
      if derefsLeft <= 0 then
        (* Hmm... pseudo var only for host? *)
        let (lvNoOff : ptaLv) = makeLv (makeHost (PDeref newRv), noOffset) in
        let (pseudo : ptaLv) = addLvalOffset (getPseudo lvNoOff) off in
        pseudo
      else
        makeLv (makeHost (PDeref newRv), off)
          
and simplifyRval (rv : ptaRv) (derefsLeft : int) : ptaRv =
  match rv.HC.node with
    PLv lv -> makeRv (PLv (simplifyLval lv derefsLeft))
  | PCast (t, r) -> makeRv (PCast (t, simplifyRval r derefsLeft))
  | PAddrOf lv -> 
      (* Disallow derefs within an addrOf *)
      makeRv (PAddrOf (simplifyLval lv 0))
        
(** Simplify an assignment *)
let rec simplifyAssign lhs rhs =
  let allowedLDerefs, allowedRDerefs = 
    (* If the RHS is an addrOf thing, don't allow any derefs *)
    if isAddrOf rhs then 0, 0 
      (* Can only allow one deref, usually leave it to the LHS *)
    else if isDeref lhs then 1, 0
    else 1, 1 in
  let newLHS = simplifyLval lhs allowedLDerefs in
  let newRHS = simplifyRval rhs allowedRDerefs in
  (newLHS, newRHS)


(** Decide to simplify an assignment, then do the actual assignment *)
let newAssign (lhs:ptaLv) (rhs:ptaRv) loc =
  let newL, newR = if !simplify then simplifyAssign lhs rhs
  else (lhs, rhs) in
  doAssign newL newR loc

(** Begin handling an assignment instruction *)
let handleAssign (lhs : ptaLv list) (rhs : ptaRv list) loc =
  List.iter
    (fun llv ->
       List.iter 
         (fun rlv ->
            newAssign llv rlv loc
         ) rhs
    ) lhs

(* MAKE simplifier connect stuff like ***x to the right temp, otherwise,
   queries directly from the code will not work? *)

let simplifyCallExps cexps =
  List.map (fun lv -> simplifyLval lv 1) cexps

let simplifyCallArgs args =
  List.map 
    (fun (actuals, index) ->
       (List.map (fun rv -> simplifyRval rv 1) actuals, index)) args


(** Make a new assignment induced by a function call (i.e., param passing, 
    or return value capture). The [index] is the index of the 
    assigned-to parameter, or a special index for the return value *)
let newCall (cinfo, args) =
  let newArgs = if !simplify then simplifyCallArgs args else args in

  let newCinfo = if !simplify 
  then { cinfo with cexp = simplifyCallExps cinfo.cexp; } 
  else cinfo in

  (* index by call expression's variable *)
  let cVars = baseVars cinfo.cexp in
  let c = (newCinfo, newArgs) in
  List.iter 
    (fun cv ->
       addToCall cv c
    ) cVars


let analyzeCall retopt fexpr actuals loc =
  let callRvs = analyze_exp fexpr in
  let callLvs = List.fold_left  
    (fun curL rv -> 
       match rv.HC.node with
         PLv lv -> lv :: curL
       | _ -> 
           prerr_string "ignoring call expr that's not an lval\n";
           curL
    ) [] callRvs in


  let ctyp = CLv.typeOfUnsafe fexpr in


  let cinfo = { cexp = callLvs;
                cloc = loc;
                ctype = cil_type_to_ptype ctyp;
              } in
  (* Generate constraints for parameter passing (actuals) *)
  let _, args = List.fold_left 
    (fun (curIndex, curArgs) actExp -> 
       (curIndex + 1,
        (analyze_exp actExp, curIndex) :: curArgs)
    ) (0, []) actuals in
  (* Generate a constraint for the return value *)
  let args = 
    (match retopt with
       Some lv ->
         (analyze_exp (Lval lv), retIndex) :: args 
     | None ->
         args) in
  let args = List.rev args in
  newCall (cinfo, args)


let analyze_instr (i : instr ) : unit =
  match i with
    Set (lval, rhs, loc) ->
      let lvs = analyze_lval lval in
      let rvs = analyze_exp rhs in

      (* TODO: Handle struct copies or does CIL make it explicit? *)
      handleAssign lvs rvs loc
        
  | Call (res, fexpr, actuals, loc) ->
      analyzeCall res fexpr actuals loc

  | Asm _ -> ()
      

let rec analyze_stmt (s : stmt ) : unit =
  match s.skind with
    Instr il -> 
      List.iter analyze_instr il
  | Return (eo, loc) -> begin
      match eo with
        Some e ->
          let fid =  !curFun.funId in
          let retLv = makeLv (makeHost (PVar (makeVar (PRet fid))), 
                              noOffset) in
          let retRHS = analyze_exp e in

  (* TODO: Handle struct copies or does CIL make it explicit? *)

          handleAssign [retLv] retRHS loc
      | None -> ()
    end
  | Goto (s', l) -> 
      () (* don't need to look at target (iterating all stmts anyway) *)
  | If (e, b, b', l) ->
      (* ignore the expression e; expressions can't be side-effecting *)
      analyze_block b;
      analyze_block b'
  | Switch (e, b, sl, l) ->
      analyze_block b;
      List.iter analyze_stmt sl
  | Loop (b, l, _, _) -> 
      analyze_block b
  | Block b -> 
      analyze_block b
  | TryFinally (b, h, _) ->
      analyze_block b;
      analyze_block h
  | TryExcept (b, (il, _), h, _) ->
      analyze_block b;
      List.iter analyze_instr il;
      analyze_block h
  | Break l -> 
      ()
  | Continue l -> 
      ()


and analyze_block (b : block ) : unit =
  List.iter analyze_stmt b.bstmts


let analyze_function (f : fundec ) : unit =
  (* Make the function header *)
  let formals = List.map 
    (fun v -> 
       match (host_of_var v).HC.node with
         PVar x -> x
       | _ -> failwith "host_of_var returned non-var"
    ) f.sformals in
  
  curFun := { funId = f.svar.vid; 
              funType = cil_type_to_ptype f.svar.vtype;
              funFormals = formals;
            };
  funsInFile := !curFun :: !funsInFile;
  
  print_string ("Analyzing function " ^ f.svar.vname ^ "\n");
  analyze_block f.sbody
    
    
let analyze_init (v:varinfo) (i:init) =
  let loc = v.vdecl in (* say that initializer runs at declaration? *)
  let rec loop_init (curLv:ptaLv) (i : init ) =
    match i with
      SingleInit e ->
        let rhses = analyze_exp e in
        List.iter 
          (fun r ->
             newAssign curLv r loc) rhses
    | CompoundInit (t, offInit) ->
        List.iter
          (fun (o, i) ->
             let outerOff = makeOff t o in
             let lhs = addLvalOffset curLv outerOff in
             loop_init lhs i
          ) offInit
  in
  let baseLv = makeLv ((host_of_var v), noOffset) in
  (loop_init baseLv i)


let analyze_global (g : global ) : unit =
  match g with
    GVarDecl (v, loc) -> 
      () (* no new constraints *)
  | GVar (v, init, loc) ->
      begin
        match init.init with
          Some i ->
            analyze_init v i
              
        | None -> () (* no new constraints *)
      end
  | GFun (f, l) ->
      analyze_function f
  | _ ->
      ()


let analyze_file (f : file) : unit =
  print_string ("Analyzing file " ^ f.fileName ^ "\n");
  iterGlobals f analyze_global



(*************** Analyze All *************************)

(** Compile constraints, and place them in files named after the 
    original source. For example if we analyze foo.c and bar.c, we
    will write constraints to foo.pta and bar.pta *)
let analyzeAll (root : string) : unit =
  Filetools.walkDir
    (fun ast file ->
       initState ();
       analyze_file ast;
       saveState file;
    ) root


let singleConsFile = "__pta_constraints"

let getConstraintFile root =
  (Filename.concat root singleConsFile)

(** Compile constraints and place them all in one single file *)
let analyzeAllInOne (root :string) : unit =
  initState ();
  Filetools.walkDir
    (fun ast file ->
       analyze_file ast;
    ) root;
  saveState (getConstraintFile root);
  initState ()

(*************** Check function info ***********)

(** Table of fun id -> funInfo *)
let funTable = Hashtbl.create 101

let funcsUninit () =
  Hashtbl.length funTable == 0

let getFunInfo (id:vid) : funInfo =
  Hashtbl.find funTable id

let isFunc (id:vid) =
  Hashtbl.mem funTable id

(** Reload func info and add to the given funTable *)
let loadFuncTypes root funTable =
  let filename = getConstraintFile root in
  let funs, _, _, _, _ = loadFor filename in
  List.iter 
    (fun finfo ->
       Hashtbl.replace funTable finfo.funId finfo
    ) (rehashFuns funs)

let loadFuncInfo root =
  if funcsUninit () then begin
    print_string "Loading function info\n";
    flush stdout;
    loadFuncTypes root funTable;
  end

let printFunTypes () =
  print_string "Printing function info\n========================\n";
  Pta_types.printFunTypes funTable

(** true if the lval is based on a global but not a function... *)
let rec isGlobalLv (lv : ptaLv) : bool =
  let host, _ = lv in
  match host.HC.node with
    PVar v ->
      (match v.HC.node with
         PGlobal (id, _) -> not (isFunc id)
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


