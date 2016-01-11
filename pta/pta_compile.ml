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
open Simplehc

module Lv = Cil_lvals

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

(*************** File interface / per-file state ****)

(* Table of base assignments involving each ptaLv *)
let baseAssigns = VarH.create 101


(* Table of complex assignments. Indexed by the base var of the RHS exp *)
let compAssigns = VarH.create 101


(* Table of function calls. Indexed by the base var of call exp/actual exp *)
let calls = VarH.create 101


(* List of functions in the file *)
let funsInFile = ref []


let initState () =
  funsInFile := [];
  VarH.clear baseAssigns;
  VarH.clear compAssigns;
  VarH.clear calls


let getPTAFile (forFile:string) = 
  changeExtension forFile ".pta"


(** Saves constraints from one file *)
let saveState (forFile:string) =
  let newFile = getPTAFile forFile in
  let out_chan = open_out_bin newFile in
  Marshal.to_channel out_chan 
    (!funsInFile, baseAssigns, compAssigns, calls)
    [Marshal.Closures] ;
  close_out out_chan

    
(** Loads constraints of one file. User is responsible for rehashing *)
let loadState (f:string) = 
  let in_chan = open_in_bin f in
  let funs, baseAss, compAss, callsTab = 
    (Marshal.from_channel in_chan : 
       funInfo list * 
       (ptaAssign list) VarH.t * 
       (ptaAssign list) VarH.t *
       (ptaCall list) VarH.t 
    ) in
  close_in in_chan;
  (funs, baseAss, compAss, callsTab)


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
                 funType = makeType (TVoid ([]));
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
  (v.vglob) && (not (Trans_alloc.isAllocVar v.vname))

let host_of_var (v : varinfo ) : ptaHost =
  let h = if isGlobal v then 
    PVar (makeVar 
            (PGlobal (v.vid, 
                      (* makeType v.vtype *) 
                      makeType (TVoid []))))
  else 
    PVar (makeVar 
            (PLocal  (v.vid, 
                      (* makeType v.vtype *)
                      makeType (TVoid [])))) in
  makeHost h
    
    
let lv_of_hostOff (ho:ptaHost) (o:Cil.offset) : ptaLv =
  makeLv (ho, makeOff o)

let rec analyze_lval (lv : lval ) : ptaLv list = 
  match lv with
    Var v, off -> 
      [(lv_of_hostOff (host_of_var v) off)]

  | Mem e, off ->
      let lvsInExp = analyze_exp e in
      let lvs = List.fold_left 
        (fun curL lv ->
           let host, innerOff = (makeDeref lv).node in
           let newOff = Cil.addOffset off innerOff.node in
           let newLv = lv_of_hostOff host newOff in
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
                       makeType (TVoid []), 
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

let newAssign (lhs:ptaLv) (rhs:ptaRv) =
  match rhs.node with
    PAddrOf lv -> begin
      let v = baseVar lv in
      try
        let assigns = VarH.find baseAssigns v in
        VarH.replace baseAssigns v (addOnceAssign (lhs, rhs) assigns)
      with Not_found ->
        VarH.add baseAssigns v [(lhs, rhs)]
    end

  (* What if there's a series of casts then an AddrOf? *)
  | _ -> begin
      let rVar = baseVarRv rhs in
      try 
        let assigns = VarH.find compAssigns rVar in
        VarH.replace compAssigns rVar (addOnceAssign (lhs, rhs) assigns)
      with Not_found ->
        VarH.add compAssigns rVar [(lhs, rhs)]
    end

let handleAssign (lhs : ptaLv list) (rhs : ptaRv list) =
  List.iter 
    (fun llv ->
       List.iter 
         (fun rlv ->
            newAssign llv rlv
         ) rhs
    ) lhs


let newCall ((cinfo, index, actExp) as c) =
  (* index by call expression's variable *)
  let cVars = baseVars cinfo.cexp in
  List.iter 
    (fun cv ->
       try 
         let callList = VarH.find calls cv in
         VarH.replace calls cv (addOnceCall c callList)
       with Not_found ->
         VarH.add calls cv [c]
    ) cVars
        

let analyze_instr (i : instr ) : unit =
  match i with
    Set (lval, rhs, loc) ->
      let lvs = analyze_lval lval in
      let rvs = analyze_exp rhs in
      handleAssign lvs rvs
        
  | Call (res, fexpr, actuals, loc) ->
      let callRvs = analyze_exp fexpr in
      let callLvs = List.fold_left  
        (fun curL rv -> 
           match rv.node with
             PLv lv -> lv :: curL
           | _ -> 
               prerr_string "ignoring call expr that's not an lval\n";
               curL
        ) [] callRvs in
      let ctyp = Lv.typeOfUnsafe fexpr in
      let cinfo = { cexp = callLvs;
                    cloc = loc;
                    ctype = makeType ctyp;
                  } in
      (* Generate constraints for actuals and return value *)
      let _ = List.fold_left 
        (fun curIndex actExp -> 
           let callOp = (cinfo, curIndex, analyze_exp actExp) in
           newCall callOp;
           curIndex + 1
        ) 0 actuals in
        
      (match res with
         Some lv ->
           let callOp = (cinfo, retIndex, analyze_exp (Lval lv)) in 
           newCall callOp
       | None ->
           ()
      )

  | Asm _ -> ()
      

let rec analyze_stmt (s : stmt ) : unit =
  match s.skind with
    Instr il -> 
      List.iter analyze_instr il
  | Return (eo, loc) -> begin
      match eo with
        Some e ->
          let fid =  !curFun.funId in
          let retLv = makeLv (makeHost 
                                (PVar (makeVar (PRet fid))), 
                              makeOff NoOffset) in
          let retRHS = analyze_exp e in
          handleAssign [retLv] retRHS
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
       match (host_of_var v).node with
         PVar x -> x
       | _ -> failwith "host_of_var returned non-var"
    ) f.sformals in
  
  curFun := { funId = f.svar.vid; 
              funType = makeType f.svar.vtype;
              funFormals = formals;
            };
  funsInFile := !curFun :: !funsInFile;
  
  print_string ("Analyzing function " ^ f.svar.vname ^ "\n");
  analyze_block f.sbody
    
    
let analyze_init (v:varinfo) (i:init) =
  let rec loop_init (curLv:ptaLv) (i : init ) =
    let host, off = curLv.node in
    match i with
      SingleInit e ->
        let rhses = analyze_exp e in
        List.iter 
          (fun r ->
             newAssign curLv r) rhses
    | CompoundInit (t, offInit) ->
        List.iter
          (fun (o, i) -> 
             let lhs = lv_of_hostOff host (Cil.addOffset o off.node) in
             loop_init lhs i
          ) offInit
  in
  (loop_init (lv_of_hostOff (host_of_var v) NoOffset) i)


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
  iterGlobals f analyze_global



(*************** Analyze All *************************)

let analyzeAll (root : string) : unit =
  Filetools.walkDir 
    (fun ast file ->
       initState ();
       analyze_file ast;
       saveState file;
    ) root
    
