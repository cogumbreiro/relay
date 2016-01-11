open Cil

module D = Cildump
module PTA = Myptranal

(*** Naming fresh global vars (used to substitute/represent alloc calls) ***)

(* use this regexp to grab the first portion of the filename that is
   valid for variable names *)
let varnameRegExp = Str.regexp "[_a-zA-Z][_a-zA-Z0-9]*"

let allocNameRegExp = Str.regexp "_a[0-9]+_[0-9]*_[_a-zA-Z][_a-zA-Z0-9]*_[0-9]*"

let nameLine ln =
  string_of_int ln

let nameByte b =
  if (b > 0) 
  then (string_of_int b)
  else ""
    
let nameFile f =
  let baseF = Filename.basename f in
  (if (Str.string_match varnameRegExp baseF 0) then
     Str.matched_string baseF
   else "alloc")
    
let nameAllocVar ( { line = ln;
                     file = f;
                     byte = b; } :location) (moreID : int) =
  "_a" ^ nameLine ln ^ "_" ^ 
    nameByte b ^ "_" ^
    nameFile f ^ "_" ^ 
    (string_of_int moreID)
    
let isAllocVar (n:string) : bool =
  Str.string_match allocNameRegExp n 0

(** Reversing the process. How to find out if an assignment was
    original a malloc call, given the RHS of the assignment. *)
let rec findMalloc exp =
  match exp with
    AddrOf (Var v, _) 
  | StartOf (Var v, _) ->  if isAllocVar v.vname then Some (v) else None
  | AddrOf (Mem _, _)
  | StartOf (Mem _, _)
  | Lval _ -> None
  | CastE (t, e) -> findMalloc e
  | Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _ 
  | AlignOf _ | AlignOfE _ | UnOp _ | BinOp _  -> None

let unknownMallocType = 
  (* turn void types in arrays of uchar (arbitrary, but unlikely size) *)
  TArray (TInt (IUChar, []), Some (Cil.integer 231), [])

 
(*** Simple wrapper around PTA call ***)
   
let collectFunsOfType ftype_str curList fdec =
  let ft = fdec.svar.vtype in
  let fts = D.string_of_ftype ft in
  if (fts = ftype_str) then fdec.svar.vname :: curList
  else curList

let resolveFP (exp:exp) : string list =
  let ftype = typeOf exp in
  let ftype_str = D.string_of_ftype ftype in    
  let fdecs = PTA.resolve_funptr exp in
  List.fold_left (collectFunsOfType ftype_str) [] fdecs

let warnHasMultipleSizeOf exp =
  let rec loop cur exp =
    match exp with
      SizeOf _ | SizeOfE _ | SizeOfStr _ -> cur + 1
    | Const _ | Lval _ | AddrOf _ | StartOf _ | AlignOf _ | AlignOfE _ -> cur
    | CastE (_, e) -> loop cur e
    | UnOp (_, e, _) -> loop cur e
    | BinOp (_, e1, e2, _) -> 
        let c = loop cur e1 in
        loop c e2
  in
  if loop 0 exp > 1 then
    Printf.fprintf stderr "Alloc: multiple sizeof (%s) @ %s\n"
      (D.string_of_exp exp) (D.string_of_loc !currentLoc)

let rec typeFromSizeOf cur exp =
  match cur with 
    Some x -> cur
  | None ->
      try findSizeOf exp
      with Not_found -> cur

and findSizeOf exp =
  match exp with
    SizeOf t -> Some t
  | SizeOfE e -> Some (typeOf e)
  | SizeOfStr _ -> Some (!stringLiteralType)
  | CastE (_, e2) -> findSizeOf e2
  | BinOp (op, e1, e2, _) ->
      (try
         let sz1 = findSizeOf e1 in
         try 
           let _ = findSizeOf e2 in
           warnHasMultipleSizeOf exp;
           sz1
         with Not_found ->
           sz1
       with Not_found ->
         findSizeOf e2)
  | UnOp (op, e1, _) ->
      findSizeOf e1
  | AlignOf _ | AlignOfE _ | Const _
  | Lval _ | AddrOf _ | StartOf _ -> raise Not_found


let pickType retType actuals =
  match List.fold_left typeFromSizeOf None actuals with
    None -> if (isVoidType retType) then unknownMallocType else retType
  | Some t ->
      if Ciltools.compare_type retType t <> 0 then
        Printf.fprintf stderr "Trans_alloc: %s <> %s\n" 
          (D.string_of_type retType) (D.string_of_type t);
      (* Should do something smarter to pick the widest? *)
      if (isVoidType t) then
        if (isVoidType retType) then unknownMallocType
        else retType
      else t


(*** The actual transformation ***)

(** A visitor that converts p = alloc(x), into p = &fresh_global *)
class allocVisitor = object (self)
  inherit nopCilVisitor 
        
  val newVars = ref []

  val alreadyUsedLocs = Hashtbl.create 11

  method checkUsed loc =
    let oldID = try Hashtbl.find alreadyUsedLocs loc with Not_found -> 0 in
    let newID = oldID + 1 in
    Hashtbl.replace alreadyUsedLocs loc newID;
    newID

  method vinst (i:instr) : instr list visitAction = 
    let rec resolveCall (exp:exp) : string list = 
      match exp with
        Lval(Var(va), NoOffset) ->
          [va.vname]

      | Lval(Mem(ptrexp), NoOffset) ->
          resolveFP ptrexp

      | CastE(t, e) ->
          resolveCall e
      | _ -> 
          prerr_string "allocVisitor: unknown callexp form\n";
          []
    in
    match i with
      Call (Some(retval), callexp, actuals, loc) ->
(* TODO: translate even if retType is Int and call is to a malloc'er *)
        let retType = typeOfLval retval in begin
        match retType with
          TPtr (t, _) ->
            let fnames = resolveCall callexp in
            if (List.exists (* not a forall? watch out on empty lists... *)
                  (fun fn -> Alloc.isAlloc fn) fnames) then
              let moreID = self#checkUsed loc in
              let name = nameAllocVar loc moreID in
              let finalT = pickType t actuals in
              let freshAllocVar = Cil.makeGlobalVar name finalT in
              newVars := freshAllocVar :: !newVars;
              ChangeTo 
                [Set (retval, Cil.mkAddrOf(Var(freshAllocVar), NoOffset), loc)]
            else
              SkipChildren
                
        | _ -> SkipChildren
        end                                                     
    | _ -> 
        SkipChildren 


  method vglob (g:global) : global list visitAction =
    let addDecls (parentFun:fundec) (loc:location) (globs:global list) =
      if (!newVars <> []) then begin
        (* Hack to know what the malloc callers are *)
        Printf.fprintf stderr "Trans_alloc: %s %s\n" parentFun.svar.vname 
          (D.string_of_loc loc);

        (* add global var decls to the list of globals *)
        let newGs = List.fold_left 
          (fun curGlobs newVar ->
             newVar.vdecl <- loc;
             GVarDecl (newVar, loc) :: curGlobs) globs !newVars in
        newVars := [];
        newGs
      end
      else
        globs
    in
    
    match g with
      GFun (f, loc) -> (* only place that can have instructions? *)
        ChangeDoChildrenPost ([g], addDecls f loc)
    | _ ->
        SkipChildren

          
end

     
(**  Entry point -- ASSUMES PTA analysis has been run
     @arg f   a parsed Cil file  *)      
let transformAlloc (f:file) =
  let vis = new allocVisitor in
  visitCilFile (vis :> cilVisitor) f



(*** Cil feature descriptor API ***)

let doTransformAllocs = ref false

let feature : featureDescr = 
  { fd_name = "trans_alloc";
    fd_enabled = doTransformAllocs;
    fd_description = "Transforms invocations of alloc functions to &global";
    fd_extraopt = 
      [];
    fd_doit = transformAlloc;
    fd_post_check = true
  } 
