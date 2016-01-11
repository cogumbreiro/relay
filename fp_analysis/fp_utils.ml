
open Cil
open Callg
open Type_utils
open Fp_types
open Fp_lattice_ops
open Fp_store
open Globals_ref
open Logging
open Cildump

(*********************************************************
 * Expression Evaluation
 *********************************************************)

(********* Ptr arithmetic ***********)

exception NotPtrConstPair

let offsetFromInt expectedT ptrT i =
  let bitsForI = (bitsSizeOf ptrT) * (Int64.to_int i) in
(*
  let containerSize = bitsSizeOf expectedT in
  if containerSize == 0 then begin
    logError ("offsetFromInt: didn't wrap / size = 0 @ " ^ 
                  (string_of_loc !currentLoc));
    bitsForI
  end else 
    bitsForI mod containerSize
*)
  bitsForI

(*
(** Update an offest given an integer valued pointer offset [i]. *)
let ptrFromOffset lookHook expectedT ptrT (i:int64) (v, off) =
  if isTopOffset off then 
    if isSpecialVar v then (v, noOff) else (v, topOff)
  else begin
    let bitsBase = off in
    let bitsForI = (bitsSizeOf ptrT) * (Int64.to_int i) in
    let containerSize = bitsSizeOf expectedT in
    let newBaseBits = 
      if containerSize == 0 then begin
        logError ("ptrFromOffset: didn't wrap / size = 0 @ " ^ 
                    (string_of_loc !currentLoc));
        (bitsBase + bitsForI)
      end else (* not using concatOffset... to get the mod *)
        (bitsBase + bitsForI) mod containerSize in
    (* hack... *)
    let diffOff = newBaseBits - bitsBase in
    if not (isNoOffset diffOff) then lookHook (AConcatOff diffOff);
    (* /hack *)
    (v, newBaseBits)
  end
*)
    (* how to detect if the containerSize is bad? *)
    (* how about multiply the containerSize * 2 before doing the mod
       so that we "unroll" the array by 2 at least? Does that help
       with the allocators that do:

       (header * )malloc(sizeof(header) + reqsize) ? 

       Only if the field accesses for the requested object are done 
       with static offsets (in which case we don't wrap w/ the mod). 
       If calculation of the field offset is through pointer arithmetic
       the we end up going here and wrapping back all the crap that
       is in the requested object but beyond the sizeof(header)! 
       Hmm... it is unsound to have this kind of inconsistency between
       ptr arith-based access and non-ptr-arith-based though...

       No, that's not even good enough because the pointer to
       the area beyond the header is a char*:

       node->first_avail = (char * )node + 
                  ((sizeof(apr_memnode_t ) + 7U) & 4294967288U);
       
       The ptrType is also (char * ). Only the raw type for "node" 
       indicates the right size... Maybe we shouldn't be doing the 
       mod here at all, and only do it at merge points.

       The whole reason we do it is for termination anyway?
    *)

let isArrayStride (v, o) ptrT expectedT =
  (* Heuristic check if it's striding array-style by checking ptrT? *)
  try
    Ciltools.compare_type (canonType (typeOfFVar v)) ptrT == 0 ||
      Ciltools.compare_type (canonType (typeModArray 
                                          (typeOfLoc (v, o)))) ptrT == 0 ||
      Ciltools.compare_type ptrT expectedT == 0
  with UnknownType ->
    Ciltools.compare_type ptrT expectedT == 0
      

(** Calculate the new pointer created by ptr arith. 
    Assumes v1 is a pointer and v2 is a constant *)
let doPtrArith lookHook (expectedT:Cil.typ) (ptrT:Cil.typ) (v1, v2) =
  match v1, v2 with
    FNFP _, _ -> v1
  | FpRef x, _ -> 
      logErrorF "Ptr arith on funptr @ %s\n" (string_of_loc !currentLoc);
      v1
  | Refs ls, FNFP _ ->
      let ptrT = typeModArray (canonType ptrT) in
      let expectedT = typeModArray (canonType expectedT) in
      let newSet = FLocSet.fold
        (fun (v, off) cur -> 
           let newOff = if isSpecialVar v then off 
           else 
             if isArrayStride (v, off) ptrT expectedT
             then off else topOff 
           in
           FLocSet.add (v, newOff) cur) ls FLocSet.empty in
      Refs newSet

  | FInt _, FNFP _ ->
      let ptrT = typeModArray (canonType ptrT) in
      let expectedT = typeModArray (canonType expectedT) in
      (match ptrT with
         TPtr (_,_) -> 
           if Ciltools.compare_type ptrT expectedT == 0 then v1
          else raise NotPtrConstPair
       | _ -> raise NotPtrConstPair)

  | Refs ls, FInt i -> 
      let ptrT = typeModArray (canonType ptrT) in
      let expectedT = typeModArray (canonType expectedT) in
      let moreOff = offsetFromInt expectedT ptrT i in
      if not (isNoOffset moreOff) then lookHook (AConcatOff moreOff);
      let newSet = FLocSet.fold 
        (fun (v, off) cur ->
           let off = if isArrayStride (v, off) ptrT expectedT
           then off else concatOffsetVar v moreOff off in
           FLocSet.add (v, off) cur) ls FLocSet.empty in
      Refs newSet
  | Refs ls1, Refs ls2 ->
      (* Check if it's striding array-style by checking ptrT? *)
      if Ciltools.compare_type expectedT ptrT == 0 then v1
      else begin
        logErrorF "doPtrArith ref + ref @ %s?\n" (string_of_loc !currentLoc);
        v1
      end
  | FInt _, FInt _ -> raise NotPtrConstPair
  | _, FInt i -> failwith "doPtrArith missing case x + Int"
  | x, _ -> 
      if isReference x then begin
        failwith (Printf.sprintf "doPtrArith case ptr + ?: %s + %s @ %s\n"
                    (string_of_val v1) (string_of_val v2) 
                    (string_of_loc !currentLoc))
      end else raise NotPtrConstPair


(** Out of a list of types, pick a Ptr type that is somewhat specific
    and return the deref'ed type *)
let rec pickDerefedTyp tList =
  match tList with
    t :: [] -> ( match t with
                   TPtr (TVoid _, _) -> TVoid []
                 | TPtr (t', _) -> t'
                 | _ -> logError "pickDerefedType left with nonptr"; t )
  | t :: tl -> ( match t with
                   TPtr (TVoid _, _) -> pickDerefedTyp tl
                 | TPtr (t', _) -> t'
                 | _ -> pickDerefedTyp tl )                 
  | [] -> failwith "pickDerefedTyp: given an empty list!"


(** Reorder a pair of values so that a ptr value comes first, then
    a constant comes second. Also returns a flag if swapped
    May raise NotPtrConstPair if arg isn't a ptr/constant pair *)
let reorderPtrConstPair (v1, v2) : fvalue * fvalue * bool =
  match (v1, v2) with 
    Refs _ , FInt _ 
  | FpRef _, FInt _ -> (v1, v2, false)
  | FInt _, Refs _ 
  | FInt _, FpRef _ -> (v2, v1, false)
  | FInt i1, FInt i2 ->
      (* TODO: test this out more... *)
      if i1 = Int64.zero then (v1, v2, false) else (v2, v1, true)
  | _ ->
      raise NotPtrConstPair
  

(********* Converting Cil expressions ************)

let rec constToVal c =
  match c with 
    CInt64 (i64, _, _) -> FInt i64
  | CChr (c) -> constToVal (charConstToInt c)
  | CEnum (e, _, _) -> (match Cil.constFold true e with
                          Const c -> constToVal c
                        | _ -> FNFP constNfpSource)
  | _ -> FNFP constNfpSource

(** Assuming the int64 i represents a bool, invert it *)
let not_of_int64 i = 
  if (i == Int64.zero) then Int64.one
  else  Int64.zero

(* Misc support *)

let flocsMap foo set =
  FLocSet.fold (fun x cur -> FLocSet.add (foo x) cur) set FLocSet.empty

(** Make a reader out of given function and program point 
    (the eval itself finds the offset and width) *)
let makeReaderOpt readOpt off sinfo =
  match readOpt with
    Some x -> GReader (x, off, sinfo)
  | None -> GNone


type evalMisc = {
  lookupHook : lookupHook;
  readerOpt : greader option;
  assumedTyp : typ option;
}

let emptyMisc = {
  lookupHook = nilLookupHook;
  readerOpt = None;
  assumedTyp = None;
}

(****************)

(** Evaluate a cil expression to a value. Takes a hook for lookups *)
let rec eval s inExp misc = 
  match inExp with
    (* Variable reference *)
    Lval(Var(vi), off) ->
      let lookupHook = misc.lookupHook in
      let readerOpt = misc.readerOpt in
      let var, off = getLocation vi off in
      lookupHook (AReadVar var);
      lookupHook (AReadField off);
      let structInfo = getStructInfo (Cil_lvals.typeOfUnsafe inExp) in
      lookupVal s var off structInfo (makeReaderOpt readerOpt off structInfo)
        
  (* Simple pointer dereference *)
  | Lval(Mem(ptrExp), off) -> 
      let ptrVal, newOff, s = getPtrVal s ptrExp off misc in
      let structInfo = getStructInfo (Cil_lvals.typeOfUnsafe inExp) in
      derefPtrVal s structInfo ptrExp newOff ptrVal misc

  (* Take addr of a lval/array*)
  | AddrOf (l) 
  | StartOf(l) -> resolveAddrOfLval s l misc

  (* Re-eval w/ an assumed type *)
  | CastE(t, e) -> eval s e { misc with assumedTyp = Some t; }

  (* Pointer arith *)
  | BinOp(PlusPI, ce1, ce2, typ) 
  | BinOp(IndexPI, ce1, ce2, typ) ->
      (* TODO: separate PlusPI and IndexPI to support the idiom for
         one-shot mallocs ?:

         struct p {

           int a;
           struct B* Bfield;
           struct C* CField;
           char *z;
           ...
         }

         p = malloc(sizeof(struct p) + sizeof(B) + sizeof(C) + n + 1);
         p->Bfield = &p[1];   
         ...
      *)
      evalPtrArithPlus s ce1 ce2 typ misc
  | BinOp(MinusPI, ce1, ce2, typ) ->
      let ce2 = UnOp (Neg, ce2, Cil_lvals.typeOfUnsafe ce2) in
      evalPtrArithPlus s ce1 ce2 typ misc

  (* May be pointer arith... check *)
  | BinOp(PlusA, ce1, ce2, typ) ->
      evalPtrArithPlusA s ce1 ce2 typ misc

  | BinOp(MinusA, ce1, ce2, typ) -> 
      let ce2 = UnOp (Neg, ce2, Cil_lvals.typeOfUnsafe ce2) in
      evalPtrArithPlusA s ce1 ce2 typ misc

  (* Ops that aren't ptr arith *)
  | UnOp(op, e, _) ->
      let v, s = eval s e misc in
      let newV = 
        (match v, op with
           FInt i, Neg -> FInt (Int64.neg i)
         | FInt i, BNot -> FInt (Int64.lognot i)
         | FInt i, LNot -> FInt (not_of_int64 i)
         | FNFP _, _ -> v
         | _, _ -> FNFP constNfpSource) in
      newV, s

  | BinOp(op, e1, e2, _) -> 
      let v1, v2, s = eval2 s e1 e2 misc in
      let newV = 
        (match v1, v2, op with
           FInt i1, FInt i2, PlusA -> FInt (Int64.add i1 i2)
         | FInt i1, FInt i2, MinusA -> FInt (Int64.sub i1 i2)
         | _, FInt i2, MinusPP ->
             if (Int64.compare Int64.zero i2 == 0) then v1
             else 
               (logErrorF "unsure of pointer distance %s\n" 
                  (string_of_exp inExp);
                FNFP constNfpSource)
                 (* other cases (e.g., NULL - p)? *)
                 
         | FInt i1, FInt i2, Mult -> FInt (Int64.mul i1 i2)
         | FInt i1, FInt i2, Div -> 
             (try FInt (Int64.div i1 i2) 
              with Division_by_zero -> FNFP constNfpSource)
         | FInt i1, FInt i2, Mod -> 
             (try FInt (Int64.rem i1 i2) 
              with Division_by_zero -> FNFP constNfpSource)
         | FInt i1, FInt i2, Shiftlt -> FInt (Int64.shift_left i1 
                                                (Int64.to_int i2))
         | FInt i1, FInt i2, Shiftrt -> FInt (Int64.shift_right i1 
                                                (Int64.to_int i2))
         | FInt i1, FInt i2, BOr -> FInt (Int64.logor i1 i2)
         | FInt i1, FInt i2, BXor -> FInt (Int64.logxor i1 i2)
         | FInt i1, FInt i2, BAnd -> FInt (Int64.logand i1 i2)
             (* not folding the compare ops... *)
         | _, _, _ ->
             FNFP constNfpSource) in
      newV, s

  (* Misc machine dependent stuff *)
  | AlignOfE _  | AlignOf _ | SizeOfE _ | SizeOf _ | SizeOfStr _ ->
      (match Cil.constFold true inExp with
         Const c -> (constToVal c, s)
       | _ ->
           logError "Not const folded: machdep";
           (FNFP constNfpSource, s))
  | Const c -> (constToVal c, s)


and getPtrVal s ptrExp cilOff misc =
  let ptrVal, s = eval s ptrExp { misc with assumedTyp = None; } in
  let baseTyp = Cil_lvals.typeOfLvalUnsafe (Mem ptrExp, NoOffset) in
  let newOff = cilOff2Offset baseTyp cilOff in
  ptrVal, newOff, s

and evalPtrArithPlus s ptrExp constExp resultT misc =
  let v1, v2, s = eval2 s ptrExp constExp misc in
  let ptrTyp = pickDerefedTyp [Cil_lvals.typeOfUnsafe ptrExp] in
  try evalPtrArithPlusVals s v1 v2 ptrTyp resultT misc
  with NotPtrConstPair ->
    match v1, v2 with
      FInt i1, FInt i2 -> FInt (Int64.add i1 i2), s
    | _, _ ->
        logErrorF "Ptr-arith can't eval %s + %s @ %s\n" (string_of_exp ptrExp)
          (string_of_exp constExp) (string_of_loc !currentLoc);
        (v1, s)
          (* Or make it a null ptr? *)
          
and evalPtrArithPlusA s ce1 ce2 resultTyp misc =
  let v1, v2, s = eval2 s ce1 ce2 misc in
  (* Maybe it's ptr arith -- but probably not... *)
  match v1, v2 with
    FInt i1, FInt i2 -> (FInt (Int64.add i1 i2), s)
  | _, _ -> 
      try 
        let v1, v2, switched = reorderPtrConstPair (v1, v2) in
        let ptrTyp = 
          if (switched) then pickDerefedTyp [Cil_lvals.typeOfUnsafe ce1]
          else pickDerefedTyp [Cil_lvals.typeOfUnsafe ce2] in
        evalPtrArithPlusVals s v1 v2 ptrTyp resultTyp misc
      with NotPtrConstPair -> 
        (FNFP constNfpSource, s)
          (* Or make it a null ptr? *)
        
and evalPtrArithPlusVals s v1 v2 ptrTyp resultTyp misc =
  let expectedTyp = 
    match misc.assumedTyp with
      Some (t) -> pickDerefedTyp [t; resultTyp]
    | None -> pickDerefedTyp [resultTyp] in
  let newV = doPtrArith misc.lookupHook expectedTyp ptrTyp (v1, v2) in
  (newV, s)

and eval2 s ce1 ce2 misc =
  let v1, s = eval s ce1 { misc with assumedTyp = None; } in
  let v2, s = eval s ce2 { misc with 
                             assumedTyp = None; 
                             lookupHook = nilLookupHook; } in 
  (* HACK: don't add v2's crap to the lookupHook for access path *)
  (v1, v2, s)
    (* Can we share assumptions on state here? Do we need to ? *)

and evalPtrTarget st readerOpt sinfo (v, o) =
  let readerOpt = makeReaderOpt readerOpt o sinfo in
  lookupVal st v o sinfo readerOpt

(** Assuming ptrVal should be treated as a pointer, deref and read
    the value stored at the target *)
and derefPtrVal s sinfo ptrExp outerOff ptrVal misc =
  let lookupHook = misc.lookupHook in
  let readerOpt = misc.readerOpt in
  match ptrVal with
    Refs ls ->
      let newLS = flocsMap (fun (v, o) -> 
                              (v, concatOffsetVar v outerOff o)) ls in
      lookupHook (ADeref ls);
      if not (isNoOffset outerOff) then
        lookupHook (AConcatOff outerOff);
      (match FLocSet.fold 
         (fun (v, o) (curVal, curS) ->
            try
              let res, s = evalPtrTarget curS readerOpt sinfo (v, o) in
              match curVal with
                None -> Some res, s
              | Some (oldVal) -> Some (combineVals oldVal res), s
            with NullException -> (curVal, curS)
         ) newLS (None, s) with
           None, s -> 
             logErrorF "No targets for lubOverTargets %s @ %s\n"
               (string_of_val ptrVal) (string_of_loc !currentLoc);
             FInt (Int64.zero), s
         | Some x, s -> x, s)
        
  | FpRef vid ->
      let vi = varinfoOfVid vid in
      logErrorF "deref'ing a function: %s @ %s\n" vi.vname
        (string_of_loc !currentLoc);
      ptrVal, s

  | Records _
  | FIRecs _ ->
      logErrorF "deref'ing a record: %s @ %s\n" (string_of_exp ptrExp)
        (string_of_loc !currentLoc);
      ptrVal, s

  | FInt i64 ->
      raise NullException
  | FNFP _ ->
      (ptrVal, s)

(** Get the ptr value that points to (addr + offset) an lval, and possibly 
    a new state due to lazy creation *)
and resolveAddrOfLval s lval misc =
  match lval with
    (Var vi, off) ->
      if isFunctionType vi.vtype 
      then (FpRef (vidOfVarinfo vi), s)
      else
        let var, newOff = getLocation vi off in
        (* For now, make it have null too *)
        (* (Refs (FLocSet.singleton (var, newOff)), s) *)
        (makeMayref (var, newOff) (nullVar#getLoc), s)

  | Mem(ptrExp), off ->
      (* don't need to make up addrs, just reuse the old pointer, 
         and tack on the new outer offset *)
      let ptrVal, outerOff, s = getPtrVal s ptrExp off misc in
      (addOffToPointer s ptrExp ptrVal outerOff, s)

and addOffToPointer curState ptrExp ptrVal off =
  if isNoOffset off then
    match ptrVal with
      Refs _ | FpRef _ | FInt _ | FNFP _ -> ptrVal
    | Records _ | FIRecs _ -> 
        (logErrorF "ptrExp not reference? %s:%s @ %s\n" 
           (string_of_exp ptrExp) (string_of_val ptrVal) 
           (string_of_loc !currentLoc); 
         ptrVal)
  else match ptrVal with
    Refs ls ->
      if setIsNull ls  (* pattern for the offsetof(T) macro *)
      then calcOffsetOf off
      else
        let newSet = FLocSet.fold 
          (fun (v, innerOff) curSet ->
             FLocSet.add (v, concatOffsetVar v off innerOff) curSet
          ) ls FLocSet.empty in
        Refs newSet

  | FpRef vid -> 
      let vi = varinfoOfVid vid in
      logErrorF "addOffToPointer given fp: %s @ %s\n" vi.vname
        (string_of_loc !currentLoc);
      ptrVal
  | FNFP _ -> ptrVal (* probably just getting char array *)
        
  (* trying to calculate the bits of offset from base of struct *)
  | FInt i64 -> calcOffsetOf off
      
  (* Shouldn't actually get a struct *)
  | Records _
  | FIRecs _ ->
      failwith ("resolveAddrOfLval given record as ptr " ^
                  (string_of_exp ptrExp))
          
and calcOffsetOf off = (* already converted to bits -- convert to bytes *)
  let bitsOff = off in
  let bytesOff = bitsOff / 8 in
  FInt (Int64.of_int bytesOff)


let fHasBody funcs fkey =
  try
    let fid = fkey_to_fid fkey in
    let fnode = FMap.find fid funcs in
    fnode.hasBody
  with Not_found -> false

let getFormals funcs fkey =
  try
    let fid = fkey_to_fid fkey in
    let fnode = FMap.find fid funcs in
    if fnode.hasBody then
      match Cilinfos.getFunc fkey fnode.defFile with
        Some fundec -> Some (fnode, fundec.sformals)
      | None -> 
          failwith ("getFormals: no CFG: " ^ fnode.name ^ ":" ^ fnode.defFile)
    else None
  with Not_found ->
    None
