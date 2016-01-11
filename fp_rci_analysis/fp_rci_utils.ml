
open Cil
open Callg
open Type_utils
open Fp_rci_types
open Fp_rci_lattice_ops
open Fp_rci_store
open Fp_rci_globals
open Fp_rci_focus
open Logging
open Cildump

(*********************************************************
 * Expression Evaluation
 *********************************************************)

(********* Ptr arithmetic ***********)

exception NotPtrConstPair

let offsetFromInt expectedT ptrT i =
  if !fullFI then
    (noOff, true)
  else 
    let bitsForI = (bitsSizeOf ptrT) * (Int64.to_int i) in
    (bitsForI, false)

let isArrayStride atts (v, o) ptrT expectedT =
  (* Heuristic check if it's striding array-style by checking ptrT? *)
  try
    Type_utils.equal_type (canonType (typeOfFVar atts v)) ptrT ||
      Type_utils.equal_type (typeModArray (typeOfLoc atts (v, o))) ptrT ||
      Type_utils.equal_type ptrT expectedT
  with UnknownType ->
    Type_utils.equal_type ptrT expectedT
      
let warnHere msg =
  logError (msg ^ " @ " ^ string_of_loc !currentLoc)


let debugPtrArith origLS newSet debugOffs =
  if not (!nonUniformPtrArith) then
    if OffsetSet.cardinal debugOffs > 1 then begin
      warnHere ("PTRARITH w/ diff offsets " ^ (string_of_val (Refs origLS)));
      Refs (FLocSet.singleton nullVar#getLoc)
    end else
      Refs newSet
  else 
    Refs newSet

(** Calculate the new pointer created by ptr arith. 
    Assumes v1 is a pointer and v2 is a constant *)
let rec doPtrArith st lookHook (expectedT:Cil.typ) (ptrT:Cil.typ) (v1, v2) =
  match v1, v2 with
    FNFP _, _ -> v1
  | FpRef x, _ -> 
      warnHere "Ptr arith on funptr";
      v1
  | Refs ls, FNFP _ ->
      let ptrT = typeModArray (canonType ptrT) in
      let expectedT = typeModArray (canonType expectedT) in
      let newSet = FLocSet.fold
        (fun (v, off) cur -> 
           let newOff = if isSpecialVar v then off 
           else 
             if isArrayStride st.vAttrs (v, off) ptrT expectedT
             then (markSumOff off) 
             else (topOff, true)
           in
           addToPtrSet (v, newOff) cur) ls FLocSet.empty in
      Refs newSet

  | FInt _, FNFP _ ->
      let ptrT = typeModArray (canonType ptrT) in
      let expectedT = typeModArray (canonType expectedT) in
      (match ptrT with
         TPtr (_,_) -> 
           if Type_utils.equal_type ptrT expectedT then v1
          else raise NotPtrConstPair
       | _ -> raise NotPtrConstPair)

  | Refs ls, FInt i -> 
      let ptrT = typeModArray (canonType ptrT) in
      let expectedT = typeModArray (canonType expectedT) in
      let moreOff = offsetFromInt expectedT ptrT i in
      if not (isNoOffsetSum moreOff) 
      then lookHook (AConcatOff (int_of_off moreOff));

      let newSet, debugOffs = FLocSet.fold 
        (fun (v, off) (cur, seenOffs) ->
           let newoff = 
             if isArrayStride st.vAttrs (v, off) ptrT expectedT
             then (markSumOff off)
             else concatOffsetVar v moreOff off in
           
           let debugOffs = 
             if isSpecialVar v then seenOffs
             else OffsetSet.add (int_of_off off) seenOffs in
     
           (addToPtrSet (v, newoff) cur, debugOffs)
        ) ls (FLocSet.empty, OffsetSet.empty) in

      debugPtrArith ls newSet debugOffs
          
  | Refs ls1, Refs ls2 ->
      (* Check if it's striding array-style by checking ptrT? *)
      if Type_utils.equal_type expectedT ptrT then v1
      else begin
        warnHere "doPtrArith ref + ref";
        v1
      end

  | Refs ls, FpRef fp ->
      warnHere "doPtrArith ref + fpref";
      v1

  | FInt _, FInt _ -> raise NotPtrConstPair
      
  | Records _, _ 
  | NCRecord _, _
  | FIRecs _, _ ->
      warnHere ("doPtrArith v1 is rec: " ^ (string_of_val v1));
      doPtrArith st lookHook expectedT ptrT (demoteFromRecVal v1, v2)

  | _, Records _ 
  | _, NCRecord _ 
  | _, FIRecs _ ->
      warnHere ("doPtrArith v2 is rec: " ^ (string_of_val v2));
      doPtrArith st lookHook expectedT ptrT (v1, demoteFromRecVal v2)
     
  | _, _ -> 
      if isReference v1 then begin
        logErrorF "doPtrArith case ptr + ?: %s + %s @ %s\n"
          (string_of_val v1) (string_of_val v2) 
          (string_of_loc !currentLoc);
        printState st;
        failwith "doPtrArith ptr + ?";
      end else raise NotPtrConstPair
        

(** Out of a list of types, pick a Ptr type that is somewhat specific
    and return the deref'ed type *)
let rec pickDerefedTyp tList =
  match tList with
    t :: [] -> 
      let t = Cil_lvals.unrollTypeNoAttrs t in
      ( match t with
          TPtr (TVoid _, _) -> TVoid []
        | TPtr (t', _) -> t'
        | _ -> 
            logErrorF "pickDerefedType picking from nonptrs %s\n"
              (string_of_type t);
            t )
  | t :: tl -> 
      let t = Cil_lvals.unrollTypeNoAttrs t in
      ( match t with
          TPtr (TVoid _, _) -> pickDerefedTyp tl
        | TPtr (t', _) -> t'
        | _ -> pickDerefedTyp tl )
  | [] -> failwith "pickDerefedTyp: given an empty list!"


let pickType assumedT resultT =
  match assumedT, resultT with
    Some (TPtr (TVoid _, _)), _ 
  | None, _ -> resultT
  | (Some t, _) -> t


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
  curFunc : fundec;
}

let emptyMisc curFunc =
  { lookupHook = nilLookupHook;
    readerOpt = None;
    assumedTyp = None;
    curFunc = curFunc; }

(** Whenever a value presented from eval, possibly update the target types *)
let tryUpdateType (v, st) expectedT =
  match v with
    FInt _ | FNFP _ | Records _ | FIRecs _ 
  | NCRecord _ | FpRef _ -> (v, st)
  | Refs locs ->
      try
        let targT = typeModArray (Cil_lvals.typeAfterDerefOfT expectedT) in
        let st = FLocSet.fold 
          (fun (var, o) curS ->
             if isSpecialVar var then curS
             else updateType curS targT var o
          ) locs st in
        (v, st)
      with Cil_lvals.TypeNotFound ->
        (v, st)

(****************)


(** Evaluate a cil expression to a value. Takes a hook for lookups *)
let rec eval s inExp misc = 
  match inExp with
    (* Variable reference *)
    Lval(Var(vi), off) ->
      let readerOpt = misc.readerOpt in
      let var, (off, sum) = getLocation false vi off in
      let resultT = Cil_lvals.typeOfUnsafe inExp in
      let structInfo = getStructInfo resultT in
      let readHook = makeReaderOpt readerOpt off structInfo in
      let expectedT = 
        match misc.assumedTyp with 
          Some t -> findTyp (combineTypes (addTyp t) (addTyp resultT))
        | None -> resultT in
      let v, st = 
        lookupVal misc.lookupHook misc.curFunc expectedT s var off readHook in
      let v, st = tryUpdateType (v, st) expectedT in
      v, st
        
  (* Simple pointer dereference *)
  | Lval(Mem(ptrExp), off) -> 
      let ptrVal, newOff, s = getPtrVal s ptrExp off misc in
      let resultT = Cil_lvals.typeOfUnsafe inExp in
      let v, st = derefPtrVal s resultT ptrExp newOff ptrVal misc in
      tryUpdateType (v, st) resultT
      

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
             else begin
               (*               (logErrorF "UNSURE of pointer distance %s\n" 
                                (string_of_exp inExp); *)
               FNFP constNfpSource
             end
         | _, _, MinusPP ->
             begin
               (*
                 (logErrorF "UNSURE of pointer distance %s\n" 
                 (string_of_exp inExp); *)
               FNFP constNfpSource
             end
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
  let newOff = cilOff2Offset false baseTyp cilOff in
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
  let newV = doPtrArith s misc.lookupHook expectedTyp ptrTyp (v1, v2) in
  (newV, s)

and eval2 s ce1 ce2 misc =
  let v1, s = eval s ce1 { misc with assumedTyp = None; } in
  let v2, s = eval s ce2 { misc with 
                             assumedTyp = None; 
                             lookupHook = nilLookupHook; } in 
  (* HACK: don't add v2's crap to the lookupHook for access path *)
  (v1, v2, s)
    (* Can we share assumptions on state here? Do we need to ? *)

and evalPtrTarget misc st expectedTyp sinfo (v, o) =
  let readerOpt = makeReaderOpt misc.readerOpt o sinfo in
  lookupVal nilLookupHook misc.curFunc expectedTyp st v o readerOpt


(** Assuming ptrVal should be treated as a pointer, deref and read
    the value stored at the target *)
and derefPtrVal s resultT ptrExp outerOff ptrVal misc =
  let lookupHook = misc.lookupHook in
  match ptrVal with
    Refs ls ->

      lookupHook (ADeref ls);
      lookupHook (AReadField (int_of_off outerOff));

      (* Why isn't targT the same as resultT, expectedT, etc. ? *)

      (* Hacky update type of targets + concat the offsets *)
      let targT = Cil_lvals.typeAfterDeref ptrExp in
      let newLS, s = FLocSet.fold 
        (fun (v, o) (curLS, curS) -> 
           if isSpecialVar v then
             curLS, curS
           else 
             let curS = updateType curS targT v o in
             let newOff = concatOffsetVar v outerOff o in

             if (isNoOffsetSum newOff) then begin
(*
               logStatusF "TODO: check updatetype: %s : %s\n"
                 (string_of_pointer (v, newOff)) (string_of_type resultT)
*)
               ()
             end;
             let curS = updateType curS resultT v newOff in 
             addToPtrSet (v, newOff) curLS, curS
        ) ls (FLocSet.empty, s) in
      (* /hacky *)

      (* Why isn't sinfo based on expectedTyp instead of resultT ?*)
      let sinfo = getStructInfo resultT in
      let expectedTyp =
        match misc.assumedTyp with
          Some t -> findTyp (combineTypes (addTyp t) (addTyp resultT))
        | None -> resultT
      in

      (match FLocSet.fold 
         (fun (v, o) (curVal, curS) ->
            try
              let bits = int_of_off o in
              let res, s = 
                evalPtrTarget misc curS expectedTyp sinfo (v, bits) in
              match curVal with
                None -> Some res, s
              | Some (oldVal) -> 
                  let combo, st = combineVarValsSt s v oldVal res in
                  Some (combo), st
            with NullException -> (curVal, curS)
         ) newLS (None, s) with
           None, s -> 
             logErrorF "No targets for lubOverTargets %s @ %s\n"
               (string_of_val ptrVal) (string_of_loc !currentLoc);
             FInt (Int64.zero), s
         | Some x, s -> x, s)
        
  | FpRef var ->
      logErrorF "deref'ing a function: %s @ %s\n" 
        (string_of_var var) (string_of_loc !currentLoc);
      ptrVal, s

  | Records _ | NCRecord _ | FIRecs _ ->
      logErrorF "deref'ing a record: %s @ %s\n" (string_of_exp ptrExp)
        (string_of_loc !currentLoc);
      derefPtrVal s resultT ptrExp outerOff (demoteFromRecVal ptrVal) misc

  | FInt i64 ->
      logErrorF "deref'ing NULL %s -> %s @ %s\n" 
        (string_of_exp ptrExp) (string_of_val ptrVal) 
        (string_of_loc !currentLoc);
      (FNFP lookupNfp, s)
  | FNFP _ ->
      (ptrVal, s)

(** Get the ptr value that points to (addr + offset) an lval, and possibly 
    a new state due to lazy creation *)
and resolveAddrOfLval s lval misc =
  match lval with
    (Var vi, off) ->
      if isFunctionType vi.vtype 
      then (FpRef (FVar (vidOfVarinfo vi)), s)
      else
        let var, newOff = getLocation false vi off in
        (Refs (FLocSet.singleton (var, newOff)), s)
          
  | Mem(ptrExp), off ->
      (* don't need to make up addrs, just reuse the old pointer, 
         and tack on the new outer offset *)
      let ptrVal, outerOff, s = getPtrVal s ptrExp off misc in
      (addOffToPointer s ptrExp ptrVal outerOff, s)

and addOffToPointer curState ptrExp ptrVal off =
  if isNoOffsetSum off then
    match ptrVal with
      Refs _ | FpRef _ | FInt _ | FNFP _ -> ptrVal
    | Records _ | NCRecord _ | FIRecs _ -> 
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
             addToPtrSet (v, concatOffsetVar v off innerOff) curSet
          ) ls FLocSet.empty in
        Refs newSet

  | FpRef var -> 
      logErrorF "addOffToPointer given fp: %s @ %s\n" 
        (string_of_var var) (string_of_loc !currentLoc);
      ptrVal
  | FNFP _ -> ptrVal (* probably just getting char array *)
        
  (* trying to calculate the bits of offset from base of struct *)
  | FInt i64 -> calcOffsetOf off
      
  (* Shouldn't actually get a struct *)
  | Records _ | NCRecord _ | FIRecs _ ->
      logErrorF "resolveAddrOf given record %s -> %s @ %s\n"
        (string_of_exp ptrExp) (string_of_val ptrVal) 
        (string_of_loc !currentLoc);
      addOffToPointer curState ptrExp (demoteFromRecVal ptrVal) off

          
and calcOffsetOf off = (* already converted to bits -- convert to bytes *)
  let bitsOff = int_of_off off in
  let bytesOff = bitsOff / 8 in
  FInt (Int64.of_int bytesOff)

let fHasBody funcs fkey =
  let fid = fkey_to_fid fkey in
  try
    let node = FMap.find fid funcs in
    node.hasBody
  with Not_found ->
    false

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


(************************************************************)

let viIsFormal formals vid =
  List.exists (fun var -> var.vid = vid) formals

let varIsTargetOfFormal formals var =
  not (isSpecialVar var) &&
    (match var with
       FInput (ap, _) ->
         let deref, fromFormal = List.fold_left 
           (fun (numDeref, hasRootV) acc ->
              match acc with
                AccDeref -> (numDeref + 1, hasRootV)
              | AccField _ -> (numDeref, hasRootV)
              | AccVar vid -> (numDeref, viIsFormal formals vid || hasRootV)
           ) (0, false) ap in
         deref = 1 && fromFormal 
     | FHeap _ | FRet _ | FVar _ -> false )
    
