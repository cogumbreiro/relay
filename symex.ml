(** Symbolic execution module w/out conservative adjustments for 
    shared variables and concurrency. 

    In the end, provides mapping of lvalues to their value at each 
    program point. Values are in terms of the initial values 
    (at function start). @see symex_types.ml  *)

open Cil
open Pretty
open Fstructs
open Symex_base
open Scope
open Logging
open Cildump

open Symex_types

module SS = Symex_sum
module BS = Backed_summary

(* Exposed alias *)
type symSt = symState

(************************************************************
            Access to mod information 
************************************************************)

let modSumms = ref (new Modsummaryi.absModSumm)

let setModSumm newSummaries =
  modSumms := newSummaries

(********* NULL Checking stuff ************************************)

module NI = struct

  let isNullHost = isNullHost

end

module NULL = SYMEX_NULL_GEN (NI)
    
(******************* Summary def'n, ops **********************)

let combineVals = StrictLatticeOps.combineVals

type symLatt = (symState, SS.sumval) IntraDataflow.stateLattice

let statesSubset s1 s2 = 
  StrictLatticeOps.statesSubset s1 s2

let statesEqual s1 s2 = 
  StrictLatticeOps.statesEqual s1 s2

let combineStates s1 s2 =
  StrictLatticeOps.combineStates s1 s2

(** Package the state operations *)
class symLattice : [symState, SS.sumval] IntraDataflow.stateLattice = object (self) 

  (* rkc: hack alert, to allow Radar to plug in seq or adj summary database *)
  val mutable thesums = SS.sum

  method sums = thesums

  method setTheSums newSum =
    thesums <- newSum

  method stateSubset st1 st2 =
    Stat.time "rad SS subset" (statesSubset st1) st2
    
  method combineStates st1 st2 =
    Stat.time "rad SS combine" (combineStates st1) st2
    
  method isBottom st =
    isBottomState st
    
  (***** Special values of the state *****)

  method bottom = bottomSymState
    
  method initialState = emptySymState

  method setInitialState (st:symState) = 
    failwith "Symex initial state should be constant?";
    
  (***** Debugging *****)
 
  (** Print the summary for the given function to the log. 
      TODO: Move this somewhere else? Hmmm, didn't supply input either? *)
  method printSummary sumKey =
    SS.printSummary thesums sumKey
      
  method printState st =
    printSymState st
  
end


(*********************************************************
 * Expression Evaluation
 *********************************************************)


(** Returns the symbolic expression associated with 'true' or 'false'. 
    Recall that in  C we have "false == 0" and "true <> 0". *)
let exp_of_bool b = match b with
    true -> Lv.abs_of_exp Cil.one
  | false -> Lv.abs_of_exp Cil.zero

(** Assuming the int64 i represents a bool, invert it *)
let not_of_int64 i = 
  if (i == Int64.zero) then
    Int64.one
  else
    Int64.zero


exception NotPtrConstPair

(** Calculate the new pointer created by ptr arith. Assumes v1 is a pointer
    and v2 is a constant *)
let doPtrArith (expectedTyp:Cil.typ) (ptrTyp:Cil.typ) 
    ((v1, v2):symVal * symVal) : symVal = 

  (* Convert an integer valued pointer offset to a field/array offset.
     May raise UnknownOffset if offset cannot be determined *) 
  let ptrFromOffset (i:int64) (addr, off) =
    let bitsBase, baseWidth = 
      match off with
        Field _ -> Cil.bitsOffset expectedTyp off
      | _ -> 0, 0 (* Ignore array index, and set to 0 offset *)
    in
    let bitsForI = (Cil.bitsSizeOf ptrTyp) * (Int64.to_int i) in
    (addr, Offset.bitsToOffset expectedTyp (bitsBase + bitsForI))     
  in

  match v1, v2 with
    Vtop, _ -> Vtop
  | Vbot, _ -> Vbot

  | _, Vbot 
  | _, Vtop -> (* could be more specific if we assume memory safety, 
                  e.g., all fields in base struct *) 
      (* Vtop *)
      v1

  (* TODO, grab host type from source of ptr target addr instead! *)
  | Vmustptr (addr, off), 
      Vval (Lv.CConst (CInt64(i,ik,_))) -> begin
      try 
        Vmustptr (ptrFromOffset i (addr, off))
      with Offset.UnknownOffset ->
        Vtop
    end
  | Vmayptr (id, addrOffSet), 
        Vval (Lv.CConst (CInt64(i,ik,_))) -> begin
      try 
        let newSet = AddrOffSet.fold 
          (fun (addr, off) curSet ->
             AddrOffSet.add (ptrFromOffset i (addr, off)) curSet
          ) addrOffSet AddrOffSet.empty in
        Vmayptr (freshPtrID (), newSet)
      with Offset.UnknownOffset ->
        Vtop
    end
  | Vextptr (id, addrOffSet), 
          Vval (Lv.CConst (CInt64(i,ik,_))) -> begin
      try 
        let newSet = AddrOffSet.fold 
          (fun (addr, off) curSet ->
             AddrOffSet.add (ptrFromOffset i (addr, off)) curSet
          ) addrOffSet AddrOffSet.empty in
        Vextptr (freshPtrID (), newSet)
      with Offset.UnknownOffset ->
        Vtop
    end
  | _ ->
      raise NotPtrConstPair


(** Out of a list of types, pick a Ptr type that is somewhat specific
    and return the deref'ed type *)
let rec pickDerefedTyp tList =
  match tList with
    [t] -> begin
      match t with
        TPtr (TVoid _, _) -> TVoid []
      | TPtr (t', _) ->  t'
      | _ -> t
    end
  | t :: tl -> begin
      match t with
        TPtr (TVoid _, _) -> pickDerefedTyp tl
      | TPtr (t', _) -> t'
      | _ -> pickDerefedTyp tl
    end
  | [] -> failwith "pickDerefedTyp: given an empty list!"


(** Reorder a pair of values so that a ptr value comes first, then
    a constant comes second. Also returns a flag if swapped
    May raise NotPtrConstPair if arg isn't a ptr/constant pair *)
let reorderPtrConstPair ((v1, v2):symVal * symVal) : symVal * symVal * bool =
  match (v1, v2) with 
    Vmustptr _, Vval e 
  | Vmayptr _ , Vval e
  | Vextptr _, Vval e 
  | Vtop, Vval e 
  | Vbot, Vval e when isConst e ->
      (v1, v2, false)
  | (Vval e, _) when isConst e ->
      (v2, v1, true)
  | _ ->
      raise NotPtrConstPair
        
        
exception PointerCastError

(* Don't want Vval expressions to blow up *)
let maxOpsInVval = 24


type valGetter = (symAddr * offset) -> symState -> (symVal * symState)


(** Read the values from each target of given [addrOffSet] and LUB them *)
let lubOverTargets addrOffSet (foo : valGetter) s =
  (* TODO: make side-effect free? *)
  let newVal, newState = AddrOffSet.fold
    (fun target (curVal, curSt) ->
       let newVal, newSt = foo target curSt in
       match curVal with
         None -> (Some newVal, newSt)
       | Some v -> (Some (combineVals v newVal), newSt)
    ) addrOffSet (None, s) in
  match newVal with
    None -> (Vtop, newState)
  | Some v -> (v, newState)


let doConstantFold exp =
  (* possibly stupid conversion back to do constant folding first *)
  try
    let cilExp = Lv.exp_of_abs_simple exp in
    let const = Cil.constFold true cilExp in 
    Lv.abs_of_exp const
  with Lv.IsAbstract ->
    exp
  
let informNotConstantFolded op exp =
  (*  OK it should happen because eval could tell us X + 1 is a const 
      if X is const

     logError ("eval helped const fold: " ^ op ^ " " ^ Lv.string_of_exp exp)
  *)
  ()
    
       
(** Evaluate a cil expression (ce) to a value, possibly creating
    new bindings in the symState *)
let rec eval (s:symState) inExp (assumedTyp:Cil.typ option) : 
    symVal * symState = 
  let foldedExp = doConstantFold inExp in

  match foldedExp with
    (* Variable reference *)
    Lv.CLval(Lv.CVar(vi) as host, off) ->
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      (* TODO, compare w/ assumed typ *)
      let baseTyp = Lv.typeOfLvalUnsafe (host, newOff) in
      evalAddr s baseTyp addr newOff

  | Lv.CLval(Lv.AbsHost _ as host, off) ->
      logError "Eval is reading value from an abshost";
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      let baseTyp = TPtr (TVoid [], []) in
      evalAddr s baseTyp addr newOff
      
  (* Simple pointer dereference *)
  | Lv.CLval(Lv.CMem(ptrExp) as host, off) -> 
      let ptrVal, newState = eval s ptrExp None in
      let newOff, isSum = CLv.canonicizeOff off in
      (* TODO, compare w/ assumed typ *)
      let baseTyp = Lv.typeOfLvalUnsafe (host, newOff) in
      derefPtrVal ptrExp newOff baseTyp newState ptrVal

  (* Take addr of a cell *)
  | Lv.CAddrOf (l) ->
      resolveAddrOfLval s l

  (* Start of an array, like taking the addr of the array *)
  | Lv.CStartOf(l) ->
      resolveAddrOfLval s l

  | Lv.CCastE(t, e) ->
      eval s e (Some t) (* re-eval w/ an assumed type *)

  (* Pointer arith *)
  | Lv.CBinOp(PlusPI, ce1, ce2, typ) 
  | Lv.CBinOp(IndexPI, ce1, ce2, typ) -> begin
      let v1, v2, newSt = eval2 s ce1 ce2 assumedTyp in
      (* Shouldn't need to reorder (ptr should be ce1, offset ce2) *)
      let ptrTyp = pickDerefedTyp [Lv.typeOfUnsafe ce1] in
      try
        let expectedTyp = 
          match assumedTyp with
            Some (t) ->
              pickDerefedTyp [t; typ]
          | None ->
              pickDerefedTyp [typ]
        in
        (doPtrArith expectedTyp ptrTyp (v1, v2), newSt)
      with NotPtrConstPair ->
        (* Assume it's an array-like access? *)
        eval newSt ce1 assumedTyp
    end

  | Lv.CBinOp(MinusPI, ce1, ce2, typ) -> begin
      let v1, v2, newSt = eval2 s ce1 
        (Lv.CUnOp (Neg, ce2, Lv.typeOfUnsafe ce2)) assumedTyp in
      (* Shouldn't need to reorder (ptr should be ce1, offset ce2) *)
      let ptrTyp = pickDerefedTyp [Lv.typeOfUnsafe ce1] in
      try
        let expectedTyp = 
          match assumedTyp with
            Some (t) ->
              pickDerefedTyp [t; typ]
          | None ->
              pickDerefedTyp [typ]
        in
        (doPtrArith expectedTyp ptrTyp (v1, v2), newSt)
      with NotPtrConstPair ->
        (* Assume it's an array-like access? *)
        eval newSt ce1 assumedTyp
    end
 
  (* May be pointer arith... check *)
  | Lv.CBinOp(PlusA, ce1, ce2, typ) -> begin
      let v1, v2, newSt = eval2 s ce1 ce2 assumedTyp in
      match v1, v2 with
        Vval (Lv.CConst(CInt64(i1,ik1,_))), 
        Vval (Lv.CConst(CInt64(i2,_,_))) ->
          informNotConstantFolded "plusA" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.add i1 i2, ik1, None))), newSt
            
      (* Non-constant binops *) 
      | Vval y, Vval z -> 
          if ((Lv.countOpsInExp y) + (Lv.countOpsInExp z) > maxOpsInVval) then
            Vtop, newSt
          else
            Vval (Lv.CBinOp(PlusA, y, z, typ)), newSt

      | Vtop, _
      |  _, Vtop ->
           Vtop, newSt
             
      | Vbot, _
      | _, Vbot ->
          Vbot, newSt (* or return bottom state? *)  

      | _, _ ->
          (* Maybe it's ptr arith *)
          try 
            let v1, v2, switched = reorderPtrConstPair (v1, v2) in
            let ptrTyp = 
              if (switched) then
                pickDerefedTyp [Lv.typeOfUnsafe ce1]
              else
                pickDerefedTyp [Lv.typeOfUnsafe ce2]
            in
            let expectedTyp =
              match assumedTyp with
                Some (t) ->
                  pickDerefedTyp [t; typ]
              | None ->
                  pickDerefedTyp [typ]
            in
            (doPtrArith expectedTyp ptrTyp (v1, v2), newSt)
          with NotPtrConstPair ->
            logError "eval PlusA, not used as pointer arith";
            (Vval foldedExp, newSt)
    end
      
  (* Sorta pointer arith *)
  | Lv.CBinOp(MinusPP, ptr1, ptr2, typ) ->
      (Vtop, s) (* Don't know the actual spacing between pt'ed to addresses *)

  (* Ops *)
  | Lv.CUnOp(unop,ce,t) -> begin
      let v, newSt = eval s ce None in
      match unop, v with
        Neg, Vval (Lv.CConst (CInt64(i1,ik1,_))) ->
          informNotConstantFolded "neg" foldedExp;
          Vval (Lv.CConst (CInt64(Int64.neg i1, ik1, None))), newSt
      | BNot, Vval (Lv.CConst (CInt64(i1,ik1,_))) ->
          informNotConstantFolded "bnot" foldedExp;
          Vval (Lv.CConst (CInt64(Int64.lognot i1, ik1, None))), newSt
      | LNot, Vval (Lv.CConst (CInt64(i1, ik1, None))) ->
          informNotConstantFolded "lnot" foldedExp;
          let newI = not_of_int64 i1 in
          Vval (Lv.CConst (CInt64(newI, ik1, None))), newSt
      | _, Vval innerOperand -> 
          if (Lv.countOpsInExp innerOperand > maxOpsInVval) then
            Vtop, newSt
          else
            Vval (Lv.CUnOp(unop, innerOperand, t)), newSt

      | _, Vtop ->
          Vtop, newSt
      | _, Vbot ->
          Vbot, newSt
      | _, _ -> 
          (* There was one case where a guy cast a struct of two longs
             to a single long long, then negated that number... 
             why wasn't it caught by the Vval innerOperand case above? *)
          logError ("eval: unop type error? " ^ (Lv.string_of_exp foldedExp));
          Vtop, newSt
    end

  | Lv.CBinOp(bop,ce1,ce2,t) -> begin
      let v1, v2, newSt = eval2 s ce1 ce2 assumedTyp in
      match bop, v1, v2 with
        (* Evaluate constant ops *)
        (PlusA, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          logError "reached redundant condition?";
          informNotConstantFolded "plusA" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.add i1 i2, ik1, None))), newSt

      | (MinusA, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "minusA" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.sub i1 i2, ik1, None))), newSt

      | (Mult, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "mult" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.mul i1 i2, ik1, None))), newSt

      | (Div, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) -> 
          begin
            informNotConstantFolded "div" foldedExp;
            try
              Vval (Lv.CConst(CInt64(Int64.div i1 i2, ik1, None))), newSt
            with Division_by_zero ->
              logError "SS: Hit div by zero";
              Vbot, newSt
          end
      | (Mod, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) -> 
          begin
            informNotConstantFolded "mod" foldedExp;
            try
              Vval (Lv.CConst(CInt64(Int64.rem i1 i2, ik1, None))), newSt
            with Division_by_zero ->
              logError "SS: Hit mod by zero";
              Vbot, newSt
          end
      | (Shiftlt, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "shiftlt" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.shift_left i1 
                                   (Int64.to_int i2), ik1, None))), newSt
            
      | (Shiftrt, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "shiftrt" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.shift_right i1 
                                   (Int64.to_int i2), ik1, None))), newSt
            
      | (BAnd, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "band" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.logand i1 i2, ik1, None))), newSt
            
      | (BOr, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "bor" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.logor i1 i2, ik1, None))), newSt

      | (BXor, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "bxor" foldedExp;
          Vval (Lv.CConst(CInt64(Int64.logxor i1 i2, ik1, None))), newSt
            
      (* Lv.CConstant tests *)
      | (Lt, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "lt" foldedExp;
          Vval (exp_of_bool (i1 < i2)), newSt 

      | (Le, Vval (Lv.CConst(CInt64(i1,ik1,_))),
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "le" foldedExp;
          Vval (exp_of_bool (i1 <= i2)), newSt

      | (Gt, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "gt" foldedExp;
          Vval (exp_of_bool (i1 > i2)), newSt

      | (Ge, Vval (Lv.CConst(CInt64(i1,ik1,_))),
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "ge" foldedExp;
          Vval (exp_of_bool (i1 >= i2)), newSt

      | (Eq, Vval (Lv.CConst(CInt64(i1,ik1,_))),
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "eq" foldedExp;
          Vval (exp_of_bool (i1 == i2)), newSt

      | (Ne, Vval (Lv.CConst(CInt64(i1,ik1,_))), 
         Vval (Lv.CConst(CInt64(i2,_,_)))) ->
          informNotConstantFolded "ne" foldedExp;
          Vval (exp_of_bool (i1 != i2)), newSt
            (* What about non-constant versions of these tests? Ask ATP? *)
            
      (* Non-constant binops *) 
      | op, Vval y, Vval z -> 
          if ((Lv.countOpsInExp y) + (Lv.countOpsInExp z) > maxOpsInVval) then
            Vtop, newSt
          else
            (* Can't check if y and z are constants, because they
               may have changed from when we first recorded them? *)
            Vval (Lv.CBinOp(op, y, z, t)), newSt

      | op, Vtop, _
      | op, _, Vtop ->
          Vtop, newSt
            
      | op, Vbot, _
      | op, _, Vbot ->
          Vbot, newSt (* or return bottom state? *)
            
      | op, _, _ ->
          logError (Printf.sprintf "eval bin op, non-Vval operands %s,  %s"
                        (string_of_val v1) (string_of_val v2));
          (Vval foldedExp, newSt)
            (* TODO: Make sure this does not create expressions that
               involve more than initial values *)
    end

  (* Misc machine dependent stuff *)
  | Lv.CAlignOfE _ 
  | Lv.CAlignOf _
  | Lv.CSizeOfE _ 
  | Lv.CSizeOf _
  | Lv.CSizeOfStr _ ->
      informNotConstantFolded "machdep" foldedExp;
      let cilExp = Lv.exp_of_abs_simple foldedExp in
      let const = Cil.constFold true cilExp in 
      let cexp = Lv.abs_of_exp const in
      Vval (cexp), s
  | Lv.CConst _ ->
      Vval foldedExp, s

and eval2 (s:symState) ce1 ce2 (assumedT:Cil.typ option) 
    : (symVal * symVal * symState) =
  let v1, newSt1 = eval s ce1 None in
  let v2, newSt2 = eval newSt1 ce2 None in
  (v1, v2, newSt2)


(** Check if lval should be trimmed for termination *)
and evalPtrTarget outerOff baseTyp (addr, innerOff) s =
  let finalAddr, fOff = limitAddrOff baseTyp addr outerOff innerOff in
  evalAddr s baseTyp finalAddr fOff    
    
(** Assuming ptrVal should be treated as a pointer, deref and read
    the value stored at the target *)
and derefPtrVal ptrExp outerOff baseTyp (s:symState) ptrVal 
    : symVal * symState =
  match ptrVal with
    Vmustptr (addr, innerOff) ->
      evalPtrTarget outerOff baseTyp (addr, innerOff) s 
        
  | Vmayptr (id, addrOffSet) ->
      (* get each value and LUB them *)
      lubOverTargets addrOffSet (evalPtrTarget outerOff baseTyp) s
        
  | Vextptr (id, addrOffSet) ->
      lubOverTargets addrOffSet (evalPtrTarget outerOff baseTyp) s

  | Vstruct _ ->
      logError ("eval: deref'ing a struct? " ^
                    (Lv.string_of_exp ptrExp));
      (Vtop, s)

  | Vbot ->
      logError ~prior:3 ("eval: nullptr dereference! " ^ 
                      (Lv.string_of_exp ptrExp));
      (Vtop, s)

  | Vval e when isConst e ->
      logError ~prior:3 ("eval: nullptr dereference! " ^ 
                      (Lv.string_of_exp ptrExp));
      (Vtop, s)

  | Vtop ->
      (Vtop, s)

  | Vval exp ->
      try
        (* convert exp to a pointer first *)
        let newPtrVal, newSt = castAsPointer s exp in
        derefPtrVal ptrExp outerOff baseTyp newSt newPtrVal
      with PointerCastError ->
        logError ("derefPtrVal: Vval not treated as ptr: " ^ 
                      (Lv.string_of_exp exp));
        (Vtop, s)


(** Try to treat an expression as a pointer value *) 
and castAsPointer (s:symState) exp : symVal * symState = 
  match exp with
    Lv.CLval(host,off) -> begin
      (* Don't try to evaluate exp... exp SHOULD only be in terms of
         unknown initial values *)
      let newOff, isSum = CLv.canonicizeOff off in
      let truncOff = CLv.simplifyOff newOff in
      let addr = getAddr host isSum in
      (makePtrValue addr truncOff, s)
    end

  | Lv.CAddrOf (l)  (* Shouldn't get these in the first place, but whatever *)
  | Lv.CStartOf(l) ->
      (* known to happen in linux 2.6.15's 
         __read_page_state : unsigned long (unsigned long ) *)
      logError "AddrOf/StartOf found as part of Vval";
      resolveAddrOfLval s l

  | Lv.CCastE(_,e) -> 
      castAsPointer s e (* treat as if cast isn't there and retry *)
        
  (* Ops *)
  | Lv.CBinOp(PlusPI, host, offsets, typ)
  | Lv.CBinOp(MinusPI, host, offsets, typ)
  | Lv.CBinOp(IndexPI, host, offsets, typ) 
  | Lv.CBinOp(PlusA, host, offsets, typ) ->
      (* Assume it is pointer arith, and only use first operand *)
      castAsPointer s host

  | Lv.CBinOp(_) ->
      raise PointerCastError
  | Lv.CUnOp(unop,ce,t) -> 
      raise PointerCastError

  (* Constants *)
  | Lv.CAlignOfE _
  | Lv.CSizeOfE _
  | Lv.CAlignOf _
  | Lv.CSizeOf _
  | Lv.CSizeOfStr _ 
  | Lv.CConst _ -> (* Ignore string literals since they are constant? *)
      raise PointerCastError
        

(** Get the ptr value that points to (addr + offset) an lval, and possibly 
    a new state due to lazy creation *)
and resolveAddrOfLval (s:symState) lval : symVal * symState =

  let rec addOffToPointer ptrExp off curState ptrVal =
    match ptrVal, off with
      _ , NoOffset ->      (* Should maybe be more careful if fst not ptr? *)
        (ptrVal, curState)
    | Vmustptr (addr, innerOff), _ ->
        (Vmustptr (addr, concatOffset addr off innerOff), curState)
          
    | Vmayptr (id, addrOffSet), _ ->
        let newSet = AddrOffSet.fold 
          (fun (addr, innerOff) curSet ->
             AddrOffSet.add (addr, concatOffset addr off innerOff) curSet
          ) addrOffSet AddrOffSet.empty in
        (Vmayptr (freshPtrID (), newSet), curState)
          
    | Vextptr (id, addrOffSet), _ ->
        let newSet = AddrOffSet.fold 
          (fun (addr, innerOff) curSet ->
             AddrOffSet.add (addr, concatOffset addr off innerOff) curSet
          ) addrOffSet AddrOffSet.empty in
        (Vextptr (freshPtrID (), newSet), curState)
          
    | Vtop, _ -> 
        (Vtop, curState)

    (* trying to calculate the bits of offset from base of struct *)
    | Vval (Lv.CConst _), _ 
    | Vbot, _ -> begin
        try
          let t = Lv.typeOfUnsafe ptrExp in
          let bitsOff, bitsWidth = Cil.bitsOffset t off in
          let bytesOff = bitsOff / 8 in
          (Vval (Lv.abs_of_exp (Cil.integer bytesOff)), curState)
        with 
          SizeOfError _
        | Errormsg.Error ->
            logError ("resolveAddrOfLval: offsetOf calculation failed" ^ 
                          (Lv.string_of_exp ptrExp));
            (Vtop, curState)
      end


    (* Shouldn't actually get Vstruct *)
    | Vstruct _, _ ->
        logError ("resolveAddrOfLval: can't deref non-pointer val " ^
                        (Lv.string_of_exp ptrExp));
        (Vtop, curState)
          
    (* try to treat as a pointer *)
    | Vval exp, _ -> begin
        try
          let newPtrVal, newSt = castAsPointer curState exp in
          addOffToPointer ptrExp off newSt newPtrVal
        with PointerCastError ->
          logError ("resolveAddrOfLval: Vval not treated as ptr: " ^ 
                          (Lv.string_of_exp ptrExp));
          (Vtop, curState)
      end
  in
  match lval with
    ((Lv.CVar _) as host, off) 
  | ((Lv.AbsHost _) as host, off) ->
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      (Vmustptr (addr, newOff), s)
        
  | host, off when isNullHost host ->
      (* If it was a null ptr, leave as a nullptr *)
      let newOff, isSum = CLv.canonicizeOff off in
      let addr = getAddr host isSum in
      (Vmustptr (addr, newOff), s)

  | Lv.CMem(ptrExp), off ->
      (* don't need to make up addrs, just reuse the old pointer, 
         and tack on the new outer offset *)
      let ptrVal, newState = eval s ptrExp None in
      let newOff, _ = CLv.canonicizeOff off in
      addOffToPointer ptrExp newOff newState ptrVal




(*********************************************************
 * Intra-proc Dataflow Analysis
 *********************************************************)


(************ Summary Support / Substitution ************)


exception NotPointer

let expEqual exp otherE = Lv.compare_exp exp otherE == 0

let lvalEqual lv otherLv = Lv.compare_lval lv otherLv == 0

let rec lvalOfActual (exp:Cil.exp) : Cil.lval =
  match exp with
    Lval lv -> lv
  | CastE (_, e) -> lvalOfActual e
  | _ ->
(*      logError ("substLval: actual not lval " ^ (string_of_exp exp)); *)
      raise CLv.SubstInvalidArg

let rec substExp (args:Cil.exp list) (expFromOtherPlanet:Lv.aExp) : Lv.aExp =
  match expFromOtherPlanet with
    Lv.CLval(lv) ->           
      let newLval = substLval args lv in
      (Lv.CLval(newLval))
        
  | Lv.CAddrOf (l) ->
      Lv.mkAddrOf (substLval args l)

  | Lv.CStartOf(l) ->
      Lv.CStartOf (substLval args l)
        
  | Lv.CCastE(t, e) ->
      Lv.CCastE (t, substExp args e)
        
  | Lv.CAlignOfE(e) ->
      Lv.CAlignOfE (substExp args e)
        
  | Lv.CSizeOfE(e) ->
      Lv.CSizeOfE (substExp args e)
        
  | Lv.CUnOp (unop, e, t) ->
      let newExp = substExp args e in
      Lv.CUnOp (unop, newExp, t)
        
  | Lv.CBinOp (bop, e1, e2, t) ->
      let newExp1 = substExp args e1 in
      (try
         let newExp2 = substExp args e2 in
         Lv.CBinOp(bop, newExp1, newExp2, t)
       with CLv.SubstInvalidArg as e ->
         (match bop with
            PlusPI
          | MinusPI
          | IndexPI ->
              newExp1 (* allow omission of constant offset *)
          | _ ->
              let expStr = Lv.string_of_exp expFromOtherPlanet in 
              logError
                ("substExp encountered unknown exp: " ^ expStr);
              raise e
         )
      )
  | Lv.CAlignOf _
  | Lv.CSizeOf _
  | Lv.CSizeOfStr _
  | Lv.CConst _ ->
      raise CLv.SubstInvalidArg

and substLval (args:Cil.exp list) (lvalFromOtherPlanet:Lv.aLval) : Lv.aLval =
  match lvalFromOtherPlanet with 
    Lv.AbsHost _, _ ->
      (* Treat as global... no subs *)
      lvalFromOtherPlanet

  | (Lv.CVar(vi), formOff) -> begin
      let scope = decipherScope (Lv.var_of_abs vi) in
      match scope with
        SGlobal -> 
          lvalFromOtherPlanet
      | SFormal n -> begin
          (* Only substitute if the actual is an lval *)
          let actual = List.nth args n in
          let lv = lvalOfActual actual in 
          let (h, o) = Lv.abs_of_lval lv in
          try Lv.attachOffset h (Cil.addOffset formOff o)
          with
            CLv.OffsetMismatch om ->
              logError ("substLval: " ^ CLv.string_of_offsetMiss om ^ " 1");
              raise CLv.SubstInvalidArg
          | Failure s ->
              logError ("substLval: failure " ^ s);
              raise CLv.SubstInvalidArg
        end
      | _ ->
          (* It was local var of the other function -- can't substitute. *)
          logError ("substLval: local variable " ^ (Lv.var_of_abs vi).vname
            ^ " stayed in summary?");
          raise CLv.SubstInvalidArg
    end


  | (Lv.CMem (Lv.CLval(Lv.CVar(vi), NoOffset)), outerOff) -> begin
      let scope = decipherScope (Lv.var_of_abs vi) in
      match scope with
        SGlobal -> 
          lvalFromOtherPlanet
      | SFormal n -> begin
          let actual = Lv.abs_of_exp (List.nth args n) in
          try
            Lv.mkMemChecked actual outerOff
          with 
            CLv.OffsetMismatch om ->
              logError ("substLval: " ^ CLv.string_of_offsetMiss om ^ " 2");
              raise CLv.SubstInvalidArg
        end
      | _ ->
          logError ("substLval: local variable " ^ (Lv.var_of_abs vi).vname
                      ^ " stayed in summary?");
          raise CLv.SubstInvalidArg
    end

  | (Lv.CMem(ptrExp), outerOff) ->
      let newExp = substExp args ptrExp in
      try
        Lv.mkMemChecked newExp outerOff
      with 
        CLv.OffsetMismatch om ->
          logError ("substLval: " ^ CLv.string_of_offsetMiss om ^ " 3");
          raise CLv.SubstInvalidArg

let rec substValue state (args:Cil.exp list) (valFromOtherPlanet:symVal) :
    symVal * symState =
  let handlePtr target curSt =
    let addr, off = target in 
    if isNullAddr addr 
    then (Vmustptr target, state) (* leave null ptrs alone *)
    else try 
      (* Update the target(s) of the pointer (syntactic substitution),
         then get a pointer back by taking the addrOf the new target *)
      let host = hostOfAddr addr in
      let newLval = substLval args (host, off) in
      resolveAddrOfLval state newLval
    with CLv.SubstInvalidArg ->
      (Vtop, state)
  in

  match valFromOtherPlanet with
    Vtop -> (Vtop, state)
  | Vbot -> (Vbot, state)
  | Vval e -> begin
      (* First syntactically substitute formals w/ actuals.
         Then evaluate expression under current state. 
         CHECK: ... or evaluate before substituting or just substitute? *)
      try 
        let newExp = substExp args e in
        eval state newExp None
      with CLv.SubstInvalidArg ->
        (Vtop, state) 
          (* exp may be in terms of other functions locals *)
    end

  | Vstruct offMap ->
      (* Update the individual values of the map *)
      let newMap, newState = OffsetMap.fold 
        (fun k v (curMap, curState) ->
           let newVal, newState = substValue curState args v in
           (OffsetMap.add k newVal curMap, newState)
        ) offMap (OffsetMap.empty, state) in
      (Vstruct newMap, newState)

  | Vmustptr target -> handlePtr target state

  | Vmayptr (_, aoSet) -> lubOverTargets aoSet handlePtr state

  | Vextptr (_, aoSet) -> lubOverTargets aoSet handlePtr state



(** Convert a pointer that points to an (addr, offset) pair 
    into an expression *)
let concretizePointerTo ptrTarget = 
  (Lv.mkAddrOf (concretizeLval ptrTarget))



(** Get the abstract PTA nodes that are the targets of this 
    pointer expression *)
let rec ptrExpToAbs ptrExp =
  (try Lv.deref_absExp ptrExp
   with Alias.UnknownLoc -> 
     logError ("mainAliases -- Vtop: unable to deref: " ^
                   (Lv.string_of_exp ptrExp));
     []
  )
  
(** Get the canonical aliases of the given pointer "origExp" *)
let rec mainAliases origExp state v : (bool * (Lv.aExp list)) =
  let mainAliasSet addrOffSet =
    AddrOffSet.fold 
      (fun target cur ->
         let e = concretizePointerTo target in
         List_utils.addOnceP expEqual cur e
      ) addrOffSet []
  in

  match v with
    Vval symExp -> begin
      (* try to promote to a pointer and retry *)
      try 
        let newVal, newState = castAsPointer state symExp in
        mainAliases origExp newState newVal 
      with PointerCastError ->
        raise NotPointer
    end

  | Vstruct _ -> 
      raise NotPointer

  | Vmustptr target ->
      (true, [concretizePointerTo target])

  | Vmayptr (id, addrOffSet) ->
      (false, mainAliasSet addrOffSet)

  | Vextptr (id, addrOffSet) ->
      (* TODO See if it should be expanded even more? *)
      (true, mainAliasSet addrOffSet)

  | Vtop ->
      (* Get the abstract PTA node that represents the 
         location of the origExp *)
      (match origExp with
         Lv.CAddrOf (Lv.CMem innerExp, off)
       | Lv.CStartOf (Lv.CMem innerExp, off) ->
           let absTargs = ptrExpToAbs innerExp in
           (true, 
            List.map (fun node -> Lv.CLval (Lv.AbsHost node, off)) absTargs)
       | Lv.CAddrOf _ 
       | Lv.CStartOf _ -> 
           (true, [origExp])
       | Lv.CCastE (t, e) ->
           mainAliases e state v
       | _ ->
           let absTargs = ptrExpToAbs origExp in
           (true, List.map 
              (fun node -> 
                (Lv.mkAddrOf (Lv.AbsHost node, NoOffset))) absTargs)
      )

  | Vbot -> 
      (false, [])
        

(** Get the addr of the main lvals associated with the value 
    of the given exp (or return nothing if it's not an lval) *)
let getAliasesExp state (exp:Lv.aExp) : (bool * (Lv.aExp list)) =
  try
    let theVal, newSt = eval state exp None in
    mainAliases exp newSt theVal
  with NotPointer ->
    (* if it's not a pointer, then just return the original lval as we
       may have mis-categorized it as an int or whatever *)
    match exp with
      Lv.CLval _
    | Lv.CStartOf _
    | Lv.CAddrOf _ ->
        (true, [exp])
    | _ ->
        (true, [])

let getAliasesLval state lval : (bool * (Lv.aExp list)) =
  match lval with
    Lv.CVar _, _ ->
      let lvExp = Lv.CLval lval in
      let mustAlias, aliases = getAliasesExp state lvExp in
      (mustAlias, aliases)

  | Lv.AbsHost _, _ ->
      (true, [Lv.CLval lval])

  | Lv.CMem ptrExp, off ->
      (* get the main aliases for the ptrExp, and reconstruct *)
      let mustAlias, aliases = getAliasesExp state ptrExp in
      let results = List.fold_left
        (fun cur aliasPtr -> 
           try 
             let canonLv = Lv.mkMemChecked aliasPtr off in
             let (simpleH, simpleO), _ = Lv.simplifyLval canonLv in
             List_utils.addOnceP expEqual cur (Lv.CLval (simpleH, simpleO))
           with
             CLv.OffsetMismatch om ->
               logError ("getAliasesLval: " ^ CLv.string_of_offsetMiss om);
               cur
        ) [] aliases in
      (mustAlias, results)


(** Substitute the formals in lvalWithFormal w/ the actuals, 
    given the current SymEx state. Also,  *)
let substActForm state actuals lvalWithFormal : bool * Lv.aExp list =
  match Lv.getScope lvalWithFormal with
    SGlobal -> (true, [Lv.CLval lvalWithFormal])
  | SFormal n ->
      (try
         let substituted = substLval actuals lvalWithFormal in
         let mustAlias, results = getAliasesLval state substituted in
         if results = [] then
           logError ~prior:3 ("substActForm returned 0 results for: " ^
                                  (Lv.string_of_lvscope lvalWithFormal))
         ;
         (mustAlias, results)
       with CLv.SubstInvalidArg -> begin
         let arg = List.nth actuals n in
         logError ~prior:3 ("substActForm unsubstitutable arg: " ^
                                (string_of_exp arg) ^ " formal: " ^ 
                                (Lv.string_of_lval lvalWithFormal));
         (true, [])
       end
      ) 
  | _ -> 
      (* It was local var of the other function -- can't substitute. *)
      logError ("substActForm: local variable " ^ 
                    (Lv.string_of_lval lvalWithFormal) 
                  ^ " stayed in summary?");
      (true, [])



(** Extract only the lvals from the list of expressions *)
let lvalsOfExps exps =
  List.fold_left 
    (fun cur e -> match e with 
       Lv.CLval lv -> lv :: cur 
     | _ -> cur) [] exps




(************** Actual Dataflow ***************)

module TopPtrHash = struct
  type t = Cil.typ * Cil.exp * Cil.offset
  let equal (t1, e1, o1) (t2, e2, o2) =
    Ciltools.equal_type t1 t2 &&
      Ciltools.compare_exp e1 e2 == 0 &&
      Ciltools.compare_offset o1 o2 == 0
  let hash (t1, e1, o1) =
    (Ciltools.hash_type t1) lxor 
      (Ciltools.hash_exp e1) lxor 
      (Ciltools.hash_offset o1)
end

module TPH = Cache.Make(TopPtrHash)

module CLVS = Set.Make(struct
  type t = lval
  let compare = Ciltools.compare_lval
end)

let compareReps (p1, o1, t1) (p2, o2, t2) = 
  let c = Alias.Abs.compare p1 p2 in
  if c == 0 then
    let c = Ciltools.compare_offset o1 o2 in
    if c == 0 then
      Ciltools.compare_type t1 t2
    else c
  else c

module ROS = Set.Make(
  struct
    type t = Alias_types.ptaNode * Cil.offset * Cil.typ
    let compare = compareReps
  end)

module ROH = Cache.Make(
  struct
    type t = Alias_types.ptaNode * Cil.offset * Cil.typ
    let equal a b = compareReps a b == 0
    let hash (p, o, t) = 
      Alias.Abs.hash p lxor Ciltools.hash_offset o lxor Ciltools.hash_type t
  end)
    
let compatibleType t1 t2 =
  isVoidType t1 || (Ciltools.equal_type t1 t2)

exception LhsTop of (Cil.exp * Cil.offset * Cil.typ)

(* DEBUG *)
let printROS caption ros =
  let len = ROS.cardinal ros in
  if len == 0 then ()
  else let doc = 
    text caption ++ text "{" ++
      seq_to_doc (Pretty.text ", ") 
      ROS.iter
      (fun (ptNode, off, typ) -> 
         text (Alias.Abs.string_of ptNode ^ 
                 sprint 80 (d_offset nil () off) ^ ":" ^
                 string_of_type typ))
      ros nil ++ dprintf "} (%d)\n" len in
  logStatusD doc

let printAddrs caption targets =
  let len = List.length targets in
  if len == 0 then ()
  else let doc = 
    text caption ++ text "{" ++
      seq_to_doc (Pretty.text ", ") 
      List.iter
      (fun (addr, off) -> 
         d_pointerTarg (addr, off))
      targets nil ++ dprintf "} (%d)\n\n" len in
  logStatusD doc

class symTransFunc (stLat: (symState, SS.sumval) IntraDataflow.stateLattice) = 
object (self)
  inherit [symState] IntraDataflow.idTransFunc stLat as superT
  inherit [symState] IntraDataflow.inspectorGadget stLat "Rad SS: "

  (* TODO: make this available from library? *)
  val mutable firstPP = ppUnknown

  val topPtrCache = TPH.create 512
  val ptNodeCache = ROH.create 512

  method private initializeInitial cfg =
    usedInitialVals := Lvs.empty;
    match cfg.sallstmts with
      h :: t -> firstPP <- getStmtPP h
    | _ -> ()

  method private isFirstStmt pp =
    pp.pp_stmt = firstPP.pp_stmt (* ignore instruction id for now *)

  method handleFunc funID cfg =
    superT#handleFunc funID cfg;
    (* TPH.clear topPtrCache; *)
    self#initializeInitial cfg;
    (* TODO: don't do this hack to set the inspect flag... 
       Actually, it might be cleaner to make them all do this !!! *)
    self#setInspect (Inspect.inspector#mem cfg.svar.vname)

    
  (**************** Handling TOP *******************)
   
  method private havocTarget curSt (targHost, targOff) =
    assignVar curSt targHost targOff Vtop
 
  (** true if the variable is in scope *)
  method private inScope (vi:Cil.varinfo) =
    (* TODO: Do something about including all the globals under the sun?
       Do something about including misc alloc sites vs laziness...
       Throw away function-typed globals as they are immutable. *)
    (vi.vglob && not (Cil.isFunctionType vi.vtype)) ||
      List.exists (fun x -> x.vid = vi.vid) self#curFunc.sformals ||
      List.exists (fun x -> x.vid = vi.vid) self#curFunc.slocals 


  method private addLvalMatchingType baseTyp outerOff curResults baseLv =
    try
      let baseHost, baseOff = baseLv in
      let newLval = Lv.attachOffset baseHost (Cil.addOffset baseOff outerOff) in
      let newType = Lv.typeOfLvalUnsafe newLval in
      if compatibleType baseTyp newType
      then newLval :: curResults
      else curResults
    with 
      CLv.OffsetMismatch om ->
      (*  logError ("aLvMgType: " ^ CLv.string_of_offsetMiss om); *)
        curResults
    | Errormsg.Error
    | Failure _ ->
        curResults


  method private collectTargetRep ptrExp outerOff baseType curSet =
    try
      let ps = Alias.Abs.deref_exp ptrExp in
      List.fold_left 
        (fun cur p -> ROS.add (p, outerOff, baseType) cur) curSet ps
    with Alias.UnknownLoc ->
      logError ("collectTargetRep: unknown " ^ string_of_exp ptrExp);
      curSet


  method private targetRepExpand ptNode outerOff baseType =
    let key = (ptNode, outerOff, baseType) in
    try
      ROH.find ptNodeCache key
    with Not_found ->
      let targets = Alias.Abs.represents ptNode in
      let results = 
        List.fold_left 
          (fun cur clv ->
             let vi = CLv.findBaseVarinfoLval clv in
             if not (self#inScope vi) then cur
             else 
               let lv = Lv.abs_of_lval clv in
               self#addLvalMatchingType baseType outerOff cur lv
          ) [] targets in
      let results = 
        List.map (fun (host, off) -> getAddr host false, off) results in
      ignore (ROH.add ptNodeCache key results);
      results
        
  method private havocPtNodes ptNode off baseType state =
    let targets = Stat.time "resolve_ptNode" 
      (self#targetRepExpand ptNode off) baseType in
(*    printAddrs "havocPtTargets: " targets;  *)
    Stat.time "havoc_ptNode" (* Segregate those that already hit TOP? *) 
      (List.fold_left 
         (fun curSt (addr, finalOff) -> 
            assignVar curSt addr finalOff Vtop
         ) state) targets


  (*********************************)

  (** Get a list of lvals corresponding targets of given ptr (w/ TOP val) *) 
  method private topPtrDerefLval baseType ptrExp outerOff =
    try
      let targets = Alias.deref_exp ptrExp in
      let results = 
        List.fold_left 
          (fun cur vi ->
             if not (self#inScope vi) then cur
             else self#addLvalMatchingType baseType outerOff cur 
               (Lv.hostOfVar vi, NoOffset)
          ) [] targets in
      List.map (fun (host, off) -> getAddr host false, off) results
    with Alias.UnknownLoc ->
      logError ("topPtrDerefLval: unknown " ^ string_of_exp ptrExp);
      []
      
      
  (** Get a list of addrs that this pointer may target *) 
  method private topPtrAddrsLval typ ptrExp outerOff : (symAddr * Cil.offset) list =
    let key =  (typ, ptrExp, outerOff) in
    try TPH.find topPtrCache key
    with Not_found ->
      let result = self#topPtrDerefLval typ ptrExp outerOff in
      ignore (TPH.add topPtrCache key result);
      result

            
  (** Lub against the current values of flow-insensitive alias analysis 
      targets specified by ptrExp and off *)
  method private assignToFITargets baseTyp ptrExp off rhsVal state =
    let addrOffs = Stat.time "resolve_topPtr" 
      (self#topPtrAddrsLval baseTyp ptrExp) off in
    Stat.time "havocTopPtr" (* Segregate those that already hit TOP? *) 
      (List.fold_left 
        (fun curSt (addr, finalOff) ->
          let curVal, midState = evalAddr curSt baseTyp addr finalOff in
          let newVal = combineVals rhsVal curVal in
          assignVar midState addr finalOff newVal
        ) state) addrOffs
        
      
  (*********** Actual transfer func stuff **************)


  (** Handle an lhsLval = rhsVal, in a given state *) 
  method private doAssign lhsLval rhsVal state =
    let rec assignToPointerTarget ptrExp off baseTyp ptrVal curState =
      match ptrVal with
        Vmustptr (addr, innerOff) ->
          (* TODO, check if the target is a summary node... 
             don't do strong update if that's the case      *)
          let newOff = concatOffset addr off innerOff in
          assignVar curState addr newOff rhsVal
            
      | Vmayptr (id, addrOffSet) ->
          (* set values to LUB of old value and rhs for each addr...
             TODO: can also strong update if only 1 target and not summ node? *)
          AddrOffSet.fold 
            (fun (addr, innerOff) curSt ->
               (* Check if lval should be trimmed for termination *)
               let finalAddr, fOff =
                 limitAddrOff baseTyp addr off innerOff in
               let curVal, midState = evalAddr curSt baseTyp finalAddr fOff in
               let newVal = combineVals rhsVal curVal in
               assignVar midState finalAddr fOff newVal
            ) addrOffSet curState
            
      | Vextptr (id, addrOffSet) ->
          (* set values to LUB of old value and rhs for each addr...
             TODO: can also strong update if only 1 target and not summ node? *)
          AddrOffSet.fold 
            (fun (addr, innerOff) curSt ->
               (* Check if lval should be trimmed for termination *)
               let finalAddr, fOff =
                 limitAddrOff baseTyp addr off innerOff in
               let curVal, midState = evalAddr curSt baseTyp finalAddr fOff in
               let newVal = combineVals rhsVal curVal in
               assignVar midState finalAddr fOff newVal
            ) addrOffSet curState

      | Vtop -> 
          (* This val is fine (already top, so any lookups using it 
             return top too), but go through store and LUB w/ other
             addrs that may be aliases of ptrExp->off *)
          raise (LhsTop (ptrExp, off, baseTyp))

      | Vval e when isConst e ->
          logError ~prior:3 ("handleAssign: nullptr deref " ^
                                 (string_of_exp ptrExp));
          curState (* not bottomSymState *)

      | Vbot -> 
          logError ~prior:3 ("handleAssign: nullptr deref " ^
                                 (string_of_exp ptrExp));
          curState (* not bottomSymState *)

      | Vstruct _ ->
          logError ("handleAssign: trying to store to a non-ptr: " ^
                        (string_of_exp ptrExp));
          curState

      | Vval exp ->
          (* convert to a pointer *)
          try 
            let newPtrVal, newState = castAsPointer curState exp in
            assignToPointerTarget ptrExp off baseTyp newPtrVal newState
          with PointerCastError ->
            logError ("handleAssign: Vval not treated as ptr: " ^ 
                          (string_of_exp ptrExp));
            curState

    in
    match lhsLval with
      (* [COPY] (vi.off) = rhsVal *)
      (Var(vi), off) ->
        let host = Lv.hostOfVar vi in
        let lhsOff, isSum = CLv.canonicizeOff off in
        let lhsAddr = getAddr host isSum in
        let newState = assignVar state lhsAddr lhsOff rhsVal in
        newState
          
    (* [STORE] *e.outerOff = rhsVal *)
    | (Mem(ptrExp), outerOff) ->
        let pe = Lv.abs_of_exp ptrExp in
        let ptrVal, lhsState = eval state pe None in
        let newOff, _ = CLv.canonicizeOff outerOff in
        let baseTyp = CLv.typeOfLvalUnsafe (Mem ptrExp, newOff) in
        assignToPointerTarget ptrExp newOff baseTyp ptrVal lhsState 


  method handleAssign lhsLval newVal loc inState =
    let rhs, midState = eval inState (Lv.abs_of_exp newVal) None in
    let finalSt = 
      try self#doAssign lhsLval rhs midState
      with LhsTop (ptrExp, off, baseTyp) ->        
        Stat.time "assign2FI" 
          (self#assignToFITargets baseTyp ptrExp off rhs) midState
    in
    Dataflow.Done (finalSt)


  method private doCallRet lv acts loc inState key = 
    (* Assume x = malloc has been converted to x = &allocSiteGlobal *)
    let summary = stLat#sums#find key in
    let summaryRetVal = SS.getRetValue summary in
    let rhs, midSt = substValue inState acts summaryRetVal in
    try 
      self#doAssign lv rhs midSt
    with LhsTop (ptrExp, off, baseTyp) ->        
      Stat.time "assign2FI" 
        (self#assignToFITargets baseTyp ptrExp off rhs) midSt
        

  method handleCallRet lv targs callexp acts loc inState =
    List.fold_left 
      (fun curSt sumKey ->
         let retted = self#doCallRet lv acts loc inState sumKey in
         stLat#combineStates curSt retted
      ) bottomSymState targs (* changed from inState *)

  method private doCallArgs actuals loc inState key =
    try
      let mods = !modSumms#getMods key in
      let toSmash, repNodes = List.fold_left
        (fun (smash, reps) ((sumHost, sumOffset), sumScope) -> 
           match sumScope with
             SFormal n -> begin
               let arg = List.nth actuals n in
               let cilLval = Lv.lval_of_abs_simple (sumHost, sumOffset) in
               try
                 let subst = CLv.substActForm arg cilLval in
                 (CLVS.add subst smash, reps)
               with CLv.SubstInvalidArg ->
                 (smash, reps)
             end
           | SGlobal -> begin
               let lv = sumHost, sumOffset in
               try 
                 (CLVS.add (Lv.lval_of_abs_simple lv) smash, reps)
               with Lv.IsAbstract ->
                 let node = Lv.node_of_absLval lv in (* TODO: get offset? *)
                 let baseTyp = TVoid [] in 
                 (* TODO... how to limit? Attach type to refine rep node? *)
                 let baseOff = NoOffset in
                 (smash, ROS.add (node, baseOff, baseTyp) reps)
             end
           | _ ->
               failwith "SS: mod scope not resolved / filtered"
        ) (CLVS.empty, ROS.empty) mods in
      let st, repNodes = 
        CLVS.fold 
          (fun clv (curSt, repNodes) -> 
             try (self#doAssign clv Vtop curSt, repNodes)
             with LhsTop (ptrExp, off, baseTyp) ->
               (curSt, self#collectTargetRep ptrExp off baseTyp repNodes)
          ) toSmash (inState, repNodes) in

(*      printROS "REPS: " repNodes; *)
      
      ROS.fold
        (fun (ptNode, off, typ) curSt ->
           self#havocPtNodes ptNode off typ curSt
        ) repNodes st
           
(*
      List.fold_left
        (fun curSt ((sumHost, sumOffset), sumScope) -> match sumScope with
           SFormal n -> begin
             (* Do straight-up subst on cil lvals / exps and 
                use handleAssign w/ Vtop as the rhs *)             
             let arg = List.nth actuals n in
             let cilLval = Lv.lval_of_abs_simple (sumHost, sumOffset) in
             try
               let subst = CLv.substActForm arg cilLval in
               self#doAssign subst Vtop curSt
             with CLv.SubstInvalidArg ->
               curSt

(*
             let absArg = Lv.abs_of_exp arg in
             try
               let _, substLvs = Lv.substActForm absArg (sumHost, sumOffset) in
               let ptrExp = (Lv.mkAddrOf substLv) in 
               (* ... messy way of getting targets-to-mod in simple form *)
               let ptrToMod, evalState = 
                 eval curSt ptrExp None in
               self#havocPtr evalState substLv ptrToMod
             with CLv.SubstInvalidArg ->
               curSt
*)
           end
         | SGlobal ->
             let newOff, isSum = CLv.canonicizeOff sumOffset in
             let newAddr = getAddr sumHost isSum in
             self#havocTarget curSt (newAddr, newOff)
  (* call doAssign instead? handle REP nodes more carefully? *)             
         | _ ->
             failwith "SS: mod scope not resolved / filtered"
        )
        inState mods
        *)
    with
      Not_found ->
        (* All summaries should be initialized *)
        failwith "SS: modSumms#get returned Not_found"
    | Modsummaryi.BottomSummary ->
        logError ("SS: modSumm is BOTTOM: " ^ (Callg.fid_to_string key));
        bottomSymState
          
          
  method handleCallExp targs callexp acts loc inState =
    List.fold_left 
      (fun curSt sumKey ->
         let argged = Stat.time "rad SS modSums" 
           (self#doCallArgs acts loc inState) sumKey in
         stLat#combineStates curSt argged
      ) bottomSymState targs (* changed from inState *)
      
  method handleGuard gexp inState =
    if stLat#isBottom inState then
      Dataflow.GUnreachable
    else
      begin
        (* TODO: eval expression ? *)
        let foldedExp = Cil.constFold true gexp in
        (* TODO: Maybe use guard const info, but will need a way
           of removing assumption at join points...
           null ptr info could help actually *)
        match foldedExp with
          BinOp (Ne, e, Const (ckind), _)
        | BinOp (Ne, Const (ckind), e, _) ->
            (* Saying e != c *)
            Dataflow.GDefault

        | BinOp (Eq, e, Const (ckind), _)
        | BinOp (Eq, Const (ckind), e, _) ->
            (* Saying e == c *)
            Dataflow.GDefault
              
        | Lval ((_, _)) ->
            (* Checking if lval is not null *)
            Dataflow.GDefault
              
        (* handle !'s in succession *)
        | UnOp (LNot, UnOp (LNot, exp, _), _) ->
            self#handleGuard exp inState
              
        (* handle negations of Ne and Eq *)
        | UnOp (LNot, BinOp (Ne, e1, e2, typ), _) ->
            self#handleGuard (BinOp (Eq, e1, e2, typ)) inState
        | UnOp (LNot, BinOp (Eq, e1, e2, typ), _) ->
            self#handleGuard (BinOp (Ne, e1, e2, typ)) inState
        | _ -> 
            Dataflow.GDefault     
      end

  method handleInstr i st =
    self#inspectInstBefore i st;
    let result = superT#handleInstr i st in
    self#inspectInstAfter i result;
    result

  method handleStmt stmt state =
    self#inspectStmtBefore stmt state;
    let result = superT#handleStmt stmt state in
    self#inspectStmtAfter stmt result;
    result

end

(************************** Packing for Radar *************************)

let fullLattice = new symLattice
let seqTrans = new symTransFunc fullLattice

let init settings cg modSum =
  setModSumm modSum;
  seqTrans#setCG cg


class symSummarizer (stLat : (symState, SS.sumval) IntraDataflow.stateLattice) 
  (sums : SS.sumval BS.base) : [symState, SS.sumval] IntraDataflow.summarizer 
  = object (self)
  
  val mutable inspect = false

  method setInspect yes =
    inspect <- yes
    
  method scopeIt (curFunc:fundec) sum = 
    SS.scopePrune curFunc sum
      
  (** Combine values at return statements *)
  method private makeSummary func getState =
    let combineV s exp curVal =
      let pp = getStmtPP s in
      let state = getState pp in
      let newVal, _ = eval state (Lv.abs_of_exp exp) None in
      combineVals curVal newVal
    in
    List.fold_left 
      (fun cur s ->
         (* Consider Return statements that return values *)
         match s.skind with
           Return (Some(exp), _) ->
             combineV s exp cur
         | _ ->
             cur
      ) Vbot func.sallstmts

  method summarize fkey func getState =
    let outVal = self#makeSummary func getState in
    let dummyInSt = stLat#initialState in
    let newOut = SS.sumOfRetVal dummyInSt outVal in
    let newOut = self#scopeIt func newOut in
    let oldOut = stLat#sums#find fkey in
    if stLat#stateSubset newOut oldOut then
      false
    else 
      let combOut = stLat#combineStates newOut oldOut in
      stLat#sums#addReplace fkey combOut;
      true

  method flushSummaries () =
    sums#serializeAndFlush (* See note above about summarize and using sums *)

end


(** Not getting pre-specified summaries for now *)
let sumIsFinal fkey =
  false



(************************************************************
           Old, non-radar Run and stuff
************************************************************)


module SymStateDF = struct

  let name = "symbolic execution"

  let debug = false (* TODO: make this a ref, so it can be toggled *)

  type st = symState

  type sum = SS.sumval

  let stMan = (fullLattice :> (st, sum) IntraDataflow.stateLattice)

  let transF = ref (seqTrans :> st IntraDataflow.transFunc)
    
end


module SymStateFwd = IntraDataflow.FlowSensForward (SymStateDF)

let getSymstate pp = 
  SymStateFwd.getDataBefore pp

let getFISymstate () =
  failwith "TODO getFISymstate"

(** Initialize the symbolic state before analzying the given func
    and initialize the dataflow facts *)
let initState funID (func:Cil.fundec) : unit = begin
  curPtrID := 0;
  SymStateFwd.initialize funID func SymStateDF.stMan#initialState;
end


(** Evaluate the symbolic store for a given function.
    Assumes func has CFG info computed (e.g., func.sallstmts is valid)
    Returns true if the return value summary is updated   *)
let doSymState funID (func:Cil.fundec) : unit =
  initState funID func;
  SymStateFwd.compute func


(** Package the symstate analysis *)
class symexAnalysis = object (self)
  
  val summarizer = new symSummarizer fullLattice SS.sum

  method setInspect (yesno:bool) =
    () (* transFuncs set themselves when you call handleFunc *)

  method isFinal key =
    sumIsFinal key
      
  method compute funID cfg =
    logStatus "doing symstate";
    flushStatus ();
    Stat.time "Computing rad_symstate DF: " (doSymState funID) cfg
      
  method summarize key cfg =
    if self#isFinal key then false
    else summarizer#summarize key cfg getSymstate
        (* In other phases, can override to not summarize! *)
      
  method flushSummaries () =
    summarizer#flushSummaries ()

end

(****************** Debug *******************)



(** Get combination of symbolic states at return statements *)
let combRetStates func =
  let combineSt s cur =
    let pp = getStmtPP s in
    let state = getSymstate pp in
    SymStateDF.stMan#combineStates cur state
  in
  List.fold_left
    (fun cur s ->
       match s.skind with
         Return (_, _) ->
           combineSt s cur
       | _ ->
           cur
    ) bottomSymState func.sallstmts
    

let printExitState () =
  let func = !SymStateFwd.curFunc in
  let outState = combRetStates func in
  fullLattice#printState outState 


(****************** External API (+ helpers) ******************)


(* MUST ALIAS FLAG set to true for now *)

(** Get the canonical pointed-to targets of an lval. 
    E.g., given lv == "*x" and the state at this point says
    that x \mapsto a pointer to y, then return y. *)
let derefALvalAt (pp:prog_point) (lv:Lv.aLval) : (bool * (Lv.aLval list)) =

  let rec derefLvalHelper ptrExp outerOff curState ptrVal =
    let derefSet addrOffSet =
      AddrOffSet.fold 
        (fun (addr,off) curList ->
           (concretizeLval (addr, Cil.addOffset outerOff off)) :: curList
        ) addrOffSet []
    in

    match ptrVal with 
      Vmustptr (addr, off) -> 
        let results = [(concretizeLval (addr, Cil.addOffset outerOff off))] in
        (true, results)

    | Vmayptr (id, addrOffSet) ->
        let results = derefSet addrOffSet in
        (true, results)
          
    | Vextptr (id, addrOffSet) ->
        let results = derefSet addrOffSet in
        (true, results)
          
    | Vtop ->
        (try
           let nodes = Lv.deref_absExp ptrExp in
           (true, List.map (fun node -> (Lv.AbsHost node, outerOff)) nodes)
         with Alias.UnknownLoc ->
           logError ("derefLvalHelper: Vtop has no targets " ^ 
                         Lv.string_of_exp ptrExp);
           (true, [])
        )

    | Vval (Lv.CConst _)
    | Vbot -> 
        logError ~prior:3 ("derefLvalHelper: deref a nullptr " ^
                        (Lv.string_of_exp ptrExp));
        (true, [])
          
    | Vval exp -> begin
        (* convert to a pointer *)
        try 
          let newPtrVal, newState = castAsPointer curState exp in
          derefLvalHelper ptrExp outerOff newState newPtrVal
        with PointerCastError ->
          logError ("derefLvalHelper: Vval not treated as ptr: " ^ 
                          (Lv.string_of_exp ptrExp));
          (true, [])
      end
    | Vstruct _ ->
        logError ("derefLvalHelper: eval returned non-ptr for ptrexp " ^
                        (Lv.string_of_exp ptrExp));
        (true, [])
  in
  match lv with
    (Lv.CMem(ptrExp), outerOff) ->
      let state = getSymstate pp in
      let ptrVal, newSt = eval state ptrExp None in
      let newOff, _ = CLv.canonicizeOff outerOff in
      let must, lvals = derefLvalHelper ptrExp newOff newSt ptrVal in
      (must, List.map (fun lv -> Lv.mergeLv lv) lvals)
  | _ ->
      (true, [])

let derefLvalAt (pp:prog_point) (lv:Cil.lval) : (bool * (Lv.aLval list)) =
  derefALvalAt pp (Lv.abs_of_lval lv)

let getAliasesAt (pp:prog_point) (lv:Lv.aLval) : (bool * (Lv.aExp list)) =
  let state = getSymstate pp in
  getAliasesLval state lv


(**************** New substitution functions *************)


(** Substitute the formals in lvalWithFormal w/ the actuals,
    given the current program point. Also, translate the substituted
    result in terms of formals and globals (or other "main aliases").
    Expect lvals returned. *)
let substActForm2 pp actuals lvalWithFormal : bool * Lv.aLval list =
  let state = getSymstate pp in
  let mustAlias, results = substActForm state actuals lvalWithFormal in
  (mustAlias, lvalsOfExps results)

let substActForm2FI actuals lvalWithFormal : bool * Lv.aLval list =
  let state = getFISymstate () in
  let mustAlias, results = substActForm state actuals lvalWithFormal in
  (mustAlias, lvalsOfExps results)


(** Substitute the formals in lvalWithFormal w/ the actuals,
    given the current program point. DO NOT translate in terms
    of formals and globals. Any expression can be returned. *)
let substActForm3 pp actuals lvalWithFormal : Lv.aLval list =
  match Lv.getScope lvalWithFormal with
    SGlobal -> [lvalWithFormal]
  | SFormal n ->
      (try
         let substituted = substLval actuals lvalWithFormal in
         [substituted]
       with CLv.SubstInvalidArg ->
         let arg = List.nth actuals n in
         logError ("substActForm3 unsubstitutable arg: " ^
                       (string_of_exp arg));
         []
       )
  | _ -> 
      (* It was local var of the other function -- can't substitute. *)
      logError ("substActForm3: local variable " ^ 
                    (Lv.string_of_lval lvalWithFormal) 
                  ^ " stayed in summary?");
      []

