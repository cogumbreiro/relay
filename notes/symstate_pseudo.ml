
open Cil
open Pretty

(*********************************************************
 * Misc setup
 *********************************************************)

module StringMap = Map.Make(String)

module OrderedInt = {
  type t = int
  let compare = Pervasives.compare
}

module IntMap = Map.Make(OrderedInt)


(*********************************************************
 * Definition of state 
 *********************************************************)

(* Symbolic expression can represent values derived from a single exp
 * or it can represent values that extending from an exp.
 * E.g., "#c1->next->next" will be represented as (SymTrans "#c1") 
 * Ideally, "#c1->next->data" will be represented as 
 * (SymTrans ("#c1", "data")), but we'll look at that later
 *
 * NOTE: symExps only reference symbolic variables, no program variables
 *) 
type symExp = 
    SymSimple of Cil.exp
  | SymTrans  of Cil.exp

(* Program variables are represented by strings based on variable name.
 * Symbolic variables just have a "#" prepended
 *)
(*
 * Variable store is map from string to symExp
 *)
type symStore = symExp StringMap.t

(* Symbolic memory is represented as a list of writes for now.
 * Newer writes pushed to front 
 *)
type symMem = (symExp * symExp) list (* = (address, value_written) list *)


(* per program-point state *)
type symState = {
  sigma : symStore ;
  mu : symMem ;
  assumptions : symExp list ; 
     (* things we have assumed to be true along this path *) 
  (** etc. **)
} 


(* Have a (reverse) map from symbolic variable names to the set of 
 * formals / globals / allocation sites they represent
 *)
type symVarMap = ...

(* Map is flow insensitive, since no instruction ever assigns to symbolic
 * variables directly. I.e., they are constants.
 *)
let (sigmaSharp:symVarMap) = ...




(*********************************************************
 * Operations on state 
 *********************************************************)

(* returns a new state in which 'value' has been written to memory heap
 * location 'address' *)
let updateMemory (oldState:symState) (address:symExp) 
    (value:symExp) : symState =
  { oldState with mu = (address,value) :: oldState.mu } 


(* returns a new state in which variable 'var' has been assigned
 * the new value 'exp' *)
let assignVar (oldState:symState) (var:string) (exp:symExp) : symState = 
  let oldSigma = oldState.sigma in 
  { oldState with sigma = StringMap.add var exp oldsigma }

(* looks up the value of 'var' in the state. May raise Not_found *)
let lookupVar (state:symState) (var:string) : symExp = 
  StringMap.find var state.sigma 

let sym_var_counter = ref 0
(* Create a symbolic variable associated w/ a set of concrete variables. 
 * Optionally pass a base name *)
let freshVariable ?(name:string) (assocSet:...) : symExp = 
  let str = 
    match name with
    | None -> "#" 
    | Some(n) -> "#" ^ n
  in
  (*** update sigmaSharp, etc... ***)

let isSymVar (name:string) =
  (* check prefix *)


(*********************************************************
 * Expression Evaluation
 *********************************************************)


(* Convert a cil expression (ce) to a symbolic expression. 
 * This will look up variables and get symbolic variables, or
 * look up heap values and get expressions involving symbolic variables.
 * It also abstracts complex expressions 
 *)
let rec eval (s:symState) (ce:Cil.exp) : symExp = match ce with
    (* Simple variable reference *)
    Lval(Var(vi),NoOffset) -> begin  
      try lookupVar s vi.vname (* can assert vi.vname is a program var *)
      with Not_found -> 
         prerr_string "lookupVar failed -- accessing uninit value?";
        freshVariable ()
    end
      
    (* Simple pointer dereference *)
  | Lval(Mem(readAddr),NoOffset) ->
      (** look up in heap **)

  (* If the lval is accessed through offsets (field offsets, array index,
     arbitrary pointer arith), it maybe be too complex, so
     abstract. E.g., list->next->next
  *) 
  | Lval(host,offsets) ->
  | Lval(host,offsets) ->
      abstract (ce)
  | BinOp(PlusPI,host,offsets,_) ->
      abstract (eval host) offsets

  (** ... Unops, other Binops etc. **)
  (** May also want to abstract arith. E.g., don't want x + 1 the first
   *  iteration, the x + 1 + 1, the next iteration
   *)
  | UnOp(unop,ce,loc) -> 
      UnOp(unop, eval s ce, loc)
  | BinOp(bop,ce1,ce2,loc) -> begin
      match bop, (eval s ce1), (eval s ce2) with
        (* in a few cases we can compute this "in-line" without pushing it 
         * off to the theorem prover -- this is typically a worthwhile
         * optimization for scalability but might nott be worth it to you
         * in this homework *) 
      | PlusA, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          Const(CInt64(Int64.add i1 i2, ik1, None))
      | MinusA, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          Const(CInt64(Int64.sub i1 i2, ik1, None))
      | Mult, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          Const(CInt64(Int64.mul i1 i2, ik1, None))
      | Shiftlt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          Const(CInt64(Int64.shift_left i1 (Int64.to_int i2), ik1, None))
      | Shiftrt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          Const(CInt64(Int64.shift_right i1 (Int64.to_int i2), ik1, None))
      | Lt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          se_of_bool (i1 < i2) 
      | Le, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          se_of_bool (i1 <= i2) 
      | Gt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          se_of_bool (i1 > i2) 
      | Ge, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
          se_of_bool (i1 >= i2) 
      | x, y, z -> BinOp(x,y,z,loc)
    end 
  | CastE(_,ce) -> eval s ce
  | x -> x
      

(* Abstract the given expression. Either it does nothing, or simplifies
 * complicated derefs. TODO determine function interface
 *)
and abstract (host) (offsets) : symExp = 
  (*** 
   * If traversing the offset chain will result in an object
   * of the same type as the host, cut off that part of the chain and
   * tag as SymTrans, otherwise tag as SymSimple
   *)



(* Trace back the values of the symExps to a primitive set of symExps *)
let rec symEval (s:symVarMap) (se:symExp) : SymExpSet.t =
  (* Keep the structure of the se in tact, but everywhere a symbolic
     variable shows up, look up its sources, and split on the expression
     E.g., if se = (SymTrans #c3->list) and #c3 ~ {x, v1} then the result is
     {(SymTrans x->list), (SymTrans v1->list)}
  *)
  


(*********************************************************
 * Initialization
 *********************************************************)
  
let initState (fun:Cil.fundec) (dfFacts:...) 
    : symState =
  (* Set all dfFacts to $BOTTOM *)
  
  (* Traverse fun, looking for formals, globals referenced, and 
   * allocation sites (calls to malloc, etc.).
   * Create fresh symbolic variables for those (which also updates sigmaSharp)
   *)

  (* return an empty symState *)


          

(*********************************************************
 * Intra-proc DF analysis
 *********************************************************)

(* Analyze a given instruction ... *)
let handleInstr (i:Cil.instr) (s:symState) =
  match i with
    (* x = newVal *)
    Set((Var(vi),NoOffset), newVal, location) -> 
      let newSymVal = eval s newVal in
      (Some (assignVar s vi.vname newSymVal))
        
    (* *x = newVal *)
  | Set((Mem(ptrExp),NoOffset),newVal,location) -> 
      let ptrSymExp = eval s ptrExp in 
      let newSymVal = eval s newVal in 
      let update_memory s (ptrSymExp) (newSymVal) 

    (* Assignment to complex lval..., e.g., x->next = newVal *)
  | Set(Lval,newVal,location) -> 

  | Call(retval_option, (Lval(Var(va),NoOffset)), args, loc) -> 

  | Call(retval_option, function_ptr_exp, args, location) ->

  | Asm(_) -> 


(* Some (-1) if sv1 is a subset of the values of sv2
   Some (0)  if sv1 = sv2
   Some (1)  if sv2 is a subset of the values of sv1
   None      if incomparable
   If they don't refer to the same symbolic variables, incomparable
   also, SymSimple x <= SymTrans x
 *)
let symValCompareStrict (sv1:symExp) (sv2:symExp) : int option =
  let valCompare = compare in
    
  match sv1, sv2 with
    SymSimple v1, SymSimple v2 ->
  | SymTrans v1, SymTrans v2 ->
      if ((valCompare v1 v2) == 0) then
        Some 0
      else
        None
  | SymTrans v1, SymSimple v2 ->
      if ((valCompare v1 v2) == 0) then
        Some 1
      else
        None
  | SymSimple v1, SymTrans v2 ->
      if ((valCompare v1 v2) == 0) then
        Some -1
      else
        None
      


(* Some (-1) if sv1 is a subset of the values of sv2
   Some (0)  if sv1 = sv2
   Some (1)  if sv2 is a subset of the values of sv1
   None      if incomparable
   If they don't refer to the same symbolic variables, check if 
   symbolic variables are linked to the same sources 
   also, SymSimple x <= SymTrans x
 *)
let symValCompareRelaxed (sv1:symExp) (sv2:symExp) : int option =
  let valCompare v1 v2 =
    let svSet1, svSet2 = symEval v1, symEval v2 in
    if (SymExpSet.equal svSet1 svSet2) then
      Some 0
    else if (SymExpSet.subset svSet1 svSet2)
      Some -1
    else if (SymExpSet.subset svSet2 svSet1)
      Some 1
    else
      None
  in
  
  match sv1, sv2 with
    SymSimple _, SymSimple _ ->
  | SymTrans _, SymTrans _ ->
      valCompare sv1 sv2
  | SymTrans v1, SymSimple v2 ->
      let c = valCompare v1 v2 in
      if (c == 0) then
        Some 1
      else
        None (* Will this ever happen? Is it comparable? *)
  | SymSimple v1, SymTrans v2 ->
      let c = valCompare v1 v2 in
      if (c == 0) then
        Some -1
      else
        None


(* Split se1 and se2 based on the longest common suffix S, 
 * leaving a prefix P1 and P2. Returns (P1, P2)
 *)
let splitConflict (se1:symExp) (se2:symExp) : (symExp * symExp * symExp) =
  (* ... *)


(* Unify conflicts *)
let unifyConflicts (conflicts:string list) (newSig:symStore) 
    (curSig1:symStore) (curSig2:symStore) : symStore =
  match conflicts with
    [] -> newSig
  | varname :: confsLeft ->
      let sv1 = StringMap.find varname curSig1 in
      let sv2 = StringMap.find varname curSig2 in

      (* 1) March along both sv1 and sv2 expressions, finding the longest
       * common suffix S, leaving a prefix P1 and P2. 
       * 2) Create a fresh variable #c to represent the P1 in sig1 
       * and P2 in sig2. #c is linked to the union of what P1 and P2
       * are linked to
       * 3) For each varname binding in conflicts, apply substitution 
       * of #c for P_i in curSig_i
       * 4) Add (varname -> #cS) binding in newSig
       * 5) Recurse on w/ new maps
       *)
      let P1, P2 = splitConflict sv1 sv2 in
      
      




(* Merge two states *)
let combineStates (s1:symState) (s2:symState) : symState =
  (* Merge sigma *)
  let sig1 = s1.sigma in
  let sig2 = s2.sigma in

  (* Partition the sigmas into the simple case (prog vars map to 
     comparable values) and the conflicts
  *)
  let newSig1Simple = 
    StringMap.fold (fun vn symval1 curNewSig ->
                      try 
                        let symval2 = StringMap.find vn sig2 in
                        () (* handle later *)
                      with Not_found -> (* sig2 doesn't have binding *)
                        StringMap.add vn symval1 curNewSig
                   ) sig1 StringMap.empty in
  let (newSigSimple, conflicts) =
    StringMap.fold (fun vn symval2 (curNewSig, curConflicts) ->
                      try 
                        let symval1 = StringMap.find vn sig1 in
                        match symValCompareRelaxed symval1 symval2 with
                          Some 0 -> 
                        | Some 1 ->
                            (StringMap.add vn symval1 curNewSig, curConflicts)
                        | Some -1 ->
                            (StringMap.add vn symval2 curNewSig, curConflicts)
                        | None ->
                            (curNewSig, vn :: curConflicts)
                      with Not_found -> (* sig1 doesn't have binding *)
                        (StringMap.add vn symval2 curNewSig, curConflicts)
                   ) sig2 (newSig1Simple, []) in


  (* Now handle conflicts by unification *)
  let finalSigma = unifyConflicts conflicts newSigSimple sig1 sig2 in
      



  (* Merge heap, assumptions? *)
  (* Need a more convenient representation of heap than just a list
     of writes. Problem is what do you do with writes to the same addr,
     but are older?
  *)



(* Check if s1 is a subset of s2 *)
let statesSubset (s1:symState) (s2:symState) : bool =
  let sig1 = s1.sigma in
  let sig2 = s2.sigma in
  (* for all symval1 in sig1, symval1 must have a mapping in sig2, and
   * the value of symval1's mapping must be <= symval2's mapping
   *) 
  StringMap.fold (fun vn symval1 maybeSubs ->
                    if (maybeSubs) then
                      try 
                        let symval2 = StringMap.find vn sig2 in
                        match symValCompareRelaxed symval1 symval2 with
                          Some 0 ->
                        | Some -1 ->
                            true
                        | false
                      with Not_found ->
                        false
                    else
                      false
                 ) sig1 true
    





(* Adaption to Cil DF framework  *)
module SymStateDF = struct

  (* Use symex state *)
  type t = symState

  (* Make a hashtable of dataflow facts for each statement
   * Indexed by statement id *)
  let stmtStartData: t IH.t = IH.create 17
    
  let copy (d: t) = d
     
  let computeFirstPredecessor (s: stmt) (d: t) : t = 
    d


  (* Combine old fact at this statement, w/ newD. Also, detect fixed point *)
  let combinePredecessors (s: stmt) ~(old: t) (newD: t) : t option =
    let result = combineStates old newD in
    if (statesSubset result old) then
      None
    else
      Some (result)
          
      
  let doInstr (i: instr) (d: t) = 
    match handleInstr i d with
      None -> DF.Default
    | Some (newD) -> DF.Done newD
      
  let doStmt (s: stmt) (d: t) = 
    DF.SDefault 

  (* TODO use value of the guard, add to assumptions *)  
  let doGuard (gexp: Cil.exp) (d: t) =
    DF.GDefault
      
      
  let filterStmt _ = true

end


