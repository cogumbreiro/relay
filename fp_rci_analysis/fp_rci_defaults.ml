

(** Generate default structures as parameters to root functions *)

open Cil
open Type_utils
open Fp_rci_types
open Fp_rci_lattice_ops
open Fp_rci_malloc
open Fp_rci_store
open Logging

(* How to handle:
   
   (1) void *
      - create on demand?
   (2) Pointers INTO an object (for use w/ container_of) 
   (3) Handle different types of values at the same offset w/ Unions ?

*)

(* Doesn't necessarily have bindings for variables mentioned 
   in the "default val" yet. This can mess up aggressive merge,
   so we should turn off aggressive merge while it is doing this... *)


module TH = Hashtbl.Make(Cil_lvals.HashedTyp)

let canonicalLocs = TH.create 17

let nextVoid = ref 0
let getNextVoidID () =
  let id = !nextVoid in
  incr nextVoid;
  id

(* Not using getInitialValue... *)

let getDefaultLocForVoid st =
  let typToMake = Trans_alloc.unknownMallocType in
  let id = getNextVoidID () in
  let var, att = makeMalloc typToMake ({pp_stmt = id; pp_instr = 0;}, -1) in
  ((var, noOffSum), 
   { st with vAttrs = VarMap.add var att st.vAttrs; })
    

let getDefaultLocForTyp typ st =
  if isVoidType typ then getDefaultLocForVoid st
  else
    let var, off, att = 
      try TH.find canonicalLocs typ
      with Not_found ->
        let newVar, newAtt = 
          Fp_rci_malloc.makeMalloc typ topLevelID in
        TH.add canonicalLocs typ (newVar, noOff, newAtt);
        (newVar, noOff, newAtt)
    in
    (* add heap attribute too *)
    ((var, (off, false)), 
     { st with vAttrs = VarMap.add var att st.vAttrs; })


let debugTypeSqliteHash typ =
  match Cil_lvals.unrollTypeNoAttrs typ with
    TComp (ci, _) -> ci.cname = "HashElem" || ci.cname = "_ht"
  | _ -> false


(** Generate a default fvalue for a given type, adding a binding to the 
    state if necessary. Visited indicates that a value is currently
    being generated to make a binding for a (variable, offset) pair *)
let rec helpGetDefaultValForTyp typ st vis =
  match typ with
   	TVoid _ -> (emptyRecord, st, vis)
  | TFloat (fk, _) -> (FNFP constNfpSource, st, vis)
  | TBuiltin_va_list _ -> (FNFP rootSource, st, vis)
  |	TNamed (tinfo, _) -> helpGetDefaultValForTyp tinfo.ttype st vis
  | TInt (ik, _) -> (FNFP constNfpSource, st, vis)
      (* hmm.. see synclink for case where unsigned int becomes ptr... *)
  |	TEnum (ei, _) -> (FNFP constNfpSource, st, vis)
  | TPtr (t, _) ->
      if isFunctionType t 
      then (FpRef extVar#getVar, st, vis)
      
      (* Temporarily make VOID * non-funptr-related ptrs *)
      else if Type_reach.isPoly typ then (FNFP rootSource, st, vis)
        (* / *)

      (* Temporarily make sqlite hashtbls non-funptr-related ptrs *)
      else if debugTypeSqliteHash t then (FNFP rootSource, st, vis)
        (* / *)

      else
        (if Type_reach.isPoly t then 
             logError "default arg is polymorphic";
         (* Get the location of target *)
         let t = canonType t in
         let (var, off), newSt = getDefaultLocForTyp t st in
         let newSt, vis = 
           if FLocSet.mem (var, off) vis then (newSt, vis)
           else begin 
             (* make a value for the target and make a binding *)
             let vis = addToPtrSet (var, off) vis in
             let v, newSt, vis = helpGetDefaultValForTyp t newSt vis in
             if isNoOffsetSum off then begin
               addBinding newSt var v, vis
             end
             else
               failwith "Not handling offsets returned by defaultLocForTyp"
           end
         in
         (* (Refs (FLocSet.singleton (var, off)), newSt, vis)) *)
         (makeMayref (var, off) (nullVar#getLoc), newSt, vis))
          
  | TArray (t, sizeExpOpt, _) ->
      (* For now, just make it the value of one element instead of a reference 
         to a location w/ that element... a little lazy to treat
         array indexing just like a pointer deref right now
         (in order to really access the element) *)
      helpGetDefaultValForTyp t st vis
        
  | TFun (retT, argTOpt, varArg, _) -> 
      failwith "helpGetDefaultValForTyp: given function directly?"
        
  |	TComp (ci, _)-> 
      getDefaultRecord ci typ st vis
      
  
and getDefaultRecord ci baseTyp st vis =
  (* Might already have tried to create the record in a previous round,
     but there might not be a binding for each of the field targets,
     for this particular state, so we should just regenerate anyway
     to get the bindings in ... *)
  let (newfm, newm), newSt, vis =
    List.fold_left 
      (fun ((fm, m), curSt, vis) fi ->
         let off, _ = cilOff2Offset false baseTyp (Field (fi, NoOffset)) in
         let newVal, newSt, vis = helpGetDefaultValForTyp fi.ftype curSt vis in
         let newfm, newm = updateOffsetMap off newVal fm m in
         ((newfm, newm), newSt, vis)
      ) ((OffsetMap.empty, noOff), st, vis) (!Cil.getCfields ci) in
  let newrecs = flattenRecord (newfm, newm) in
  let result = makeNormalizedRec newrecs in
  (result, newSt, vis)


and updateOffsetMap off v fm curMax =
  let newVal = 
    try 
      let oldV = OffsetMap.find off fm in 
      combineVals valBaseNone oldV v
    with Not_found -> v
  in
  (OffsetMap.add off newVal fm, maxOff curMax off)
    (* For now, we assume this is only used w/ null function pointers, 
       so the merge shouldn't be bad *)


let getDefaultValForTyp typ st =
  let vis = FLocSet.empty in
  let v, st, vis = helpGetDefaultValForTyp typ st vis in
  (v, st)
    
