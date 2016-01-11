open Logging
open Pretty
open Fp_rci_types
open Fp_rci_lattice_ops
open Fp_rci_store
open Fp_rci_globals

(*********************************************************
 * Focusing state based on a target 
 *********************************************************)

(************************************************************)

let rec consumeConcats o curPath =
  match curPath with
    AConcatOff off :: rest 
  | AReadField off :: rest -> consumeConcats (concatOffset off o) rest
      (* TODO: double-check if we can consume AReadField too *)
  | _ -> o, curPath
      
exception UnreachableState
exception ConcurrentModRec of recordPart list


let getAccGlobalBindings accPath startState =
  let addGlobal (var, _) cur = 
    if isSpecialVar var then cur
    else 
      if not (hasBinding var cur) && 
        (isGlobalDebug "getAccGlob" true startState.vAttrs var) then begin
          (* already registered the reader while getting accPath... *)
          logStatusF "Bringing in global for focus: %s\n" (string_of_var var);
          let newSt = bringInGlobalNoRep var cur GNone in
          newSt
        end else cur
  in
  List.fold_left 
    (fun cur acc ->
       match acc with
         AReadVar var -> addGlobal (var, noOff) cur
       | AReadField _ -> cur
       | AConcatOff _ -> cur
       | ADeref locs ->
           FLocSet.fold addGlobal locs cur
    ) startState accPath


(* TODO: w/ all these summary nodes... we need to be careful!!! 
   E.g., if x is a summary node with
   x->fp = {f1, f2} and x->next = {null, x}, then
   x->fp(x) should yield a call to 
   - f1 w/ (y->fp = f1, and y->next = {null, x})
   - f2 w/... similar
   instead of 
   - f1 w/ ... and y->next = {null, y}
*)

let raiseUnreach from =
(*
  logErrorF "focus unreach %s\n" from;
*)
  raise UnreachableState

let filterReach accPath (targV, targO) startState shouldReach =
  logStatusF "filterReach w/ accessPath: ";
  printAccPath accPath;
  logStatusF "Target (%b): %s\n" shouldReach (string_of_pointer (targV, targO));
  flushStatus ();
    
  let startState = getAccGlobalBindings accPath startState in
  let curState = ref startState in

  let goalReached reaches terminal = 
    (shouldReach && reaches) || 
      (not shouldReach && (not terminal || not reaches)) 
      (* Only prune the "not shouldReach" at the terminal case *)
  in

  let rec filterReachVar curVar curPath : bool =
    if nullVar#isVar curVar 
    then raiseUnreach ("filterReachVar null ")
    else if nfpVar#isVar curVar then true 
    else if extVar#isVar curVar then true
      (* TODO: Nfp only means any non-FP... what if reach-target is func??? 
         Ugh... totally need a 3rd value *)
    else
      try
        let oldVal = getBinding curVar !curState in
        let newVal, reaches = filterReachVal curVar oldVal curPath in
        if goalReached reaches false then  (* others check terminal *)
          ( if eqVals oldVal newVal then ()
            else
              curState := addBinding !curState curVar newVal;
            reaches )
        else raiseUnreach "filterReachVar not goal"
      with Not_found ->
        raiseUnreach ("WTF: filterReachVar var NF" ^ (string_of_var curVar))
          
  and filterReachPtrTarget (v,o) curPath =
    let bitsOff = int_of_off o in 
    let bitsOff, curPath = consumeConcats bitsOff curPath in
    (* Make up a field read first if the offset is non-zero, 
       or the accessed var is a record *)
    
    let newPath = 
      if (isScalar (structInfoFVar startState.vAttrs v) 
          && (isNoOffset bitsOff)
          && not (recordValued v))
      then curPath else AReadField bitsOff :: curPath in
    filterReachVar v newPath

  and recordValued var =
    try
      match getBinding var !curState with
        Records _ | FIRecs _ | NCRecord _ -> true
      | Refs _ | FpRef _ | FInt _ | FNFP _ -> false
    with Not_found ->
      if isSpecialVar var then ()
      else logErrorF "WTF: recordValued var NF %s\n" (string_of_var var);
      false
      
  and filterReachVal curVar curVal curPath : (fvalue * bool) =
    match curVal with
      FNFP _ -> 
        logError ("filterReachVal hit Nfp?"); 
        (curVal, false)
          (* TODO: need a 3-value thing ... even though most of
             the time it is correct in assuming NFP never reaches an FP *)
          
    | FInt _ -> 
        let reaches, terminal = 
          if curPath = [] then (nullVar#isVar targV, true)
          else
            (logStatus ("filterReachVal hit Null?"); 
             (false, false)) in
        if goalReached reaches terminal then (curVal, reaches)
        else raiseUnreach "filterReachVal not goal"

    | FpRef var -> 
        filterReachFP curVal var curPath
          
    | Refs ls ->
        filterReachRef ls curPath

    | Records recs ->
        filterReachRecs curVar recs curPath

    | FIRecs fir ->
        let o, rest, terminal = checkPathRecord curPath in
        let _, reaches = filterReachVal curVar fir rest in
        (* Don't actually modify the guy *)
        (curVal, reaches)

    | NCRecord (fm, m) ->
        (* Don't actually modify the guy *)
        let off, rest, terminal = checkPathRecord curPath in
        let oldVal = OffsetMap.find off fm in
        let _, reaches = filterReachVal curVar oldVal rest in
        (curVal, reaches)

  and filterReachFP curVal var curPath =
    match curPath with
      [] ->
        let reaches = eqFVar var targV && isNoOffsetSum targO in
        if goalReached reaches true then (curVal, reaches)
        else raiseUnreach "filterReachFP not goal"
          (* TODO: what if reach-target is EXT? Ugh... need a 3rd value *)

    | (AReadField o as h) :: rest
    | (AConcatOff o as h) :: rest ->
        if isNoOffset o then filterReachFP curVal var rest
        else begin
          logErrorD (text "filterReach hit FP instead of rec? " 
                     ++ d_acc h ++ line);
          (curVal, false)
        end
    | h :: rest ->
        logErrorD (text "filterReach hit FP? " ++ d_acc h ++ line);
        (curVal, false)
          
  and filterReachRef ls curPath =
    (match curPath with
       AReadField o :: rest 
     | AConcatOff o :: rest ->
         if isNoOffset o then
           filterReachRef ls rest
         else 
           failwith "filterReachRef mismatch"
     | _ ->
         let checkReachNonTerm rest loc =
           filterReachPtrTarget loc rest
         in
         let checkReachTerm loc =
           (* Ignore the summary bit on the offset here... *)
           compFLocNoSum (targV, targO) loc == 0
         in

         let checkReach, ls, terminal = 
           (match curPath with
              ADeref ls2 :: rest ->
                checkReachNonTerm rest, ls, false
            | [] -> checkReachTerm, ls, true
            | h :: rest -> 
                logError "filterReachVal path Mayref mismatch";
                logErrorD (defPrint#d_ptrSet (text "LS1: ") ls ++ line);
                logErrorD (text "Acc2: " ++ d_acc h ++ line);
                checkReachNonTerm rest, ls, false
           ) in
         let newLS, newReaches = FLocSet.fold 
           (fun loc (curSet, curReach) ->
              try
                let reaches = checkReach loc in
                let newSet = 
                  if goalReached reaches terminal 
                  then addToPtrSet loc curSet
                  else curSet in
                (newSet, curReach || reaches)
              with UnreachableState ->
                curSet, curReach
           ) ls (FLocSet.empty, false) in
(*         
         logStatusD (defPrint#d_ptrSet (text "filtered locs: ") newLS ++ line);
*)
         let newSize = FLocSet.cardinal newLS in
         if newSize == 0 then raiseUnreach "filterReachRef empty"
         else (Refs newLS, newReaches)
    )

  and filterReachRecs curVar recs curPath =
    let checkConcurrentMod () =
      try
        let maybeNewVal = getBinding curVar !curState in
        match maybeNewVal with
          Records maybeNewRecs ->
            if eqVals maybeNewVal (Records recs) then ()
            else raise (ConcurrentModRec maybeNewRecs)
        | _ -> failwith 
            (Printf.sprintf
               "checkConcurrentMod: no longer rec %s -> %s\n"
               (string_of_var curVar) (string_of_val maybeNewVal));
      with Not_found ->
        failwith "checkConcurrentMod can't find binding?"
    in
    (try
       let o, rest, terminal = checkPathRecord curPath in
       let newrecs, reaches = List.fold_left
         (fun (curRecs, curReach) r -> 
            try
              let newfm, newm, reaches = 
                checkReachRecord curVar r o rest terminal in
              checkConcurrentMod ();
              let newRecs = 
                if goalReached reaches terminal
                then (newfm, newm) :: curRecs
                else curRecs in
              (newRecs, curReach || reaches)
            with UnreachableState -> 
              (curRecs, curReach)
         ) ([], false) recs in
(*       
       logStatusD (text "filter newrecs: "
                   ++ defPrint#d_recordSet None newrecs ++ line);
*)
       if newrecs = [] 
       then raiseUnreach ("filterReachRec empty " ^ (string_of_var curVar))
       else (makeNormalizedRec newrecs), reaches
     with ConcurrentModRec newrecs ->
       logStatusF "filterReachRecs concurrent mod: %s\n" 
         (string_of_var curVar);
       filterReachRecs curVar newrecs curPath
    )
      

  and checkPathRecord curPath =
    match curPath with
      AReadField o :: rest -> (o, rest, false)
    | [] -> (noOff, [], true)
    | h :: rest -> 
        logError "filterReachVal path Record mismatch";
        logErrorD (text "Acc2: " ++ d_acc h ++ line); 
        printState !curState;
        failwith "filterReach record mismatch"
          
  and checkReachRecord curVar (fm, m) off rest terminal =
    if OffsetMap.mem off fm then
      let newFM, reaches = 
        checkReachOffmap curVar fm off rest terminal in 
      (newFM, m, reaches)
    else raiseUnreach 
      ("checkReachRec no field " ^ (string_of_var curVar)
         ^ "@" ^ (string_of_int off))
      
  and checkReachOffmap curVar fm off rest terminal =
    let oldVal = OffsetMap.find off fm in
    let newVal, reaches = filterReachVal curVar oldVal rest in
    if goalReached reaches terminal then 
      (OffsetMap.add off newVal fm, reaches)
    else raiseUnreach "checkReachOffmap no field" 
  in

  match accPath with
    AReadVar fv :: rest ->
      let reaches = filterReachVar fv rest in
      (!curState, reaches)
  | ADeref ls :: rest ->
      if FLocSet.is_singleton ls then
        let v, o = FLocSet.choose ls in
        (* May hit this case with *(array + i) since we get the address of 
           the "array" w/ out reading a pointer variable, and jump
           straight into the derefence. *)
        let reaches = filterReachPtrTarget (v, o) rest in
        if goalReached reaches false then (!curState, reaches)
        else raiseUnreach "start doesn't hit goal"
      else 
        failwith "filterReach accessPath doesn't begin with Var?"
          
  | _ -> failwith "filterReach accessPath doesn't begin with Var?"


(** return a new state where the ptrExp MUST point to the given target *)
let focus revAccPath (v, o) st =
  try
    let newState, reaches = filterReach (List.rev revAccPath) (v, o) st true in
    newState
  with UnreachableState ->
    logError ("focus: hit unreachable state");
    bottomState


(** return a new state where the ptrExp MUST NOT point to the given target *)
let focusNot revAccPath (v, o) st =
  try 
    let newState, reaches = filterReach (List.rev revAccPath) (v, o) st false in
    (* can't assert that it doesn't reach at all... *)
    newState
  with UnreachableState ->
    logError ("focus: hit unreachable state");
    bottomState
