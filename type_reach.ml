
(** Various functions for testing reachability between different types.
    Mostly between a src type and target structs *)

open Cil
open Cilinfos
open Cil_lvals
open Stdutil
open Pretty
open Offset
open Logging
open Cildump

module IS = Sparsebitv


(* TODO: set up some more flags *)

let fwdPoly = ref true
  (** If true, then we consider "void *" and other polymorphic pointers
      reaching ANYTHING *)

let intPoly = ref false
  (** If true, then "int *" and "char *" are considered polymorphic *)


(************************************************************
  Check "forward" reachability. Read field, derefence a ptr
 ************************************************************)

(** Reaches FP w/ 0-steps of derefence (but allow field accesses and 
    array indexing) *)
let rec hasImmediateFP t =
  match t with
    TPtr (t, _) ->
      (* Ignore poly *)
      if isFunctionType t then true
      else false
  | TArray (t, _, _) ->
      hasImmediateFP t
  | TFun _ -> true
  | TComp (ci, _) ->
      List.exists (fun fi -> hasImmediateFP fi.ftype) (!Cil.getCfields ci)
  | TFloat _ | TInt _ | TEnum _ | TVoid _ | TBuiltin_va_list _ -> false
  | TNamed (ti, _) -> hasImmediateFP ti.ttype

let rec hitsFunptr curType =
  match curType with
    TPtr (t, _) -> 
      if isFunctionType t then true 
      else hitsFunptr (unrollTypeNoAttrs t)
  | TFun _ -> false (* straight-up function, not funptr *)
  | TArray (t, _, _) -> 
      if isFunctionType t then true
      else hitsFunptr (unrollTypeNoAttrs t)
  | _ -> false

let lvalIsFunptr lv =
  hitsFunptr (typeOfLvalUnsafe lv)

let isPoly curType =
  match curType with
    TPtr (t, _) ->
      (match unrollTypeNoAttrs t with
         TVoid _ -> true
       | TComp (ci, _) -> (!Cil.getCfields ci) = []
       | TInt _ -> !intPoly
       | _ -> false )
  | TVoid _ -> true
  | TComp (ci, _) -> (!Cil.getCfields ci) = []
  | _ -> false

let isPolyInt curType =
  match curType with
    TPtr (t, _) ->
      (match unrollTypeNoAttrs t with
         TVoid _ -> true
       | TComp (ci, _) -> (!Cil.getCfields ci) = []
       | TInt _ -> true
       | _ -> false )
  | TVoid _ -> true
  | TInt _ -> true
  | TComp (ci, _) -> (!Cil.getCfields ci) = []
  | _ -> false

let rec hitsFunptrDeepHelper curType visited =
  match curType with
    TPtr (t, _) -> 
      if isFunctionType t then true 
      else if isPoly curType then true
      else hitsFunptrDeepHelper (unrollTypeNoAttrs t) visited
  | TArray (t, _, _) -> 
      if isFunctionType t then true
      else hitsFunptrDeepHelper (unrollTypeNoAttrs t) visited
  | TComp (ci, _) ->
      if Hashtbl.mem visited ci.ckey then false
      else begin
        Hashtbl.add visited ci.ckey ();
        List.exists 
          (fun fi -> 
             hitsFunptrDeepHelper (unrollTypeNoAttrs fi.ftype) visited
          ) (!Cil.getCfields ci)
      end
  | TFun _ -> false (* straight-up function, not funptr *)      
  | TFloat _ | TInt _ | TEnum _ | TVoid _ | TBuiltin_va_list _ -> false
  | TNamed (ti, _) -> hitsFunptrDeepHelper ti.ttype visited

let hitsFunptrDeep curType =
  hitsFunptrDeepHelper (unrollTypeNoAttrs curType) (Hashtbl.create 17)



let rec fwdReachesStructHelper targetCi curCi visited =
  curCi.ckey == targetCi.ckey || 
    List.exists (fun fi -> fwdReachesStructTypeHelper targetCi fi.ftype visited) 
    (!Cil.getCfields curCi)
    
and fwdReachesStructTypeHelper targetCi curType visited =
  let curType = unrollTypeNoAttrs curType in
  match curType with
    TPtr (t, _)
  | TArray (t, _, _) ->
      if !fwdPoly && isPoly t then true
      else fwdReachesStructTypeHelper targetCi t visited
  | TComp (ci, _) ->
      if ci.ckey == targetCi.ckey then true
      else if Inthash.mem visited ci.ckey then false
      else begin
        Inthash.add visited ci.ckey ();
        fwdReachesStructHelper targetCi ci visited
      end
  | TFun _ | TFloat _ | TEnum _ | TInt _
  | TVoid _ | TBuiltin_va_list _ -> false
  | TNamed (ti, _) -> 
      fwdReachesStructTypeHelper targetCi ti.ttype visited

let fwdReachesStruct targetCi sourceCi =
  fwdReachesStructHelper targetCi sourceCi (Inthash.create 7)

let fwdTypeReachesStructs targetCis srcType =
  List.exists 
    (fun ci -> fwdReachesStructTypeHelper ci srcType (Inthash.create 7)) 
    targetCis

let argReachesStructs targetCis (_, argType, _) =
  fwdTypeReachesStructs targetCis argType

let getFPArgs fi =
  let rec argsFromType curType =
    match unrollTypeNoAttrs curType with
      TPtr (t, _)
    | TArray (t, _, _) -> 
        (match unrollTypeNoAttrs t with
           TFun (_, args, _, _) -> Some (argsToList args)
         | _ -> argsFromType t)
    | TFun _ -> None
    | _ -> None
  in
  argsFromType fi.ftype

(************************************************************
  Check "backward" reachability w/ macros like CONTAINER_OF
 ************************************************************)

(** One-step embedding relation is (childKey, (parentKey, bitsOff)) *)
type embeddingRelation = 
    (int * int) list Inthash.t

let getEmbeddingRelation () =
  logStatus "Constructing embedding relation";
  let embeds = Inthash.create 7 in
  let addRelation parent child cfield =
    let pars = try Inthash.find embeds child.ckey with Not_found -> [] in
    try
      let foff, _ = bitsOffset (TComp (parent, [])) 
        (Field (cfield, NoOffset)) in
      if List.mem (parent.ckey, foff) pars then ()
      else begin
        logStatusF "Adding embed rel. %s(%d) in %s(%d) @ %s(%d)\n" 
          child.cname child.ckey parent.cname parent.ckey cfield.fname foff;
        Inthash.replace embeds child.ckey ((parent.ckey, foff) :: pars)
      end
    with SizeOfError (s, t) ->
      logErrorF "SizeOfError %s : %s\n" s (string_of_type t);
      logErrorF "Error adding embed rel. %s(%d) in %s(%d) @ %s\n" 
        child.cname child.ckey parent.cname parent.ckey cfield.fname;
  in
  Cilinfos.iterCompinfos 
    (fun parent ->
       List.iter 
         (fun finfo ->
            let unrolled = unrollTypeNoAttrs finfo.ftype in
            match unrolled with
              TComp (child, _) -> addRelation parent child finfo
            | _ -> ()
         ) (!Cil.getCfields parent)
    );
  embeds

let printEmbeddingRelation embeds =
  let doc = map_to_doc line Inthash.iter
    (fun pk (ck, off) ->
       let cci = getCinfo ck in
       let pci = getCinfo pk in
       dprintf "struct %s(%d) embedded in %s(%d) @ off %d\n" 
         cci.cname ck pci.cname pk off) embeds 
    (text "Embedding Relation:\n==============================\n") in
  logStatusD doc



(* Reachability using just the embeddings *)

let transEmbeds embeds src targ =
  let visited = Inthash.create 7 in
  let rec transEmbedsHelp cur =
    if cur == targ then true
    else if Inthash.mem visited cur then false
    else begin
      Inthash.add visited cur ();
      try 
        let pars = Inthash.find embeds cur in
        List.exists (fun (pk, foff) -> transEmbedsHelp pk) pars
      with Not_found -> false
    end
  in
  transEmbedsHelp src

let transAncestors embeds child =
  let visited = Inthash.create 7 in
  let ancestors = ref IS.empty in
  let rec transAncestorsHelp cur =
    if Inthash.mem visited cur then ()
    else begin
      Inthash.add visited cur ();
      try 
        let pars = Inthash.find embeds cur in
        List.iter (fun (pk, foff) -> 
                     ancestors := IS.add pk !ancestors;
                     transAncestorsHelp pk) pars
      with Not_found -> ()
    end
  in
  transAncestorsHelp child;
  !ancestors


(******** Version that uses set of fields that are used w/ offsetof **********)

(** Map from (parent struct key) -> bits offset *)
type offsetOfSet = int list Inthash.t


(** Check for patterns that look like uses of the offsetof macro.
    The macro may be different depending on implementation, but
    they all use &(((struct t * )0)->offsets) (plus some more casts, etc.) *)
class offsetOfVis = object (self)
  inherit nopCilVisitor
    
  val offsetOfs = Inthash.create 17
  method offsetOfs =
    offsetOfs

  method vexpr e =
    (match e with
       AddrOf (lv) -> self#tryMatchOffsetof lv
     | _ -> ());
    DoChildren

  method private tryMatchOffsetof lv =
    match lv with
      Var _, _ -> ()
    | Mem e, outerOff -> 
        if isZero e then
          let parentType = typeOfLvalUnsafe (Mem e, NoOffset) in
          (match unrollTypeNoAttrs parentType, outerOff with
             TComp (ci, _), NoOffset -> 
               logStatusF "tryMatchOffsetof given no offset %s\n" ci.cname
           | TComp (ci, _), _ ->
               self#addOffsetof ci outerOff
           | _, _ ->
               logStatusF "tryMatchOffsetof given non struct %s?\n"
                 (string_of_lval lv))

  method private addOffsetof ci off =
    try
      let bitsOff, _ = bitsOffset (TComp (ci, [])) off in
      let old = try Inthash.find offsetOfs ci.ckey with Not_found -> [] in
      if List.mem bitsOff old then ()
      else begin
        logStatusF "Adding offsetof: %s(%d) %s (%d)\n" ci.cname ci.ckey
          (string_of_offset off) bitsOff;
        Inthash.replace offsetOfs ci.ckey (bitsOff :: old)
      end
    with SizeOfError (str, _) ->
      logStatusF "Failed to add offsetof: %s(%d) %s -- %s\n" ci.cname 
        ci.ckey (string_of_offset off) str
    | Errormsg.Error  ->
      logStatusF "Failed to add offsetof: %s(%d) %s\n" ci.cname 
        ci.ckey (string_of_offset off);



end

(* TODO: make visitor that composes side-effect-free visitors so that
   we can batch up all the visits *)

let getOffsetOfSet root =
  logStatus "Constructing offsetof set";
  let vis = new offsetOfVis in
  Filetools.walkDir (fun ast file ->
                       visitCilFileSameGlobals (vis :> cilVisitor) ast) root;
  vis#offsetOfs


(*** Finally, use the offsetof to check reachability / ancestry ***)

let possibleAncestors offs (baseType, bitsOff) =
  let baseType = unrollTypeNoAttrs baseType in
  let reaches = ref IS.empty in
  let addReach curComp =
    (reaches := IS.add curComp.ckey !reaches; true)
  in
  let rec tryReach curComp curOff =
    if curOff == bitsOff then addReach curComp
    else if IS.mem curComp.ckey !reaches then true
    else 
      try
        let offsets = Inthash.find offs curComp.ckey in
        if List.exists (getNext curComp curOff) offsets then addReach curComp
        else false
      with Not_found -> false

  and getNext curComp curOff off =
    (* Note: curOff / nextOff is always relative to the baseType *)
    let nextOff = curOff + off in
    try
      let nextFields = Offset.bitsToOffsetAll baseType nextOff in
      List.exists 
        (fun nextField ->
           match unrollTypeNoAttrs (typeOffsetUnsafe baseType nextField) with
             TComp (ci, _) -> tryReach ci nextOff
           | _ ->
(*               logStatusF "possibleAncestors next isn't struct: %s %s\n"
                 curComp.cname (string_of_offset nextField);
*)
               false) nextFields
    with Offset.UnknownOffset ->
(*      logStatusF "possibleAncestors no next field: %s %d\n"
        curComp.cname nextOff;
*)
      false
  in
  (match baseType with
     TComp (ci, _) -> 
       let (_:bool) = tryReach ci 0 in ()
   | _ -> ();
       (*
         logStatusF "possibleAncestors on non-struct: %s\n"
         (string_of_type baseType) *)
  );
  !reaches

    
