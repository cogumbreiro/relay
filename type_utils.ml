

(** Assign types to integers and back *)

open Cil
open Pretty
open Cildump
open Logging

let equal_type a b =
  Ciltools.compare_type a b == 0 

let hash_type x = 
  Ciltools.hash_type x


module TH = Hashtbl.Make (
  struct
    type t = typ
    let equal a b = equal_type a b
    let hash x = hash_type x
  end)

type type_id = int

let typToInt = TH.create 17
let intToTyp = Inthash.create 17
let typIds = ref 0

let findTyp id =
  try Inthash.find intToTyp id
  with Not_found ->
    failwith ("findTyp NF: " ^ string_of_int id)

let addTyp typ =
  try TH.find typToInt typ
  with Not_found ->
    let next = !typIds in
    incr typIds;
    TH.add typToInt typ next;
    Inthash.add intToTyp next typ;
    next

(************************************************************)

let hashTypID type_id =
  Hashtbl.hash type_id


(************ Canonicizing types *********)

let unknownMallocIndex = addTyp Trans_alloc.unknownMallocType
  
let isImpreciseMallocID typID =
  unknownMallocIndex = typID
  
let isImpreciseMalloc t =
  equal_type (Trans_alloc.unknownMallocType) t

let rec canonType t =
  let t = Cil_lvals.dropTypeAttribs (Cil_lvals.unrollTypeNoAttrs t) in
  match t with
    TArray (eltT, sz, a) ->
      if isImpreciseMalloc t then t
      else TArray (eltT, None, a)
  | TPtr (t', a) -> TPtr (canonType t', a)
  | _ -> t

(* Hack to match our modeling of arrays *)
let rec typeModArray typ =
  match typ with
    TComp _ -> typ
  | TNamed (t, _) -> typeModArray t.ttype
  | TArray (t, _, _) -> canonType t
  | _ -> canonType typ


(************************************************************)

(** Check if targets mentioned in RHS are even subtypes of the recipient LHS *)
let rec structSubtype t1 t2 =
  let t1 = canonType t1 in
  let t2 = canonType t2 in
  equal_type t1 t2 ||
    Type_reach.isPoly t2 || isImpreciseMalloc t2 ||
    match t1, t2 with
      TArray (tt1, _, _), TArray (tt2, _, _) ->
        (* Hmm... in ML tt1 :> tt2 and tt2 :> tt1 *)
        not (isImpreciseMalloc t1) && 
          structSubtype tt1 tt2 && structSubtype tt2 tt1
    | TArray (t, _, _), _ ->
        (* funky modulo array thing *)
        not (isImpreciseMalloc t1) && structSubtype t t2
    | TInt _, TInt _ -> true (* don't care what kind of const for now... *)
    | TPtr (tt1, _), TPtr (tt2, _) ->
        (* Hmm... in ML tt1 :> tt2 and tt2 :> tt1 *)
        structSubtype tt1 tt2 && structSubtype tt2 tt1
    | TComp (ci1, _), TComp (ci2, _) ->
        hasPrefix ci1.cfields ci2.cfields
    | TComp (ci, _), _ ->
        (match ci.cfields with
           h :: _ -> structSubtype h.ftype t2
         | [] -> false)
    | _, _ ->
        false
          
and hasPrefix fields1 fields2 =
  match fields1, fields2 with
    [], [] -> true
  | [], _ -> false
  | _, [] -> true
  | h1 :: t1, h2 :: t2 ->
      let ty1 = canonType h1.ftype in
      let ty2 = canonType h2.ftype in
      if equal_type ty1 ty2 then
        hasPrefix t1 t2
      else
        (* Check for embedded records *)
        (match ty1, ty2 with
           TComp (ci, _), _ ->
             hasPrefix (ci.cfields @ t1) fields2
         | _, TComp (ci, _) ->
             hasPrefix fields1 (ci.cfields @ t2)
         | _, _ -> false)

let pickWidest t1 t2 : Cil.typ =
  let t1 = canonType t1 in
  let t2 = canonType t2 in
  let breakTies t1 t2 =
    let c = Ciltools.compare_type t1 t2 in
    if c <= 0 then 1
    else 2
  in
  let checkSize t1t t2t =
    let sz1 = sizeOf t1t in
    let sz2 = sizeOf t2t in
    if sz1 > sz2 then 1 
    else if sz2 > sz1 then 2
    else breakTies t1t t2t
  in
  let rec pickHelper t1 t2 =
    match t1, t2 with
      TPtr (t1t, _), TPtr (t2t, _) 
    | TArray (t1t, _, _), TArray (t2t, _, _)
    | TPtr (t1t, _), TArray (t2t, _, _) -> pickHelper t1t t2t
    | TArray(t1t, _, _), TPtr (t2t, _) -> 
        (* Ugh... hack to prefer TPtr over TArray *)
        let p = pickHelper t1t t2t in
        if p = 1 && equal_type t1t t2t then 2
        else p
    | TNamed (ti1, _), _ -> pickHelper ti1.ttype t2
    | _, TNamed (ti2, _) -> pickHelper t1 ti2.ttype
    | TPtr _, TInt _ -> 1 (* Int may be wider, but we care about ptrs more *)
    | TInt _, TPtr _ -> 2
    (* Prefer functions over others *)
    | TFun (_, Some _, _, _), TFun (_, Some _, _, _) -> breakTies t1 t2
    | TFun (_, None, _, _), _ -> 1 (* Prefer None args *)
    | _, TFun (_, None, _, _) -> 2 
    | TFun _, _ -> 1
    | _, TFun _ -> 2
    | _, _ -> checkSize t1 t2
  in
  let chosen = pickHelper t1 t2 in
  if chosen = 1 then t1 else t2

(************************************************************)

let argsCompatible args1 args2 =
  match args1, args2 with
    None, _ 
  | _, None -> true
  | Some l1, Some l2 -> List.length l1 == List.length l2


(** Assume sig2 is the calling expression, and sig1 is the target.
    Therefore, we allow NO return val in the callExp even if the 
    target returns something (funky, but it happens...) *)
let funSigSubtype (rt1, args1, va1) (rt2, args2, va2) =
  (va1 == va2) && argsCompatible args1 args2 &&
    let rt1Void = isVoidType rt1 in
    let rt2Void = isVoidType rt2 in
    if rt2Void then
      if rt1Void then true
      else (logErrorF "return ignored @ %s\n" (string_of_loc !currentLoc); true)
    else (not rt1Void)

let getFunSig typ = 
  match Cil_lvals.unrollTypeNoAttrs typ with
    TFun (rt, args, vararg, _) ->
      Some (rt, args, vararg)
  | _ -> None

let getFunSigPtrExp exp =
  getFunSig (Cil_lvals.typeAfterDeref exp)
      

let functionTypeMatches fpType callSig =
  match getFunSig fpType, callSig with
    Some fpSig, Some callSig -> 
      funSigSubtype fpSig callSig 
  | None, Some _ -> false
  | _, None ->
      failwith "couldn't get sig from callexp?"

let d_funSig fsig =
  match fsig with
    None -> text "nf"
  | Some (rt, args, vararg) ->
      d_type () (TFun (rt, args, vararg, []))
