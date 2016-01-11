


(** Figure out what types are cast into other types to check "compatibility" *)

open Cil
module L = Logging

let equalType t1 t2 =
  Ciltools.compare_type t1 t2 == 0

let hashType t =
  Ciltools.hash_type t

module TyHash = struct

  type t = typ
  let equal = equalType
  let hash = hashType

end

module TH = Hashtbl.Make(TyHash)

type typeGraph = (typ list) TH.t

let addTypeConstraint tyg subT usedT =
  let useAs = try TH.find tyg subT with Not_found -> [] in
  TH.replace tyg subT (Stdutil.addOnceP equalType useAs usedT)
    
let isSubT tyg possibleSubT usedT =
  equalType possibleSubT usedT || (* unroll some *)
    let vis = TH.create 10 in
    let rec search curT =
      equalType curT usedT ||
        if TH.mem vis curT then false
        else try
          TH.add vis curT ();
          let possibleUses = TH.find tyg possibleSubT in
          List.exists search possibleUses
        with Not_found ->
          L.logError ("No cast info for: " ^ Cildump.string_of_type curT);
          false
    in
    search possibleSubT


(************************************************************)

class castVisitor = object(self)
  inherit nopCilVisitor

  val typeG = TH.create 37

  method typeInfo = typeG

  method vexpr exp : exp visitAction =
    match exp with
      CastE (t, exp) ->
        let eTyp = typeOf exp in
        addTypeConstraint typeG eTyp t;
        DoChildren
    | _ ->
        DoChildren

end

let castFile = ".cast_info"

let loadCastInfo fname =
  let ic = open_in_bin fname in
  let result = Marshal.from_channel ic in
  close_in ic;
  result
 
let saveCastInfo fname ci =
  let oc = open_out_bin fname in
  Marshal.to_channel oc ci [Marshal.Closures];
  close_out oc

let generateCastInfo outname root =
  let visitor = new castVisitor in
  Filetools.walkDir 
    (fun ast fname ->
       Cil.visitCilFileSameGlobals (visitor :> cilVisitor) ast
    ) root;
  let result = visitor#typeInfo in
  saveCastInfo outname result;
  result

let getCastInfo root =
  let cfile = Filename.concat root castFile in
  if Sys.file_exists cfile then
    loadCastInfo cfile
  else
    generateCastInfo cfile root

(****************** Debug ********************)

let disallowedCharsIDs = Str.regexp "[^_a-zA-Z0-9]"

let sanitizeIDs str =
(*  Str.global_replace disallowedCharsIDs "_" str *)
  "\"" ^ str ^ "\"" (* should probably strip any escaped quotes from str? *)

module TyGraphDot = struct
  
  type graph = typeGraph
  type key = typ
  type node = typ list

  let getNode g k =
    TH.find g k

  let listSuccs g n =
    n

  let iter = TH.iter

  let label_of_node k n =
    "\\N" (* use id of node *)

  let id_of_node k n =
    sanitizeIDs (Cildump.string_of_type k)

  let graph_label = "cast_hierarchy"

  let digraph = true

end

module TypeDot = Graph.MakeDot(TyGraphDot)

