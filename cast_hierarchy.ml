


(** Figure out what types are cast into other types to check "compatibility" *)

open Cil
open Logging

module type TypHashSig = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val toString : t -> string
end

module CastGraph (T:TypHashSig) = struct

  module TyHash = struct

    type t = T.t
    let equal = T.equal
    let hash = T.hash

  end

  module TH = Hashtbl.Make(TyHash)

  type typeGraph = (T.t list) TH.t

  let newCastGraph () =
    TH.create 17

  let addCast tyg subT usedT =
    let useAs = try TH.find tyg subT with Not_found -> [] in
    TH.replace tyg subT (List_utils.addOnceP T.equal useAs usedT)

  let combineTypLists ts1 ts2 =
    List_utils.unionEq T.equal ts1 ts2

  let combine tyg1 tyg2 =
    let result = newCastGraph () in
    TH.iter 
      (fun t ts1 ->
         let finalTs = 
           try let ts2 = TH.find tyg2 t in combineTypLists ts1 ts2
           with Not_found -> ts1 in
         TH.add result t finalTs
      ) tyg1;
    result
      
  let isSubT tyg possibleSubT usedT =
    T.equal possibleSubT usedT || (* unroll some *)
      let vis = TH.create 10 in
      let rec search curT =
        T.equal curT usedT ||
          if TH.mem vis curT then false
          else try
            TH.add vis curT ();
            let next = TH.find tyg curT in
            List.exists search next
          with Not_found -> false
      in
      search possibleSubT

  (************ I/O *************)

  let loadCastInfo fname =
    let ic = open_in_bin fname in
    let result = Marshal.from_channel ic in
    close_in ic;
    result
      
  let saveCastInfo fname ci =
    let oc = open_out_bin fname in
    Marshal.to_channel oc ci [Marshal.Closures];
    close_out oc

  (****************** Debug ********************)

  let disallowedCharsIDs = Str.regexp "[^_a-zA-Z0-9]"

  let sanitizeIDs str =
    (*  Str.global_replace disallowedCharsIDs "_" str *)
    "\"" ^ str ^ "\"" (* should probably strip any escaped quotes from str? *)

  module TyGraphDot = struct
    
    type graph = typeGraph
    type key = T.t
    type node = T.t list

    let getNode g k =
      TH.find g k
        
    let listSuccs g n =
      n
        
    let iter = TH.iter
      
    let label_of_node k n =
      "\\N" (* use id of node *)
        
    let id_of_node k n =
      sanitizeIDs (T.toString k)
        
    let graph_label = "cast_hierarchy"
      
    let digraph = true
      
  end
    
  module TypeDot = Graph.MakeDot(TyGraphDot)

end



module CilTypHash = struct
  type t = Cil.typ
  let equal t1 t2 = (t1 == t2 || Ciltools.compare_type t1 t2 == 0)
  let hash x = Ciltools.hash_type x
  let toString t = Cildump.string_of_type t
end

module CilCastG = CastGraph (CilTypHash)


(************************************************************)

let canonicizeType t = 
  Type_utils.canonType (Cil_lvals.unrollTypeNoAttrs t)

class castVisitor = object(self)
  inherit nopCilVisitor

  val typeG = CilCastG.newCastGraph ()

  method typeInfo = typeG

  method vexpr exp : exp visitAction =
    match exp with
      CastE (t, exp) ->
        let eTyp = typeOf exp in
        CilCastG.addCast typeG (canonicizeType eTyp) (canonicizeType t);
        DoChildren
    | _ -> DoChildren

end

let castFile = ".cast_info"

let generateCastInfo outname root =
  let visitor = new castVisitor in
  Filetools.walkDir 
    (fun ast fname ->
       Cil.visitCilFileSameGlobals (visitor :> cilVisitor) ast
    ) root;
  let result = visitor#typeInfo in
  CilCastG.saveCastInfo outname result;
  result

let getCastInfo root restart =
  let cfile = Filename.concat root castFile in
  if not restart && Sys.file_exists cfile then CilCastG.loadCastInfo cfile
  else generateCastInfo cfile root

