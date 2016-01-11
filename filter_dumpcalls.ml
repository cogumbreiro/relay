

(** Version of dumpcalls that allows various filters to help
    test callgraph construction hypotheses *)

open Cil
open Dumpcalls
open Fstructs
open Callg
open Cildump

module FieldP = Field_partition

let dropFPFields = ref false
let dropFP = ref false
let filterClasses = ref ""
let strictClasses = ref false
let fieldBased = ref false
let dropCallbacks = ref false


(** A filter that is a chain of filters *)
class chainFilter = object (self)

  val mutable filters = []

  method addFilter (f:cgFilter) =
    filters <- f :: filters

  method filter (funs:fKey list) (caller:fundec) (callerK:fKey) (callexp:exp) = 
    List.fold_left (fun cur filt -> 
                      filt#filter cur caller callerK callexp) funs filters

end


(** Filter all funptrs *)
class allFPFilter = object (self)
  method filter (funs:fKey list) (caller:fundec) (callerK:fKey) callexp =
    if isIndirect callexp then [] else funs

end


(** Filter based on funptr fields *)
class fpFieldFilter = object (self)
  method filter (funs: fKey list) (caller:fundec) (f:fKey) callexp =
    match callexp with
      CastE(_, e) -> self#filter funs caller f e
    | Lval (Var(_), _)
    | AddrOf _
    | StartOf _ -> funs
    | Lval (Mem(e), off) ->
        (match e, off with
           Lval (_, NoOffset), NoOffset -> 
             (* don't see an offset in current layer of inner exp *)
             self#filter funs caller f e
         | Lval (_, _), NoOffset -> 
             (* inner exp has offset *)
             []
         | _, Field _
         | _, Index _ -> 
             (* orig exp has offset *)
             []
         | _ -> self#filter funs caller f e
        )
    | _ -> failwith "unknown call expression"
end


(** Filter based on heuristic class partitioning. 
    Only keep calls between functions in the same class partition.  *)
class classFilter partitions = object (self)

  (* assume these partitions are context insensitive! *)
  method private samePartition callerPart targ =
    let inSamePart = 
      FSet.exists (fun fid ->
                     let fkey = fid_to_fkey fid in
                     targ = fkey
                  ) callerPart in
    if not (inSamePart) then
      Printf.printf "filtered %d\n" targ
    ;
    inSamePart
  

  method filter (funs:fKey list) (caller:fundec) (callerK: fKey) callexp =
    if isIndirect callexp then begin
      try 
        let callerPart = Hashtbl.find partitions callerK in
        List.filter (self#samePartition callerPart) funs
      with Not_found ->
        if !strictClasses then []
        else funs
    end else
      funs
end


(** Filter based on field-based bucketting *)
class fieldBasedFilter root = object (self)
  
  val partitions = FieldP.partitionFuncs root

  val mutable topFuns = None

  method private tryFind tk = 
    try FieldP.TH.find partitions tk 
    with Not_found -> 
      (Printf.printf "Field partition not found for %s\n" 
         (FieldP.string_of_taker tk));
      FSet.empty

  method private getTopFuns () = 
    match topFuns with
      None ->
        Printf.printf "Generating Unknown funcs for the first time\n";
        flush stdout;
        let unknown = self#tryFind FieldP.FpUnknown in
        let arg = self#tryFind FieldP.FpArg in
        let ret = self#tryFind FieldP.FpRet in
        let local = self#tryFind FieldP.FpLocal in
        let glob = self#tryFind FieldP.FpGlobal in
        topFuns <- Some (FSet.union unknown 
                           (FSet.union arg 
                              (FSet.union ret
                                 (FSet.union local glob))));
        Printf.printf "Done generating unknown funcs\n";
        flush stdout;
        self#getTopFuns ()
    | Some x -> x

  method private inMatchingFuns matchingFuns k =
    let matches = 
      FSet.exists (fun id -> 
                     let k2 = fid_to_fkey id in
                     k = k2
                  ) matchingFuns in
    if not matches then 
      Printf.printf "filtered %d\n" k;
    matches
      

  method private filterWith funs tk =
    try
      let matchingFuns = 
        match tk with
          FieldP.FpUnknown
        | FieldP.FpArg
        | FieldP.FpRet
        | FieldP.FpLocal
        | FieldP.FpGlobal -> self#getTopFuns () (* unsound now anyway... *)
        | FieldP.FpField _ -> self#tryFind tk (* unsound now anyway... *)
      in
      List.filter (self#inMatchingFuns matchingFuns) funs
    with Not_found ->
      Printf.printf "from tk Not_found ? %s\n" (FieldP.string_of_taker tk);
      raise Not_found

  method private filterFromOff funs off =
    try
      match FieldP.findBinderOff off None with
        None -> 
          Printf.printf "filter can't find the offset %s\n" 
            (string_of_offset off);
        self#filterWith funs FieldP.FpUnknown
      | Some x -> self#filterWith funs x
    with Not_found ->
      Printf.printf "from off Not_found ? %s\n" (string_of_offset off);
      raise Not_found

  method filter (funs:fKey list) (caller:fundec) (callerK: fKey) callexp =
    try
      match callexp with
        CastE(_, e) -> self#filter funs caller callerK e
      | Lval (Var(_), _)
      | AddrOf _
      | StartOf _ -> funs
      | Lval (Mem(e), off) ->
          (match e, off with
             Lval (_, NoOffset), NoOffset -> 
               (* don't see an offset in current layer of inner exp *)
               self#filterWith funs FieldP.FpUnknown
           | Lval (_, off), NoOffset ->
               (* inner exp has offset *)
               self#filterFromOff funs off
           | _, Field _
           | _, Index _ -> 
               (* orig exp has offset -- not possible? *)
               self#filterFromOff funs off
           | _ -> 
               Printf.printf "filter doesn't know FP exp %s\n" 
                 (string_of_exp e);
               self#filterWith funs FieldP.FpUnknown
          )
      | _ -> failwith "unknown call expression"
    with Not_found ->
      Printf.printf "Not_found? %s\n" (string_of_exp callexp);
      raise Not_found 
end

(** Filter callback-style calls: 
    drop calls where the function pointer is an argument, or
    only requires one field access from an argument *)
class callbackFilter = object (self)
  
  (* TODO: check that the caller struct is not something that can
     reach the FP instead of checking the number of derefs
     instead of only checking that the FP is a formal
  *)
  method private isCallback caller fpExp =
    try
      let vi = Cil_lvals.findBaseVarinfoExp fpExp in
      if Ciltools.isFormal caller vi then
        let rec atMostNMem limit fpExp count = 
          match fpExp with
            Lval (Var vi, _) -> true
          | Lval (Mem e, _) -> 
              if count >= limit then false
              else atMostNMem limit e (count + 1)
          | CastE (_, e) -> atMostNMem limit e count
          | BinOp (op, e1, e2, _) -> atMostNMem limit e1 count
          | _ -> 
              Printf.printf "filter doesn't know FP exp %s\n" 
                (string_of_exp fpExp);
              false
        in
        atMostNMem 0 fpExp 0
      else false
    with Cil_lvals.BaseVINotFound ->
      Printf.printf "VI not found for %s\n" (string_of_exp fpExp);
      false

  method filter (funs: fKey list) caller (callerK: fKey) callexp =
    match callexp with 
      CastE(_, e) -> self#filter funs caller callerK e
    | Lval (Var(_), _)
    | AddrOf _
    | StartOf _ -> funs (* direct call *)
    | Lval (Mem(e), off) -> 
        if self#isCallback caller e then begin
          Printf.printf "filtered callback: %s\n" (string_of_exp callexp);
          [] 
        end
        else funs
    | _ -> failwith "unknown call expression"
        
end


(** Pick a debugging filter *)
let pickFilter root =
  let chainFilter = new chainFilter in
  if !dropFPFields then chainFilter#addFilter (new fpFieldFilter);
  if !dropFP then chainFilter#addFilter (new allFPFilter);
  if !filterClasses <> "" then chainFilter#addFilter 
      (new classFilter (Oo_partition.readPartitions !filterClasses));
  if !fieldBased then chainFilter#addFilter (new fieldBasedFilter root);
  if !dropCallbacks then chainFilter#addFilter (new callbackFilter);
  Dumpcalls.curFilter := (chainFilter :> cgFilter)

