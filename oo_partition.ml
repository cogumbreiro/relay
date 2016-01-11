

(** Partition functions based on where their address is taken.
    A ~ B iff there exists a common location where A and B both
    had their address taken *)

open Callg
open Cil
open Cildump
open Cil_lvals
open Fstructs
open Pretty

module L = Logging
module Dist = Distributions


(************************************************************
Map functions w/ addr taken to sets of functions that take it
************************************************************)

type addrTaker = 
    TkAssign of fKey
  | TkArg of fKey
  | TkRet of fKey
  | TkInit of varinfo * int list
      
let takerEq a b = 
  match a, b with
    TkInit (vi1, _), TkInit (vi2, _) -> Ciltools.compare_var vi1 vi2 == 0
  | _ -> a = b
      
let takerHash x =
  match x with
    TkInit (vi, _) -> Ciltools.hash_var vi
  | _ -> Hashtbl.hash x

module TH = Hashtbl.Make 
  (struct
     type t = addrTaker
     let equal = takerEq
     let hash = takerHash
   end)

let funEq a b = Ciltools.compare_var a b == 0

let funHash x = Ciltools.hash_var x

module FH = Hashtbl.Make 
  (struct 
     type t = varinfo
     let equal = funEq
     let hash = funHash
   end)


(* Passes:
   (1) Track functions w/ addrTaken in initializers and in other functions
   (2) Track vars w/ addrTaken where the var was initialized to take funptrs
   (3) Track functions that (transitively) directly call functions that 
   bind function pointers
*)

(* Pass 1 *)
class fpAddrTakenVisitor = object(self)
  inherit nopCilVisitor

  val funcToAddrTaker = FH.create 10
  method funcToAddrTaker () = funcToAddrTaker
  val takerToFuncs = TH.create 10
  method takerToFuncs () = takerToFuncs

  val mutable curFunc = dummyFunDec

  method private addBinding takenFunc taker =
    let old = 
      try FH.find funcToAddrTaker takenFunc
      with Not_found -> [] in
    FH.replace funcToAddrTaker takenFunc (List_utils.addOnceP takerEq old taker);
    let old = 
      try TH.find takerToFuncs taker
      with Not_found -> [] in
    TH.replace takerToFuncs taker (List_utils.addOnceP funEq old takenFunc)

  method vfunc f = 
    curFunc <- f;
    DoChildren

  method vinst i =
    match i with
      Set (lv, rhs, loc) -> 
        self#processAssign loc lv rhs
    | Asm (_, _, _, _, _, _) -> ()
    | Call(lvopt, callexp, args, loc) ->
        self#processCall loc callexp args;
    ;
    DoChildren

  method vinit var off init : Cil.init Cil.visitAction =
    curFunc <- dummyFunDec;
    let rec checkInit curOff curInit =
      (match curInit with
         SingleInit exp -> self#processAssign var.vdecl (Var var, curOff) exp
       | CompoundInit (t, moreInitList) -> 
           List.iter (fun (moreOff, moreInit) ->
                        checkInit (addOffset moreOff curOff) moreInit) 
             moreInitList
      ) in
    checkInit off init;
    DoChildren
      
  method private findAddrTakenFunc rhs =
    match rhs with
      AddrOf lv ->
        (match lv with 
           Var(vi), NoOffset -> 
             if isFunctionType vi.vtype 
             then Some vi else None
         | _ -> None
        )
    | StartOf _
    | Lval _ -> None
    | CastE (_, e) -> self#findAddrTakenFunc e
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _ -> None
    | AlignOfE _
    | SizeOfE _ -> None
    | UnOp (op, _, t) -> None
    | BinOp (op, _, _, t) -> None

  method vstmt (s:stmt) : stmt visitAction =
    (match s.skind with
       Instr _ 
     | Goto _
     | Break _
     | Continue _
     | Loop _
     | Block _
     | TryFinally _
     | TryExcept _ 
     | Return (None, _) -> ()
     | Return (Some e, _) ->
         (match self#findAddrTakenFunc e with
            None -> ()
          | Some vi -> 
              self#addBinding vi (TkRet (getFkey curFunc))
         )
     | Switch (e, _, _, _)
     | If (e, _, _, _) -> 
         (* Don't count ptr cast here -- used w/ checking against null *)
         (* not counted as an instruction *)
         ()
    ); DoChildren


  method private processAssign loc lv rhs =
    match self#findAddrTakenFunc rhs with
      None -> ()
    | Some vi ->
        if curFunc = dummyFunDec then 
          try
            let initer = findBaseVarinfoLval lv in
            self#addBinding vi (TkInit (initer, []))
          with BaseVINotFound ->
            Printf.printf "VI not found for %s\n" (string_of_lval lv)
        else
          self#addBinding vi (TkAssign (getFkey curFunc))
           

  method private processCall loc exp args =
    match exp with
      Lval lv -> self#processCallLv loc lv args
    | _ -> failwith "Skipping non-lval call exp\n"
        
  method private processCallLv loc lv args =
    List.iter 
      (fun exp -> 
         match self#findAddrTakenFunc exp with
           None -> ()
         | Some vi -> self#addBinding vi (TkArg (getFkey curFunc))
      ) args
          
end

(* TODO: Pass 2 -- if we need it... *)
class initAddrTakenVisitor = object(self)
  inherit nopCilVisitor

end



(* Pass 3 *)
module FQ = Queueset.Make(
  struct
    type t = funID
    let compare = compareFunID
  end)

class addrTakerCallers cg seedTakers = object(self)
  
  val mutable reachConstructors = seedTakers
  method reachConstructors () = reachConstructors

  val mutable missedIndirectCalls = Hashtbl.create 17

  method addMissedIndirectCall caller callee =
    let old = 
      try Hashtbl.find missedIndirectCalls callee 
      with Not_found -> [] in
    Hashtbl.replace missedIndirectCalls callee (List_utils.addOnce old caller)

  method printMissedIndirectCalls () =
    Hashtbl.iter 
      (fun callee cs ->
         Printf.printf " Indirect call [%s ...] -> %s\n" (List.hd cs) callee
      ) missedIndirectCalls;
    Printf.printf "\n TOTAL: %d\n\n" (Hashtbl.length missedIndirectCalls)

  method initWork work =
    Printf.printf "===============================================\n";
    Printf.printf "Initial set of constructors: %d\n" 
      (FSet.cardinal reachConstructors);
    FSet.iter 
      (fun fk ->
         let node = FMap.find fk cg in
         Printf.printf " %s\n" node.name;
         FQ.addOnce fk work
      ) reachConstructors
      
  method findDirectCallers fk =
    try
      let node = FMap.find fk cg in
      List.fold_left 
        (fun res ck ->
           let cnode = FMap.find ck cg in
           List.fold_left 
             (fun res candidate ->
                match candidate with
                  CDirect (_, k) -> 
                    if k = fk then FSet.add ck res else res
                | CIndirect (_, ks) -> 
                    if (List.exists (fun k -> k = fk) ks) then
                      self#addMissedIndirectCall cnode.name node.name ;
                    res (* don't use for now *)
             ) res (calleeDetail cnode)
        ) FSet.empty (callerKeys node)
    with Not_found -> failwith "findDirectCallers Not_found?"


  method transitivelyClose () =
    let work = FQ.create () in
    self#initWork work;
    Printf.printf "===============================================\n";
    Printf.printf "Transitively closing calls to constructors %d\n"
      (FQ.length work);
    while not (FQ.is_empty work) do
      let next = FQ.pop work in
      let directCallers = self#findDirectCallers next in
      FSet.iter 
        (fun ck -> 
           if not (FSet.mem ck reachConstructors) then begin
             FQ.addOnce ck work;
             reachConstructors <- FSet.add ck reachConstructors
           end
        ) directCallers
    done;
    self#printMissedIndirectCalls ()

end

let getBindingFuncs binders =
  TH.fold 
    (fun tk _ cur -> match tk with
       TkAssign fk 
     | TkRet fk -> FSet.add (fkey_to_fid fk) cur
     | TkArg fk -> cur (* not considering that a constructor *)
     | TkInit (_) -> cur
    ) binders FSet.empty

let getSlicedCg cg bindFuncs =
  let closer = new addrTakerCallers cg bindFuncs in
  closer#transitivelyClose ();
  let keepers = closer#reachConstructors () in
  sliceCg cg keepers


(********** Printing helpers **********)

let string_of_funcNI name fkey =
  name ^ "(" ^ string_of_int fkey ^ ")"
  
let string_of_funcVar v =
  string_of_funcNI v.vname v.vid

let string_of_func cg fkey =
  (* Assuming cg is context-insensitive! *)
  let fid = fkey_to_fid fkey in
  let name = 
    try let node = FMap.find fid cg in node.name
    with Not_found -> "?" in
  string_of_funcNI name fkey

let printHeader h =
  Printf.printf "========================================\n";
  Printf.printf "%s\n" h;
  Printf.printf "========================================\n"

let printTakerSeg n =
  Printf.printf "Funcs w/ %d takers ::\n======================\n" n

let printFuncSeg n =
  Printf.printf "Takers w/ %d funcs ::\n======================\n" n

let printFunHeader f =
  Printf.printf "Fun %s -> [\n" (string_of_funcVar f)

let printTakerHeader cg tk =
  Printf.printf "Taker %s -> [\n" 
    (match tk with 
       TkAssign id -> Printf.sprintf "(Fun %s)" (string_of_func cg id)
     | TkArg id -> Printf.sprintf "(Arg Call %s)" (string_of_func cg id)
     | TkRet id -> Printf.sprintf "(Ret %s)" (string_of_func cg id)
     | TkInit (vi, moreTakers) -> Printf.sprintf "(Init %s:%s)" 
         vi.vname (string_of_loc vi.vdecl)
    )

let printATaker cg tk =
  match tk with 
    TkAssign id -> Printf.printf "  Fun %s\n" (string_of_func cg id)
  | TkArg id -> Printf.printf "  Arg Call %s\n" (string_of_func cg id)
  | TkRet id -> Printf.printf "  Ret %s\n" (string_of_func cg id)
  | TkInit (vi, moreTakers) -> Printf.printf "  Init %s:%s\n" 
      vi.vname (string_of_loc vi.vdecl)

let printAFunc f = 
  Printf.printf "  %s\n" (string_of_funcVar f)


let lenCompare (_, _, x) (_, _, y) = Pervasives.compare x y

let sortMap map fold = 
  let asList = fold (fun f tks cur -> (f, tks, List.length tks) :: cur) map [] in
  List.sort lenCompare asList
        
let printMap map fold header printSeg printKey printVal distDescr =
  let map = sortMap map fold in
  printHeader header;
  let numTk = Dist.makeDistro () in
  let _, _ = List.fold_left 
    (fun (prev, count) (key, vals, numInSeg) ->
       Dist.updateDistro numTk numInSeg;
       let newVals, newCount = 
         if prev != numInSeg then begin
           printSeg numInSeg;
           (numInSeg, 1)
         end else
           (prev, count + 1) in
       printKey key;
       List.iter printVal vals;
       Printf.printf "  ] (%d)\n" numInSeg;
       newVals, newCount
    ) (-1, 0) map in
  Dist.printDistroSortKey numTk (string_of_int) distDescr

let printPaths cg roots sinks =
  let total = List.length roots * List.length sinks in
  let fin = ref 0 in
  let updateProgress () =
    incr fin;
    if !fin mod 100 == 0 then Printf.printf "pairs: %d/%d\n" !fin total
  in  
  printHeader ("Printing paths: " ^ string_of_int total); 
  List_utils.listIterOrderedPairs
    (fun (k1, n) k2 ->
       let p = Dot_lib.findShortestPath cg k1 k2 in
       updateProgress ();
       match p with
         [x] -> () | [] -> ()
       | _ -> Dot_lib.printPath p
    ) roots sinks

let printCg cg sinks outfile =
  printHeader "Roots of sliced cg";
  let roots = getRoots cg in
  List.iter 
    (fun (fk, n) -> Printf.printf "  %s(%s)\n" n.name (fid_to_string fk)) 
    roots;
  Printf.printf "(%d)\n\n" (List.length roots);
  printPaths cg roots (FSet.elements sinks)

(************************************************************)

let getBinders root cg =
  let pass1 = new fpAddrTakenVisitor in
  Filetools.walkDir (fun ast file -> 
                       visitCilFileSameGlobals (pass1 :> cilVisitor) ast) root;
  let takersToFuncs = pass1#takerToFuncs () in
  let curMap = pass1#funcToAddrTaker () in
  (curMap, takersToFuncs)

    
let printBinders root cg : unit =
  let (curMap, takersToFuncs) = getBinders root cg in
  printMap takersToFuncs TH.fold "Map from takers -> funcs w/ addr taken"
    printFuncSeg (printTakerHeader cg) printAFunc "num funcs";
  printMap curMap FH.fold "Map from funcs w/ addr taken -> takers"
    printTakerSeg printFunHeader (printATaker cg) "num binders"
  


(************************************************************
 Do the partitioning
************************************************************)

module U = Uf.NotTracked

module FSH = Hashtbl.Make
  (struct
     type t = FSet.t
     let equal a b = FSet.equal a b
     let hash x = 
       FSet.fold (fun k cur -> Hashtbl.hash k lxor cur) x 0
   end)

(*********** Conversions ***********)

let uniqueParts fkeyToPart =
  let pset = FSH.create 17 in
  Hashtbl.iter 
    (fun fk p -> 
       let rep = U.deref p in
       if FSH.mem pset rep then ()
       else FSH.add pset rep ()
    ) fkeyToPart;
  pset

let serializableParts fkeyToPart =
  let fkToP = Hashtbl.create 17 in
  Hashtbl.iter 
    (fun fk p -> 
       let rep = U.deref p in
       Hashtbl.replace fkToP fk rep
    ) fkeyToPart;
  fkToP
  
(*********** Main partitioner ***********)
       
let doPartitioning binders =
  let fkeyToPart = Hashtbl.create 17 in
  let binderToPart = TH.create 17 in
  let findPart fkey =
    try Hashtbl.find fkeyToPart fkey
    with Not_found ->
      let newPart = U.uref (FSet.singleton fkey) in
      Hashtbl.add fkeyToPart fkey newPart;
      newPart
  in
  let addFun fkey binder =
    try 
      let old = TH.find binderToPart binder in
      let other = findPart fkey in
      U.unify 
        (fun (s1, s2) -> 
           FSet.union (U.deref s1) (U.deref s2)) (old, other);
    with Not_found ->
      TH.add binderToPart binder (findPart fkey)
  in
  FH.iter (fun f bs -> List.iter (addFun (fkey_to_fid f.vid)) bs) binders;
  (serializableParts fkeyToPart, uniqueParts fkeyToPart)

(******** Testing **********)

let funsInPartitions partitions =
  FSH.fold (fun p () cur -> FSet.union p cur) partitions FSet.empty

let sanityCheckPartitions root cg partitions =
  Printf.printf "\n==== Starting sanity check ====\n\n";
  let addrTaken = Faddr_taken.getFAddrTaken root in
  Faddr_taken.printAddrTaken addrTaken;
  let addrTaken = Faddr_taken.toSet addrTaken in
  let partSet = funsInPartitions partitions in
  Printf.printf "==== Diff of addr taken and part set\n";
  Printf.printf "===================================\n";
  L.logStatusD (d_funcset cg (FSet.diff addrTaken partSet))


(****************************)

let writePartitions parts outfile =
  let oc = open_out_bin outfile in
  Marshal.to_channel oc parts [Marshal.Closures];
  close_out oc
    
let readPartitions infile =
  let ic = open_in_bin infile in
  let res = Marshal.from_channel ic in
  close_in ic;
  res

let printPartitions cg parts =
  let partHisto = Dist.makeDistro () in
  print_string "========================\n";
  print_string "Function partitions\n";
  print_string "========================\n";
  FSH.iter 
    (fun p () ->
       let size = FSet.cardinal p in
       Dist.updateDistro partHisto size;
       let doc = text "{ " ++
         (L.seq_to_doc 
            (text ", ") 
            FSet.iter 
            (fun k -> text (string_of_func cg (fid_to_fkey k)))
            p nil)
         ++ dprintf "} (%d)" size ++ line in
       L.logStatusD doc;
    ) parts;
  Dist.printDistroSortKey partHisto string_of_int "partition sizes"


let partitionFuncs root cg outfile =
  if outfile = "" then
    failwith "No target file to write partitions?!"
  ;
  let (funsToTakers, takersToFuncs) = getBinders root cg in
  printMap takersToFuncs TH.fold "Map from takers -> funcs w/ addr taken"
    printFuncSeg (printTakerHeader cg) printAFunc "num funcs";
  printMap funsToTakers FH.fold "Map from funcs w/ addr taken -> takers"
    printTakerSeg printFunHeader (printATaker cg) "num binders";
  let fkToPart, partitions = doPartitioning funsToTakers in
  sanityCheckPartitions root cg partitions;
  printPartitions cg partitions;
  writePartitions fkToPart outfile


(************************************************************)
(* Print pruned cg of callers to "constructors" *)


let printConstructionCG root cg outfile =
  if outfile = "" then
    failwith "No target file to write partitions?!"
  ;
  let _, takersToFuncs = getBinders root cg in
  let seed = getBindingFuncs takersToFuncs in
  let slicedCg = getSlicedCg cg seed false in
  Printf.printf "Sliced cg! Old nodes: %d    New nodes: %d\n"
    (Stdutil.mapSize cg FMap.fold) 
    (Stdutil.mapSize slicedCg FMap.fold);
  printCg slicedCg seed outfile
