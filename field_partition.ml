

(** Partition functions based on how their address is taken.
    A ~ B iff 
     - there exists a common field into which A & B are dumped into... 
     - or we are in stricter-mode and A or B "escape"
    we say foo escapes if 
     - &foo was passed as an argument
     - &foo was returned
     - or &foo was captured by a field-less variable.
*)

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

type fpTaker = 
    FpField of int * string
  | FpArg
  | FpRet
  | FpLocal
  | FpGlobal
  | FpUnknown

module TH = Hashtbl.Make 
  (struct
     type t = fpTaker
     let equal = (=)
     let hash = Hashtbl.hash
   end)

let funEq a b = Ciltools.compare_var a b == 0

let funHash x = Ciltools.hash_var x

module FH = Hashtbl.Make 
  (struct 
     type t = varinfo
     let equal = funEq
     let hash = funHash
   end)

let binderOfVar vi =
  if vi.vglob then FpGlobal else FpLocal

let rec findBinderOff off cur =
  match off with
    NoOffset -> cur
  | Field (fi, moreOff) -> 
      findBinderOff moreOff (Some (FpField (fi.fcomp.ckey, fi.fname)))
  | Index (i, moreOff) ->
      findBinderOff moreOff cur
        
let findBinderLv lhs =
  match lhs with
    Var vi, off ->
      (match findBinderOff off None with
         Some b -> b
       | None -> binderOfVar vi)
  | Mem (e), off ->
      (match findBinderOff off None with
         Some b -> b
       | None -> 
           try 
             let vi = findBaseVarinfoLval lhs in
             Printf.printf "%s took the address\n" (string_of_lval lhs);
             binderOfVar vi;
           with BaseVINotFound ->
             Printf.printf "VI not found for %s\n" (string_of_lval lhs);
             FpUnknown )

let rec findAddrTakenFunc rhs =
  match rhs with
    AddrOf lv ->
      (match lv with 
         Var(vi), NoOffset -> 
           if isFunctionType vi.vtype 
           then Some vi else None
       | _ -> None )
  | StartOf _
  | Lval _ -> None
  | CastE (_, e) -> findAddrTakenFunc e
  | Const _
  | SizeOf _
  | SizeOfStr _
  | AlignOf _ -> None
  | AlignOfE _
  | SizeOfE _ -> None
  | UnOp (op, _, t) -> None
  | BinOp (op, _, _, t) -> None


(* Track functions w/ addrTaken and put them in the appropriate bucket *)
class fpBucketer = object(self)
  inherit nopCilVisitor

  val funcToAddrTakers = FH.create 10
  method funcToAddrTakers () = funcToAddrTakers
  val fpTakerToFuncs = TH.create 10
  method fpTakerToFuncs () = fpTakerToFuncs

  val mutable curFunc = dummyFunDec

  method private addBinding takenFunc taker =
    let old = 
      try FH.find funcToAddrTakers takenFunc
      with Not_found -> [] in
    FH.replace funcToAddrTakers takenFunc (List_utils.addOnce old taker);
    let old =
      try TH.find fpTakerToFuncs taker
      with Not_found -> [] in
    TH.replace fpTakerToFuncs taker (List_utils.addOnceP funEq old takenFunc)

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
        (* not matching the ret of the called function w/ the lvopt *)
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
         (match findAddrTakenFunc e with
            None -> ()
          | Some vi -> 
              self#addBinding vi (FpRet)
         )
     | Switch (e, _, _, _)
     | If (e, _, _, _) -> 
         (* Don't count ptr cast here -- used w/ checking against null *)
         (* not counted as an instruction *)
         ()
    ); DoChildren


  method private processAssign loc lhs rhs =
    match findAddrTakenFunc rhs with
      None -> ()
    | Some vi ->
        let b = findBinderLv lhs in
        self#addBinding vi b


  method private processCall loc exp args =
    match exp with
      Lval lv -> self#processCallLv loc lv args
    | _ -> failwith "Skipping non-lval call exp\n"

        
  method private processCallLv loc lv args =
    List.iter 
      (fun exp -> 
         match findAddrTakenFunc exp with
           None -> ()
         | Some vi ->
             self#addBinding vi FpArg
      ) args
          
end


let lenCompare (_, _, x) (_, _, y) = Pervasives.compare x y

let sortMap map = 
  let asList = 
    FH.fold 
      (fun f tks cur -> (f, tks, List.length tks) :: cur) map [] in
  List.sort lenCompare asList

let string_of_funcNI name fkey =
  name ^ "(" ^ string_of_int fkey ^ ")"
  
let string_of_funcVar v =
  string_of_funcNI v.vname v.vid

let string_of_taker tk =
  match tk with 
    FpField (id,n) -> 
      Printf.sprintf "Field %s(%d)" n id
  | FpArg -> "Arg"
  | FpRet -> "Ret"
  | FpLocal -> "Local"
  | FpGlobal -> "Global"
  | FpUnknown -> "Array / ??"

let printFun2Take map =
  let map = sortMap map in
  Printf.printf "========================================\n";
  Printf.printf "Map from funcs w/ addr taken -> takers\n";
  Printf.printf "========================================\n";
  let numFp = Dist.makeDistro () in
  let _, _ = List.fold_left 
    (fun (prev, count) (f, takers, numTakers) ->
       Dist.updateDistro numFp numTakers;
       let newTakers, newCount = 
         if prev != numTakers then begin
           Printf.printf "Funcs w/ %d takers ::\n======================\n" 
             numTakers;
           (numTakers, 1)
         end else
           (prev, count + 1) in
       Printf.printf "Fun %s -> [\n" (string_of_funcVar f);
       List.iter (fun tk -> Printf.printf "  %s\n" (string_of_taker tk)) takers;
       Printf.printf "  ] (%d)\n" numTakers;
       newTakers, newCount
    ) (-1, 0) map in
  Dist.printDistroSortKey numFp (string_of_int) "num binders"

let printTake2Fun take2Fun =
  let partHisto = Dist.makeDistro () in
  print_string "========================\n";
  print_string "Function partitions\n";
  print_string "========================\n";
  TH.iter 
    (fun tk funs ->
       let size = List.length funs in
       Dist.updateDistro partHisto size;
       let doc = dprintf "%s: { " (string_of_taker tk) ++
         (L.seq_to_doc 
            (text ", ") 
            List.iter 
            (fun vi -> text (string_of_funcVar vi))
            funs nil)
         ++ dprintf "} (%d)" size ++ line in
       L.logStatusD doc;
    ) take2Fun;
  Dist.printDistroSortKey partHisto string_of_int "partition sizes"

let getBinders root =
  Printf.printf "Bucketing each function into fields\n";
  flush stdout;
  let pass1 = new fpBucketer in
  Filetools.walkDir (fun ast file -> 
                       visitCilFileSameGlobals (pass1 :> cilVisitor) ast) root;
  Printf.printf "Done bucketing each function into fields\n";
  flush stdout;
  (pass1#funcToAddrTakers (), pass1#fpTakerToFuncs ())

let writePartitions parts outfile =
  let oc = open_out_bin outfile in
  Marshal.to_channel oc parts [Marshal.Closures];
  close_out oc
    
let readPartitions infile =
  let ic = open_in_bin infile in
  let res = Marshal.from_channel ic in
  close_in ic;
  res

let convertList2FS fs =
  List.fold_left (fun cur vi -> FSet.add (fkey_to_fid vi.vid) cur) FSet.empty fs
    
let convert tk2Fun =
  let res = TH.create 17 in
  TH.iter (fun tk fs -> TH.replace res tk (convertList2FS fs)) tk2Fun; 
  res

let partitionAndSave root outfile =
  if outfile = "" then
    failwith "No target file to write partitions?!"
  ;
  let fun2Take, take2Fun = getBinders root in
  printFun2Take fun2Take;
  printTake2Fun take2Fun;
  Printf.printf "Done partitioning and printing, now saving\n";
  flush stdout;
  writePartitions (convert take2Fun) outfile

let partitionFuncs root =
  let fun2Take, take2Fun = getBinders root in
  printFun2Take fun2Take;
  printTake2Fun take2Fun;
  Printf.printf "Done partitioning and printing, now returning\n";
  flush stdout;
  convert take2Fun
