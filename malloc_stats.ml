
(** Print stats on types returned by malloc *)

open Trans_alloc
open Ciltools
open Cil

module D = Cildump
module L = Logging
module Dist = Distributions

let varEq x y = compare_var x y == 0

module VH = Hashtbl.Make
  (struct 
     type t = varinfo
     let equal x y = varEq x y
     let hash x = Hashtbl.hash_param 10 20 x
   end)

let typEq x y = equal_type x y

module TH = Hashtbl.Make
  (struct 
     type t = typ
     let equal x y = typEq x y
     let hash x = Hashtbl.hash_param 10 20 x
   end)


class mallocVisitor = object(self)
  inherit nopCilVisitor

  val varToType = VH.create 10
  method varToType () = varToType
  val typeToVar = TH.create 10
  method typeToVar () = typeToVar

  val mutable curFunc = dummyFunDec

  method private addBinding var typ =
    let old = 
      try VH.find varToType var
      with Not_found -> [] in
    VH.replace varToType var (List_utils.addOnceP typEq old typ);
    let old = 
      try TH.find typeToVar typ
      with Not_found -> [] in
    TH.replace typeToVar typ (List_utils.addOnceP varEq old var)

  method vfunc f = 
    curFunc <- f;
    DoChildren

  method vinst i =
    (match i with
       Set (lv, rhs, loc) -> 
         self#processAssign loc lv rhs
     | Asm (_, _, _, _, _, _) -> ()
     | Call(lvopt, callexp, args, loc) -> ()
    );
    DoChildren
      
  method private processAssign loc lv rhs =
    match findMalloc rhs with
      None -> ()
    | Some vi -> self#addBinding vi vi.vtype
        (* It's all in the VI, but track type mapping for convenience *)
      
  method private printUnknownTypes () =
    try 
      let utVars = TH.find typeToVar unknownMallocType in
      L.logStatus "\nUnknown-typed Malloc vars\n===========================\n";
      List.iter (fun x -> L.logStatusF " %s : %s\n" x.vname 
                   (D.string_of_loc x.vdecl)) utVars
    with Not_found ->
      L.logStatus "No unknown-typed Malloc vars\n"
  
  method printStats () = 
    L.logStatus "Malloc Var -> Type\n=========================\n";
    let typeDistro = Dist.makeDistro () in
    VH.iter (fun v ts -> 
               match ts with
                 [t] -> 
                   L.logStatusF "  %s -> %s\n" v.vname (D.string_of_type t);
                   Dist.updateDistro typeDistro (D.string_of_type t);
               | _ -> failwith (" > 1 type for a malloc var? " ^ v.vname ^ "\n")
            ) varToType;
    L.logStatusF "========================\n";
    Dist.printDistroSortFreq typeDistro (fun x -> x) "Type Histogram";
    self#printUnknownTypes ()


end

let printAllocTypes root =
  let vis = new mallocVisitor in
  Filetools.walkDir 
    (fun ast f -> visitCilFileSameGlobals (vis :> cilVisitor) ast) root;
  vis#printStats ()
