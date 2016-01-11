
open Cil
open Cilinfos

module CilPTA = Myptranal
module D = Cildump
module Dist = Distributions

(** Expected interface for testing a function analysis *)

class type fpTestable = object

  method init : Cil.file -> unit

  method resolve_fp : Cil.exp -> int list

  method name : string

end


(** Test interface for CIL *)

class cilFPTest : fpTestable = object (self) 

  val aliasCache = Hashtbl.create 10 (* alias file-info cache *)

  method init f =
    (* TODO: cache PTA state?? *)
    if (not (Hashtbl.mem aliasCache f.fileName)) then
      begin
        CilPTA.reset_globals ();
        CilPTA.analyze_file f;
        CilPTA.compute_results false;
        Hashtbl.clear aliasCache;
        Hashtbl.add aliasCache f.fileName true;
      end
        

  method resolve_fp fexp =
    (* Should also match ftypes, since the PTA may munge a bunch
       when it comes to structs, etc. *)
    let ftype = Cil.typeOf fexp in
    let ftype_str = D.string_of_ftype ftype in    
    let fdecs = 
      try
        CilPTA.resolve_funptr fexp
      with 
        Not_found
      | CilPTA.UnknownLocation ->
          []
    in
    List.fold_left 
      (fun curList fdec ->
         let ft = fdec.svar.vtype in
         let fts = D.string_of_ftype ft in
         if (fts = ftype_str) then
           fdec.svar.vid :: curList
       else
         curList
      ) [] fdecs

  method name = "CIL"

end


(** The test driver *)

module StringSet = Set.Make(String)

class fpTestDriver (testable1 : fpTestable) (testable2 : fpTestable) = object (self)
  inherit nopCilVisitor

  val mutable numBothEmpty = 0
  val mutable numEmpty1 = 0
  val mutable numEmpty2 = 0
  val mutable numDiffsNonEmpty = 0
  val mutable numAgree = 0

  val distro1 = Dist.makeDistro ()
  val distro2 = Dist.makeDistro ()
    
  method private printFSet name set =
    Printf.printf "  %s: " name;
    StringSet.iter (fun fn -> print_string (fn ^ ", ")) set;
    print_newline ()

  method private ids_to_names ids addTo =
    List.iter 
      (fun fid ->
         let varinfo = getVarinfo fid in
         addTo := StringSet.add varinfo.vname !addTo
      ) (List.fast_sort (fun a b -> a - b) ids)

  method private printDiffs a aname b bname d1 d2 =
    self#printFSet aname !a;
    self#printFSet bname !b;
    self#printFSet "<< Diff " d1;
    self#printFSet ">> Diff " d2;
    print_newline ()

  method vinst (i:instr) : instr list visitAction =
    match i with
      Call(_, callexp, _, _) -> begin
        match callexp with
          Lval (Var(vi),NoOffset) ->
            SkipChildren

        | Lval (Mem(ptrExp), NoOffset) ->
            let this = ref StringSet.empty in
            let fids = testable1#resolve_fp ptrExp in
            self#ids_to_names fids this;

            let other = ref StringSet.empty in
            let fids = testable2#resolve_fp ptrExp in
            self#ids_to_names fids other;
            
            let diff1 = StringSet.diff !this !other in
            let diff2 = StringSet.diff !other !this in
            Printf.printf "cexp: %s @ %s\n" 
              (D.string_of_exp callexp) (D.string_of_loc !currentLoc);
            
            let s1 = StringSet.cardinal !this in
            let s2 = StringSet.cardinal !other in
            Dist.updateDistro distro1 s1;
            Dist.updateDistro distro2 s2;
            if s1 == 0 then
              if s2 == 0 then begin
                print_endline "no targets for both?";
                numBothEmpty <- numBothEmpty + 1
              end else begin
                self#printDiffs this testable1#name other testable2#name 
                  diff1 diff2;
                numEmpty1 <- numEmpty1 + 1
              end
            else if s2 == 0 then begin
              self#printDiffs this testable1#name other testable2#name 
                diff1 diff2;
              numEmpty2 <- numEmpty2 + 1
            end else if StringSet.is_empty diff1 && StringSet.is_empty diff2 
            then begin
              print_endline "same!";
              self#printFSet testable1#name !this;
              numAgree <- numAgree + 1;
            end else begin
              self#printDiffs this testable1#name other testable2#name 
                diff1 diff2;
              numDiffsNonEmpty <- numDiffsNonEmpty + 1              
            end;

            print_newline ();

            SkipChildren
        | _ ->
            SkipChildren
      end
    | _ ->
        SkipChildren
          
  method private printDiffStats () = begin
    Printf.printf "=============================\nDiff stats:\n";
    Printf.printf "Both empty: %d\n" numBothEmpty;
    Printf.printf "First empty: %d\n" numEmpty1;
    Printf.printf "Second empty: %d\n" numEmpty2;
    Printf.printf "Non-empty diff: %d\n" numDiffsNonEmpty;
    Printf.printf "Non-empty agree: %d\n" numAgree;
    print_string "\n";
    Dist.printDistroSortKey distro1 
      string_of_int ("FP targets " ^ testable1#name);
    Dist.printDistroSortKey distro2 
      string_of_int ("FP targets " ^ testable2#name);
  end
    

  method testFunPtrs root =
    print_string "PTA testing function pointers\n";
    flush stdout;
    Filetools.walkDir 
      (fun ast filename ->
         Printf.printf "\tCalls from %s\n" filename;
         testable1#init ast;
         testable2#init ast;
         Cil.visitCilFileSameGlobals (self :> cilVisitor) ast
      ) root;

    self#printDiffStats ();
    print_string "Done\n\n"

end
