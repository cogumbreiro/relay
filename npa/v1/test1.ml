open Cil


module VarSet = Set.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end
)

type relativeNullSet =
  { 
    nplus : VarSet.t;
    nminus : VarSet.t
  }

let equalRns a b =
  VarSet.equal a.nplus b.nplus
    && VarSet.equal a.nminus b.nminus

let mergeRns a b =
  {  nplus = VarSet.inter a.nplus b.nplus
     ; nminus = VarSet.union a.nminus b.nminus }

let string_of_rns rns : string =
    "N+ = {"
      ^ VarSet.fold (fun s acc -> acc ^ s ^ " ") rns.nplus " "
      ^ "} N- = {"
      ^ VarSet.fold (fun s acc -> acc ^ s ^ " ") rns.nminus " "
      ^ "}"

module SafePtrTransfer =
  struct
    let name = "Safe Pointer Analysis"
    
    let debug = ref false
    
    type t = relativeNullSet
    
    let copy (rns : t) = rns
    
    let pretty () (rns : t) = Pretty.nil
    
    let add_nplus s rns = 
      { nplus = VarSet.add s rns.nplus;
        nminus = rns.nminus }

    let add_nminus s rns =
      { nplus = VarSet.remove s rns.nplus; (* remove in case it's in nplus *)
        nminus = VarSet.add s rns.nminus }

    let stmtStartData =
      Inthash.create 32 
    
    let computeFirstPredecessor stmt t = t
    
    let combinePredecessors (stmt : Cil.stmt) ~(old : t) (br : t) =
      if equalRns old br then None
      else Some (mergeRns old br)

    let rec getPtrName exp =
      (*let parse t vname =
        let typeString = Pretty.sprint 80 (Cil.d_type () t) in
        let i = (String.index typeString ' ') + 1 in
        let stars = String.sub typeString i ((String.length typeString) - i) in
          vname
      in*)
      match exp with
      | Lval (Var v, _) -> v.vname
      | Lval (Mem (Lval (_, _) as lval), _) -> "*" ^ getPtrName lval
        (* XXX: right now, not checking that t is pointer type *)
      | CastE (t, (Lval (Var v, _))) -> v.vname
        (* XXX: need to handle this erroneous case *)
      | _ -> ""
    
    let doInstr (i: Cil.instr) (rns: t) =
      match i with
      | Set ((a, b), exp, _) ->
          (match exp with
           | AddrOf (Var v, _) ->
               if v.vglob then
                 Dataflow.Done (add_nplus (getPtrName (Lval (a, b))) rns)
               else
                 Dataflow.Done (add_nminus (getPtrName (Lval (a, b))) rns)

           | _ -> 

            (* XXX: think about how to do this *)
            let lname, rname = getPtrName (Lval (a, b)), getPtrName exp in
              if VarSet.mem rname rns.nplus then
                Dataflow.Done (add_nplus lname rns)
              else
                Dataflow.Done (add_nminus lname rns))

      | _ -> Dataflow.Default

    let doStmt (s: Cil.stmt) (rns: t) =
      Dataflow.SDefault

    let rec doGuard cond rns = 
      match cond with

      (* assuming x != NULL gets cilly-fied to
         (unsigned int )x != (unsigned int )((void * )0)
         and i'm not checking for the types of the cast here *)
      | BinOp (Ne,
               CastE (_, (Lval (_, _) as lval)),
               CastE (_, CastE (_, Const (CInt64 (0L, _, _)))),
               _)
      | BinOp (Ne,
               CastE (_, CastE (_, Const (CInt64 (0L, _, _)))),
               CastE (_, (Lval (_, _) as lval)),
               _) ->
          Dataflow.GUse (add_nplus (getPtrName lval) rns)

      | BinOp (Eq,
               CastE (_, (Lval (_, _) as lval)),
               CastE (_, CastE (_, Const (CInt64 (0L, _, _)))),
               _)
      | BinOp (Eq,
               CastE (_, CastE (_, Const (CInt64 (0L, _, _)))),
               CastE (_, (Lval (_, _) as lval)),
               _) ->
          Dataflow.GUse (add_nminus (getPtrName lval) rns)

      (* handle !'s in succession *)
      | UnOp (LNot, UnOp (LNot, exp, _), _) ->
          doGuard exp rns

      (* handle negations of Ne and Eq *)
      | UnOp (LNot, BinOp (Ne, e1, e2, typ), _) ->
          doGuard (BinOp (Eq, e1, e2, typ)) rns
      | UnOp (LNot, BinOp (Eq, e1, e2, typ), _) ->
          doGuard (BinOp (Ne, e1, e2, typ)) rns

      | _ ->
          Dataflow.GDefault
    
    let filterStmt _ = true
  end

module SafePtrDataFlow = Dataflow.ForwardsDataFlow(SafePtrTransfer)

let rec print_npa (l: stmt list) ht =

  let rec print_instrlist_rns stmt = function
    | instr :: tail ->
        let rns = SafePtrDataFlow.getStateAtInstr stmt instr in
         (match instr with
          | Set (_, _, loc) ->
              Printf.printf "########## instr\n%s\n########## %s\n\n"
                (Pretty.sprint 80 (Cil.d_instr () instr)) (string_of_rns rns)
          | _ ->
              Printf.printf "  XXX no rns at Call or Asm\n");
          print_instrlist_rns stmt tail
    | [] -> () in

  let print_stmt_rns stmt rns =
    Printf.printf "********** stmt %d\n%s\n********** %s\n\n"
      stmt.sid
      (Pretty.sprint 80 (Cil.d_stmt () stmt))
      (string_of_rns rns) in
  
  match l with
  | stmt :: tail ->
      (print_stmt_rns stmt (Inthash.find ht stmt.sid);
      (match stmt.skind with
       | Instr il ->
           print_instrlist_rns stmt il
       | _ -> ()));
       print_npa tail ht
  | [] -> ()
            

let performAnalysis (f: fundec) =
  Printf.printf "ANALYZING [%s]...\n" f.svar.vname;
  Printf.printf "Formals: %s\n"
    (List.fold_right (fun s acc -> acc ^ s.vname ^ " ") f.sformals "");
  Printf.printf "Locals: %s\n\n"
    (List.fold_right (fun s acc -> acc ^ s.vname ^ " ") f.slocals "");

  let localVars =
    List.fold_right (fun v set -> VarSet.add v.vname set) f.slocals VarSet.empty in

  (* a var escapes if it is not local *)
  let varEscapes s =
    (VarSet.mem s localVars) = false in

  let filterEscapedVars rns =
    { nplus = VarSet.filter varEscapes rns.nplus;
      nminus = VarSet.filter varEscapes rns.nminus } in

  match f.sbody.bstmts with
  | [] -> ()
  | start ::  _ ->
        
      prepareCFG f;
      computeCFGInfo f false;
      Inthash.clear SafePtrTransfer.stmtStartData;
      
      Inthash.add SafePtrTransfer.stmtStartData start.sid
        { nplus = VarSet.empty; nminus = VarSet.empty };
      
      SafePtrDataFlow.compute [start];

      let exitid = (List.hd (List.rev f.sallstmts)).sid in
      let exitFact = Inthash.find SafePtrTransfer.stmtStartData exitid in

      print_npa f.sallstmts SafePtrTransfer.stmtStartData;
   
      Inthash.replace SafePtrTransfer.stmtStartData
                      exitid 
                      (filterEscapedVars exitFact);

      Printf.printf "SUMMARY FOR [%s] : %s\n\n"
        f.svar.vname
        (string_of_rns (Inthash.find SafePtrTransfer.stmtStartData exitid))

let _ =
  let f = Cil.loadBinaryFile
    "/home/ravi/research/relay-race/npa/v1/cfiles/ciltrees/a.c" in
  let name = f.fileName in
    Printf.printf "File: %s\n\n" name;
    Cil.iterGlobals f (function
      GFun(dec, _) -> performAnalysis dec;
      | _ -> ())
