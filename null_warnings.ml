
(** Dereference checker *)

open Cil
open Callg
open Pretty
open Cildump

module L = Logging
module Lv = Lvals
module CLv = Cil_lvals
module SPTA = Symstate2
module LocSet = Lockset_partitioner.LocSet
module Par = Pseudo_access
module LvalHash = Par.LvalHash
module BS = Backed_summary

(************************************************************
  Collection of warnings
************************************************************)

type imprec = 
    Shown of Lv.imprecision
  | NotShown (* warning generated from a deref check 
                that never matched w/ N+ or N- *)
  | Blob

type nullChain = funID list

type nullData =
    { nullLval : Lv.aLval;
      nullChain : nullChain;
      nullCrumbs : Rns.BreadCrumbs.t;
      nullImprec : imprec; }      


(** Expected interface for tracking warnings seen so far *)
module NullW = struct

  type key = location

  type data = nullData
    
  let equalKey k1 k2 = 
    compareLoc k1 k2 == 0
    
  let hashKey = Hashtbl.hash

  let hashConsKey = CLv.distillLoc
    
  let equalData a b =
    Lv.compare_lval a.nullLval b.nullLval == 0 &&
      a.nullChain = b.nullChain && 
        Rns.BreadCrumbs.equal a.nullCrumbs b.nullCrumbs &&
        a.nullImprec = b.nullImprec
    
  let hashConsData a =
    { a with 
        nullLval = Lv.mergeLv a.nullLval; }
      
  let pDataXML d =
    Pretty.nil

end

module WarnR = Warn_reports.MakeRep (NullW)


(* dummy data to be used in safe dereference "warning" report *)
let dummyNullData alv =
    { nullLval = alv;
      nullChain = [];
      nullCrumbs = Rns.BreadCrumbs.empty;
      nullImprec = NotShown; }



    
(****************** Printing Warning Diffs *********************)


let pCluster loc data =
  let locDoc = text "null deref(s) at: " ++ Cil.d_loc () loc in
  let pBody data = 
    locDoc ++ text " of [" ++
      L.seq_to_doc 
      (text ", ")
      List.iter 
      (fun w ->
         text (Lv.string_of_lvscope w.nullLval)
         ++ text " < "
         ++ text (List.fold_left
                    (fun s fk -> s ^ fid_to_string fk ^ " ") "" w.nullChain)
         ++ text ">"
      ) data 
      Pretty.nil ++ 
      text "]"
  in
  let body = pBody data in
  let len = List.length data in
  body ++ dprintf "(%d)" len


let pReport rep =
  L.map_to_doc 
    Pretty.line 
    WarnR.KH.iter
    pCluster
    rep
    Pretty.nil


let pDiff loc diff =
  match diff with
    WarnR.OnlyFirst data ->
      text "Only first:" ++ line ++
        text "     " ++ pCluster loc data
  | WarnR.OnlySecond data ->
      text "Only second:" ++ line ++
        text "     " ++ pCluster loc data
  | WarnR.BothButDiff (lvs1, lvs2) ->
      text "Both different: " ++ line ++
      text " [1] " ++ pCluster loc lvs1 ++ line ++
        text " [2] " ++ pCluster loc lvs2

let printWarnDat datFile rep =
  L.logStatusF "Warnings from: %s\n" datFile;
  L.logStatus "==============================";
  let doc = pReport (rep#getData) in
  L.logStatusD doc;
  let size = WarnR.KH.length (rep#getData) in
  L.logStatusF "\nTotal: %d\n\n" size


let printDiffs datFile1 datFile2 : unit =
  let rep1 = WarnR.deserialize datFile1 in
  let rep2 = WarnR.deserialize datFile2 in
(*DEBUG*)
  printWarnDat datFile1 rep1;
  printWarnDat datFile2 rep2;
(*/DEBUG*)
  let diff = rep1#diffReport rep2 in
  let diffDoc = L.map_to_doc
    Pretty.line 
    WarnR.KH.iter
    pDiff
    diff
    Pretty.nil in
  let diffDoc = diffDoc ++ line in
  L.logStatusF "\nDelta:%d\n" (WarnR.KH.length diff); 
  L.logStatusD (indent 2 diffDoc);
  L.logStatus "\n"

(********** Delta Summary Info **********)

let getFname fid =
(*  try *)
    let fk = fid_to_fkey fid in
    let varinfo = Cilinfos.getVarinfo fk in
    varinfo.vname
(*  with Cilinfos.NoVarinfo _ -> "?" *)

module ChainHash = Hashtbl.Make(
  struct
    type t = nullChain
    let equal a b = a = b
    let hash = Hashtbl.hash
  end
)

let string_of_chain chain =
  "<" ^ List.fold_left (fun s i ->
          s ^ getFname i ^ ":" ^ 
            fid_to_string i ^ " ") " " chain ^ ">" 

let string_of_crumb = function
  | Rns.PaFound (pakh, _) -> Par.string_of_pakeyhead pakh
  | Rns.PaNotFound lv     -> "?" ^ Lvals.string_of_lval lv
  | Rns.PaNotFoundPlaceHolder -> "?"

let string_of_crumbs crumbs =
  Rns.BreadCrumbs.fold (fun c s -> s ^ string_of_crumb c ^ " ") crumbs ""

let string_of_imprec imp =
  match imp with
    NotShown -> "Not in N+/N-"
  | Shown i -> Lv.string_of_imp i
  | Blob -> "Blob"

let printDeltaReport datFile1 datFile2 =
  let rep1 = WarnR.deserialize datFile1 in
  let rep2 = WarnR.deserialize datFile2 in
  let diff = rep1#diffReport rep2 in

  (* the summary report: fkey -> (lv -> (chain -> (crumbs, locs, imprec))) *)
  let ht = FH.create 10 in

  let allCrumbs = ref Rns.BreadCrumbs.empty in
  let countTotal, countLvals, countFuncs, countPAs =
    ref 0, ref 0, ref 0, ref 0 in
  let clusFirst, clusSecond, clusBoth = ref 0, ref 0, ref 0 in
  let crumbsGood, crumbsBad = ref 0, ref 0 in

  let cat d x = d := !d ++ (text x) ++ text "\n" in

  (* updates summary report with a warning *)
  let add fk loc lv chain crumbs imp =
    incr countTotal;
    allCrumbs := Rns.BreadCrumbs.union crumbs !allCrumbs;

    if FH.mem ht fk then
      let oldFkBinding = FH.find ht fk in
      begin
        if LvalHash.mem oldFkBinding lv then
          let oldChainTable = LvalHash.find oldFkBinding lv in
          begin
            if ChainHash.mem oldChainTable chain then
              let oldCrumbs, oldLocs, oldImp = 
                ChainHash.find oldChainTable chain in
              let newLocs = LocSet.add loc oldLocs in
              let newCrumbs = Rns.BreadCrumbs.union crumbs oldCrumbs in
              ChainHash.replace oldChainTable chain (newCrumbs, newLocs, oldImp);
            else
              (* first time for this chain *)
              ChainHash.replace oldChainTable chain 
                (crumbs, LocSet.singleton loc, imp);
          end
        else begin
          (* first time for this lval *)
          incr countLvals;
          let newChainTable = ChainHash.create 1 in
          ChainHash.replace newChainTable chain 
            (crumbs, LocSet.singleton loc, imp);
          LvalHash.replace oldFkBinding lv newChainTable;
        end
      end
    else begin
      (* first time for this function *)
      incr countFuncs;
      incr countLvals;
      let newFkBinding = LvalHash.create 10 in
      let newChainTable = ChainHash.create 2 in
      ChainHash.replace newChainTable chain (crumbs, LocSet.singleton loc, imp);
      LvalHash.replace newFkBinding lv newChainTable;
      FH.replace ht fk newFkBinding
    end in

  let makeDoc () =
    let doc = ref (text "") in

    FH.iter (fun fk lvTable ->
      cat doc ("Function " ^ fid_to_string fk ^ " " ^ getFname fk ^ "\n");
    LvalHash.iter (fun lv chainTable ->

      cat doc ("     LVAL: " ^ Lv.string_of_lvscope lv);

    ChainHash.iter (fun chain (crumbs, locs, imprec) ->

      cat doc ("    chain: " ^ string_of_chain chain);
      cat doc ("   crumbs: " ^ string_of_crumbs crumbs);
      cat doc ("   imprec: " ^ string_of_imprec imprec);
      cat doc ("     locs:");
      LocSet.iter
        (fun loc -> cat doc ("           " ^ string_of_loc loc)) locs;
      cat doc "";

    ) chainTable;
    ) lvTable;
    ) ht;

    !doc in

  (* walk through clusters in the diff and drive the report
     accumulation by calling add *) 
  WarnR.KH.iter
    (fun loc d -> 
       let f clus = 
         List.iter
           (fun w ->
              add (List.hd w.nullChain) loc 
                w.nullLval w.nullChain w.nullCrumbs w.nullImprec) clus in

       match d with
         WarnR.OnlyFirst _ ->
           incr clusFirst
       | WarnR.OnlySecond cluster ->
           incr clusSecond;
           f cluster
       | WarnR.BothButDiff (_, cluster) ->
           incr clusBoth;
           f cluster
    ) diff;

  (* count types of crumbs *)
  Rns.BreadCrumbs.iter (function
      Rns.PaFound (_, _) -> incr crumbsGood
    | Rns.PaNotFound _   -> incr crumbsBad
    | Rns.PaNotFoundPlaceHolder -> incr crumbsBad
    ) !allCrumbs;

  (* now that reports have been accumulated, create documents for them
     and print *)
  let doc1 = makeDoc () in

  L.logStatusF
    "Delta Report -- %d lvals at %d locations in %d functions\n"
    !countLvals !countTotal !countFuncs;
  L.logStatusF
    "   %d OnlyFirst, %d OnlySecond, %d BothDifferent\n"
    !clusFirst !clusSecond !clusBoth;
  L.logStatusF "   %d crumbs, %d good %d bad\n\n"
    (Rns.BreadCrumbs.cardinal !allCrumbs) !crumbsGood !crumbsBad;
  L.logStatusD (indent 2 doc1);
  L.logStatus "\n";


(************************************************************
  Summary of dereferences should be checked by the caller.
  For now, just re-use the Warn_report structure 
************************************************************)

type sum = {
  nSafe : WarnR.warnData;
  nUnsafe : WarnR.warnData;
  nDelayed : WarnR.warnData;
}

(* Have to be more careful, because they are mutable structures *)

let bottomSumm : sum = 
  let me = (WarnR.KH.create 1) in
  WarnR.KH.add me locUnknown (WarnR.emptyCluster);
  { nSafe = me;
    nUnsafe = WarnR.KH.create 0;
    nDelayed = WarnR.KH.create 0;
  } (* Invoked once? *)

let emptyReps () =
  WarnR.KH.create 0

let emptySumm () : sum =
  { nSafe = emptyReps ();
    nUnsafe = emptyReps ();
    nDelayed = emptyReps (); }


module NSum = struct
  type t = sum
  type simpleSum = t
  let simplify s = s
  let desimplify s = s
  let initVal = bottomSumm
  let unknownSummary = emptySumm ()
end

module NSummary = Safer_sum.Make (NSum)

let sums = ref (new NSummary.data (Backed_summary.makeSumType "nwarn"))

let _ = L.logStatus "Waiting to set null warning summary\n"

let setSums sumToUse =
  sums := sumToUse;
  let _ = Backed_summary.registerType !sums in
  L.logStatus "Null warning summary set!\n"

let prepareNullSums cg =
  L.logStatus ("Preparing NWARN summaries in case of restart\n");
  let toPrep = FMap.fold
    (fun k n cur ->
       if n.hasBody then k :: cur else cur
    ) cg [] in
  try
    Manage_sums.prepareSumms toPrep (Backed_summary.getDescriptors 
                                       [!sums#sumTyp])
  with Request.SummariesNotFound ->
    L.logError "prepareNullSums: some summaries not found, but maybe unneeded"
    

let getUnsafe () =
  !sums#fold 
    (fun key sum cur ->
       let unsafe = new WarnR.report ~initialData:sum.nUnsafe () in
       cur#joinReports unsafe;
       cur
    ) (new WarnR.report ())
    
let getSafe () =
  !sums#fold 
    (fun key sum cur ->
       let safe = new WarnR.report ~initialData:sum.nSafe () in
       cur#joinReports safe;
       cur
    ) (new WarnR.report ())

let isBottomSum s =
  bottomSumm = s
  (* Should be able to avoid using compare_lval if bottomSum has no lvals? *)


(** Null-pointer dereference warning analyzer *)
class derefAnalyzer stMan (getRns : prog_point -> Rns.RS.st) curFkey curCG = 
object(self)
  inherit Pp_visitor.ppVisitor 
    
  val mutable confirmations = []
  val mutable warnings = []

  val mutable numDerefsSafe = 0
  val mutable numDerefsWarn = 0
  val mutable numDelayed = 0

  val localSafe = new WarnR.report ()
  val localUnsafe = new WarnR.report ()
  val delayedChecks = new WarnR.report ()
    

  method getConfirmations = confirmations
  method getWarnings = warnings
  method getNumSafe = numDerefsSafe
  method getNumWarn = numDerefsWarn
  method getNumDelayed = numDelayed
    
  (* TODO: record the original lval that was dereferenced + the
     lval that was used to prove nullness (in the case that the check 
     was delayed). Otherwise, the error messages are confusing *)

  method private tagResult loc alv caption : string =
    if compareLoc loc !currentLoc == 0 then
      Printf.sprintf "%s %s %s"  (Cildump.string_of_loc loc)
        caption (Lvals.string_of_lval alv)
    else 
      Printf.sprintf "%s <- %s %s %s"
        (Cildump.string_of_loc loc) (Cildump.string_of_loc !currentLoc)
        caption (Lvals.string_of_lval alv)


  method confirmDereference loc alv chain =
    match localSafe#addWarning loc (dummyNullData alv) with
      None | Some WarnR.OldCluster -> 
        confirmations <- (self#tagResult loc alv "(S)") :: confirmations;
        numDerefsSafe <- numDerefsSafe + 1;
    | Some WarnR.MaxWarnings | Some WarnR.DupeWarning -> () (* didn't record *)

  method generateWarning loc alv chain crumbs imprec = 
    let w = { nullLval = alv;
              nullChain = chain;
              nullCrumbs = crumbs;
              nullImprec = imprec; } in
    match localUnsafe#addWarning loc w with
      None | Some WarnR.OldCluster ->
        warnings <- (self#tagResult loc alv "(N)") :: warnings;
        numDerefsWarn <- numDerefsWarn + 1;
    | Some WarnR.MaxWarnings | Some WarnR.DupeWarning -> ()

  method delayCheck loc alv chain =
    let w = { nullLval = alv;
              nullChain = chain;
              nullCrumbs = Rns.emptyCrumbs;
              nullImprec = NotShown; (* make a dummy imprec thing for now *)
            } in
    match delayedChecks#addWarning loc w with
      None | Some WarnR.OldCluster ->
        numDelayed <- numDelayed + 1;
    | Some WarnR.MaxWarnings | Some WarnR.DupeWarning -> () (* hmm... hit max? *)

  method isInPlus rns alv : bool =
    Rns.RNS.S.mem alv (stMan#getPlus rns)

  method isInMinus rns alv : (imprec * Rns.BreadCrumbs.t) =
    let imprec, crumbs = Rns.RNS.fold 
      (fun otherLv otherCrumbs (worstImprec, crumbs) ->
         match worstImprec with 
           None -> 
             (match Lv.sameLval alv otherLv with
                None -> (None, crumbs)
              | x -> (x, otherCrumbs))
         | Some oldImp -> 
             (* already found an example in minus... what if there's 
                a more precise one? Find least imprecise *)
             (match Lv.sameLval alv otherLv with
                None -> (worstImprec, crumbs)
              | (Some newImp as b) -> 
                  if Lv.compare_imprec oldImp newImp > 0 
                  then
                    (b, otherCrumbs)
                  else (worstImprec, crumbs)
             )
      ) (stMan#getMinus rns) (None, Rns.emptyCrumbs) in
    match imprec with
      None -> (NotShown, crumbs)
    | Some i -> (Shown i, crumbs)


  (** Substituting the formalLv w/ the actuals does not result in
      an lval. See if it is a NULL expression or the address of an lval *)
  method checkArgsNull args formalLv =
    let unexpected arg =
      L.logErrorF "isArgNull: unexpected expression: %s\n" (string_of_exp arg);
      true      
    in
   let rec isArgNull arg = 
      match arg with
        Const _
      | SizeOf _
      | SizeOfStr _
      | SizeOfE _ 
      | AlignOf _
      | AlignOfE _-> true
      | AddrOf _
      | StartOf _ -> false
      | CastE (_, e) -> isArgNull e
      | BinOp (op, e1, _, _) ->
          (match op with
             PlusPI
           | IndexPI
           | MinusPI -> isArgNull e1
           | _ -> unexpected arg
          )
      | _ ->
          unexpected arg
    in
    (match Lv.getScope formalLv with
        Scope.SFormal n -> 
          let origArg = List.nth args n in
          isArgNull origArg
     | _ -> 
         L.logError "checkArgsNull: arg substitution failed for non-formal";
         true
    )

  (** Base check (after aliasing is accounted) *)
  method baseCheckNull rns loc origLv exp chain =
    let warnNullExp () =
      let imprec, crumbs = self#isInMinus rns origLv in
      self#generateWarning loc origLv chain crumbs imprec
    in
    let unexpected arg =
      L.logError ("isArgNull: unexpected expression: " ^ 
                    (Lv.string_of_exp arg));
      warnNullExp ()
    in
    match exp with
      Lv.CLval alv -> begin
        if self#isInPlus rns alv then
          self#confirmDereference loc alv chain
(*
        else match self#isInMinus rns alv with
          Some imp ->
            let crumbs = Rns.RNS.S.find alv (stMan#getMinus rns) in
            self#generateWarning loc alv chain crumbs (Shown imp)
        | None ->
*)
        else
            self#delayCheck loc alv chain
      end
    | Lv.CConst _
    | Lv.CSizeOf _
    | Lv.CSizeOfStr _
    | Lv.CSizeOfE _ 
    | Lv.CAlignOf _
    | Lv.CAlignOfE _ -> warnNullExp ()
    | Lv.CAddrOf _
    | Lv.CStartOf _ -> self#confirmDereference loc origLv chain
    | Lv.CCastE (_, e) -> self#baseCheckNull rns loc origLv e chain
    | Lv.CBinOp (op, e1, _, _) ->
        (match op with
           PlusPI
         | IndexPI
         | MinusPI -> self#baseCheckNull rns loc origLv e1 chain
         | _ -> unexpected exp
        )
    | _ -> unexpected exp

        

  method analyzeALval pp rns loc alv chain =
    (* First check alv in the N+/N-, then try its aliases *)
    if self#isInPlus rns alv then
      self#confirmDereference loc alv chain
    else 
      let imp, crumbs = self#isInMinus rns alv in
      if (Rns.BreadCrumbs.is_empty crumbs) then (* not caused by adjust? *)
        (* Check aliases and try to delay those instead *)
        let mustAlias, aliases = SPTA.getAliasesAt pp alv in
        (match SPTA.NULL.mayBeNull aliases with
           SPTA.NULL.Null ->
             self#generateWarning loc alv chain crumbs imp             
         | SPTA.NULL.RepNode -> 
             (*
               List.iter 
               (fun e -> 
               if SPTA.expMayBeNullBool e then
               L.logError ("target may be null: " ^ 
               (Lv.string_of_exp e));
               ) aliases;
             *)
             self#generateWarning loc alv chain crumbs Blob
         | SPTA.NULL.NotNull ->
             List.iter (fun exp -> 
                          self#baseCheckNull rns loc alv exp chain) aliases
        )
      else (* caused by adjust, cut off now! *)
        self#generateWarning loc alv chain crumbs imp


  (** Given that exp was deref'ed, check if is based on a null pointer *)
  method analyzeDeref pp rns loc exp chain =
    let warn msg =
      L.logError (msg ^ ": " ^ (string_of_exp exp))
    in
    match exp with
      Lval lv ->
        let alv = Lv.abs_of_lval lv in
        self#analyzeALval pp rns loc alv chain
    | BinOp (op, e1, e2, _) ->
        (match op with 
           PlusPI
         | IndexPI
         | MinusPI ->
             self#analyzeDeref pp rns loc e1 chain
         | PlusA
         | MinusA ->
             warn "deref of an arith expression, checking left operand";
             self#analyzeDeref pp rns loc e1 chain
         | _ ->
             warn "deref of odd binop"
        )
    | CastE (_, e) ->
        self#analyzeDeref pp rns loc e chain
    | _ -> 
        warn "deref of odd exp"

  method analyzeLval pp rns loc (lhost, _) chain =
    match lhost with
      Var _ -> ()
    | Mem (exp) ->
        self#analyzeDeref pp rns loc exp chain;
        self#analyzeExp pp rns loc exp chain (* search for more derefs *)

  method analyzeExp pp rns loc exp chain =
    match exp with
      Lval lv -> self#analyzeLval pp rns loc lv chain
    | BinOp(_,lhs,rhs,_) ->
        self#analyzeExp pp rns loc lhs chain;
        self#analyzeExp pp rns loc rhs chain
    | UnOp(unop,e,_) ->
        self#analyzeExp pp rns loc e chain
    | CastE(_,e) -> 
        self#analyzeExp pp rns loc e chain
    | AddrOf(Mem(e), _) ->
        self#analyzeExp pp rns loc e chain
    | _ -> ()


  (********* Handle delayed checks **********)


  method handleCallFkey pp args loc rns fk =
    let sum = !sums#find fk in
    if isBottomSum sum then
      L.logErrorF "delayed null deref sum for: %s is bottom?\n" 
        (fid_to_string fk)
    else
      WarnR.KH.iter
        (fun origLoc data ->
           (* Translate each of the lvals and check *)
           List.iter 
             (fun w ->
                let updatedChain = w.nullChain @ [curFkey] in
                let targs = SPTA.substActForm3 pp args w.nullLval in
                if targs = [] then begin
                  (* The result of the substitution isn't an lval, 
                     find out what kind of expression it is *)
                  if self#checkArgsNull args w.nullLval then
                    self#generateWarning origLoc w.nullLval updatedChain 
                      Rns.emptyCrumbs NotShown
                  else 
                    self#confirmDereference origLoc w.nullLval updatedChain
                end
                else
                  List.iter 
                    (fun subbed ->
                       self#analyzeALval pp rns origLoc subbed updatedChain
                         (* Automatically chains up further delays *)
                    ) targs
             ) data
        ) sum.nDelayed

  method handleCallExp pp callexp args loc rns =
    (* Get summary of un-proven null-derefs from caller *)
    let called = callTargsAtPP curCG curFkey pp in
    List.iter (self#handleCallFkey pp args loc rns) called


  (* TODO: make a more general filtering mechanism *)
  method scopeCluster fdec loc data =
    List.filter 
      (fun w -> 
         match Lv.getScopeParanoid fdec w.nullLval with
           Scope.SGlobal
         | Scope.SFormal _ -> true
         | _ -> 
             (* local -- caller won't be able to prove, so prune + warn *)
             L.logStatus ("Pruned from delayed deref check: " ^ 
                            (Lv.string_of_lval w.nullLval));
             self#generateWarning loc w.nullLval w.nullChain 
               Rns.emptyCrumbs NotShown;
             false
      ) data

      
  method scopeWarnData fdec data =
    (* Scope and prune locals (but not formals that are just the var)... *)
    let result = WarnR.KH.copy data in
    WarnR.KH.iter 
      (fun loc data ->
         let scopedData = self#scopeCluster fdec loc data in
         if scopedData = [] then WarnR.KH.remove result loc
         else WarnR.KH.replace result loc scopedData
      ) data;
    result
      (* TODO: make more efficient... all this copying... *)

  method summarize fkey fdec =
    (* Will delayed checks need fix-pointing??? *)
    let newDelayed = self#scopeWarnData fdec (delayedChecks#getData) in
    let newSafe = localSafe#getData in
    let newUnsafe = localUnsafe#getData in
    let newSumm = { nSafe = newSafe;
                    nUnsafe = newUnsafe;
                    nDelayed = newDelayed; } in
    !sums#addReplace fkey newSumm


  (********* Main visitor ***********)

  method vinst instr = 
    self#setInstrPP instr;
    let pp = getCurrentPP () in
    (try match getRns pp with
       None -> ()
     | Some rns ->
         match instr with
           Set (lval, exp, loc) ->
             self#analyzeLval pp rns loc lval [curFkey];
             self#analyzeExp pp rns loc exp [curFkey]
         | Call (lvalopt, callexp, args, loc) -> begin
             (match lvalopt with
                Some lval -> self#analyzeLval pp rns loc lval [curFkey]
              | None -> ());
             List.iter
               (fun exp -> self#analyzeExp pp rns loc exp [curFkey]) 
               args;
             self#analyzeExp pp rns loc callexp [curFkey];
             self#handleCallExp pp callexp args loc rns
           end
         | _ -> ()
     with Not_found ->
       L.logStatusF "vinst: rns data at pp not found: %s\n" (string_of_pp pp)
    );
    self#bumpInstr 1;
    DoChildren

      
  method vstmt stmt =
    self#setStmtPP stmt;
    let pp = getCurrentPP () in
    (try match getRns pp with 
       None -> ()
     | Some rns -> begin
         match stmt.skind with
           Instr il ->
            () (* visitor will visit instrs *)
         | Return (Some exp, loc) ->
             self#analyzeExp pp rns loc exp [curFkey]
         | If (exp, b1, b2, loc) ->
             self#analyzeExp pp rns loc exp [curFkey]
         | Switch (exp, b, s, loc) ->
             self#analyzeExp pp rns loc exp [curFkey]
         | _ -> ()
       end
     with Not_found ->
       L.logStatusF "vstmt: rns data at pp not found: %s\n" (string_of_pp pp)
    );
    DoChildren

end




(** Package up the null warnings analysis *)
class nwAnalysis rnsSkipped rnsLattice getRnsData safeCount unsafeCount = 
object (self)
  (* TODO: use something else for counting? *)
  
  method setInspect (_:bool) = ()
      
  method isFinal key : bool =
    rnsSkipped key

  val mutable checker = new derefAnalyzer rnsLattice getRnsData dummyFID emptyCG

  val mutable curCG = emptyCG
  method setCG cg = curCG <- cg

  method compute funID (cfg:fundec) =
    (* re-initialize *)
    checker <- new derefAnalyzer rnsLattice getRnsData funID curCG;
    ignore (Cil.visitCilFunction (checker :> cilVisitor) cfg);

  method summarize key (cfg : fundec) =
    if self#isFinal key then
      !sums#addReplace key (emptySumm ())
    else 
      checker#summarize key cfg;
    let numDerefsSafe = checker#getNumSafe in
    let numDerefsWarn = checker#getNumWarn in
    let numDelayed = checker#getNumDelayed in
    let confirmations = checker#getConfirmations in
    let warnings = checker#getWarnings in
    L.logStatus "Null Pointer Warnings";
    L.logStatusF 
      "derefs for function (%s) - safe: %d, unsafe: %d, delayed %d\n" 
      (cfg.svar.vname) numDerefsSafe numDerefsWarn numDelayed;
    L.logStatus "Safe Pointer Derefs:";
    List.iter (fun s -> L.logStatus s) confirmations;
    L.logStatus "Possibly Null Pointer Derefs:";
    List.iter (fun s -> L.logStatus s) warnings;
    safeCount := !safeCount + numDerefsSafe;
    unsafeCount := !unsafeCount + numDerefsWarn;
    false

  method flushSummaries () =
    !sums#serializeAndFlush

end


(*********** Print delayed checks left-over at roots ***********)

let printDanglingDerefChecks cg cgDir =
  let rooter = new Entry_points.rootGetter cg cgDir in
  let roots = rooter#getUntaggedRoots () in
  List.iter 
    (fun (sumKey, fn) ->
       let sum = !sums#find sumKey in
       if not (isBottomSum sum) then begin
         let doc = pReport sum.nDelayed in
         if doc = Pretty.nil then ()
         else begin
           L.logStatus ("Dangling dereference checks for: " ^ fn.name);
           L.logStatusD (indent 2 doc);
           L.logStatus "\n"
         end
       end;
       !sums#evictOne sumKey
    ) roots
    

(****************** Other printing functions ********************)

let printDerefReport caption safe unsafe =
  let total = safe + unsafe in
  let safePct = float_of_int safe /. float_of_int total in
  let unsafePct = float_of_int unsafe /. float_of_int total in
  L.logStatus ("Deref Report -- " ^ caption);
  L.logStatusF "%d\n%d\n%d\n%f\n%f\n" safe unsafe total safePct unsafePct
 
let printDerefReportF caption safe unsafe =
  let total = safe +. unsafe in
  let safePct = safe /. total in
  let unsafePct = unsafe /. total in
  L.logStatus ("Deref Report -- " ^ caption);
  L.logStatusF "%f\n%f\n%f\n%f\n%f\n" safe unsafe total safePct unsafePct
 
