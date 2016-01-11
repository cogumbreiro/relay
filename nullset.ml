
(** Nullness analysis for Radar *)

open Cil
open Logging
open Callg
open Fstructs
open IntraDataflow
open Radar
open Pretty
open Cildump
open Sym_types
(*open Radar_rel
open Radar_anal*)

module DF = Dataflow
module LocSet = Lockset_partitioner.LocSet
module LvalHash = Lvals.LvalHash
module Stat = Mystats

(***************************************************************************)
(* CREATE RADAR RELATIVE TYPES                                             *)
(***************************************************************************)

module VT = struct
  type value = unit
  let compareV a b = Some 0
  let combineV a b = ()
  let equalV a b = true
end

module P1 = struct
  module VT = VT
  let name = "nullset analysis"
  let summaryExt = "nul"
end

module S = Radar_rel.Radar_RMS_1 (P1)

(***************************************************************************)
(* RELATIVE NULLSET DATAFLOW                                               *)
(***************************************************************************)

type relChangeFromAssign = APlus | AMinus | ADefault

let combine a b =  match a, b with
  | (ADefault, c1), (ADefault, c2) -> ADefault, Crumbs.empty
  | (ADefault, c1), (AMinus,   c2) -> AMinus,   c2
  | (ADefault, c1), (APlus,    c2) -> ADefault, Crumbs.empty
  | (APlus,    c1), (ADefault, c2) -> ADefault, Crumbs.empty
  | (APlus,    c1), (AMinus,   c2) -> AMinus,   c2
  | (APlus,    c1), (APlus,    c2) -> APlus,    Crumbs.empty
  | (AMinus,   c1), (ADefault, c2) -> AMinus,   c1
  | (AMinus,   c1), (AMinus,   c2) -> AMinus,   Crumbs.union c1 c2
  | (AMinus,   c1), (APlus,    c2) -> AMinus,   c1


class nullTransF stMan = object(self)
  inherit [S.st, S.relSt, S.part, S.info] 
    Radar_rel.rmsSeqTransFunc stMan as super

  method private effectOfAssign exp inState = 
    try
      let rs = stMan#projRel inState in
      match exp with
        | AddrOf (Var v, _)
        | StartOf (Var v, _) ->
            APlus, Crumbs.empty

        | BinOp (PlusPI, ptr, _, _)
        | BinOp (MinusPI, ptr, _, _)
        | BinOp (IndexPI, ptr, _, _) ->
            self#effectOfAssign ptr inState

        | Lval ((_, _) as rhs) ->
            let alv = Lvals.abs_of_lval rhs in
            if stMan#mem alv (stMan#getPlus rs) then
              APlus, Crumbs.empty
            else if stMan#mem alv (stMan#getMinus rs) then
              AMinus, projCrumb (stMan#find alv (stMan#getMinus rs))
            else (* mark as modded *)
              AMinus, Crumbs.empty

        | _ ->
            AMinus, Crumbs.empty


    with Radar_rel.IllegalProject -> ADefault, Crumbs.empty

  method handleGuard gexp inState =
    let foldedExp = Cil.constFold true gexp in
    let castFreeGuard = Cil_lvals.omitCast foldedExp in
    let rec checkExp gexp inState =
      try
        match gexp with
        | BinOp (Ne, 
                 (Lval ((_, _) as lval)),
                 (Const (_) as con), _)
        | BinOp (Ne,
                 (Const (_) as con),
                 (Lval ((_, _) as lval)), _) ->
            if isZero con then DF.GUse (self#wrapDoPlus inState lval ())
            else DF.GDefault
              
        | Lval ((_, _) as lval) ->
            DF.GUse (self#wrapDoPlus inState lval ())

        (* handle !'s in succession *)
        | UnOp (LNot, UnOp (LNot, exp, _), _) ->
            checkExp exp inState

        (* handle negations of Ne and Eq *)
        | UnOp (LNot, BinOp (Ne, e1, e2, typ), _) ->
            checkExp (BinOp (Eq, e1, e2, typ)) inState

        | UnOp (LNot, BinOp (Eq, e1, e2, typ), _) ->
            checkExp (BinOp (Ne, e1, e2, typ)) inState

        | _ ->
            DF.GDefault

      with (* inState is None *)
        Radar_rel.IllegalProject -> DF.GUnreachable
    in
    checkExp castFreeGuard inState

    method handleAssign lv exp loc st =
      try
        let _ = stMan#projRel st in
          match self#effectOfAssign exp st with
            | APlus, _    -> DF.Done (self#wrapDoPlus st lv ())
            | AMinus, cr  -> DF.Done (self#wrapDoMinus st lv () ~cr:cr)
            | ADefault, _ -> DF.Default
      with Radar_rel.IllegalProject -> DF.Default

    (* TODO: do substitution on the return value? 
       Ideally have symex do this... *)
    method handleCallRet lv funs callexp actuals loc inState =
      let effectOfFun f = begin
        match Symsummary.sum#find f with
          
          | Vbot -> AMinus, Crumbs.empty
              
          | Vmustptr (symaddr, offset) -> begin
              let alv = (symaddr.saHost, offset) in
              match Lvals.lvals_of_abs alv with
                | [x] -> self#effectOfAssign (Cil.mkAddrOf x) inState
                | _   -> ADefault, Crumbs.empty
            end

          | Vmayptr (_, syms)
          | Vextptr (_, syms) -> begin
              (* collect aLvals *)
              let alvs = AddrOffSet.fold
                           (fun (symaddr, offset) l ->
                              (symaddr.saHost, offset) :: l
                           ) syms [] in
              (* convert aLvals to Lvals *)
              let lvs = List.fold_left
                          (fun l alv ->
                             match Lvals.lvals_of_abs alv with
                               | [rhs] -> rhs :: l
                               | _     -> l
                          ) [] alvs in
              (* compute effect of the union of these may Lvals *)
              List.fold_left
                (fun a x ->
                   combine a (self#effectOfAssign (Cil.mkAddrOf x) inState)
                ) (APlus, Crumbs.empty) lvs
            end

          | Vval _ -> AMinus, Crumbs.empty

          | _ -> ADefault, Crumbs.empty
            
      end (* end effectOfFun *) in

      if funs = [] then inState
      else
        let overallEffect = List.fold_left
                              (fun a b -> combine a (effectOfFun b))
                              (APlus, Crumbs.empty) 
                              funs
        in
        match overallEffect with
          | ADefault, _ -> inState
          | APlus, _    -> self#wrapDoPlus inState lv ()
          | AMinus, cr  -> self#wrapDoMinus inState lv () ~cr:cr

    method handleCallExp funs callexp actuals loc inState =
      Stat.time "handleCall TF" 
        (super#handleCallExp funs callexp actuals loc) inState

end




module P2 = struct
  type st = S.st
  class seq = nullTransF S.relStMan
end

module RadarDFs = Radar_rel.Radar_RMS_2 (S) (P2)


(***************************************************************************)
(* ANALYZE DEREFS VISITOR PASS                                             *)
(***************************************************************************)

 
  (* Taken from null_warnings.ml *)


(************************************************************
  Collection of warnings
************************************************************)

type imprec = 
    Shown of Lvals.imprecision
  | NotShown (* warning generated from a deref check 
                that never matched w/ N+ or N- *)
  | Blob

type nullChain = funID list

type nullData =
    { nullLval : Lvals.aLval;
      nullChain : nullChain;
      nullCrumbs : Crumbs.t;
      nullImprec : imprec; }      


(** Expected interface for tracking warnings seen so far *)
module NullW = struct

  type key = location

  type data = nullData
    
  let equalKey k1 k2 = 
    compareLoc k1 k2 == 0
    
  let hashKey = Hashtbl.hash

  let hashConsKey = Cil_lvals.distillLoc

  let chainSubsumed ch1 ch2 =
    (* try subsuming warnings if the begin and end with the same function *)
(*     a.nullChain = b.nullChain  *)
    let rec endSame l1 l2 =
      match l1, l2 with
        [e1], [e2] -> e1 = e2
      | [e1], _ :: t2 -> endSame l1 t2
      | _ :: t1, [e2] -> endSame t1 l2
      | _ :: t1, _ :: t2 -> endSame t1 t2
      | [], [] -> true
      | [], _ | _, [] -> false
    in
    match ch1, ch2 with
      h1 :: t1, h2 :: t2 -> h1 = h2 && endSame t1 t2
    | [], [] -> true
    | [], _ | _, [] -> false

        
  let equalData a b =
    Lvals.compare_lval a.nullLval b.nullLval == 0 &&
      chainSubsumed a.nullChain b.nullChain && 
      Crumbs.equal a.nullCrumbs b.nullCrumbs &&
      a.nullImprec = b.nullImprec
    
  let hashConsData a =
    { a with 
        nullLval = Lvals.mergeLv a.nullLval; }
      
  let pDataXML d =
    Pretty.nil

end

module WarnR = Warn_reports.MakeRep (NullW)


(* dummy data to be used in safe dereference "warning" report *)
let dummyNullData alv =
    { nullLval = alv;
      nullChain = [];
      nullCrumbs = Crumbs.empty;
      nullImprec = NotShown; }


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
  WarnR.KH.add me locUnknown [];
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

(*let _ = logStatus "Waiting to set null warning summary\n"*)

let setSums settings cg sccCG sumToUse =
  sums := sumToUse;
  let _ = Backed_summary.registerType !sums in
  !sums#cleanup ();
  !sums#initSummaries settings cg sccCG;
  logStatus "Null warning summary set!\n"

let prepareNullSums cg =
  logStatus ("Preparing NWARN summaries just in case\n");
  flushStatus ();
  let toPrep = FMap.fold
    (fun k n cur ->
       if n.hasBody then k :: cur
       else cur
    ) cg [] in
  try
    logStatus ("rkcdebug: prepareNullSums prepare "
                ^ Backed_summary.string_of_sumType !sums#sumTyp);
    Manage_sums.prepareSumms toPrep (Backed_summary.getDescriptors 
                                       [!sums#sumTyp])
  with Request.SummariesNotFound ->
    logError "prepareNullSums: some summaries not found, but maybe unneeded"
    

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


  
(** Null-pointer dereference warning analyzer 
    (make a new one each pass to clear state) *)
class dereferenceAnalyzer
  (stMan : (S.st, S.relSt, S.part, Lvals.aLval, S.info, S.st) relativeState)
  (getRns : prog_point -> S.st)
  (curFkey : funID) 
  (curCG : callG) (debug:bool) =
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
    if debug then
      if compareLoc loc !currentLoc == 0 then
        Printf.sprintf "%s %s %s"  (Cildump.string_of_loc loc)
          caption (Lvals.string_of_lval alv)
      else 
        Printf.sprintf "%s <- %s %s %s"
          (Cildump.string_of_loc loc) (Cildump.string_of_loc !currentLoc)
          caption (Lvals.string_of_lval alv)
    else ""

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
              nullCrumbs = Crumbs.empty;
              nullImprec = NotShown; (* make a dummy imprec thing for now *)
            } in
    match delayedChecks#addWarning loc w with
      None | Some WarnR.OldCluster ->
        numDelayed <- numDelayed + 1;
    | Some WarnR.MaxWarnings (* hmm... hit max? *)
    | Some WarnR.DupeWarning -> () 

  method isInPlus rns alv : bool =
    stMan#mem alv (stMan#getPlus rns)

  method isInMinus rns alv : (imprec * Crumbs.t) =
    let imprec, crumbs = stMan#fold_rel 
      (fun otherLv (_, otherCrumbs) (prevImprec, prevCrumbs) ->
         (match Lvals.sameLval alv otherLv with
            None -> (prevImprec, prevCrumbs)
          | (Some newImp as b) -> begin 
              (* see if we already found an example in minus... 
                 Find least imprecise *)
              match prevImprec with 
                None -> (b, otherCrumbs)
              | Some oldImp -> 
                  if Lvals.compare_imprec oldImp newImp > 0 
                  then (b, otherCrumbs)
                  else (prevImprec, prevCrumbs)
            end
         )
      ) (stMan#getMinus rns) (None, Crumbs.empty) in
    match imprec with
      None -> (NotShown, crumbs)
    | Some i -> (Shown i, crumbs)


  (** Substituting the formalLv w/ the actuals does not result in
      an lval. See if it is a NULL expression or the address of an lval *)
  method checkArgsNull args formalLv =
    let unexpected arg =
      logError ("checkArgsNull: unexpected expression: " ^ 
                    (Cildump.string_of_exp arg));
      true      
    in
   let rec isArgNull arg = 
      match arg with
        Const _ | SizeOf _ | SizeOfStr _ | SizeOfE _ 
      | AlignOf _ | AlignOfE _-> true
      | AddrOf _ | StartOf _ -> false
      | CastE (_, e) -> isArgNull e
      | BinOp (op, e1, _, _) ->
          (match op with
             PlusPI
           | IndexPI
           | MinusPI -> isArgNull e1
           | _ -> unexpected arg )
      | UnOp _ -> 
          unexpected arg
      | Lval _  ->
          unexpected arg (* why did substitution fail? *)
    in
    (match Lvals.getScope formalLv with
        Scope.SFormal n -> 
          let origArg = List.nth args n in
          isArgNull origArg
     | _ -> 
         logError "checkArgsNull: arg substitution failed for non-formal";
         true
    )

  (** Base check (after aliasing is accounted) *)
  method baseCheckNull rns loc origLv exp chain =
    let warnNullExp () =
      let imprec, crumbs = self#isInMinus rns origLv in
      self#generateWarning loc origLv chain crumbs imprec
    in
    let unexpected arg =
      logError ("baseCheckNull: unexpected expression: " ^ 
                    (Lvals.string_of_exp arg));
      warnNullExp ()
    in
    match exp with
      Lvals.CLval alv -> begin
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
    | Lvals.CConst _ | Lvals.CSizeOf _ | Lvals.CSizeOfStr _
    | Lvals.CSizeOfE _ | Lvals.CAlignOf _ | Lvals.CAlignOfE _ -> warnNullExp ()
    | Lvals.CAddrOf _
    | Lvals.CStartOf _ -> self#confirmDereference loc origLv chain
    | Lvals.CCastE (_, e) -> self#baseCheckNull rns loc origLv e chain
    | Lvals.CBinOp (op, e1, _, _) ->
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
      if (Crumbs.is_empty crumbs) then (* not caused by adjust? *)
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
               logError ("target may be null: " ^ 
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
      logError (msg ^ ": " ^ (Cildump.string_of_exp exp))
    in
    match exp with
      Lval lv ->
        let alv = Lvals.abs_of_lval lv in
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
    | AddrOf (lv2) -> self#analyzeLval pp rns loc lv2 chain
    | StartOf (lv) -> 
        let alv = Lvals.abs_of_lval lv in
        self#confirmDereference loc alv chain (* array should be non-null?*)
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


  method handleCallFkey pp args loc rns key =
    let sum = !sums#find key in
    if isBottomSum sum then
      logErrorF "delayed null sum for: %s is bottom?\n" (fid_to_string key)
    else
      WarnR.KH.iter
        (fun origLoc data ->
           (* Translate each of the lvals and check *)
           List.iter 
             (fun w ->
                let updatedChain = w.nullChain @ [curFkey] in
                let targs = 
                  Stat.time "substLval" 
                    (SPTA.substActForm3 pp args) w.nullLval in
                if targs = [] then begin
                  (* The result of the substitution isn't an lval, 
                     find out what kind of expression it is *)
                  if self#checkArgsNull args w.nullLval then
                    self#generateWarning origLoc w.nullLval updatedChain 
                      Crumbs.empty NotShown
                  else 
                    self#confirmDereference origLoc w.nullLval updatedChain
                end
                else
                  List.iter 
                    (fun subbed ->
                       Stat.time "analyzeALval" 
                         (self#analyzeALval pp rns origLoc subbed) updatedChain
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
         match Lvals.getScopeParanoid fdec w.nullLval with
           Scope.SGlobal
         | Scope.SFormal _ -> true
         | _ -> 
             (* local -- caller won't be able to prove, so prune + warn *)
             logStatus ("Pruned from delayed deref check: " ^ 
                            (Lvals.string_of_lval w.nullLval));
             self#generateWarning loc w.nullLval w.nullChain 
               Crumbs.empty NotShown;
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
    let newDelayed = Stat.time "scopeDelay" 
      (self#scopeWarnData fdec) (delayedChecks#getData) in
    let newSafe = localSafe#getData in
    let newUnsafe = localUnsafe#getData in
    let newSumm = { nSafe = newSafe;
                    nUnsafe = newUnsafe;
                    nDelayed = newDelayed; } in
    !sums#addReplace fkey newSumm;
    !sums#serializeAndFlush


  (********* Main visitor ***********)

  method vinst instr = 
    self#setInstrPP instr;
    let pp = getCurrentPP () in
    begin
      try let state = getRns pp in
      try let rns = stMan#projRel state in
        match instr with
          Set (lval, exp, loc) ->
            self#analyzeLval pp rns loc lval [curFkey];
            self#analyzeExp pp rns loc exp [curFkey]
        | Call (lvalopt, callexp, explist, loc) -> begin
            (match lvalopt with
               Some lval -> self#analyzeLval pp rns loc lval [curFkey]
             | None -> ());
            List.iter
              (fun exp -> self#analyzeExp pp rns loc exp [curFkey]
              ) explist;
            self#analyzeExp pp rns loc callexp [curFkey];            
            Stat.time "delayCalls"
              (self#handleCallExp pp callexp explist loc) rns
          end
        | _ -> ()
            
      with Radar_rel.IllegalProject -> ()

      with Not_found -> logStatus ("vinst: rns data at pp not found: "
                                       ^ (Cildump.string_of_pp pp))
    end;
    self#bumpInstr 1;
    DoChildren

      
  method vstmt stmt =
    self#setStmtPP stmt;
    let pp = getCurrentPP () in
    begin
      try let state = getRns pp in
      try let rns = stMan#projRel state in
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

      with Radar_rel.IllegalProject -> ()

      with Not_found -> logStatus ("vstmt: rns data at pp not found: "
                                       ^ (Cildump.string_of_pp pp))
    end;
    DoChildren

end

(************************************************************)

(* Printing stuff *)

let getFname fid =
(*  try *)
    let fk = fid_to_fkey fid in
    let varinfo = Cilinfos.getVarinfo fk in
    varinfo.vname
(*  with Cilinfos.NoVarinfo _ -> "?" *)

let string_of_chain chain =
  "<" ^ List.fold_left (fun s i ->
          s ^ getFname i ^ ":" ^ fid_to_string i ^ " ") " " chain ^ ">" 

let string_of_crumb (cr:crumb) =
  match cr with
    | Some pakh -> Pseudo_access.string_of_pakeyhead pakh
    | None      -> "?"

let string_of_crumbs crumbs =
  if Crumbs.is_empty crumbs then "empty"
  else Crumbs.fold (fun c s -> s ^ string_of_crumb c ^ " ") crumbs ""

let string_of_imprec imp =
  match imp with
    NotShown -> "Not in N+/N-"
  | Shown i -> Lvals.string_of_imp i
  | Blob -> "Blob"


  
(* PRINT DIFF STUFF *)

let pOneWarn w : doc =
  text (Lvals.string_of_lvscope w.nullLval) ++ 
    text (string_of_chain w.nullChain) ++
    text (" [" ^ string_of_imprec w.nullImprec ^ "]")
    
let pCluster loc data : doc =
  let locDoc = text "null deref(s) at: " ++ Cil.d_loc () loc in
  locDoc ++ text " of [" ++
    seq_to_doc (text ", ") List.iter pOneWarn data Pretty.nil ++ 
    text "]"

let pReport rep =
  map_to_doc 
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
  logStatusF "Warnings from: %s\n" datFile;
  logStatus "==============================";
  let doc = pReport (rep#getData) in
  logStatusD doc;
  let size = WarnR.KH.length (rep#getData) in
  logStatusF "\nTotal: %d\n\n" size

let printDiffs datFile1 datFile2 : unit =
  let rep1 = WarnR.deserialize datFile1 in
  let rep2 = WarnR.deserialize datFile2 in
(*DEBUG*)
  logStatus "PRINTING NULL diffs";
  printWarnDat datFile1 rep1;
  printWarnDat datFile2 rep2;
(*/DEBUG*)
  let diff = rep1#diffReport rep2 in
  let diffDoc = map_to_doc
    Pretty.line 
    WarnR.KH.iter
    pDiff
    diff
    Pretty.nil in
  let diffDoc = diffDoc ++ line in
  logStatusF "\nDelta:%d\n" (WarnR.KH.length diff); 
  logStatusD (indent 2 diffDoc);
  logStatus "\n"


(* PRINT DELTA SUMMARY STUFF *)

module ChainHash = Hashtbl.Make(
  struct
    type t = nullChain
    let equal a b = a = b
    let hash = Hashtbl.hash
  end
)


let printDeltaReport datFile1 datFile2 = begin
  let rep1 = WarnR.deserialize datFile1 in
  let rep2 = WarnR.deserialize datFile2 in
  let diff = rep1#diffReport rep2 in

  (* the summary report: fkey -> (lv -> (chain -> (crumbs, locs, imprec))) *)
  let ht = FH.create 10 in

  let allCrumbs = ref Crumbs.empty in
  let countTotal, countLvals, countFuncs, countPAs =
    ref 0, ref 0, ref 0, ref 0 in
  let clusFirst, clusSecond, clusBoth = ref 0, ref 0, ref 0 in
  let crumbsGood, crumbsBad = ref 0, ref 0 in

  let cat d x = d := !d ++ (text x) ++ line in

  (* updates summary report with a warning *)
  let add fk loc lv chain crumbs imp =
    incr countTotal;
    allCrumbs := Crumbs.union crumbs !allCrumbs;

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
              let newCrumbs = Crumbs.union crumbs oldCrumbs in
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

  let makeDocs () =
    let docs =
    FH.fold 
      (fun fk lvTable docs ->
       let doc = ref nil in
       cat doc ("Function " ^ fid_to_string fk ^ " " ^ getFname fk ^ "\n");
    LvalHash.iter
      (fun lv chainTable ->
       cat doc ("     LVAL: " ^ Lvals.string_of_lvscope lv);
              
    ChainHash.iter 
      (fun chain (crumbs, locs, imprec) ->
                                
       cat doc ("    chain: " ^ string_of_chain chain);
       cat doc ("   crumbs: " ^ string_of_crumbs crumbs);
       cat doc ("   imprec: " ^ string_of_imprec imprec);
       cat doc ("     locs:");
    LocSet.iter
      (fun loc -> 
       cat doc ("           " ^ string_of_loc loc)
      ) locs;
       cat doc "";
    ) chainTable;
    ) lvTable;
      !doc :: docs
    ) ht [] in

    docs in

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
  Crumbs.iter (function
                 | Some _ -> incr crumbsGood
                 | None   -> incr crumbsBad) !allCrumbs;

  (* now that reports have been accumulated, create documents for them
     and print *)
  let docs = makeDocs () in

  logStatusF
    "Delta Report -- %d lvals at %d locations in %d functions\n"
    !countLvals !countTotal !countFuncs;
  logStatusF
    "   %d OnlyFirst, %d OnlySecond, %d BothDifferent\n"
    !clusFirst !clusSecond !clusBoth;
  logStatusF "   %d crumbs, %d good %d bad\n\n"
    (Crumbs.cardinal !allCrumbs) !crumbsGood !crumbsBad;
  List.iter (fun doc -> logStatusD (indent 2 doc)) docs;
  logStatus "\n";
end (* printDeltaReport *)


class nullWarningAnalysis stMan getRns debug =
object(self)
  
    
  val mutable checker = new dereferenceAnalyzer 
    stMan getRns dummyFID emptyCG debug

  val mutable curCG = emptyCG
  method setCG cg = curCG <- cg

  method setInspect (_:bool) =  ()

  method isFinal key = false

  method compute funID (cfg:fundec) = 
    (* re-initialize *)
    checker <- new dereferenceAnalyzer 
      stMan getRns funID curCG debug;
    Stat.time "derefCheck" ignore (Cil.visitCilFunction (checker :> cilVisitor) cfg)

  method private printSafeUnsafe funname = begin
    let numDerefsSafe = checker#getNumSafe in
    let numDerefsWarn = checker#getNumWarn in
    let numDelayed = checker#getNumDelayed in
    logStatus "Null Pointer Warnings";
    logStatusF 
      "derefs for (%s) - safe: %d, unsafe: %d, delayed %d\n" 
      funname numDerefsSafe numDerefsWarn numDelayed;
    if debug then begin
      (* DEBUG *)
      let confirmations = checker#getConfirmations in
      logStatus "Safe Pointer Derefs:";
      List.iter (fun s -> logStatus s) confirmations;
      let warnings = checker#getWarnings in
      logStatus "Possibly Null Pointer Derefs:";
      List.iter (fun s -> logStatus s) warnings
    end
   end
    

  method summarize key (cfg:fundec) = 
    if self#isFinal key then !sums#addReplace key (emptySumm ())
    else checker#summarize key cfg;
    self#printSafeUnsafe cfg.svar.vname;
    false

  method flushSummaries () =
    !sums#serializeAndFlush

end



(***************************************************************************)
(* KNOWLEDGE VISITOR PASS                                                  *)
(***************************************************************************)


class nullKnowVisitor
  (stMan : (S.st, S.relSt, S.part, Lvals.aLval, S.info, S.st) relativeState)
  (getRns : prog_point -> S.st)
  (caption : string) =
object(self)
  inherit Knowledge_pass.knowledgeVisitor caption
    
  method incrementKnowledge () =
    let pp = getCurrentPP () in
    let data = getRns pp in
    try
      let rs = stMan#projRel data in
      let nplus = stMan#getPlus rs in
      let count = stMan#cardinal nplus in
      knowledge <- knowledge + count
    with
      Radar_rel.IllegalProject -> ()

end



(***************************************************************************)
(* GET EVERYTHING PACKAGED UP                                              *)
(***************************************************************************)

let noSkip (_:Callg.funID) = false

let mkAna kvis =
  new Knowledge_pass.knowledgeAnalysis noSkip kvis

let stM = S.relStMan
let getSeq = RadarDFs.getSeqDataBefore
let getPess = RadarDFs.getPessDataBefore
let getAdjL = RadarDFs.getAdjLDataBefore
let getAdjNL = RadarDFs.getAdjNLDataBefore

module N = struct

  let debugNW = false

  let nwSeq = new nullWarningAnalysis stM getSeq debugNW
  let nwAdjL = new nullWarningAnalysis stM getAdjL debugNW
  let nwAdjNL = new nullWarningAnalysis stM getAdjNL debugNW
  let nwPess = new nullWarningAnalysis stM getPess debugNW

  let seqNonfixPasses =
    [ (nwSeq :> IntraDataflow.analysis);
      mkAna (new nullKnowVisitor stM getSeq "SEQ")]
    
  let adjLNonfixPasses =
    [ (nwAdjL :> IntraDataflow.analysis);
      mkAna (new nullKnowVisitor stM getAdjL "RADAR")]

  let adjNLNonfixPasses =
    [ (nwAdjNL :> IntraDataflow.analysis);
      mkAna (new nullKnowVisitor stM getAdjNL "RADAR-NL")]

  let pessNonfixPasses =
    [ (nwPess :> IntraDataflow.analysis);
      mkAna (new nullKnowVisitor stM getPess "STEENS")]

  (* Ugly as hell??? *)
  let setCG cg sccCG =
    nwSeq#setCG cg;
    nwAdjL#setCG cg;
    nwAdjNL#setCG cg;
    nwPess#setCG cg

end


let warnDoBefore st1 st2 settings cg sccCG =
  let s, ext = match st1, st2 with
                 | SEQ, _  -> "seqWarnDoBefore", "nwarn1"
                 | ADJ, LOCKS -> "adjLocksWarnDoBefore", "nwarn2"
                 | ADJ, NOLOCKS -> "adjNoLocksWarnDoBefore", "nwarn3" 
                 | PESS, _ -> "pessWarnDoBefore", "nwarn4"
  in
  logStatus ("rkcdebug: " ^ s);
  let nwSums = new NSummary.data (Backed_summary.makeSumType ext) in
  setSums settings cg sccCG nwSums

let warnDoAfter st1 st2 (_:callG) cgDir = 
  let unsafe_seq_data = Filename.concat cgDir "unsafe_derefs_seq.dat" in
  let a, b, c = "safe_derefs", "unsafe_derefs", ".dat" in
  let s, safeF, unsafeF =
    match st1, st2 with
      | SEQ, _ -> "seqWarnDoAfter", (catSeq a) ^ c, (catSeq b) ^ c
      | PESS, _ -> "pessWarnDoAfter", (catPess a) ^ c, (catPess b) ^ c
      | ADJ, LOCKS -> "adjLocksWarnDoAfter", (catAdjL a) ^ c, (catAdjL b) ^ c
      | ADJ, NOLOCKS -> "adjNoLocksWarnDoAfter", 
          (catAdjNL a) ^ c, (catAdjNL b) ^ c
  in
  logStatus ("rkcdebug: " ^ s);
  let safeDerefs = getSafe () in
  let unsafeDerefs = getUnsafe () in
  let safe_data = Filename.concat cgDir safeF in
  let unsafe_data = Filename.concat cgDir unsafeF in
  safeDerefs#serialize safe_data;
  unsafeDerefs#serialize unsafe_data;
  match st1 with
    | SEQ -> ()
    | ADJ | PESS -> begin
        (* printDiffs unsafe_seq_data unsafe_data; *)
        printDeltaReport unsafe_seq_data unsafe_data
      end
 


module Radarayed = Radar (RadarDFs) (N)

module SeqAna = struct
  module Transfer = Radarayed.SeqTransfer
  module Dataflow = Radarayed.SeqDataflow
  let beforeDataflow = warnDoBefore SEQ LOCKS (* dummy *)
  let afterDataflow = warnDoAfter SEQ LOCKS (* dummy *)
end

module PessAna = struct
  module Transfer = Radarayed.PessTransfer
  module Dataflow = Radarayed.PessDataflow
  let beforeDataflow = warnDoBefore PESS LOCKS (* dummy *)
  let afterDataflow = warnDoAfter PESS LOCKS (* dummy *)
end

module PseudoRacePass = Radarayed.PseudoRacePass

module AdjLAna = struct
  module Transfer = Radarayed.AdjLTransfer
  module Dataflow = Radarayed.AdjLDataflow
  let beforeDataflow = warnDoBefore ADJ LOCKS
  let afterDataflow = warnDoAfter ADJ LOCKS
end

module AdjNLAna = struct
  module Transfer = Radarayed.AdjNLTransfer
  module Dataflow = Radarayed.AdjNLDataflow
  let beforeDataflow = warnDoBefore ADJ NOLOCKS
  let afterDataflow = warnDoAfter ADJ NOLOCKS
end



