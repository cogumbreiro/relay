(** Functor to generate RADAR passes (sequential, adjust, etc.),
    given that the client conforms to the interface *)

open Cil
open Fstructs
open Callg
open Scc_cg
open Logging
open Cildump

module Intra = IntraDataflow
module AU = All_unlocks
module RS = Racestate.RS
module SPTA = Racestate.SPTA (* assume it's the same in Racestate_temp *)
module LP = Lockset_partitioner
module LSHash = Pseudo_access.LSHash
module PPHash = Ciltools.PPHash
module LvalHash = Pseudo_access.LvalHash
module Stat = Mystats


(******************************************************************************)
(*** DATA STRUCTURES AND TYPES                                              ***)
(******************************************************************************)

type radarMode = SEQ | ADJ | PESS

type relayMode = LOCKS | NOLOCKS

module Lvs = Set.Make (Lvals.OrderedLval)

type lval = Lvals.aLval

type lvals = Lvs.t

type lockset = RS.lockState

type guardedReads = lockset * lvals

type racyStatus =
  | PaRacy of Pseudo_access.paKeyHead
  | PaNotRacy
  | PaNotFound (* of lval *)

type crumb = Pseudo_access.paKeyHead option 
    (* Some for PaRacy, None for PaNotFound *) 

module Crumbs = Set.Make
  (struct 
     type t = crumb 
     let compare a b = Pervasives.compare a b
   end)

module CrumbyLval = struct
  type t = lval * crumb
  let compare a b =
    let x = Lvals.compare_lval (fst a) (fst b) in
    match x with
      | 0 -> Pervasives.compare (snd a) (snd b)
      | _ -> x
end

module CrumbyLvs = Set.Make (CrumbyLval)

type crumbsandlvals = CrumbyLvs.t

let addCrumb (a, b) c = 
  (a, Crumbs.add c b)

let projValue = fst
let projCrumb = snd


let catSeq s = s ^ "_seq"
(*let catAdj s = s ^ "_adj"*)
let catAdjL s = s ^ "_adj_l"
let catAdjNL s = s ^ "_adj_nl"
(*let catPar s = s ^ "_par"*)
let catParL s = s ^ "_par_l"
let catParNL s = s ^ "_par_nl"
let catPess s = s ^ "_pess"



(******************************************************************************)
(*** CLASS TYPES                                                            ***)
(******************************************************************************)

(* A radarSEQTRANSFUNC is an sequential transFunc plus two operations *)
class type ['st] radarSEQTRANSFUNC = object inherit ['st] Intra.transFunc 

  method dependsOn : 'st -> lvals

  method havoc : 'st -> crumbsandlvals -> 'st

end

class type ['st] radarADJTRANSFUNC = object inherit ['st] radarSEQTRANSFUNC

  method loadPAInfo : funID -> unit

  method setSetStateAt : (prog_point -> 'st -> unit) -> unit

end

module type RadarInputTransFunc = sig

  type st
  type sum
  class seq : [st] radarSEQTRANSFUNC

end

(******************************************************************************)
(*** RADAR ADJUST TRANSFER FUNCTIONS                                        ***)
(******************************************************************************)

module RadarAdjTFs (RT:RadarInputTransFunc) = struct

  class virtual basicAdjust 
    (stMan : (RT.st, RT.sum) Intra.stateLattice) 
    (pardb : Pseudo_access.PARsummary.data) = object(self)
      inherit RT.seq as seq

      method private virtual getRacyReads : guardedReads -> CrumbyLvs.t
        
      val mutable setStateAt = (fun pp x -> ())
        
      method setSetStateAt foo = setStateAt <- foo
        
      (* state for the function being processed: fid and pseudo-acc summary *)
      val mutable fid = dummyFID
      method private setKey key =
        fid <- key

      val mutable par = LSHash.create 0
      val mutable pp2int = PPHash.create 0
      val mutable int2ls = Inthash.create 0
        
      (* before processing any function, this should get called *)
      method loadPAInfo fk =
        logStatus ("rkcdebug: loadPAInfo called with " ^ fid_to_string fk);
        self#setKey fk;
        (* get summaries *)
        let p2i, i2l = LP.sums#find fid in
        (*LP.printSumm fk (p2i, i2l);*)
        let parfromdisk, _, _ = pardb#find fid in
        logStatus
          ("rkcdebug: loadPAInfo from " ^ 
             Backed_summary.string_of_sumType pardb#sumTyp);
        (* copy summaries into tables *)
        par <- LSHash.copy parfromdisk;
        pp2int <- PPHash.copy p2i;
        int2ls <- Inthash.copy i2l;
        LP.sums#evictOne fid;
        pardb#evictOne fid;
        ()
                    
      method private getCurrentLockset () =
        let pp = getCurrentPP () in
        try LP.getLocksetAtPP pp (pp2int, int2ls)
        with LP.LsAtPP ->
          begin
            logError "getCurrentLockset failed";
            Racesummary.emptyLS
          end

      method private getInstructionLockset i =
        match i with
        | Set (_, _, _)
        | Asm (_, _, _, _, _, _) ->
            self#getCurrentLockset ()
        | Call (_, _, _, _) ->
            AU.getWeakerLS self#curCG self#curFunID
              (self#getCurrentLockset ()) i
              
      method handleInstr i state =
        let ls = self#getInstructionLockset i in
        let lvs = seq#dependsOn state in
        let races = self#getRacyReads (ls, lvs) in
        let adjstate = seq#havoc state races in
        let newstate =
          match seq#handleInstr i adjstate with
          | Dataflow.Post _  -> adjstate (* report error? *)
          | Dataflow.Default -> adjstate
          | Dataflow.Done st -> 
              setStateAt (getCurrentPP ()) adjstate; (* update table *)
              st
        in
        Dataflow.Done newstate

      method handleGuard g state =
        let ls = self#getCurrentLockset () in
        let lvs = seq#dependsOn state in
        let races = self#getRacyReads (ls, lvs) in
        let adjstate = seq#havoc state races in
        let newstate =
          match seq#handleGuard g adjstate with
          | Dataflow.GUnreachable -> adjstate (* report error? *)
          | Dataflow.GDefault     -> adjstate
          | Dataflow.GUse st      ->
              setStateAt (getCurrentPP ()) adjstate; (* update table *)
              st
        in 
        Dataflow.GUse newstate


      method handleStmt s state =
        let ls = self#getCurrentLockset () in
        let lvs = seq#dependsOn state in 
        let races = self#getRacyReads (ls, lvs) in
        let adjstate = seq#havoc state races in
        let newstate =
          match seq#handleStmt s adjstate with
          | Dataflow.SDone    -> adjstate (* report error? *)
          | Dataflow.SDefault -> adjstate
          | Dataflow.SUse st  ->
              setStateAt (getCurrentPP ()) adjstate; (* update table *)
              st
        in
        Dataflow.SUse newstate

    end                    


  class radarAdjTransFunc
    (stMan : (RT.st, RT.sum) Intra.stateLattice) 
    (pardb : Pseudo_access.PARsummary.data) : [RT.st] radarADJTRANSFUNC =
  object(self) 
    inherit basicAdjust stMan pardb
      
    method private getRacyReads ((ls, lvs) : guardedReads) =
      let checkRacy lv =
        (try let _, paBindings = LSHash.find par ls in
         (try let pakh, targetStatus = LvalHash.find paBindings lv in
          match Pseudo_access.areTargetsSafe targetStatus with
          | true  -> 
              (* logStatus ("rkcdebug: racy kill false " ^ Lvals.string_of_lval lv); *)
              PaNotRacy 
          | false ->
              logStatus ("rkcdebug: racy kill true " ^ Lvals.string_of_lval lv);
              PaRacy pakh

          with Not_found ->
            logErrorF 
              "getRacyReads (%s) sumKey:%s - targetStatus NF\n"
              (Lvals.string_of_lval lv) (fid_to_string fid);
            PaNotFound (* (Lvals.mergeLv lv) *) )
           
         with Not_found ->
           logErrorF 
             "getRacyReads (%s) sumKey:%s - paBindings NF:\n" 
             (Lvals.string_of_lval lv) (fid_to_string fid);
           Racesummary.printLockset ls;
           flushStatus ();
           PaNotFound (* (Lvals.mergeLv lv) *) )
      in
      List.fold_left
        (fun set lv ->
           match checkRacy lv with
           | PaNotRacy   -> set
           | PaRacy pakh -> CrumbyLvs.add (lv, Some pakh) set
           | PaNotFound  -> CrumbyLvs.add (lv, None) set
        ) CrumbyLvs.empty (Lvs.elements lvs)
  end


  class pessAdjTransFunc
    (stMan : (RT.st, RT.sum) Intra.stateLattice) 
    (pardb : Pseudo_access.PARsummary.data) : [RT.st] radarADJTRANSFUNC =
  object(self) 
    inherit basicAdjust stMan pardb
      
    method private getRacyReads ((_, lvs) : guardedReads) =
      let checkEscape lv =
        match Shared.escapeableAbs lv with
        | true  ->
            logStatus ("rkcdebug: racy kill true " ^ Lvals.string_of_lval lv);
            PaNotFound (* give dummy pakh instead? *)
              (*PaRacy (0,0)*)
        | false ->
            PaNotRacy
      in
      List.fold_left
        (fun set lv ->
           match checkEscape lv with
           | PaNotRacy   -> set
           | PaRacy pakh -> CrumbyLvs.add (lv, Some pakh) set
           | PaNotFound  -> CrumbyLvs.add (lv, None) set
        ) CrumbyLvs.empty (Lvs.elements lvs)


    method loadPAInfo fk =
      ()

    method private getCurrentLockset () =
      RS.emptyLS

    method private getInstructionLockset i =
      RS.emptyLS

  end

end

(******************************************************************************)
(*** PSEUDO ACCESS VISITOR                                                  ***)
(******************************************************************************)

(* TODO consider FlowInsensitive *)


class ['st] pseudoAccessVisitor (dependsOn : 'st -> lvals)
                                (getState : prog_point -> 'st)
                                (pardb1 : Pseudo_access.PARsummary.data)
                                (pardb2 : Pseudo_access.PARsummary.data)  =
  object(self) inherit Pp_visitor.ppVisitor

  val mutable fid = dummyFID
  method private setKey key = fid <- key

  val mutable curCG = emptyCG
  method setCG cg = curCG <- cg
    
  val mutable paCount = -1

  val mutable pp2int : LP.pp2intTable = PPHash.create 0

  val mutable int2ls : LP.int2lsTable = Inthash.create 0

  val mutable par : Pseudo_access.paRegions = LSHash.create 0

  method par = par


  method initState fk = 
    self#setKey fk;
    (* clear state from last run *)
    par <- LSHash.create 0;
    paCount <- -1;
    (* load LSP tables *)
    let p2i, i2l = LP.sums#find fid in
    pp2int <- PPHash.copy p2i;
    int2ls <- Inthash.copy i2l;
    logStatusF "rkcdebug: paVisitor initState %s\n" (fid_to_string fid);
    LP.sums#evictOne fid


  method private lsError s pp =
    logErrorF "paVisitor#%s : LsAtPP %s\n" s (string_of_pp pp);
    RS.emptyLS


  method vinst i =
    self#setInstrPP i;
    let pp = getCurrentPP () in
    let ls =
      try LP.getLocksetAtPP pp (pp2int, int2ls)
      with LP.LsAtPP -> self#lsError "vinst" pp in
    let state = getState pp in begin
    match i with
      | Set (_, _, _)
      | Asm (_, _, _, _, _, _) ->
          self#processState state ls
      | Call (_, _, _, _) ->
          let ls = AU.getWeakerLS curCG fid ls i in
          self#processState state ls
    end;
    self#bumpInstr 1;
    DoChildren


  method vstmt s = 
    self#setStmtPP s;
    let pp = getCurrentPP () in
    let ls =
      try RS.hcLockstate (LP.getLocksetAtPP pp (pp2int, int2ls))
      with LP.LsAtPP -> self#lsError "vstmt" pp in
    let state = getState pp in
    self#processState state ls;
    DoChildren


  method private processState state ls =
    let lvs = dependsOn state in
    Lvs.iter (self#processLval ls) lvs


  method private processLval ls lv =
    let regionLoc, paBindings =
      if LSHash.mem par ls then (* this lockset region already exists *)
        LSHash.find par ls
      else (* this is the first location accessed in this lockset region,
            * so we'll arbitrarily use it for the entire region *)
        let loc = !Cil.currentLoc in
        let empty = LvalHash.create 0 in
        LSHash.add par ls (loc, empty);
        loc, empty in

    let pakh, targetStatus =
      if LvalHash.mem paBindings lv then (* lval already in this region *)
        LvalHash.find paBindings lv
      else
        let newpakh = self#genPaKeyHead in
        let t = LvalHash.create 0 in
        newpakh, t in

    let targetLvs =
      match lv with
        | Lvals.CVar vi, _ ->
            [lv]
        | Lvals.CMem ptrExp, _ ->
            snd (SPTA.derefALvalAt (getCurrentPP()) lv) (* mayPt set *)
        | _ ->
            logError ~prior:1 "processLval: lv has unknown structure";
            [] in

    let targetLvs = List.map Lvals.mergeLv targetLvs in (* note: shadowing *)

    let _ =
      List.fold_left
        (fun pakeytail t ->
           (* initialize (or re-initialize) a target's status to None *)
           LvalHash.replace targetStatus t (pakeytail, None);
           pakeytail + 1
        ) 0 targetLvs in
    
    (* TODO: make sure I don't have to "replace" in paBindings and par *)
    (* TODO: actually it seems like I do... *)
    LvalHash.replace paBindings lv (pakh, targetStatus);
    LSHash.replace par ls (regionLoc, paBindings)


  method private genPaKeyHead =
    paCount <- paCount + 1;
    fid, paCount

  method addPseudoAccesses fdec =
    (** Add pseudo-accesses to Guarded access summaries *)
    (*pardb#addReplace sumKey (Pseudo_access.wrapSummary par); (* get rid of wrap? *)*)
    pardb1#addReplace fid (Pseudo_access.wrapSummary par);
    pardb2#addReplace fid (Pseudo_access.wrapSummary par);
    (* create guarded accesses *)
    let sum = ref (RS.sum#find fid) in
    let corrs = ref (!sum.RS.sum_out.RS.cState) in
    LSHash.iter (fun ls (loc, paBindings) ->
      LvalHash.iter (fun origLv (pakh, targetStatus) ->
        LvalHash.iter (fun t (pakt, _) ->
          match Shared.isShareableAbs fdec t with
            | None -> ()
            | Some _ ->
                let pak = Pseudo_access.smashPaKey pakh pakt in
                corrs := RS.addPseudoCorr ls !corrs t loc fid pak;
                sum := { !sum
                           with RS.sum_out =
                             { !sum.RS.sum_out with RS.cState = !corrs }}
        ) targetStatus
      ) paBindings
    ) par;
    sum := RS.scopeSummary fdec !sum;
    sum := RS.hcSummary !sum;
    RS.sum#addReplace fid !sum;
    RS.findPrintSumm fdec.svar.vname fid;
    RS.sum#flushOne fid;
    AU.sums#evictSummaries

end



(******************************************************************************)
(*** INTRA-PROCEDURAL GLUE                                                  ***)
(******************************************************************************)


module type RADAR_PARAMS = sig

  type st
  type sum

  val name : string
  val summaryExt : string  

(*  val seq : st radarSEQTRANSFUNC *)

  (* TODO I've put this in and taken it out several times.  I think ideally
   * it should be out, since the radar adjusted transfer function should be
   * constructed in the same way every time. *)
  (* val adj : st radarADJTRANSFUNC *)
  
  val stMan : (st, sum) Intra.stateLattice
  class seq : [st] radarSEQTRANSFUNC
  
  val seqSums : sum Backed_summary.base
  val adjSumsL : sum Backed_summary.base
  val adjSumsNL : sum Backed_summary.base
  val pessSums : sum Backed_summary.base
  val parL : Pseudo_access.PARsummary.data  (* PAR db for race queries w/ locksets *) 
  val parNL : Pseudo_access.PARsummary.data (* PAR db for race queries w/o locksets *)

  class summarizer : sum Backed_summary.base -> [st, sum] Intra.summarizer

end


module type RADAR_ANALYSIS = sig

  include RADAR_PARAMS
  val mode : radarMode
  val sums : sum Backed_summary.base 
  val lockMode : relayMode

end


module type ExtendedDFTransfer = sig

  include Intra.DFTransfer
  val mode : radarMode
  val lockMode : relayMode
  val explicitSeq : st radarSEQTRANSFUNC
  (*val explicitAdj : st radarADJTRANSFUNC*)
  val explicitPess : st radarADJTRANSFUNC
  val explicitAdjL : st radarADJTRANSFUNC
  val explicitAdjNL : st radarADJTRANSFUNC

end


module MakeExtendedDFTransfer (RA : RADAR_ANALYSIS) 
  : ExtendedDFTransfer with type st = RA.st = struct

  let debug = false

  let name = RA.name

  type st = RA.st

  type sum = RA.sum

  let stMan = RA.stMan

  let mode = RA.mode

  let lockMode = RA.lockMode

  module RadarTFOut = RadarAdjTFs (RA)

  let explicitSeq = new RA.seq

  (*let explicitAdj = new radarAdjTransFunc RA.seq RA.stMan RA.pardb*)
  
  let explicitAdjL = new RadarTFOut.radarAdjTransFunc RA.stMan RA.parL

  let explicitAdjNL = new RadarTFOut.radarAdjTransFunc RA.stMan RA.parNL

  let explicitPess = new RadarTFOut.pessAdjTransFunc RA.stMan RA.parL

  (* pick the correct transF later  *)
  let transF = ref (explicitSeq :> st Intra.transFunc)

end


module type ExtendedIntraProcAnalysis = sig

  include Intra.IntraProcAnalysis
  include ExtendedDFTransfer with type st = T.st
  val initializeSecondPhase : funID ->  fundec -> unit

end


module ExtendDF
         (E : ExtendedDFTransfer )
         (I : Intra.IntraProcAnalysis with type T.st = E.st) 
         : ExtendedIntraProcAnalysis with type T.st = E.st = 
struct
  include I
  include E
    
  (* "extending" behavior of initialize for ADJ phase *)
  let initialize funID (func:fundec) (input:I.T.st) : unit =
    (match E.mode with
     | SEQ -> logStatus "rkcdebug: initialize SEQ"
     | ADJ
     | PESS ->
         (* load in sequential transfer function *)
         logStatus "rkcdebug: loading in seq transfer func";
         I.T.transF := (E.explicitSeq :> I.T.st Intra.transFunc));
    I.initialize funID func input

            
  let initializeSecondPhase funID (func:fundec) =
    logStatus "rkcdebug: initializeSecondPhase, loading in adj transfer func";
    (match E.mode with
     | SEQ ->
         failwith "radar: initializeSecondPhase for SEQ mode"
     | ADJ -> begin
         let adjF = match E.lockMode with
           | LOCKS   -> E.explicitAdjL
           | NOLOCKS -> E.explicitAdjNL in
         (* load pseudo access info *)
         adjF#loadPAInfo !I.curFunID;
         (* TODO don't need to reset this _every_ time... *)
         adjF#setSetStateAt I.setDataBefore;
         (* load in adjusted transfer function *)
         I.T.transF := (adjF :> I.T.st Intra.transFunc)
       end
     | PESS -> begin
         logStatus "rkcdebug: explicitPess set";
         E.explicitPess#setSetStateAt I.setDataBefore;
         I.T.transF := (E.explicitPess :> I.T.st Intra.transFunc)
       end);
    !I.T.transF#handleFunc funID func
 
end




(******************************************************************************)
(*** INTER-PROCEDURAL GLUE                                                  ***)
(******************************************************************************)

module type NONFIX_PASSES =
  sig
    val seqNonfixPasses : Intra.analysis list 
    val pessNonfixPasses : Intra.analysis list 
    val adjLNonfixPasses : Intra.analysis list 
    val adjNLNonfixPasses : Intra.analysis list 

    (* Ugh... in case it needs to know the callgraph *)
    val setCG : callG -> sccGraph -> unit
  end

module MakeBUTransfer
         (RA : RADAR_ANALYSIS)
         (I : ExtendedIntraProcAnalysis with type T.st = RA.st)
         (N : NONFIX_PASSES ) = struct

  let inspect = ref false

  let sccsDone = ref 0

  let sccsTotal = ref 0

  let curCG = ref emptyCG

  let curSCCCG = ref emptySCCCG

  let updateStats lastSCC = begin
    incr sccsDone;
    logStatusF ">>> PROGRESS %d/%d SCCs DONE!\n\n" !sccsDone !sccsTotal;
    flushStatus ();
  end

  type state = RA.st


  class symexAnalysis = object inherit SPTA.symexAnalysis as super
    (* don't write out summaries, since they're already done! *)
    method summarize key (cfg:fundec) = false

    method compute funID cfg : unit = begin
      super#compute funID cfg;
      RS.sum#evictSummaries;
    end

  end

  class parAnalysis = object(self)
  
    val paVisitor = new pseudoAccessVisitor I.explicitSeq#dependsOn
      I.getDataBefore (*RA.pardb *) RA.parL RA.parNL
  
    method setCG cg =
      paVisitor#setCG cg
    
    method setInspect yesno =
      inspect := yesno

    (* TODO pass isFinal method? *)
    method isFinal fk =
     false 

    method compute fid cfg =
      paVisitor#initState fid;
      Stat.time "PAR visit" ignore 
        (visitCilFunction (paVisitor :> cilVisitor) cfg)

    method summarize fk cfg =
      (match self#isFinal fk with
        | true ->  () (* TODO *)
        | false ->
            paVisitor#addPseudoAccesses cfg;
            let par, _, _ = RA.parL#find fk (*RA.pardb#find fk*) in
            Pseudo_access.printPaRegions par fk
      );
      false

    method flushSummaries () =
      RA.parL#serializeAndFlush;
      RA.parNL#serializeAndFlush
    
  end

  class dfAnalysis = object(self)
  
    val summarizer = new RA.summarizer RA.sums 

    method setInspect yesno =
      inspect := yesno;
      summarizer#setInspect yesno
      (* TODO invoke setInspect on client transFunc *)

    method isFinal fk = false

    method private string_of_mode =
      match RA.mode, RA.lockMode with 
        SEQ, _ -> "SEQ" 
      | ADJ, LOCKS -> "ADJ_L"
      | ADJ, NOLOCKS -> "ADJ_NL"
      | PESS, _ -> "PESS"          

    method private printSizeState caption =
      if !Osize.checkSizes then
        Osize.p_size_kb caption (I.sizeOfState ())

    method compute fid cfg =
      logStatus "doing dataflow analysis";
      flushStatus ();
      let input = RA.stMan#initialState in
      
      I.initialize fid cfg input;
      (* let modeStr = self#string_of_mode in *)

      match RA.mode with
        | SEQ ->
            (* self#printSizeState (modeStr ^ "(pre) stmtStartData"); *)
            Stat.time "DF: " I.compute cfg;
            (* self#printSizeState (modeStr ^ "(post) stmtStartData") *)

        | ADJ
        | PESS -> begin
            logStatus "ADJ Phase 1";
            (* self#printSizeState (modeStr ^ " P1(pre) stmtStartData"); *)
            flushStatus ();
            Stat.time "DF: " I.compute cfg;
            (* self#printSizeState (modeStr ^ " P1(post) stmtStartData"); *)
            I.initializeSecondPhase fid cfg; 
            logStatus "ADJ Phase 2";
            flushStatus ();
            Stat.time "DF: " I.compute cfg;
            (* self#printSizeState (modeStr ^ " P2(post) stmtStartData"); *)
            flushStatus ();
          end

    method summarize fkey cfg =
      if self#isFinal fkey then
        false
      else begin
        (* get the possibly-new output state for this function *)
        summarizer#summarize fkey cfg I.getDataBefore
      end
            
    method flushSummaries () =
      summarizer#flushSummaries () (* TODO get rid of redundancy *)

  end

  let needsFixpoint =
    let ssAna = new symexAnalysis in
    let dfAna = new dfAnalysis in
    [ ssAna; dfAna ]

  let paAna = new parAnalysis

  let nonFixpoint =
    match RA.mode, RA.lockMode with
      | PESS, _      -> N.pessNonfixPasses
      | ADJ, LOCKS   -> N.adjLNonfixPasses 
      | ADJ, NOLOCKS -> N.adjNLNonfixPasses
      | SEQ, _       -> (paAna :> Intra.analysis) :: N.seqNonfixPasses

  let initStats cg sccCG =
    begin
      RA.stMan#setTheSums RA.sums; (* important! *)
      curCG := cg;
      curSCCCG := sccCG;
      (* TODO need this, but clean up *)
      Intra.curCG := cg;
      Intra.curSCCCG := sccCG;
      N.setCG cg sccCG;
      paAna#setCG cg;

      (* Set them all... ugly!!! *)
      I.explicitSeq#setCG cg;
      I.explicitPess#setCG cg;
      I.explicitAdjL#setCG cg;
      I.explicitAdjNL#setCG cg;

      sccsTotal := Stdutil.mapSize sccCG IntMap.fold;
      sccsDone := 0;
    end



  let flushSummaries () = begin
    Backed_summary.printSizeOfAll "Summarize (pre-flush)";
    RS.sum#serializeAndFlush;
    SPTA.SS.sum#evictSummaries;
    AU.sums#evictSummaries;
    LP.sums#evictSummaries;
    RA.sums#serializeAndFlush;
    (*RA.pardb#serializeAndFlush*)
    RA.parL#serializeAndFlush;
    RA.parNL#serializeAndFlush;
    Backed_summary.printSizeOfAll "Summarize (post-flush)";
  end


  let doFunc ?(input:state = RA.stMan#initialState) fid node :
      state InterDataflow.interResult =
    let fn, defFile = node.name, node.defFile in
    logStatusF "Summarizing function: %s(%s):%s\n" 
      fn (fid_to_string fid) defFile;
    logStatus "-----";
    flushStatus ();
    match Cilinfos.getFunc (fid_to_fkey fid) defFile with
      | Some cfg ->
          if Intra.runFixpoint needsFixpoint fid cfg then
            InterDataflow.NewOutput (input, input) 
          else
            InterDataflow.NoChange
      | None ->
          (* Don't have function definition *)
          logErrorF "doFunc can't get CFG for: %s:%s\n" fn defFile;
          InterDataflow.NoChange
            
  let filterFunc _ = true

  (* TODO make sure loading correct summaries for SEQ and ADJ passes *)

  let loadSumms () =
    Backed_summary.getDescriptors [RS.sum#sumTyp; SPTA.SS.sum#sumTyp;
                                   LP.sums#sumTyp; AU.sums#sumTyp;]

  let loadSeqCallees () =
    loadSumms () @ Backed_summary.getDescriptors [RA.sums#sumTyp;]

  let loadSeqOwnFuns () = loadSumms ()

  let loadAdjCallees () =
    loadSumms () @ Backed_summary.getDescriptors [RA.parL#sumTyp;
                                                  RA.parNL#sumTyp;
                                                  RA.sums#sumTyp;]
  let loadAdjOwnFuns () =
    loadSumms () @ Backed_summary.getDescriptors [RA.parL#sumTyp;
                                                  RA.parNL#sumTyp;]

  let sccStart scc = begin
    logStatus ("rkcdebug: sccStart " ^ string_of_int scc.scc_num);
    let calleeSumTypes, ownFunSumTypes =
      match RA.mode with
        | PESS
        | SEQ -> loadSeqCallees (), loadSeqOwnFuns ()
        | ADJ -> loadAdjCallees (), loadAdjOwnFuns () in

    logStatus "Acquiring callee summaries";
    flushStatus ();
    Manage_sums.prepareSCCCalleeSums !curSCCCG scc calleeSumTypes;

    logStatus "Acquiring summaries for current SCC";
    flushStatus ();
    Manage_sums.prepareSCCSums scc ownFunSumTypes;
  end

  let sccDone scc (byThisGuy:bool) =
    let summPaths = if byThisGuy then
      let sumKeys = Manage_sums.sumKeysOfScc scc [] in
      let prevFKey = !I.curFunID in
      logStatusF "rkcdebug: Run Nonfixpoint, prevFKey=%s\n" 
         (fid_to_string prevFKey);
      flushStatus ();
      Intra.runNonFixpoint nonFixpoint needsFixpoint prevFKey scc;
      
      flushSummaries ();
      
      let tokenMap = RA.sums#locate sumKeys in
      
      List.fold_left
        (fun paths (fkey, tok) ->
           if List.mem fkey sumKeys then
             let path = Backed_summary.pathFromToken tok in
             (fkey, path) :: paths
           else paths
        ) [] tokenMap
    else [] in
    updateStats scc;
    summPaths
          
end



(******************************************************************************)
(*** CREATE BOTTOM UP DATAFLOWS                                             ***)
(******************************************************************************)

module type RADAR_FIXPOINT_PASS =
  sig
    module RA_SEQ : RADAR_ANALYSIS
    module RA_PESS : RADAR_ANALYSIS with type st = RA_SEQ.st
    module RA_ADJ_L : RADAR_ANALYSIS with type st = RA_SEQ.st
    module RA_ADJ_NL : RADAR_ANALYSIS with type st = RA_SEQ.st

    module SeqDF
           : ExtendedIntraProcAnalysis with type T.st = RA_SEQ.st
    module PessDF 
           : ExtendedIntraProcAnalysis with type T.st = RA_PESS.st
    module AdjLDF
           : ExtendedIntraProcAnalysis with type T.st = RA_ADJ_L.st
    module AdjNLDF
           : ExtendedIntraProcAnalysis with type T.st = RA_ADJ_NL.st

    val getSeqDataBefore : prog_point -> SeqDF.T.st
    val getPessDataBefore : prog_point -> PessDF.T.st
    val getAdjLDataBefore : prog_point -> AdjLDF.T.st
    val getAdjNLDataBefore : prog_point -> AdjNLDF.T.st
  end


module MakeRadarForwardDFs (R : RADAR_PARAMS)
       : RADAR_FIXPOINT_PASS with type RA_SEQ.st = R.st = struct

  (* TODO should these be registered inside each RADAR_ANALYSIS instead,
     so that they're all not registered for each pass? *)
  (* TODO split up par into par_locks and par_nolocks for different
     relay modes *)
  let _ = Backed_summary.registerType R.seqSums
  let _ = Backed_summary.registerType R.adjSumsL
  let _ = Backed_summary.registerType R.adjSumsNL
  let _ = Backed_summary.registerType R.pessSums
  let _ = Backed_summary.registerType R.parL
  let _ = Backed_summary.registerType R.parNL

  (* Sequential Stuff *)
  module RA_SEQ = struct
    include R
    let mode = SEQ
    let sums = seqSums
    let lockMode = LOCKS (* doesn't matter *)
  end
  module Transfer = MakeExtendedDFTransfer (RA_SEQ)
  module RelayForwardDF_ = Intra.FlowSensForward (Transfer)
  module SeqDF = ExtendDF (Transfer) (RelayForwardDF_)
  let getSeqDataBefore = SeqDF.getDataBefore

  (* Radar Adjusted Stuff *)
  module RA_ADJ_L = struct
    include R
    let mode = ADJ
    let sums = adjSumsL
    let lockMode = LOCKS
  end
  module Transfer2 = MakeExtendedDFTransfer (RA_ADJ_L)
  module RelayForwardDF_2 = Intra.FlowSensForward (Transfer2)
  module AdjLDF = ExtendDF (Transfer2) (RelayForwardDF_2)
  let getAdjLDataBefore = AdjLDF.getDataBefore

  (* Pessimistic Adjust Stuff *)
  module RA_PESS = struct
    include R
    let mode = PESS
    let sums = pessSums
    let lockMode = LOCKS (* doesn't matter *)
  end
  module Transfer3 = MakeExtendedDFTransfer (RA_PESS)
  module RelayForwardDF_3 = Intra.FlowSensForward (Transfer3)
  module PessDF = ExtendDF (Transfer3) (RelayForwardDF_3)
  let getPessDataBefore = PessDF.getDataBefore

  (* Radar-NoLocks Adjusted Stuff *)
  module RA_ADJ_NL = struct
    include R
    let mode = ADJ
    let sums = adjSumsNL
    let lockMode = NOLOCKS
  end
  module Transfer4 = MakeExtendedDFTransfer (RA_ADJ_NL)
  module RelayForwardDF_4 = Intra.FlowSensForward (Transfer4)
  module AdjNLDF = ExtendDF (Transfer4) (RelayForwardDF_4)
  let getAdjNLDataBefore = AdjNLDF.getDataBefore

end


module MakeRadarBackwardDFs (R : RADAR_PARAMS)
       : RADAR_FIXPOINT_PASS with type RA_SEQ.st = R.st = struct

  (* TODO should these be registered inside each RADAR_ANALYSIS instead,
     so that they're all not registered for each pass? *)
  (* TODO split up par into par_locks and par_nolocks for different
     relay modes *)
  let _ = Backed_summary.registerType R.seqSums
  let _ = Backed_summary.registerType R.adjSumsL
  let _ = Backed_summary.registerType R.adjSumsNL
  let _ = Backed_summary.registerType R.pessSums
  let _ = Backed_summary.registerType R.parL
  let _ = Backed_summary.registerType R.parNL

  (* Sequential Stuff *)
  module RA_SEQ = struct
    include R
    let mode = SEQ
    let sums = seqSums
    let lockMode = LOCKS (* doesn't matter *)
  end
  module Transfer = MakeExtendedDFTransfer (RA_SEQ)
  module RelayBackwardDF_ = Intra.FlowSensBackward (Transfer)
  module SeqDF = ExtendDF (Transfer) (RelayBackwardDF_)
  let getSeqDataBefore = SeqDF.getDataBefore

  (* Radar Adjusted Stuff *)
  module RA_ADJ_L = struct
    include R
    let mode = ADJ
    let sums = adjSumsL
    let lockMode = LOCKS
  end
  module Transfer2 = MakeExtendedDFTransfer (RA_ADJ_L)
  module RelayBackwardDF_2 = Intra.FlowSensBackward (Transfer2)
  module AdjLDF = ExtendDF (Transfer2) (RelayBackwardDF_2)
  let getAdjLDataBefore = AdjLDF.getDataBefore

  (* Pessimistic Adjust Stuff *)
  module RA_PESS = struct
    include R
    let mode = PESS
    let sums = pessSums
    let lockMode = LOCKS (* doesn't matter *)
  end
  module Transfer3 = MakeExtendedDFTransfer (RA_PESS)
  module RelayBackwardDF_3 = Intra.FlowSensBackward (Transfer3)
  module PessDF = ExtendDF (Transfer3) (RelayBackwardDF_3)
  let getPessDataBefore = PessDF.getDataBefore

  (* Radar-NoLocks Adjusted Stuff *)
  module RA_ADJ_NL = struct
    include R
    let mode = ADJ
    let sums = adjSumsNL
    let lockMode = NOLOCKS
  end
  module Transfer4 = MakeExtendedDFTransfer (RA_ADJ_NL)
  module RelayBackwardDF_4 = Intra.FlowSensBackward (Transfer4)
  module AdjNLDF = ExtendDF (Transfer4) (RelayBackwardDF_4)
  let getAdjNLDataBefore = AdjNLDF.getDataBefore

end




(************************************************************)

(* Lift to Inter-procedural analysis (or at least an intra-proc analysis
   that iters over every function) *)

module type RADAR_PROC_TRANSFER =
  sig
    include InterDataflow.ProcTransfer
    val initStats : Callg.callG -> Scc_cg.sccGraph -> unit
  end


module type RADAR =
  sig
    (* PASS 2 : sequential analysis plus pseudo access creation *)
    module SeqTransfer : RADAR_PROC_TRANSFER
    module SeqDataflow : InterDataflow.S

    (* PASS 2.5 : pessimistic adjusted analysis (Steensgaard) *)
    module PessTransfer : RADAR_PROC_TRANSFER
    module PessDataflow : InterDataflow.S

    (* PASS 3 : answer pseudo races using Relay and RelayNoLocks *)
    module PseudoRacePass : Radar_race.PASS3DRIVER

    (* PASS 4 : Radar-adjusted analysis *)
    module AdjLTransfer : RADAR_PROC_TRANSFER
    module AdjLDataflow : InterDataflow.S

    (* PASS 4.5 : RadarNoLocks-adjusted analysis *)
    module AdjNLTransfer : RADAR_PROC_TRANSFER
    module AdjNLDataflow : InterDataflow.S
  end


module Radar (R : RADAR_FIXPOINT_PASS) (N : NONFIX_PASSES) : RADAR =
  struct
    module SeqTransfer = MakeBUTransfer (R.RA_SEQ) (R.SeqDF) (N)
    module SeqDataflow = InterDataflow.BottomUpDataflow (SeqTransfer)

    module PessTransfer = MakeBUTransfer (R.RA_PESS) (R.PessDF) (N)
    module PessDataflow = InterDataflow.BottomUpDataflow (PessTransfer)

    module PseudoRacePass = Radar_race.MakePseudoRacePass 
      (struct
         let parL = R.RA_ADJ_L.parL
         let parNL = R.RA_ADJ_NL.parNL
       end)
      (* parL/NL are same in all RA's, so pick any *)

    module AdjLTransfer = MakeBUTransfer (R.RA_ADJ_L) (R.AdjLDF) (N)
    module AdjLDataflow = InterDataflow.BottomUpDataflow (AdjLTransfer)

    module AdjNLTransfer = MakeBUTransfer (R.RA_ADJ_NL) (R.AdjNLDF) (N)
    module AdjNLDataflow = InterDataflow.BottomUpDataflow (AdjNLTransfer)
  end

class type anaClassType = object
  method initStats : Callg.callG -> Scc_cg.sccGraph -> unit
  method computeAll : Callg.callG -> Scc_cg.sccGraph -> unit
  method beforeDataflow : Config.settings -> Callg.callG -> Scc_cg.sccGraph -> unit
  method afterDataflow : Callg.callG -> string -> unit
end

module type ANA = sig
  class anaClass : anaClassType
(*  module Transfer : RADAR_PROC_TRANSFER
  module Dataflow : InterDataflow.S
  val beforeDataflow : Config.settings -> Callg.callG -> unit
  val afterDataflow : Callg.callG -> string -> unit
*)
end

 
(******************************************************************************)
(*** TESTING                                                                ***)
(******************************************************************************)

(*
module IntSumm_ = struct
  type t = int
  type simpleSum = t
  let simplify s = s
  let desimplify s = s
  let initVal = 0
end

module IntSumm = Safer_sum.Make (IntSumm_)

let seqsums = new IntSumm.data (Backed_summary.makeSumType (catSeq "test"))
let adjsums = new IntSumm.data (Backed_summary.makeSumType (catAdj "test"))
let pardb = new Pseudo_access.PARsummary.data 
  (Backed_summary.makeSumType (catPar "test"))
  
class latticeTest = object
  method stateSubset (a:int) (b:int) = a < b
  method combineStates (a:int) (b:int) = max a b
  method isBottom (a:int) = a = 0
  method bottom = -1
  method initialState = 0
  method summaryOutput (fk:fKey) (a:int) = a  
  method updateSummary (fk:fKey) (a:int) (b:int) = ()
  method serializeAndFlush = ()
  method printSummary (fk:fKey) = ()
  (* TODO abstract this common setup out *)
  val mutable thesums = seqsums (* BEWARE arbitrary init value *)
  method setTheSums s = thesums <- s
  method sumTyp = thesums#sumTyp
end

class ['st] seqTest stMan = object(self) inherit ['st] idTransFunc stMan
  method dependsOn (_:'st) = Lvs.empty
  method havoc (state:'st) (_:crumbsandlvals) = state
end

module R = struct
  type st = int
  let name = "dummy_dataflow"
  let summaryExt = "test"

  let stMan = (new latticeTest :> int stateLattice) (* ugly *)
  let printState = print_int
  
  let seqsums = seqsums
  let adjsums = adjsums
  let pardb = pardb

  let seq = new seqTest stMan
  (*let adj = new radarAdjTransFunc seq stMan pardb*)

  let scopeIt (_:fundec) summ = summ
end


module MyRadar = Radar (R)
module MySeqDF = MyRadar.SeqDataflow
module MyAdjDF = MyRadar.AdjDataflow
*)

