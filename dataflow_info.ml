(** Dataflow info that can be paged in and out (at a function granularity) *)

open Cil
open Fstructs


module PPH = Ciltools.PPHash
  
(* Hmm... bundle it with the worklist too? 
   The rciDataflow engine ties the dataflowInfo cache w/ the worklist 
   to prevent thrashing (but that may end up changing worklist policy)... 
   Leave this as something simple... *)

(** Hook this into each individual analysis class (the compute method) *)


(************************************************************)

(*** Helper types to allow different kinds of dataflow info to be tracked ***)

type 'a dfKind = 
    FlowSens of 'a PPH.t
  | FlowInsens of 'a
  | Abandoned of 'a dfKind

let makeAbandonedData oldKind =
  match oldKind with
    Abandoned _ -> oldKind
  | _ -> Abandoned oldKind


(************************************************************)

(** Functor inputs for generating a dataflow info manager *)
module type DataflowInfoRequire = sig
  
  type funID
  type dfInfo
  type miscData  (* Other data that may be attached to the df info 
                   e.g., what was the original input *)

  val equal : funID -> funID -> bool
  val hash : funID -> int
  val funIDToSumKey : funID -> Summary_keys.sumKey

  val cacheSize : int
  val sumType : string  (** short identifier to use as summary type *)

  val bottom : dfInfo
  val initialMisc : miscData

end

(** Functor output: a dataflow info manager *)
module type DataflowInfoManage = sig

  type dfInfo
  type miscData
  type funID

  type funData = {
    mutable dfInfo : dfInfo dfKind; (** allow this to manage flow insens too *)
    mutable dfMisc : miscData;
  }

  (** Check out from store and initialize dataflow info with given input
      (if it is not initialized) *)
  val checkOutFlowSens : funID -> fundec -> dfInfo -> funData
  val checkOutFlowInsens: funID -> fundec -> dfInfo -> funData
    (* Never need to initialize abandoned *)
    
  (** Check into the store the updated dataflow info  *)
  val updateFunData : funID -> funData -> unit

end


(** Functor that generates the DF info manager *)
module Make(I:DataflowInfoRequire) : DataflowInfoManage = struct

  type funID = I.funID
  type dfInfo = I.dfInfo
  type miscData = I.miscData

  type funData = {
    mutable dfInfo : dfInfo dfKind; (** allow this to manage flow insens too *)
    mutable dfMisc : miscData;
  }
  let dummyFunData = None

  module HFuns = 
  struct 
    type t = funID
    let equal a b = I.equal a b
    let hash x = I.hash x
  end

  module FH = Cache.Make (HFuns)

  module FPDFInfo  = struct
    type t  = funData option
    type simpleSum  = t
    let simplify x = x
    let desimplify x = x
    let initVal = dummyFunData
    let unknownSummary = dummyFunData
  end

  module FPDFSums = Backed_summary.Make(FPDFInfo)


  (* In-memory bits *)
  let funCache = FH.create I.cacheSize

  (* Another copy of it for writing to disk *)
  let dfinfoSums = new FPDFSums.data 
    (Backed_summary.makeSumType (I.sumType ^ "_df"))
  let () = Backed_summary.registerType dfinfoSums

  let addToCache sumKey funID fInfo =
    dfinfoSums#addReplace sumKey fInfo;
    match FH.add funCache funID fInfo with
      None -> () (* nothing evicted *)
    | Some funID' -> 
        let sumKey' = I.funIDToSumKey funID' in
        dfinfoSums#flushOne sumKey'


  (************************************************************)
  let checkOut funID cfg input initialize = 
    let sumKey = I.funIDToSumKey funID in
    let fInfoOpt = dfinfoSums#find sumKey in
    match fInfoOpt with
      None ->
        let fInfo = initialize funID cfg input in
        addToCache sumKey funID (Some fInfo);
        fInfo
    | Some fInfo ->
        (* in backed_summary already, but need to keep count also *)
        addToCache sumKey funID fInfoOpt; 
        fInfo

  (************************************************************)

  let initializeFlowSens funID cfg input =
    let dfInfo = PPH.create (List.length cfg.sallstmts) in
    (match cfg.sallstmts with
       start :: rest -> 
         PPH.add dfInfo (getStmtPP start) input;
         List.iter (fun s -> 
                      PPH.add dfInfo (getStmtPP s) I.bottom) rest;
     | [] -> failwith "cfg has no statements?");
    { dfInfo = FlowSens dfInfo;
      dfMisc = I.initialMisc; }


  let checkOutFlowSens funID cfg input = 
    checkOut funID cfg input initializeFlowSens

  (************************************************************)

  let initializeFlowInsens funID cfg input =
    { dfInfo = FlowInsens input;
      dfMisc = I.initialMisc; }

  let checkOutFlowInsens funID cfg input =
    checkOut funID cfg input initializeFlowInsens

  (************************************************************)
      
  let updateFunData funID fInfo =
    let sumKey = I.funIDToSumKey funID in
    addToCache sumKey funID (Some fInfo)

end


(************************************************************)

(** Copy per-program-point data over to per statement hashtbl *)
let ppHashToSHash pph destStmtHash = 
  PPH.iter 
    (fun pp d ->
       match isStmtPP pp with
         Some sid -> Inthash.replace destStmtHash sid d
       | None -> ()
    ) pph
  
