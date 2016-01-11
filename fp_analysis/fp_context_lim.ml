
(** Module that tracks known function contexts to match them / may limit *)

open Fp_types
open Fp_lattice_ops
open Fp_store
open Fp_summary
open Logging
open Summary_keys

(** Nop / not matching version *)
class noLimitContext = object
  
  method matchContext (fkey: Fstructs.fKey) (con:fpState) = 
    (con, makeSumKey (fkey, con))

end 


(**** Support for context-limiters that need history of old contexts ****)

(* Use summary module as map from fKey to list of used contexts *)
module ContextTrack = struct

  type t = (fpState * sumKey) list  
  type simpleSum = t

  let simplify stList =
    List.map (fun (st, sk) -> (Fp_hashcons.hc_state st, sk)) stList

  let desimplify stList =
    simplify stList

  let initVal = []
  let unknownSummary = []

end

module ContextTrackSum = Cache_sum.Make (ContextTrack)

(************************************************************)

(** Basic k-limiter *)

(* let dummyContext = topState *)
let dummyContext = bottomState

class virtual kLimitContext limit settings cg sccCg = object (self)

  val contexts = new ContextTrackSum.data 64 
    (Backed_summary.makeSumType "contexts")
  
  initializer begin
    Backed_summary.registerType contexts;    
      (* Bleh... to force initialization of summaries for body-less funcs *)
    contexts#initSummaries settings cg sccCg;
    logStatusF "limiting contexts to %d\n" limit
  end  

  method private doLimit fkey conDBKey con knownContexts =
    let numConts = List.length knownContexts in
    if limit > 0 && numConts >= limit then begin
      logStatus "lim:> context";
      (dummyContext, (fkey, string_of_int (numConts + 1)))
    end else
      let sumKey = (fkey, string_of_int numConts) in
      let knownContexts = (con, sumKey) :: knownContexts in
      contexts#addReplace conDBKey knownContexts;
      con, sumKey

  method virtual matchContext : Fstructs.fKey -> fpState -> (fpState * sumKey)
      
end



(** Reuse contexts for new inputs that in the same equivalence class
    as an old input where the equiv relation is roughly same vars
    and FP refs are the same *)
class equivModFPContext limit settings cg sccCG = object (self)
  inherit kLimitContext limit settings cg sccCG as super
  
  initializer begin
    logStatus "Merging contexts that are equiv modulo FP / C type";
  end

  val dummySK = (-1, "")

  method private findUpdateMatch con knownContexts =
    (* Hmm... turn it into a map to make search faster? *)
    List_utils.listFindReplace 
      (fun (st, sk) (con, _) -> eqStModFP st con)
      (fun (st, sk) (con, _) ->
         let newMatchSt = combineStates st con in
         let newMatchSt = 
           if newMatchSt == st then newMatchSt
           else Fp_hashcons.hc_state newMatchSt in
         (newMatchSt, sk))
      (con, dummySK) knownContexts

  method matchContext (fkey: Fstructs.fKey) (con:fpState) =
    let sumKey = inputFreeSumKey fkey in 
    let knownContexts = contexts#find sumKey in
    try
      let (oldCon, oldSk), knownContexts = 
        self#findUpdateMatch con knownContexts in
      contexts#addReplace sumKey knownContexts;
      (oldCon, oldSk)
    with Not_found ->
      self#doLimit fkey sumKey con knownContexts
        
end


(************************************************************)


(** Limit RESTARTS (for individual contexts, not limit contexts) *)

let numRestarts = Hashtbl.create 7

let restartLim = ref 10
let set_restartLim x = 
  logStatusF "Limiting restarts to %d\n" x;
  restartLim := x

let switchToFlowInsensitive = ref true

let limitRestarts (sumKey: sumKey) state =
  let old = try Hashtbl.find numRestarts sumKey with Not_found -> 0 in
  if old > !restartLim then begin
    logStatus "lim:> restart";
    if !switchToFlowInsensitive then
      (state, true)
    else
      (makeFieldInsens state, false)
  end else begin
    Hashtbl.replace numRestarts sumKey (old + 1);
    (state, false)
  end
  
