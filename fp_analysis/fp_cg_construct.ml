
(** Construct and dump the callgraph based on the dataflow-information *)

(* TODO: still dump direct-call edges when unreachable... *)

open Cil
open Fstructs
open Callg
open Pretty
open Type_utils
open Fp_types
open Fp_lattice_ops
open Fp_store
open Fp_utils
open Fp_intraproc
open Logging
open Cildump


module IDF = Fp_analysis.InterDF
module PPH = Ciltools.PPHash

(* Visitor for function (caller) *)
class funcLevelCGVis stLat transF = object (self) 
  inherit Pp_visitor.ppVisitor as super

  val mutable curInfo = IDF.dummyState
  val mutable curFunID = dummyFID
  val mutable curGraph = emptyCG
  val mutable curFunc = dummyFunDec
  val mutable curFile = ""

  method setFile filename =
    logStatusF "Starting file %s\n" filename;
    curFile <- filename

  method getGraph () : callG =
    curGraph

  method setGraph g =
    curGraph <- g

  method private freshNode funcVar defF hasBod = 
    { name = funcVar.vname;
      typ = string_of_ftype funcVar.vtype;
      defFile = defF;
      hasBody = hasBod;
      ccallees = [];
      ccallers = []; }

  method setFunc f funID st = 
    curFunc <- f;
    curFunID <- funID;
    curInfo <- st;
    (* Make sure there's a node even if it doesn't have any outgoing edges.
       Also, update the defFile if needed... *)
    try 
      let node = FMap.find curFunID curGraph in
      node.defFile <- curFile;
      node.hasBody <- true;
    with Not_found ->
      let node = self#freshNode curFunc.svar curFile true in
      curGraph <- FMap.add curFunID node curGraph

  method private noteCallUnreachable loc =
    logStatusF "Call unreachable @ %s\n" (string_of_loc loc)

  method private noteFunkyCall str loc =
    logStatusF "%s call @ %s\n" str (string_of_loc loc)
    
  method private getDataflowInfo pp =
    try IDF.getState curInfo pp
    with Not_found -> stLat#bottom

  method vinst (i:instr) : instr list visitAction =
    super#setInstrPP i;
    let pp = getCurrentPP () in
    let st = self#getDataflowInfo pp in
    (match i with 
       Call (_, callexp, acts, loc) ->
         if stLat#isBottom st then self#noteCallUnreachable loc
         else if isTopState st then self#handleTopCall callexp pp loc st 
         else 
           (match callexp with
              Lval (Mem(ptrExp), NoOffset) ->
                (* Find targets *)
                (* TODO: sync up w/ fp_intraproc ... *)
                let accessPath = ref [] in
                let emisc = { emptyMisc with
                                lookupHook = captureAccessPath accessPath; } in
                (try 
                   let ptrVal, st = eval st ptrExp emisc in
                   let callSig = getFunSigPtrExp ptrExp in
                   self#handleFPCall callSig acts pp loc st ptrVal accessPath
                 with NullException ->
                   self#noteFunkyCall "null" loc)
                  
            | Lval (Var(funVar), NoOffset) ->
                (try
                   (match transF#getCallContext acts loc st funVar.vid with
                      Some (sumKey, _, _, _, _) ->
                        self#addDirectCall pp sumKey
                    | None ->
                        self#warnNoBody funVar.vname)
                 with NullException -> (* maybe from evaluating the args *)
                   self#noteFunkyCall "null" loc)
                    
            | _ -> failwith "unknown callexp" )
             
     | _ -> ()
    ); 
    self#bumpInstr 1;
    DoChildren


  val warnedNoBody = Hashtbl.create 17
  method private warnNoBody name =
    if Hashtbl.mem warnedNoBody name then ()
    else begin
      Hashtbl.add warnedNoBody name ();
      logStatusF "No body for func: %s\n" name
    end

  (* TODO: sync up w/ FP_intraproc? *)
  method private handleTopCall callexp pp loc st =
    match callexp with
      Lval(Var(vi),NoOffset) ->
        let sumKey = Fp_summary.makeTopSumKey vi.vid in
        self#addDirectCall pp sumKey

    | Lval(Mem(ptrExp), NoOffset) ->
        let callSig = getFunSigPtrExp ptrExp in
        let targets = Alias.deref_funptr ptrExp in
        List.iter
          (fun vid ->
             let vi = varinfoOfVid vid in 
             if functionTypeMatches vi.vtype callSig then
               let sumKey = Fp_summary.makeTopSumKey vid in
               self#addIndirectCall pp sumKey
          ) targets

    | CastE (t, e') ->
        self#handleTopCall e' pp loc st
    | Lval _ | StartOf _ | AddrOf _ | BinOp _ | UnOp _ | Const _ 
    | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ | AlignOf _ ->
        failwith "unknown call expression"

  (* TODO: sync up w/ FP_intraproc? *)
  method private handleFPCall callSig acts pp loc st ptrVal accessPath =
    match ptrVal with
      FpRef vid ->
        if nullVar#isVID vid then 
          self#noteFunkyCall "null" loc
        else if nfpVar#isVID vid then
          self#noteFunkyCall "nfp" loc
        else if extVar#isVID vid then
          self#noteFunkyCall "ext" loc 
        else begin 
          let vi = varinfoOfVid vid in 
          if functionTypeMatches vi.vtype callSig then
            (try
               match transF#getCallContext acts loc st vid with
                 Some (sumKey, _, _, _, _) ->
                   self#addIndirectCall pp sumKey
               | None ->
                   self#warnNoBody vi.vname
             with NullException ->
               self#noteFunkyCall "null" loc)
        end
    | Refs ls ->
        FLocSet.iter
          (fun (addr, o) ->
             if nullVar#isVar addr then 
               self#noteFunkyCall "null" loc
             else if nfpVar#isVar addr then
               self#noteFunkyCall "nfp" loc
             else if extVar#isVar addr then
               self#noteFunkyCall "ext" loc 
             else 
               match addr with
                 FVar vid ->
                   let vi = varinfoOfVid vid in
                   if functionTypeMatches vi.vtype callSig then
                     let st = focus accessPath (addr, o) st in
                     (try
                        match transF#getCallContext acts loc st vid with
                          Some (sumKey, _, _, _, _) ->
                            self#addIndirectCall pp sumKey
                        | None ->
                            self#warnNoBody vi.vname
                      with NullException ->
                        self#noteFunkyCall "null" loc)
                   else () 
               | FHeap _ | FInput _ | FRet _ -> ()
          ) ls
     | FInt _ ->
         self#noteFunkyCall "null" loc
     | FNFP ts ->
         self#noteFunkyCall "nfp" loc
     | _ -> ()

  method private checkCaller () =
    try FMap.find curFunID curGraph 
    with Not_found -> self#freshNode curFunc.svar curFile true

  (** Make sure there's a node for the callee... or change the dump format? *)
  method private checkCallee calleeID =
    try 
      let _ = FMap.find calleeID curGraph in
      ()
    with Not_found ->
      let fkey = fid_to_fkey calleeID in
      let fvar = Cilinfos.getVarinfo fkey in
      let node = self#freshNode fvar "tbd" false in
      curGraph <- FMap.add calleeID node curGraph

  (* TODO: port fixes from other callgraph constructor? *)
  method private addIndirectCall pp calleeID =
    let oldNode = self#checkCaller () in
    self#checkCallee calleeID;
    let newNode = 
      { oldNode with 
          ccallees = 
          List_utils.addOnce oldNode.ccallees (CIndirect (pp, [calleeID]));
      } in
    curGraph <- FMap.add curFunID newNode curGraph

  method private addDirectCall pp calleeID =
    let oldNode = self#checkCaller () in
    self#checkCallee calleeID;
    let newNode = 
      { oldNode with 
          ccallees = List_utils.addOnce oldNode.ccallees (CDirect (pp, calleeID))
      } in
    curGraph <- FMap.add curFunID newNode curGraph

end


(* Visitor for file *)
class contextSensCGConst stLat transF = object (self) 
  inherit nopCilVisitor

  val mutable curGraph = emptyCG
  val funcVis = new funcLevelCGVis stLat transF

  method setFile filename =
    funcVis#setFile filename

  method getGraph () : callG =
    curGraph

  method vfunc f : fundec Cil.visitAction =
    let hasBod = not (f.sbody.bstmts = []) in
    if hasBod then begin
      logStatusF "Getting call edges for %s\n" f.svar.vname;
      IDF.foldOnFKey 
        (fun funID st () ->
           funcVis#setGraph curGraph; (* err... why bother? *)
           logStatusF "Processing instance %s\n" (fid_to_string funID);
           funcVis#setFunc f funID st;
           let _ = visitCilFunction (funcVis :> cilVisitor) f in
           curGraph <- funcVis#getGraph ()
        ) (funToKey f) (); (* Use () on fold op to simulate iter *)
    end;
    SkipChildren


end


let buildCSCG root outFile =
  logStatusF "Dumping the cg: %s\n------------------------------\n" outFile;
  let vis = new contextSensCGConst lattice transF in
  Filetools.walkDir 
    (fun ast filename ->
       vis#setFile ast.fileName;
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
    ) root;
  let oc = open_out outFile in
  let cg = vis#getGraph () in
  writeSomeCalls oc cg;
  close_out oc
