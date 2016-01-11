
(** Construct and dump the callgraph based on the dataflow-information *)

open Cil
open Fstructs
open Callg
open Pretty
open Type_utils
open Fp_rci_types
open Fp_rci_lattice_ops
open Fp_rci_store
open Fp_rci_focus
open Fp_rci_utils
open Fp_rci_intraproc
open Fp_rci_summary
open Logging
open Cildump


module IDF = Fp_rci_anal.InterDF
module PPH = Ciltools.PPHash

type unreachKind = 
    UnreachIndir
  | UnreachDir of fKey

(* Visitor for function (caller) *)
class funcLevelCGVis ciCG stLat transF = object (self) 
  inherit Pp_visitor.ppVisitor as super

  val mutable curInfo = IDF.dummyDfInfo
  val mutable curFunID = (-1, "")
  val mutable curGraph = emptyCG
  val mutable curFunc = dummyFunDec
  val mutable curFile = ""

  method setFile (filename : string) : unit =
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

  method private getCurFunID () =
    curFunID

  method setFunc f funID st = 
    curFunc <- f;
    curFunID <- funID;
    curInfo <- st;
    (* Make sure there's a node even if it doesn't have any outgoing edges.
       Also, update the defFile if needed... *)
    try 
      let node = FMap.find (self#getCurFunID ()) curGraph in
      node.defFile <- curFile;
      node.hasBody <- true;
    with Not_found ->
      let node = self#freshNode curFunc.svar curFile (f.sbody.bstmts <> []) in
      curGraph <- FMap.add (self#getCurFunID ()) node curGraph
    
  method private getDataflowInfo pp =
    try IDF.getDfInfo curInfo pp
    with Not_found -> stLat#bottom

  method private evalFunPtr st ptrExp accessPath =
    let emisc = { (emptyMisc curFunc) with
                    lookupHook = captureAccessPath accessPath; } in
    eval st ptrExp emisc

  method vinst (i:instr) : instr list visitAction =
    super#setInstrPP i;
    let pp = getCurrentPP () in
    let st = self#getDataflowInfo pp in
    (match i with 
       Call (_, callexp, acts, loc) ->
         if isTopState st then 
           self#handleTopCall callexp pp loc st 
         else begin match callexp with
           Lval (Mem(ptrExp), NoOffset) ->
             (* TODO: sync up w/ fp_intraproc ... *)
             if stLat#isBottom st then 
               self#noteCallUnreachable UnreachIndir pp loc
             else 
               let accessPath = ref [] in
               (try 
                  let ptrVal, st = self#evalFunPtr st ptrExp accessPath in
                  let callSig = getFunSigPtrExp ptrExp in
                  self#handleFPCall callSig acts pp loc st ptrVal accessPath
                with NullException ->
                  self#noteFunkyCall "null" pp loc)
                 
         | Lval (Var(funVar), NoOffset) ->
             if stLat#isBottom st then begin
               self#noteCallUnreachable (UnreachDir funVar.vid) pp loc;
             end else 
               let withBody sum sumKey mContext =
                 self#addDirectCall pp sumKey in
               self#doCall acts withBody funVar st
         | _ -> failwith "unknown callexp" 
         end
     | _ -> ()
    ); 
    self#bumpInstr 1;
    DoChildren

  method private withoutBody acts funVar st withBody () =
    let fkey = funVar.vid in
    self#warnNoBody funVar.vname;
    (* Should still add an edge *)
    withBody bottomSummary (fkey_to_fid fkey) 
      (makeMatchContext curFunc None st curFunc.svar.vname 
         curFunc.sformals acts)

  method private doCall acts withBody funVar st =
    let fkey = funVar.vid in
    let readerOpt = None in
    let withoutBody = self#withoutBody acts funVar st withBody in
    let rec useContextOrRefine state =
      try
        getUseCallContext ciCG curFunc readerOpt state acts fkey 
          withBody withoutBody
      with SubstFPImprecise (revAccs, targs, cont) ->
        FLocSet.iter
          (fun focusTarg ->
             let startSt = focus revAccs focusTarg cont.cState in
             useContextOrRefine startSt
          ) targs
    in
    (try useContextOrRefine st
     with Fp_rci_summary.ContextNotReady (mContext, tempsum, sumKey) ->
       logErrorF "cg construct ContextNotReady %s\n"
         (Summary_keys.string_of_sumKey sumKey);
       withBody tempsum sumKey mContext
    )


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
        let sumKey = Fp_rci_summary.makeTopSumKey vi.vid in
        self#addDirectCall pp sumKey

    | Lval(Mem(ptrExp), NoOffset) ->
        let callSig = getFunSigPtrExp ptrExp in
        let targets = Alias.deref_funptr ptrExp in
        List.iter
          (fun vid ->
             let vi = varinfoOfVid vid in 
             if functionTypeMatches vi.vtype callSig then
               let sumKey = Fp_rci_summary.makeTopSumKey vid in
               self#addIndirectCall pp sumKey
          ) targets

    | CastE (t, e') ->
        self#handleTopCall e' pp loc st
    | Lval _ | StartOf _ | AddrOf _ | BinOp _ | UnOp _ | Const _ 
    | SizeOf _ | SizeOfStr _ | SizeOfE _ | AlignOfE _ | AlignOf _ ->
        failwith "unknown call expression"

  (* TODO: sync up w/ FP_intraproc? *)
  method private handleFPCall callSig acts pp loc st ptrVal accessPath =
    let handleVar only var =
      if nullVar#isVar var then 
        self#noteFunkyCall "null" pp loc
      else if nfpVar#isVar var then
        self#noteFunkyCall "nfp" pp loc
      else if extVar#isVar var then
        self#noteFunkyCall "ext" pp loc 
      else 
        match var with
          FVar vid ->
            let vi = varinfoOfVid vid in
            if functionTypeMatches vi.vtype callSig then
              let st = 
                if not only && !accessPath <> [] 
                then focus !accessPath (var, noOffSum) st 
                else st
              in
              let withBody sum sumKey mContext =
                self#addIndirectCall pp sumKey in
              self#doCall acts withBody vi st
                
        | FHeap _ | FRet _ -> self#noteFunkyCall "heap" pp loc
        | FInput _ -> self#noteFunkyCall "input" pp loc
    in
    match ptrVal with
      FpRef var ->
        handleVar true var
    | Refs ls ->
        let ls = stripSpecial ls in
        let targs = FLocSet.cardinal ls in
        let only = targs <= 1 in
        FLocSet.iter (fun (addr, o) -> handleVar only addr) ls;
        if targs == 0 then
          self#noteFunkyCall "null" pp loc
    | FInt _ ->
        self#noteFunkyCall "null" pp loc
    | FNFP ts ->
        self#noteFunkyCall "nfp" pp loc
    | _ -> ()

  method private checkCaller () =
    try FMap.find (self#getCurFunID ()) curGraph
    with Not_found ->
      failwith "should already have caller node"

  (** Make sure there's a node for the callee... or change the dump format? *)
  method private checkCallee calleeID =
    if not (FMap.mem calleeID curGraph) then begin
      let fkey = fid_to_fkey calleeID in
      let fvar = Cilinfos.getVarinfo fkey in
      let node = self#freshNode fvar "tbd" false in
      curGraph <- FMap.add calleeID node curGraph
    end

  method private addIndirectCallees pp oldCallees newIds =
    try
      snd (List_utils.listFindReplace
             (fun oldcall _ ->
                match oldcall with CDirect _ -> false
                | CIndirect (pp2, _) -> pp = pp2)
             (fun oldcall _ ->
                match oldcall with CDirect _ -> failwith "mismatch"
                | CIndirect (pp2, old) ->
                    CIndirect (pp2, mergeIndirectTargs old newIds) )
             (CIndirect (pp, newIds)) oldCallees)
    with Not_found ->
      addCallee (CIndirect (pp, newIds)) oldCallees
        

  method private addIndirectCall pp calleeID =
    let oldNode = self#checkCaller () in
    self#checkCallee calleeID;
    let newcallees = self#addIndirectCallees pp oldNode.ccallees [calleeID] in
    let newNode = 
      { oldNode with ccallees = newcallees; } in
    curGraph <- FMap.add (self#getCurFunID ()) newNode curGraph
      
  method private addDirectCall pp calleeID =
    let oldNode = self#checkCaller () in
    self#checkCallee calleeID;
    let newNode = 
      { oldNode with 
          ccallees = addCallee (CDirect (pp, calleeID)) oldNode.ccallees
      } in
    curGraph <- FMap.add (self#getCurFunID ()) newNode curGraph

  method private addNoOutEdges pp =
    let oldNode = self#checkCaller () in
    let newcallees = self#addIndirectCallees pp oldNode.ccallees [] in
    let newNode = 
      { oldNode with ccallees = newcallees; } in
    curGraph <- FMap.add (self#getCurFunID ()) newNode curGraph

  method private noteCallUnreachable indir pp loc =
    logStatusF "Call unreachable @ %s\n" (string_of_loc loc);
    (* At least add and 0-target edge if it is indirect *)
    match indir with
      UnreachIndir -> self#addNoOutEdges pp
    | UnreachDir fkey ->
        (* Call the "wildcard" context -- still kind of funky.
           Because we didn't analyze this wildcard context, it will look
           like we don't have the body for the callee... *)
        let fid = fkey_to_fid fkey in
        self#addDirectCall pp fid
      
  method private noteFunkyCall str pp loc =
    logStatusF "%s call @ %s\n" str (string_of_loc loc);
    (* At least add and 0-target edge *)
    self#addNoOutEdges pp
      
end


(* Visitor for file *)
class contextSensCGConst ciCg funcVis stLat transF = object (self) 
  inherit nopCilVisitor

  val mutable curGraph = emptyCG

  method setFile (filename : string) : unit =
    funcVis#setFile filename

  method getGraph () : callG =
    curGraph

  method vfunc f : fundec Cil.visitAction =
    let hasBod = not (f.sbody.bstmts = []) in
    if hasBod then begin
      logStatusF "Getting call edges for %s\n" f.svar.vname;
      IDF.foldOnFKey 
        (fun funID st () ->
           funcVis#setGraph curGraph;
           logStatusF "Processing instance %s\n" 
             (fid_to_string funID);
           funcVis#setFunc f funID st;
           let _ = visitCilFunction (funcVis :> cilVisitor) f in
           curGraph <- funcVis#getGraph ()
        ) (funToKey f) (); (* Use () on fold op to simulate iter *)
    end else
      logStatusF "Skipping call edges (no body) for %s\n" f.svar.vname;
    SkipChildren
      
end


let buildCSCG ciCg root outFile =
  logStatusF "Dumping the cg: %s\n------------------------------\n" outFile;
  let funcVis = new funcLevelCGVis ciCg lattice transF in
  let vis = new contextSensCGConst ciCg funcVis lattice transF in
  Filetools.walkDir 
    (fun ast filename ->
       vis#setFile ast.fileName;
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
    ) root;
  let oc = open_out outFile in
  let cg = vis#getGraph () in
  writeSomeCalls oc cg;
  close_out oc
    

    
(************************************************************)
    

(* Test precision of context insensitivity *)
    
(* Visitor for function (caller) *)
class contextInsFunVis ciCG stLat transF = object (self) 
  inherit funcLevelCGVis ciCG stLat transF as super

  method private ciKey key = 
    fkey_to_fid key
      
  method private projectKey funID = 
    let fk = fid_to_fkey funID in
    self#ciKey fk

  (** Override to pick input-free context always *)
  method private getCurFunID () =
    self#projectKey curFunID

  (** Override to pick input-free context always *)
  method private doCall acts withBodyOld funVar st =
    let fkey = funVar.vid in
    let readerOpt = None in
    (* Override this *)
    let withBody sum sumKey mContext =
      let ciKey = self#projectKey sumKey in
      withBodyOld sum ciKey mContext
    in
    let withoutBody = self#withoutBody acts funVar st withBody in
    match getFormals ciCG fkey with
      Some (fnode, formals) ->
        if fnode.Callg.hasBody then 
          let sum, sumKey, mContext = 
            (* Skip the matching here *)
            bottomSummary, (self#ciKey fkey), 
            (makeMatchContext curFunc readerOpt st curFunc.svar.vname
               curFunc.sformals acts) in
          withBody sum sumKey mContext
        else withoutBody ()
    | None -> withoutBody ()

end

(* Test precision of field insensitivity  + context insensitivity *)

let buildCICG ciCg root outFile =
  let outFile = outFile ^ ".cont_ins" in
  logStatusF "Dumping the context ins cg: %s\n------------------------------\n"
    outFile;
  let ciVis = new contextInsFunVis ciCg lattice transF in
  let vis = new contextSensCGConst ciCg ciVis lattice transF in
  Filetools.walkDir 
    (fun ast filename ->
       vis#setFile ast.fileName;
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
    ) root;
  let oc = open_out outFile in
  let cg = vis#getGraph () in
  writeSomeCalls oc cg;
  close_out oc



(************************************************************)

(* Visitor for function (caller) *)
class fieldInsContextInsFunVis ciCG stLat transF = object (self) 
  inherit contextInsFunVis ciCG stLat transF as super
    
  (** Override to merge fields before eval'ing *)
  method private evalFunPtr st ptrExp accessPath =
    let accessPath = ref [] in
    let emisc = { (emptyMisc curFunc) with
                    lookupHook = captureAccessPath accessPath; } in
    (* HACK TO bring in globals : run eval 2x *)
    let _, st = eval st ptrExp emisc in
    let fiSt = makeFieldInsens st in
    eval fiSt ptrExp emisc
  
end

(* Test precision of field insensitivity  + context insensitivity *)

let buildFICICG ciCg root outFile =
  let outFile = outFile ^ ".field_ins_cont_ins" in
  logStatusF "Dumping the field insens cg: %s\n------------------------------\n"
    outFile;
  let fiVis = new fieldInsContextInsFunVis ciCg lattice transF in
  let vis = new contextSensCGConst ciCg fiVis lattice transF in
  Filetools.walkDir 
    (fun ast filename ->
       vis#setFile ast.fileName;
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
    ) root;
  let oc = open_out outFile in
  let cg = vis#getGraph () in
  writeSomeCalls oc cg;
  close_out oc
