
(** Generate witnesses from pointer analysis results *)

open Pta_types
open Pta_shared
open Cil
open Pretty
open Logging

module HC = Simplehc
module PC = Pta_compile
module Dist = Distributions

(******* Manage output *******)

(** Write output to a file in [outDir], or write to stdout if not specified *)
let outDir = ref ""

let nastyChars = Str.regexp "[^_a-zA-Z0-9]"

(** Filter nasty characters from filename *)
let filterFilename fname =
  Str.global_replace nastyChars "" fname

let writeOutFile fname doc =
  let out_chan = open_out fname in
  fprint out_chan ~width:80 doc;
  flush out_chan;
  let written = out_channel_length out_chan in
  logStatusF "Wrote %d bytes to %s\n" written fname;
  close_out out_chan
    
let writeOutStdout doc =
  fprint stdout ~width:80 doc

let writeOutFor elems string_of outDir doc =
  if outDir <> "" then
    if elems = [] then 
      failwith "writeOutFor: No elems"
    else
      let fname = List.fold_left (fun cur x -> cur ^ string_of x) "" elems in
      let fname = Filename.concat outDir (filterFilename fname) in
      writeOutFile fname doc
  else
    writeOutStdout doc

let writeOutFor2 lv1 lv2 doc =
  writeOutFor [lv1; lv2] string_of_ptaLv !outDir doc
    
let writeOutFor3 lv1 lv2 lv3 doc =
  writeOutFor [lv1; lv2; lv3] string_of_ptaLv !outDir doc


(******* Parse lvals in user query ******************************)

let matchVar vname vid =
  (try
     let var = Cilinfos.getVarinfo vid in
     var.vname = vname 
   with Not_found -> false)


let localMatcher idOpt vname (v:vinfo) = 
  match v.HC.node with
    PLocal (vid, _) -> 
      (match idOpt with
         None -> if matchVar vname vid then Some v else None
       | Some id -> if id = vid then Some v else None)
  | _ -> None

let globalMatcher idOpt vname (v:vinfo) =
  match v.HC.node with
    PGlobal (vid, _) -> 
      (match idOpt with
         None -> if matchVar vname vid then Some v else None
       | Some id -> if id = vid then Some v else None)
  | _ -> None


exception VarNotFound of string

let parseLvStr knownLvs lvStr : ptaLv list =
  assert (LvalH.length knownLvs > 0);
  
  (* Expect lvals to be specified as "[l, g]:[vid]?:***vname" *)
  let scope, idOpt, lvStr =
    match Str.split (Str.regexp ":") lvStr with
      [scope; lv] -> 
        (scope, None, lv)
    | [scope; id; lv] -> 
        (scope, Some (int_of_string id), lv)
    | _ -> failwith ("parseVar: " ^ lvStr)
  in

  let len = String.length lvStr in

  let findVar vname : vinfo list =
    let matcher = 
      if scope = "l" 
      then localMatcher idOpt vname
      else globalMatcher idOpt vname
    in
    let vars = LvalH.fold 
      (fun lv _ cur ->
         match matcher (baseVar lv) with
           Some v -> List_utils.addOnce cur v
         | None -> cur 
      ) knownLvs [] in
    if vars = [] then raise (VarNotFound vname) else vars
  in

  let parseVar vname : ptaHost list =
    let vars = findVar vname in 
    List.map (fun v -> makeHost (PVar v)) vars
  in

  let rec parse curIndex : ptaLv list =
    if curIndex == len then
      failwith ("parsed too much in " ^ lvStr ^ " -> " ^ string_of_int curIndex)
    else if lvStr.[curIndex] = '*' then
      let lvs = parse (curIndex + 1) in
      List.map (fun lv -> makeDeref (makeRv (PLv lv)) noOffset) lvs
    else (* assume derefs are done *)
      let varStr = (String.sub lvStr curIndex (len - curIndex)) in
      let varHosts = parseVar varStr in
      List.map (fun h -> makeLv (h, noOffset)) varHosts
  in
  parse 0


(********* Query interface ************)

module type AliasExampleGen = sig

  (* For reanalysis (if needed) *)
  val analyzeAll : string -> bool -> unit

  val whyAliased : string * string -> unit

  val whyPointsTo : string * string -> unit
    
  (* Stats *)
  val resetStats : unit -> unit

  val printStats : unit -> unit

end


(********* Steensgaard support ******************************)

module CheckSteens = struct
  
  module Steens = Pta_fi_eq
  module T = Steens.TrackedSteps
  module U = Uf.Tracked (T)
  module Solver = Steens.MakeSolver(U)

  let re_solve root =
    (* Solve and get fresh results *)
    logStatus "Processing constraints";
    flush stdout;
    Solver.solve root;
    Solver.saveState root;
    Steens.Final.saveAll root;
    logStatus "PTA Simple analysis done";
    flush stdout

  (** Load solver data (not Final data) or reanalyze to generate *)
  let analyzeAll (root : string) (fresh:bool) = begin
    loadSharedState root;
    if (not fresh) then try
      Solver.loadState root
    with Not_found -> re_solve root
    else re_solve root
  end

  let tryFindNode ptr errprefix =
    try LvalH.find Solver.lvToNode ptr
    with Not_found ->
      logError (errprefix ^ string_of_ptaLv ptr);
      raise Not_found


  (*********** Statistics ***********)

  let string_of_explain explain =
    "TODO"

  (* Just keep tracking and going... *)
  let edgeStats = ref (Dist.makeDistro ())

  let printStats () =
    Dist.printDistroSortFreq !edgeStats string_of_explain "Steens Alias Flows"

  let resetStats () =
    edgeStats := Dist.makeDistro ()

  let updateStats step =
    Dist.updateDistro !edgeStats step
      (* TODO: actually call this dude *)


  (********************************************)
        
  (** Return list of steps that cause two pointers to be aliased *)
  let explainAlias ptr1 ptr2 =
    try 
      let node1 = tryFindNode ptr1 "Can't find first ptr " in
      let node2 = tryFindNode ptr2 "Can't find second ptr " in
      
      (* TODO: add assignments that did not require unification... *)
      let steps = U.whyLinked node1 node2 in
      let doc = dprintf "Links for %s / %s\n"
        (string_of_ptaLv ptr1) (string_of_ptaLv ptr2) in
      writeOutFor2 ptr1 ptr2 
        (doc ++ indent 2 (T.dotSteps steps));

      List.iter updateStats steps;
    with Not_found ->
      () (* See if we get more... *)


  let lvParse = parseLvStr Solver.lvToNode
    
  let whyAliased ((lv1, lv2) : string * string) =
    logStatusF "Explaining aliasing between ptr %s and %s\n" lv1 lv2;
    let ptrs1 = lvParse lv1 in
    let ptrs2 = lvParse lv2 in
    logStatusF "Considering pairs of (%d, %d) ptrs\n"
      (List.length ptrs1) (List.length ptrs2);
    List_utils.listIterOrderedPairs explainAlias ptrs1 ptrs2;
    Solver.printHistories ()

  let whyPointsTo ((lv1, lv2) : string * string) =
    prerr_string "!!! whyPointsTo not implemented for CheckSteens !!!\n"

end

(********** Andersen's support *******************************)

module CheckAnders = struct
  
  (***** Settings *****)

  let maxTargets = 2

  let recurseLimit = ref (Some 1)

  let setLimit n =
    if n <= 0 then 
      recurseLimit := None
    else 
      recurseLimit := Some n

  (********************)

  module Anders = Pta_fs_dir
  module Arr = GrowArray
  module Solver = Anders.Solver
  module NS = Solver.NS

  type nodeID = Solver.nodeID
  type nodeInfo = Solver.nodeInfo

  (**** Tracking explainations ****)

  type explaination =
      Original of ptaAssign
        (** Constraint that appears in program text *)
        
    | UnexpL of ptaAssign * moreExplain
        (** Constraint that is found in the end, but needs more
            explaination for the left hand-side. 
            Supply the first avenue to try exploring further *)
        
    | UnexpR of ptaAssign * moreExplain
        (** Constraint that is found in the end, but needs more
            explaination for the rhs *)
    | UnexpFP of ptaAssign * moreExplain
        (** Unexplained function pointer-related assignment *)

  and moreExplain =
      NeedExplain of ptaLv * ptaLv
    | IsExplained of ptaLv * ptaLv * explainPath

  and explainPath = explaination list

  and explainGraph = (explaination, unit) Hashtbl.t


  (**********************************)
      
  (** Saving / Loading previous results *)

  type exlainerState = {
    baseAssigns : ((nodeID, (nodeID * Cil.location) list) Hashtbl.t);
    simpleAssigns : ((nodeID * nodeID, Cil.location) Hashtbl.t);

    complexL : ((nodeID, (nodeID * Cil.location) list) Hashtbl.t);
    complexR :  ((nodeID, (nodeID * Cil.location) list) Hashtbl.t);
    (** Complex subs different from pta_fs_dir.ml in that given *y = x ..., 
        we store the info with x instead of with y *)

    fpCalls : ((nodeID, (int * nodeID * Cil.location * ctyp) list) Hashtbl.t);
    (** Later, we will be receiving assignments of the form: 

        (formal_param = actual_param) or for returns (actual_ret = formal_ret)

        Index funpointer calls so that these mysterious new assignments
        can be explained by looking up for an actual, 
        what FP call did it participate in, that given final pts to info,
        can explain this assignment to the formal
        
        To assist in these explainations, keep this map from 
        actual -> list of (argument index, fpLval, callsite info)  *)
    
    lvToID :  nodeID LvalH.t;
    idToNode : nodeInfo Arr.t;
    idToLv : (nodeID, ptaLv) Hashtbl.t;
  }
 
  let freshState () =
    {
      baseAssigns = Hashtbl.create 17;
      simpleAssigns = Hashtbl.create 17;
      complexL = Hashtbl.create 17;
      complexR = Hashtbl.create 17;
      fpCalls = Hashtbl.create 17;
      lvToID = LvalH.create 17;
      idToNode = Arr.make 8 (Arr.Elem Solver.dummyNI);
      idToLv = Hashtbl.create 17;
    }

  let st = ref (freshState ())

  let getResultsFilename (root : string) =
    Filename.concat root (".pta_traces.anders")
      
  let saveAll root =
    let outName = getResultsFilename root in
    let out_chan = open_out_bin outName in
    Marshal.to_channel out_chan !st [Marshal.Closures] ;
    close_out out_chan
      
  let loadAll oldFile =
    let in_chan = open_in_bin oldFile in
    st := Marshal.from_channel in_chan;
    close_in in_chan

  let initID2LV () =
    LvalH.iter (fun lv id -> Hashtbl.add !st.idToLv id lv) !st.lvToID

  let copySolverState () =    
    st := { !st with lvToID = Solver.lvToID; };
    Arr.iteri (fun id (fwdId, nodeinfo) -> Arr.setg !st.idToNode id nodeinfo)
      Solver.idToNewIDInfo;
    initID2LV ()


  (***** Query final solution ******)

  let lvalOfID id =
    try 
      Hashtbl.find !st.idToLv id
    with Not_found ->
      logErrorF "Couldn't find lval for id %d\n" id;
      raise Not_found

  let addrOfID rhsID : ptaRv =
    let rhsLv = lvalOfID rhsID in
    makeAddr rhsLv

  let rvalOfID id =
    let lv = lvalOfID id in
    makeRv (PLv lv)

  let isFuncID id =
    let lv = lvalOfID id in
    isFuncLv lv

  let simplifyQueryLv ptrLv = 
    (* match w/ temp vars? Hmm, doesn't work if the ptrLv is in
       a form that was never seen in the program text ... *)
    ptrLv

  let node_of_id id =
    Arr.get !st.idToNode id

  exception IDNot_found of ptaLv

  let id_of_lval (lv : ptaLv) =
    try
      LvalH.find !st.lvToID lv
    with Not_found ->
      logErrorF "id_of_lval can't find id for: %s\n" (string_of_ptaLv lv);
      raise (IDNot_found lv)

  let node_of_lval (lv : ptaLv) =
    let id = id_of_lval lv in
    (id, node_of_id id)


  (*** Convert explainations back to search edges ***)

  let edge_of_assign assign =
    let lv = assign.lhs in
    let rv = assign.rhs in
    try
      let rvLv = getLval rv in
      (id_of_lval lv, id_of_lval rvLv)
    with
      Not_found ->
        failwith ("Unable to convert assign to edge: " ^  
                    (string_of_assign assign))
    | IDNot_found lv ->
        failwith ("Unable to convert assign to edge: " ^  
                    (string_of_assign assign) ^ " " ^ 
                    (string_of_ptaLv lv))

  (** Convert the assignment used in the explaination back into
      an (lhs, rhs) edge *)
  let explainToEdge exp =
    match exp with
      Original (assign)
    | UnexpL (assign, _)
    | UnexpR (assign, _)
    | UnexpFP (assign, _) ->
        edge_of_assign assign


  (***** Reindex original constraints ****)

  class myIndexer = object(self)
    inherit [nodeID] Anders.constraintIndexer

    (** Handle x = &y *)
    method addBaseAssign ifAdded lhs rhs loc =
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      let old = try Hashtbl.find !st.baseAssigns rhsID with Not_found -> [] in
      Hashtbl.replace !st.baseAssigns rhsID (List_utils.addOnce old (lhsID, loc))

    (** Index original constraint x = y *)
    method addSimpleAssign ifAdded lhs rhs loc =
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      Hashtbl.replace !st.simpleAssigns (lhsID, rhsID) loc

    (** Index original constraint *y = x *)
    method addComplexL ifAdded lhs rhs loc =
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      let old = try Hashtbl.find !st.complexL rhsID with Not_found -> [] in
      Hashtbl.replace !st.complexL rhsID (List_utils.addOnce old (lhsID, loc))

    (** Index original constraint x = *y *)
    method addComplexR ifAdded lhs rhs loc = 
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      let old = try Hashtbl.find !st.complexR lhsID with Not_found -> [] in
      Hashtbl.replace !st.complexR lhsID (List_utils.addOnce old (rhsID, loc))

    (** Index original constraint funptr(args) @ callinfo *)
    method addFPCall funptr (callinfo, args) = 
      let callID, _ = node_of_lval funptr in
      List.iter 
        (fun (actualRvs, position) ->
           (* Index regardless of whether it is in the return value
              position or in an actual argument position.
              Take care of that check later instead *)
           List.iter 
             (fun argRv ->
                try
                  let actualArgLv = getLval argRv in
                  let rhsID, _ = node_of_lval actualArgLv in
                  let old = 
                    try Hashtbl.find !st.fpCalls rhsID 
                    with Not_found -> [] in
                  let call = 
                    (position, callID, callinfo.cloc, callinfo.ctype) in
                  Hashtbl.replace !st.fpCalls rhsID (List_utils.addOnce old call)
                with Not_found -> ()) actualRvs
        ) args
        
  end

  let indexer = new myIndexer


  (** Record the simple assignments that were originally in the program *)
  let getOriginalAssigns base other calls = begin
    
    (* Must have generated or loaded indexes *)
    assert (LvalH.length !st.lvToID > 0);

    indexer#addBaseAssigns base;
    indexer#addAssignEdges other;
    indexer#addCallEdges calls;
      
    (* debug *)
    logStatus "Indexed FP Calls\n===================";
    Hashtbl.iter 
      (fun actID calls ->
         let doc = dprintf "actual %d ->\n" actID in
         let doc2 = indent 2 
           (seq_to_doc (text ", ") List.iter 
              (fun (position, callLvID, cloc, ctyp) ->
                 dprintf "(*%d) (%d) at %s" callLvID position
                   (string_of_loc cloc))
              calls nil) ++ line in
         logStatusD (doc ++ doc2)
      ) !st.fpCalls
  end

  (** Re-analyze to get original vs final constraints and lval <-> id mapping *)
  let analyzeAll (root:string) (fresh:bool) = begin
    loadSharedState root;
    
    let oldFile = getResultsFilename root in
    if ((not fresh) && (Sys.file_exists oldFile)) then begin
      (* load existing results *)
      logStatus "Using old PTA results";
      loadAll oldFile;
      logStatus "Old PTA results loaded";
      
    end else begin
      (* Re-run the solver to get the final constraints *)
      Solver.setCycleDetect false; 
      (* turn off cycle detection so that there aren't missing flows *)
      Solver.init root;
      Solver.solve root;
      (* don't do the final rehashing *)
      copySolverState ();

      (* Remember the initial constraints -- do this afterwards 
         so that the lval -> ID mappings are the same as if
         only the solver is run *)
      let constraintFile = PC.getConstraintFile root in
      let _, baseAssign, otherAssign, calls, _ = PC.loadFor constraintFile in
      getOriginalAssigns baseAssign otherAssign calls;
        
      saveAll root;

    end ;

  end
      

  (***** Dump results *****)

  let linearizeGraph expGraph = 
    (* TODO, make a nicer topological sort, w/ the addrOf assign at the head *)
    Hashtbl.fold (fun exp () cur -> exp :: cur) expGraph []

  let d_explain moreExplain explain : doc =
    match explain with
      Original assign ->
        text ("Orig: " ^ string_of_assign assign)
    | UnexpR (assign, more) ->
        (dprintf "UnexpR: %s\n" (string_of_assign assign)) ++
          (indent 2 (moreExplain more))
    | UnexpL (assign, more) ->
        (dprintf "UnexpL: %s\n" (string_of_assign assign)) ++
          indent 2 (moreExplain more)
    | UnexpFP (assign, more) ->
        (dprintf "FP: %s\n" (string_of_assign assign)) ++
          indent 2 (moreExplain more)

  let rec d_moreExplain moreExp : doc =
    match moreExp with
      NeedExplain (ptr, targ) ->
        dprintf "? %s ptsTo %s"
          (string_of_ptaLv ptr) 
          (string_of_ptaLv targ)
    | IsExplained (ptr, targ, exps) ->
        let listed = exps
          (* linearizeGraph exps *) in
        dprintf "%s ptsTo %s ::\n"
          (string_of_ptaLv ptr) 
          (string_of_ptaLv targ) ++
          if listed = [] then text "[???]\n"
          else seq ~sep:Pretty.line ~doit:(d_explain d_moreExplain) ~elements:listed

  let d_moreExplainTrunc moreExp : doc =
    match moreExp with
      NeedExplain (ptr, targ) ->
        dprintf "? %s ptsTo %s"
          (string_of_ptaLv ptr) 
          (string_of_ptaLv targ)
    | IsExplained (ptr, targ, exps) ->
        dprintf "%s ptsTo %s :: ..."
          (string_of_ptaLv ptr) 
          (string_of_ptaLv targ)

  let string_of_explain exp =
    sprint 80 (d_explain d_moreExplain exp)

  let string_of_explain_trunc exp =
    sprint 80 (d_explain d_moreExplainTrunc exp)

  let printExplains expPath =
    if expPath = [] then indent 2 (text "NONE\n")
    else
      List.fold_left
        (fun curDoc exp ->
         let newPart = d_explain d_moreExplain exp in
         curDoc ++ newPart ++ line
        ) Pretty.nil expPath

      
  let printFlows1 ptr targ exp =
    (* TODO: make a dot graph *)
    let header = dprintf "Flows %s  <-- &%s\n"
      (string_of_ptaLv ptr) 
      (string_of_ptaLv targ) in
    let body = printExplains exp in
    writeOutFor2 ptr targ (header ++ (indent 2 (body ++ line)))

  let printFlows2 ptr1 ptr2 targetID exp1 exp2 =
    (* TODO: make a dot graph *)
    let header = dprintf "Flows %s  <-- %s --> %s\n"
      (string_of_ptaLv ptr1) 
      (string_of_ptaRv (addrOfID targetID))
      (string_of_ptaLv ptr2) in
    let body1 = printExplains exp1 in
    let body2 = printExplains exp2 in
    let targetLv = lvalOfID targetID in
    writeOutFor3 ptr1 ptr2 targetLv
      (header ++ 
         (indent 2 
            (
              (text "ptr1:\n") ++ indent 2 body1 ++ line ++ 
                (text "ptr2:\n") ++ indent 2 body2 ++ line
            )
         )
      )

  let dotFlows1 ptr targ exp =
    ()

  let dotFlows2 ptr1 ptr2 targetID exp1 exp2 =
    ()



  (*********** Statistics ***********)

  (* Just keep tracking and going... *)
  let edgeStats = ref (Dist.makeDistro ())
  let lvStats = ref (Dist.makeDistro ())
  let modCrossStats = ref (Dist.makeDistro ())

  let checkCrossStats isCrossing distro steps =
    let rec iterStats steps = 
      let _ = List.fold_left 
        (fun prev curStep ->
           let curAssign = checkCurStep curStep in
           (match prev with 
              None -> ()
            | Some prevAssign ->
                (match isCrossing prevAssign curAssign with
                   None -> () 
                 | Some diff ->
                     Dist.updateDistro distro diff
                )
           );
           Some curAssign
        ) None steps in
      ()

    (* Recurse on the curStep if needed and get the assignment of curStep *)
    and checkCurStep curStep =
      match curStep with
        Original a -> a
      | UnexpFP (a, more) 
      | UnexpL (a, more)
      | UnexpR (a, more) ->
          (match more with
             IsExplained (_, _, moreSteps) ->
               iterStats moreSteps
           | NeedExplain (_, _) ->
               ()
          );
          a
    in
    iterStats steps

  let moduleCrossing assign1 assign2 =
    let f1 = assign1.aloc.file in
    let f2 = assign2.aloc.file in
    if f1 = f2 then
      None
    else
      let lv = assign1.lhs in
      (* rval of assign2 should be the same *)
      Some (f1, f2, lv)

  let string_of_modCross (f1, f2, lv) =
    f1 ^ " -> " ^ string_of_ptaLv lv ^ " -> " ^ f2

  let printStats () = begin
    Dist.printDistroSortFreq !edgeStats 
      string_of_explain_trunc "Anders alias flows";
    Dist.printDistroSortFreq !lvStats 
      string_of_ptaLv "Anders lvals";
    Dist.printDistroSortFreq !modCrossStats 
      string_of_modCross "Anders module crossings";
  end
      
  let resetStats () = begin
    edgeStats := Dist.makeDistro ();
    lvStats := Dist.makeDistro ();
    modCrossStats := Dist.makeDistro ();
  end
    
  let rec updateStatsExp explain =
    let updateStatsAssign ({lhs = lv; rhs = rv;}) =
      Dist.updateDistro !lvStats lv;
      (try 
         let rhs = getLval rv in 
         Dist.updateDistro !lvStats rhs;
       with Not_found -> ()
      )
    in
    Dist.updateDistro !edgeStats explain;
    (match explain with
       Original a ->
         updateStatsAssign a
     | UnexpFP (a, moreExp)
     | UnexpR (a, moreExp)
     | UnexpL (a, moreExp) ->
         updateStatsAssign a;
         (* recurse on moreExp *)
         (match moreExp with
            NeedExplain _ -> ()
          | IsExplained (_, _, exps) ->
              List.iter updateStatsExp exps
         )
    )
  

  let updateStats exps =
    List.iter updateStatsExp exps;
    checkCrossStats moduleCrossing !modCrossStats exps


  (*************** Core search *****************)

    
  (**** Goal checking ****)
      
  type goal  = nodeID -> bool

  let goalMetVar ptrID flowSrcID =
    (ptrID = flowSrcID) 

  let goalMetDeref ptrInfo flowSrcID =
    NS.mem flowSrcID ptrInfo.Solver.ptTargets

  let decideGoalBase ptrLv ptrID ptrInfo =
    let host, _ = ptrLv in
    match host.HC.node with
      PVar v -> goalMetVar ptrID
    | PDeref (innerPtr) -> 
        (try
           let innerHost, _ = getLval innerPtr in
           (match innerHost.HC.node with 
              PVar _ -> goalMetDeref ptrInfo
            | PDeref _ -> failwith "lvals not simplified"
           )
         with Not_found ->
           failwith ("decideGoal can't find lval: " ^ string_of_ptaRv innerPtr)
        )

  let decideGoal ptr =
    let ptrID, ptrInfo = node_of_lval ptr in
    decideGoalBase ptr ptrID ptrInfo

  (**** Result management ****)

  let addEdge expGraph explain =
    Hashtbl.replace expGraph explain ()

  let newExplainG () =
    Hashtbl.create 17

  (** add edges from g2 into g1 *)
  let mergeGraphs g1 g2 =
    Hashtbl.iter 
      (fun explain () -> 
         Hashtbl.replace g1 explain () 
           (* Treated as sets... doing straightforward union *)
      ) g2

  let addEdgePath explain curPath =
    explain :: curPath
 
  let newExplainPath () = []

  (**** The Search ****)

  (*** Convert search edges to explainations ***)

  let pickComplex target ptr loc =
    let ptrInfo = node_of_id ptr in
    if NS.mem target ptrInfo.Solver.ptTargets then
      Some (loc, ptr, target)
    else 
      None

  let wasComplex target possiblePtrs =
    (* Just 1 possibility -- by not trying them all, it could mean 
       that we never find the real reason though *)
     let possiblePtrs = List.filter 
      (fun (ptr, loc) ->
         match pickComplex target ptr loc with
           Some _ -> true
         | None -> false
      ) possiblePtrs in
    if possiblePtrs = [] then None
    else
      let chosen = (List_utils.pickK (List.length possiblePtrs) 1) in
      let chosenI = List.hd chosen in
      let res, _ = List.fold_left 
      (fun (cur, i) (ptr, loc) ->
         let next =
           match cur with
             Some x -> 
               (* Try to get one w/ a location that's useful *)
               if loc = Cil.locUnknown then
                 pickComplex target ptr loc
               else cur
           | None ->
               if i == chosenI then
                 pickComplex target ptr loc
               else cur
         in
         (next, i + 1)
      ) (None, 0) possiblePtrs in
      res

  let wasComplexL lID rID =
    try
      let complexLs = Hashtbl.find !st.complexL rID in
      wasComplex lID complexLs
    with Not_found ->
      None

  let wasComplexR lID rID =
    try
      let complexRs = Hashtbl.find !st.complexR lID in
      wasComplex rID complexRs
    with Not_found ->
      None

  (************************************************************)

  (** Convert fid to PTA id *)
  let getTargetFun fid =
    let lv = makeLv (makeHost (PVar (makeVar (PGlobal (fid, getFunType fid)))),
                     noOffset) in
    id_of_lval lv 

  (** Pick a funptr call that may have propagated to or from
      the lval "possibleFormOrRet" (based on argIndex) *)
  let wasFPAssigned possibleFormOrRet callLvID argIndex callTyp callLoc =
    let ptrInfo = node_of_id callLvID in
    let funs = Anders.LCDSolver.getFuncs callTyp ptrInfo.Solver.ptTargets in
    Anders.IDS.fold
      (fun fid cur ->
         try
           let possLv =
             if argIndex = retIndex then
               makeLv (Anders.host_of_ret fid, noOffset)
             else 
               makeLv (Anders.host_of_formal fid argIndex, noOffset)
           in
           let possID = id_of_lval possLv in
           if possID = possibleFormOrRet then
             (callLvID, getTargetFun fid, argIndex, callLoc) :: cur
           else cur
         with 
           Not_found ->
             Anders.warnExtern "wasFPAssigned" fid;
             cur
         | Failure "nth" ->
             Anders.warnVarargExtern "wasFPAssigned" fid argIndex;
             cur
      ) funs []

  let wasFPFormal lID rID =
    try
      let fpFormalCalls = Hashtbl.find !st.fpCalls rID in
      List.fold_left
        (fun cur (argPos, callLvID, callLoc, callTyp) ->
           if argPos <> retIndex then
             let more = wasFPAssigned lID callLvID argPos callTyp callLoc in
             List.rev_append more cur
           else 
             cur
        ) [] fpFormalCalls
    with Not_found -> []
      
  let wasFPReturn lID rID =
    try
      let fpReturnCalls = Hashtbl.find !st.fpCalls lID in
      List.fold_left
        (fun cur (argPos, callLvID, callLoc, callTyp) ->
           if argPos == retIndex then
             let more = wasFPAssigned rID callLvID argPos callTyp callLoc in
             List.rev_append more cur
           else 
             cur
        ) [] fpReturnCalls
    with Not_found -> []
      
  let wasFP lID rID =
    match wasFPFormal lID rID, wasFPReturn lID rID with
      (callLvID, targID, argIndex, callLoc) :: _, _ 
    | [], (callLvID, targID, argIndex, callLoc) :: _ ->
        Some (callLvID, targID, argIndex, callLoc)
    | [], [] ->
        None
      
  (************************************************************)

  let checkExplain lID rID =
    let lv = lvalOfID lID in
    let rv = rvalOfID rID in
    (* see if this constraint was originally available or not *)
    try 
      let loc = Hashtbl.find !st.simpleAssigns (lID, rID) in
      Original (makeAssign lv rv loc)
    with Not_found ->
      (* find out which side to expand *)
      (match wasComplexL lID rID with
         None -> 
           (match wasComplexR lID rID with
              None ->
                (match wasFP lID rID with
                   Some (callLvID, targFun, argIndex, callLoc) -> 
                     let ptrLv = lvalOfID callLvID in
                     let targLv = lvalOfID targFun in
                     let moreExp = NeedExplain (ptrLv, targLv) in
                     UnexpFP (makeAssign lv rv callLoc, moreExp)
                 | None ->
                     logErrorF
                       "Unsure why flow exists %s (%d) <- %s (%d)\n"
                       (string_of_ptaLv lv) lID (string_of_ptaRv rv) rID;
                     failwith "Unknown flow"
                )
            | Some (loc, ptr, targ) ->
                assert (targ == rID);
                let ptrLv = lvalOfID ptr in
                let rvLv = lvalOfID rID in
                let moreExp = NeedExplain (ptrLv, rvLv) in
                UnexpR (makeAssign lv rv loc, moreExp)
           )
       | Some (loc, ptr, targ) ->
           assert (targ == lID);
           let ptrLv = lvalOfID ptr in
           let moreExp = NeedExplain (ptrLv, lv) in
           UnexpL (makeAssign lv rv loc, moreExp)
      )

  (*********************************************)

  module NQueue = Queueset.Make
    (struct
       type t = nodeID
       let compare = Pervasives.compare
     end)

  module EdgeS = Set.Make 
    (struct
       type t = nodeID * nodeID
       let compare a b = Pervasives.compare a b
     end)

  type edgeSet = EdgeS.t
  let emptyEdges : edgeSet = EdgeS.empty


  (** Return shortest explaination for a flow from startNode to the goal,
      if it does flow. Doesn't use edges found in the blacklist. *)
  let rec shortestFlowTo (goal : goal) (startNode:nodeID) 
      (blacklist : edgeSet) : explainPath option =
    (* Do BFS instead to find shortest path to goal from start *)

    let reverseEdges = Hashtbl.create 7 in
    let visited = Hashtbl.create 7 in
    let worklist = NQueue.create () in
    let goalNode = ref (-1) in (* marks whether or not we found goal too *)

    let found () =
      !goalNode <> (-1)
    in

    let setFound endNode =
      goalNode := endNode
    in

    let addReverseEdge node prevNode =
      (* Take the first explaination of how to get to node *)
      if Hashtbl.mem reverseEdges node then ()
      else Hashtbl.add reverseEdges node prevNode
    in

    let rec makePathTo curPath key =
      (* Go from backwards from key to the startNode *)
      try
        if key = startNode then 
          curPath
        else if Hashtbl.mem visited key then begin
          logErrorF "makePathTo stopped on dupe node %d\n" key;
          curPath
        end else begin
          Hashtbl.add visited key ();
          let backNode = Hashtbl.find reverseEdges key in
          let explain = checkExplain key backNode in
          let newPath = addEdgePath explain curPath in
          makePathTo newPath backNode
        end
      with Not_found ->
        logErrorF "makePathTo: no reverse edge for %d\n" key;
        curPath
    in

    (* Begin body *)
    Hashtbl.clear visited;
    NQueue.addOnce startNode worklist;
    while not (found ()) && not (NQueue.is_empty worklist) do
      let cur = NQueue.pop worklist in
      if goal cur then setFound cur
      else if Hashtbl.mem visited cur then ()
        (* Avoid flowing through functions 
           (shouldn't be in subsCons in the first place, but...) *) 
      else if isFuncID cur then ()
      else begin
        Hashtbl.add visited cur ();
        let nodeInfo = node_of_id cur in
        (* Randomize the elements of subsCons first? *)
        let elts = NS.elements nodeInfo.Solver.subsCons in
        let elts = List_utils.shuffleList elts in
        List.iter 
          (fun next ->
             if EdgeS.mem (next, cur) blacklist then ()
             else begin
               addReverseEdge next cur;
               NQueue.addOnce next worklist
             end
          ) elts
      end
    done;
    
    if found () then begin
      (* Get the path to the goal *)
      Hashtbl.clear visited; (* reuse visited table... *)
      let shortestPath = makePathTo (newExplainPath ()) !goalNode in
      Some (shortestPath)
    end else
      None


  module EPC = Cache.Make (
    struct
      type t = ptaLv * nodeID * edgeSet
      let hash = Hashtbl.hash_param 16 32
      let equal = (=)
    end
  )

  let explainCache = EPC.create 64

  let doExplainPtsTo goal target blacklist =
    (* Find assignments involving &target *)
    try
      let initialFlows = Hashtbl.find !st.baseAssigns target in
      (* Pick one of the initialFlows *)
      List.fold_left 
        (fun (curResult) (firstHop, loc) ->
           if curResult <> [] then curResult
           else 
             (match shortestFlowTo goal firstHop blacklist with
                None -> curResult
              | Some expPath ->
                  (* Base edges are explained *)
                  let firstExp = 
                    Original 
                      (makeAssign (lvalOfID firstHop) (addrOfID target) loc) in
                  let newPath = addEdgePath firstExp expPath in
                  newPath
             ) 
        ) [] initialFlows
    with Not_found ->
      let lv = lvalOfID target in
      failwith ("Can't find base assign for: " ^ string_of_int target ^ " "
                ^ string_of_ptaLv lv)

  let explainPtsTo goalPtr goal target (blacklist:edgeSet) : explainPath =
    logStatusF "Size of blacklist: %d\n" (EdgeS.cardinal blacklist);
    flush stdout;
    try
      let result = EPC.find explainCache (goalPtr, target, blacklist) in
      logStatus "Found dupe query\n";
      result
    with Not_found ->
      let result = doExplainPtsTo goal target blacklist in
      ignore (EPC.add explainCache (goalPtr, target, blacklist) result);
      result

  let doExplainMore ptr targ blacklist =
    let goal = decideGoal ptr in
    let targID, _ = node_of_lval targ in
    explainPtsTo ptr goal targID blacklist


  let replaceExp g oldE newE =
    Hashtbl.remove g oldE;
    Hashtbl.add g newE ()



  (** Expand a "top-level" explaination path *)
  let explainMore expPath : explainPath * bool =
    let rec iter blacklist curPath =
      let changed = ref false in

      let helpExplainMore origAssign ptr targ =
        let blacklist = EdgeS.add (edge_of_assign origAssign) blacklist in
        let exps = doExplainMore ptr targ blacklist in
        if exps = [] then begin
          logErrorF "Oh No! doExplainMore returned 0 explainations: %s\n"
            (string_of_assign origAssign);
          logError ""
        end;
        changed := true;
        exps
      in

      let newG = List.map
        (fun oldExp -> 
           match oldExp with
             Original _ -> oldExp
           | UnexpL (a, NeedExplain (ptr, targ)) ->
               let exps = helpExplainMore a ptr targ in
               UnexpL (a, IsExplained (ptr, targ, exps))

           | UnexpR (a, NeedExplain (ptr, targ)) ->
               let exps = helpExplainMore a ptr targ in
               UnexpR (a, IsExplained (ptr, targ, exps))

           | UnexpFP (a, NeedExplain (ptr, targ)) ->
               let exps = helpExplainMore a ptr targ in
               UnexpFP (a, IsExplained (ptr, targ, exps))
              
           | UnexpL (a, IsExplained (ptr, targ, expG)) ->
               (* recurse on the inner graph *)
               let blacklist = EdgeS.add (edge_of_assign a) blacklist in
               let innerG, innerChange = iter blacklist expG in
               if innerChange then begin
                 changed := true;
                 UnexpL (a, IsExplained (ptr, targ, innerG))
               end else
                 oldExp
                   
           | UnexpR (a, IsExplained (ptr, targ, expG)) -> 
               (* recurse on the inner graph *)
               let blacklist = EdgeS.add (edge_of_assign a) blacklist in
               let innerG, innerChange = iter blacklist expG in
               if innerChange then begin
                 changed := true;
                 UnexpR (a, IsExplained (ptr, targ, innerG))
               end else
                 oldExp
               
           | UnexpFP (a, IsExplained (ptr, targ, expG)) -> 
               (* recurse on the inner graph *)
               let blacklist = EdgeS.add (edge_of_assign a) blacklist in
               let innerG, innerChange = iter blacklist expG in
               if innerChange then begin
                 changed := true;
                 UnexpFP (a, IsExplained (ptr, targ, innerG))
               end else
                 oldExp
               
        ) curPath in
      newG, !changed
    in
    iter EdgeS.empty expPath

  let iterExplain expGraph limit =
    let rec loop curGraph curLimit curIter =
      logStatusF "iterExplain -- %d\n" curIter;
      let shouldTry, nextLimit = match curLimit with
          Some n when n <= 0 -> false, curLimit
        | Some n -> true, Some (n-1)
        | None -> true, None
      in
      if shouldTry then
        let nextGraph, changed = explainMore curGraph in
        if changed 
        then loop nextGraph nextLimit (curIter + 1)
        else nextGraph
      else
        curGraph
    in
    loop expGraph limit 1

    
  (* TODO: add randomization flag *)
  let pickTargets numTargets targSet =    
    (* randomly pick numTargets from targSet *)
    let chosenInd = List_utils.pickK (NS.cardinal targSet) numTargets in
    logStatus "Picked indices: ";
    List.iter (fun i -> print_string ((string_of_int i) ^ ", ")) chosenInd;
    print_newline ();
    let chosen, _ = NS.fold 
      (fun target (cur, i) ->
         if List.mem i chosenInd 
         then (target :: cur, i + 1)
         else (cur, i + 1)
      ) targSet ([], 0) in
    chosen


  (** Get a set of paths (original + final assignments) explaining a 
      (or set of) common pointer-value flow between ptr1 and ptr2 *)
  let explainAliased (ptr1:ptaLv) (ptr2:ptaLv) : unit =
    let id1, info1 = node_of_lval ptr1 in
    let id2, info2 = node_of_lval ptr2 in
    let goal1 = decideGoalBase ptr1 id1 info1 in
    let goal2 = decideGoalBase ptr2 id2 info2 in
    let isect = NS.inter info1.Solver.ptTargets info2.Solver.ptTargets in
    let numTargets = (NS.cardinal isect) in
    logStatusF "Pointers have %d targets in common\n" numTargets;
    let targets = pickTargets maxTargets isect in
    List.iter 
      (fun target ->
         let flowsTo1 = explainPtsTo ptr1 goal1 target emptyEdges in
         let flowsTo1 = iterExplain flowsTo1 !recurseLimit in
         let flowsTo2 = explainPtsTo ptr2 goal2 target emptyEdges in
         let flowsTo2 = iterExplain flowsTo2 !recurseLimit in
         printFlows2 ptr1 ptr2 target flowsTo1 flowsTo2;
         updateStats flowsTo1;
         updateStats flowsTo2;
      ) targets;
    if numTargets == 0 
    then print_newline () (* to get nicer separation *)

  let explainPointed (ptr:ptaLv) (targ:ptaLv) : unit =
    let targID, targInfo = node_of_lval targ in
    let goal = decideGoal ptr in
    let flowsTo = explainPtsTo ptr goal targID emptyEdges in
    let flowsTo = iterExplain flowsTo !recurseLimit in
    printFlows1 ptr targ flowsTo;
    updateStats flowsTo


  (******** User interface *******)
      
     
  (** Return a list of possible contraints explaining why the pointer
      described by [lvStr1] may point to the same things as [lvStr2] *)
  let whyAliased (lvStr1, lvStr2) =
    (* Must have generated or loaded indexes *)
    assert (LvalH.length !st.lvToID > 0);

    logStatusF "Explaining aliasing between ptr %s and %s\n" lvStr1 lvStr2;
    let ptrs1 = parseLvStr !st.lvToID lvStr1 in
    let ptrs2 = parseLvStr !st.lvToID lvStr2 in
    logStatusF "Considering pairs of (%d, %d) ptrs\n"
      (List.length ptrs1) (List.length ptrs2);
    List_utils.listIterOrderedPairs explainAliased ptrs1 ptrs2
      
  let whyPointsTo (ptrStr, targStr) =
    (* Must have generated or loaded indexes *)
    assert (LvalH.length !st.lvToID > 0);

    logStatusF "Explaining why ptr %s --> %s\n" ptrStr targStr;
    let ptrs = parseLvStr !st.lvToID ptrStr in
    let targs = parseLvStr !st.lvToID targStr in
    logStatusF "Considering pairs of (%d, %d) lvals\n"
      (List.length ptrs) (List.length targs);
    List_utils.listIterOrderedPairs explainPointed ptrs targs


(* TODO: Have something to check representative nodes that
   are not dereferenced, or have client figure out the ptr that was used *)

end

(** CheckAnders that has more guarantees of completeness and shortness *)
module CheckAndersShortest = struct

  type complexKind =
      ComplexL | ComplexR | FPCall

  (* source is a special kind of node "&var" w/ distance 0 ... *)
  type edge = 
      Orig of ptaAssign
    | Complex of complexKind * ptaAssign * recEdges
    | Impossible

  and recEdges = ptaLv * ptaLv * edge list option
      (* for complex constraints like "*ptr = v" that are treated as
         "targ = v", have edges that show how "&targ" flowsTo "ptr" *)

  let infWeight = max_int

  let sumWeight w1 w2 = 
    if w1 = infWeight || w2 = infWeight then infWeight
    else w1 + w2

  let rec weightOfEdge edge =
    match edge with
      Orig _ -> 1
    | Complex (_, _, (_, _, Some moreEdges)) ->
        List.fold_left 
          (fun cur e2 -> sumWeight cur (weightOfEdge e2)) 1 moreEdges
    | Complex (_, _, (_, _, None)) ->
        failwith "weightOfEdge given incomplete complex edge"
    | Impossible -> infWeight
        (* TODO: memoize the weights? *)

  (**** Utils to record Anders solver data ****)

  module Anders = Pta_fs_dir
  module Arr = GrowArray
  module Solver = Anders.Solver
  module NS = Solver.NS

  type nodeID = Solver.nodeID
  type nodeInfo = Solver.nodeInfo

  type exlainerState = {
    baseAssigns : ((nodeID, (nodeID * Cil.location) list) Hashtbl.t);
    simpleAssigns : ((nodeID * nodeID, Cil.location) Hashtbl.t);

    complexL : ((nodeID, (nodeID * Cil.location) list) Hashtbl.t);
    complexR :  ((nodeID, (nodeID * Cil.location) list) Hashtbl.t);
    (** Complex subs different from pta_fs_dir.ml in that given *y = x ..., 
        we store the info with x instead of with y *)

    fpCalls : ((nodeID, (int * nodeID * Cil.location * ctyp) list) Hashtbl.t);
    (** Later, we will be receiving assignments of the form: 

        (formal_param = actual_param) or for returns (actual_ret = formal_ret)

        Index funpointer calls so that these mysterious new assignments
        can be explained by looking up for an actual, 
        what FP call did it participate in, that given final pts to info,
        can explain this assignment to the formal
        
        To assist in these explainations, keep this map from 
        actual -> list of (argument index, fpLval, callsite info)  *)
    
    lvToID :  nodeID LvalH.t;
    idToNode : nodeInfo Arr.t;
    idToLv : (nodeID, ptaLv) Hashtbl.t;
  }
 
  let freshState () =
    {
      baseAssigns = Hashtbl.create 17;
      simpleAssigns = Hashtbl.create 17;
      complexL = Hashtbl.create 17;
      complexR = Hashtbl.create 17;
      fpCalls = Hashtbl.create 17;
      lvToID = LvalH.create 17;
      idToNode = Arr.make 8 (Arr.Elem Solver.dummyNI);
      idToLv = Hashtbl.create 17;
    }

  let st = ref (freshState ())

  let getResultsFilename (root : string) =
    Filename.concat root (".pta_traces.anders")
      
  let saveAll root =
    let outName = getResultsFilename root in
    let out_chan = open_out_bin outName in
    Marshal.to_channel out_chan !st [Marshal.Closures] ;
    close_out out_chan
      
  let loadAll oldFile =
    let in_chan = open_in_bin oldFile in
    st := Marshal.from_channel in_chan;
    close_in in_chan

  let initID2LV () =
    LvalH.iter (fun lv id -> Hashtbl.add !st.idToLv id lv) !st.lvToID

  let copySolverState () =    
    st := { !st with lvToID = Solver.lvToID; };
    Arr.iteri (fun id (fwdId, nodeinfo) -> Arr.setg !st.idToNode id nodeinfo)
      Solver.idToNewIDInfo;
    initID2LV ()


  (***** Query final solution ******)

  let lvalOfID id =
    try 
      Hashtbl.find !st.idToLv id
    with Not_found ->
      logErrorF "Couldn't find lval for id %d\n" id;
      raise Not_found

  let addrOfID rhsID : ptaRv =
    let rhsLv = lvalOfID rhsID in
    makeAddr rhsLv

  let rvalOfID id =
    let lv = lvalOfID id in
    makeRv (PLv lv)

  let isFuncID id =
    let lv = lvalOfID id in
    isFuncLv lv

  let simplifyQueryLv ptrLv = 
    (* match w/ temp vars? Hmm, doesn't work if the ptrLv is in
       a form that was never seen in the program text ... *)
    ptrLv

  let node_of_id id =
    Arr.get !st.idToNode id

  exception IDNot_found of ptaLv

  let id_of_lval (lv : ptaLv) =
    try
      LvalH.find !st.lvToID lv
    with Not_found ->
      logErrorF "id_of_lval can't find id for: %s\n" (string_of_ptaLv lv);
      raise (IDNot_found lv)

  let node_of_lval (lv : ptaLv) =
    let id = id_of_lval lv in
    (id, node_of_id id)


  (*** Convert explainations back to search edges ***)

  let edge_of_assign assign =
    let lv = assign.lhs in
    let rv = assign.rhs in
    try
      let rvLv = getLval rv in
      (id_of_lval lv, id_of_lval rvLv)
    with
      Not_found ->
        failwith ("Unable to convert assign to edge: " ^  
                    (string_of_assign assign))
    | IDNot_found lv ->
        failwith ("Unable to convert assign to edge: " ^  
                    (string_of_assign assign) ^ " " ^ 
                    (string_of_ptaLv lv))


  (** Convert the assignment used in the explaination back into 
      a simple (lhs, rhs) edge *)
  let explainToEdge exp =
    match exp with
      Orig (assign)
    | Complex (_, assign, _) ->
        edge_of_assign assign
    | Impossible -> failwith "explainToEdge given 'Impossible' edge"
        

  (***** Reindex original constraints ****)

  class myIndexer = object(self)
    inherit [nodeID] Anders.constraintIndexer

    (** Handle x = &y *)
    method addBaseAssign ifAdded lhs rhs loc =
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      let old = try Hashtbl.find !st.baseAssigns rhsID with Not_found -> [] in
      Hashtbl.replace !st.baseAssigns rhsID (List_utils.addOnce old (lhsID, loc))

    (** Index original constraint x = y *)
    method addSimpleAssign ifAdded lhs rhs loc =
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      Hashtbl.replace !st.simpleAssigns (lhsID, rhsID) loc

    (** Index original constraint *y = x *)
    method addComplexL ifAdded lhs rhs loc =
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      let old = try Hashtbl.find !st.complexL rhsID with Not_found -> [] in
      Hashtbl.replace !st.complexL rhsID (List_utils.addOnce old (lhsID, loc))

    (** Index original constraint x = *y *)
    method addComplexR ifAdded lhs rhs loc = 
      let lhsID, _ = node_of_lval lhs in
      let rhsID, _ = node_of_lval rhs in
      let old = try Hashtbl.find !st.complexR lhsID with Not_found -> [] in
      Hashtbl.replace !st.complexR lhsID (List_utils.addOnce old (rhsID, loc))

    (** Index original constraint funptr(args) @ callinfo *)
    method addFPCall funptr (callinfo, args) = 
      let callID, _ = node_of_lval funptr in
      List.iter 
        (fun (actualRvs, position) ->
           (* Index regardless of whether it is in the return value
              position or in an actual argument position.
              Take care of that check later instead *)
           List.iter 
             (fun argRv ->
                try
                  let actualArgLv = getLval argRv in
                  let rhsID, _ = node_of_lval actualArgLv in
                  let old = 
                    try Hashtbl.find !st.fpCalls rhsID 
                    with Not_found -> [] in
                  let call = 
                    (position, callID, callinfo.cloc, callinfo.ctype) in
                  Hashtbl.replace !st.fpCalls rhsID (List_utils.addOnce old call)
                with Not_found -> ()) actualRvs
        ) args
        
  end

  let indexer = new myIndexer


  (** Record the simple assignments that were originally in the program *)
  let getOriginalAssigns base other calls = begin
    
    (* Must have generated or loaded indexes *)
    assert (LvalH.length !st.lvToID > 0);

    indexer#addBaseAssigns base;
    indexer#addAssignEdges other;
    indexer#addCallEdges calls;
  end

  (** Re-analyze to get original vs final constraints and lval <-> id mapping *)
  let analyzeAll (root:string) (fresh:bool) = begin
    loadSharedState root;
    
    let oldFile = getResultsFilename root in
    if ((not fresh) && (Sys.file_exists oldFile)) then begin
      (* load existing results *)
      logStatus "Using old PTA results";
      loadAll oldFile;
      logStatus "Old PTA results loaded";
      
    end else begin
      (* Re-run the solver to get the final constraints *)
      Solver.setCycleDetect false; 
      (* turn off cycle detection so that there aren't missing flows *)
      Solver.init root;
      Solver.solve root;
      (* don't do the final rehashing *)
      copySolverState ();

      (* Remember the initial constraints -- do this afterwards 
         so that the lval -> ID mappings are the same as if
         only the solver is run *)
      let constraintFile = PC.getConstraintFile root in
      let _, baseAssign, otherAssign, calls, _ = PC.loadFor constraintFile in
      getOriginalAssigns baseAssign otherAssign calls;
      saveAll root;
    end ;

  end
    
  (************************************************************)
  (* Check how a non-original constraint edge could have come about *)

  (** Return a list of ptrs that point to the given target in the end *)
  let wasComplex target possiblePtrs =
    let doesPointTo target ptr loc =
      let ptrInfo = node_of_id ptr in
      NS.mem target ptrInfo.Solver.ptTargets
    in
    List.filter 
      (fun (ptr, loc) -> doesPointTo target ptr loc) possiblePtrs

  let wasComplexL lID rID =
    try
      let complexLs = Hashtbl.find !st.complexL rID in
      let ptrs = wasComplex lID complexLs in
      let lv = lvalOfID lID in
      let rv = rvalOfID rID in
      List.map (fun (ptr, loc) ->
                  let ptrLv = lvalOfID ptr in
                  let moreExplain = (ptrLv, lv, None) in
                  let assign = makeAssign lv rv loc in
                  Complex (ComplexL, assign, moreExplain)
               ) ptrs
    with Not_found ->
      []

  let wasComplexR lID rID =
    try
      let complexRs = Hashtbl.find !st.complexR lID in
      let ptrs = wasComplex rID complexRs in
      let lv = lvalOfID lID in
      let rv = rvalOfID rID in
      let rLv = lvalOfID rID in
      List.map (fun (ptr, loc) ->
                  let ptrLv = lvalOfID ptr in
                  let moreExplain = (ptrLv, rLv, None) in
                  let assign = makeAssign lv rv loc in
                  Complex (ComplexR, assign, moreExplain)
               ) ptrs
    with Not_found ->
      []

  (** Convert fid to PTA id *)
  let getTargetFun fid =
    let lv = makeLv (makeHost (PVar (makeVar (PGlobal (fid, getFunType fid)))),
                     noOffset) in
    id_of_lval lv 

  (** Pick a funptr call that may have propagated to or from
      the lval "possibleFormOrRet" (based on argIndex) *)
  let wasFPAssigned possibleFormOrRet callLvID argIndex callTyp callLoc =
    let ptrInfo = node_of_id callLvID in
    let funs = Anders.LCDSolver.getFuncs callTyp ptrInfo.Solver.ptTargets in
    Anders.IDS.fold
      (fun fid cur ->
         try
           let possLv =
             if argIndex = retIndex then
               makeLv (Anders.host_of_ret fid, noOffset)
             else 
               makeLv (Anders.host_of_formal fid argIndex, noOffset)
           in
           let possID = id_of_lval possLv in
           if possID = possibleFormOrRet then
             (callLvID, getTargetFun fid, argIndex, callLoc) :: cur
           else cur
         with 
           Not_found ->
             Anders.warnExtern "wasFPAssigned" fid;
             cur
         | Failure "nth" ->
             Anders.warnVarargExtern "wasFPAssigned" fid argIndex;
             cur
      ) funs []

  let wasFPFormal lID rID =
    try
      let fpFormalCalls = Hashtbl.find !st.fpCalls rID in
      List.fold_left
        (fun cur (argPos, callLvID, callLoc, callTyp) ->
           if argPos <> retIndex then
             let more = wasFPAssigned lID callLvID argPos callTyp callLoc in
             List.rev_append more cur
           else 
             cur
        ) [] fpFormalCalls
    with Not_found -> []
      
  let wasFPReturn lID rID =
    try
      let fpReturnCalls = Hashtbl.find !st.fpCalls lID in
      List.fold_left
        (fun cur (argPos, callLvID, callLoc, callTyp) ->
           if argPos == retIndex then
             let more = wasFPAssigned rID callLvID argPos callTyp callLoc in
             List.rev_append more cur
           else 
             cur
        ) [] fpReturnCalls
    with Not_found -> []
      
  let wasFP lID rID =
    let wasForm = wasFPFormal lID rID in
    let wasRet = wasFPReturn lID rID in
    let allFP = List.rev_append wasForm wasRet in
    let lv = lvalOfID lID in
    let rv = rvalOfID rID in
    List.map (fun (callLvID, targID, _, callLoc) ->
                let assign = makeAssign lv rv callLoc in
                let ptrLv = lvalOfID callLvID in
                let targLv = lvalOfID targID in
                let moreExplain = (ptrLv, targLv, None) in
                Complex (FPCall, assign, moreExplain)
             ) allFP

  (************************************************************)

  (* Blacklist edges that have already been used so that we 
     don't get circular proofs *)
  module EdgeS = Set.Make 
    (struct
       type t = nodeID * nodeID
       let compare a b = Pervasives.compare a b
     end)

  (* let dist : (int) LvalH.t = LvalH.create 10 
     let prev : edge LvalH.t = LvalH.create 10 *)

    (* store the actual prev edge... as there can be many edges between
       node u and v *)
  let stateStack : ((int LvalH.t) * (edge LvalH.t)) list ref = ref []
  let pushNewState () = 
    stateStack := (LvalH.create 10, LvalH.create 10) :: !stateStack
  let dist () =
    fst (List.hd !stateStack)
  let prev () =
    snd (List.hd !stateStack)
  let popState () =
    stateStack := List.tl !stateStack


  module NodeS = Set.Make
    (struct
       type t = ptaLv
       let compare a b =
         let compLv = compareLv a b in
         if compLv == 0 then 0
         else 
           let distComp = 
             try 
               let d1 = LvalH.find (dist ()) a in
               try
                 let d2 = LvalH.find (dist ()) b in
                 Pervasives.compare d1 d2
               with Not_found -> -1
             with Not_found ->
               if LvalH.mem (dist ()) b then 1
               else 0 (* both are at infinity... *)
           in
           if distComp == 0 then compLv else distComp
     end) 
    (* Remember to
       - update dist before dumping a node into the queue,
       - remove/re-add a node when its dist is updated *)

  (** Cache of known reasons for complex edges *)
  module CompC = Hashtbl.Make
    (struct 
       
      type t = complexKind * ptaAssign * ptaLv * ptaLv * EdgeS.t
      let equal (ck1, a1, ptr1, lv1, edgs1) (ck2, a2, ptr2, lv2, edgs2) = 
        let c1 = Pervasives.compare (ck1, a1, ptr1, lv1) (ck2, a2, ptr2, lv2) in
        if c1 == 0 then EdgeS.equal edgs1 edgs2
        else false

      let hash (ck1, a1, ptr1, lv1, edgs1) = 
        Hashtbl.hash (ck1, a1, ptr1, lv1)

    end)

  let compCache = Hashtbl.create 10

  (** Show how &src --flowsTo--> target w/ the fewest number of edges *)
  let rec shortestFlow (src:ptaLv) (target:ptaLv) (blacklist:EdgeS.t) =
    pushNewState ();
    let fringe = ref NodeS.empty in (* nodes sorted by distance ... *)
    (******* Helper crap *******)
    let removeMin () = 
      let min = NodeS.min_elt !fringe in
      fringe := NodeS.remove min !fringe;
      min
    in
    let addNode node =
(*      logStatusF "add: %s, depth %d\n" (string_of_ptaLv node)
        (List.length !stateStack); *)
      fringe := NodeS.add node !fringe
    in
    let updateDist node newdist =
      if NodeS.mem node !fringe then begin
        fringe := NodeS.remove node !fringe;
        LvalH.replace (dist ()) node newdist;
        addNode node 
      end else 
        LvalH.replace (dist ()) node newdist
    in
    let updatePrev node newEdge =
      LvalH.replace (prev ()) node newEdge
    in
    let relaxNode targNode prevNode newEdge = 
      let edgew = weightOfEdge newEdge in
      assert (edgew > 0);
      let prevsDist = LvalH.find (dist ()) prevNode in
      let sum = sumWeight edgew prevsDist in
      let oldDist = 
        try LvalH.find (dist ()) targNode 
        with Not_found -> infWeight
      in
      if (sum < oldDist) then begin
        updateDist targNode sum;
        updatePrev targNode newEdge;
        addNode targNode;
        if oldDist <> infWeight then begin
          logStatusF "Shorter dist for %s: %d < %d\n" 
            (string_of_ptaLv targNode) sum oldDist
        end
      end
    in
    let rec returnPath curPath curNode =
      let rec checkNextNode newPath rv = 
        match rv.HC.node with
          PAddrOf (lv) -> 
            if lv = src then newPath
            else failwith "returnPath hit &x where x != src"
        | PLv (lv) -> returnPath newPath lv
        | PCast (_, rv) -> checkNextNode newPath rv
      in
      try
        let nextEdge = LvalH.find (prev ()) curNode in
        match nextEdge with
          Orig assign
        | Complex (_, assign, _) -> 
            checkNextNode (nextEdge :: curPath) assign.rhs
        | Impossible -> failwith "returnPath hit Impossible edge"
      with Not_found ->
        curPath
    in
    let addFirstEdges () = 
      let srcID = id_of_lval src in
      let initialFlows = Hashtbl.find !st.baseAssigns srcID in
      List.iter
        (fun (firstHop, loc) ->
           let firstHopLv = lvalOfID firstHop in
           let assign = makeAssign firstHopLv (addrOfID srcID) loc in
           let edge = Orig assign in
           updateDist firstHopLv 1;
           updatePrev firstHopLv edge;
           addNode firstHopLv
        ) initialFlows
    in
    (******* /Helper crap *******)
    let iter () =
      let found = ref false in
      while not !found && not (NodeS.is_empty !fringe) do
        let cur = removeMin () in
        if cur = target then found := true
        else begin
          let curID, curNode = node_of_lval cur in
          NS.iter 
            (fun next ->
               if EdgeS.mem (next, curID) blacklist then ()
               else 
                 let lv = lvalOfID next in
                 try
                   let loc = Hashtbl.find !st.simpleAssigns (next, curID) in
                   let rv = rvalOfID curID in
                   let edge = Orig (makeAssign lv rv loc) in
                   relaxNode lv cur edge
                 with Not_found ->
                   (* must have been complex... *)
                   let comps = expandComplexEdges next curID blacklist in
                   List.iter (fun edge -> relaxNode lv cur edge) comps
            ) curNode.Solver.subsCons
        end
      done
    in
    addFirstEdges ();
    iter ();
    let path = returnPath [] target in
    popState ();
    path

  and expandComplexEdges lID rID blacklist =
    let compLs = wasComplexL lID rID in
    let compRs = wasComplexR lID rID in
    let fp = wasFP lID rID in
    let all = compLs @ compRs @ fp in
    List.map 
      (fun comp ->
         match comp with
           Complex (compKind, assign, (ptrLv, targLv, None)) -> begin
             (* including blacklist in the key makes the depth matter *)
             let key = (compKind, assign, ptrLv, targLv, blacklist) in
             try Hashtbl.find compCache key
             with Not_found ->
               let shortest = shortestFlow targLv ptrLv 
                 (EdgeS.add (edge_of_assign assign) blacklist) in
               let result = 
                 if shortest = [] then begin
                   (*
                     logErrorF "Hit dead end w/ %s (%s pts to %s)\n"
                     (string_of_assign assign) 
                     (string_of_ptaLv ptrLv) (string_of_ptaLv targLv);
                   *)
                   Impossible
                 end
                 else 
                   Complex (compKind, assign, (ptrLv, targLv, Some shortest))
               in
               Hashtbl.add compCache key result;
               result
           end
         | _ -> failwith "expandComplexEdges working w/ other"
      ) all
      
  (************************************************************)

  let d_explain moreExplain explain : doc =
    match explain with
      Orig assign ->
        text ("Orig: " ^ string_of_assign assign)
    | Complex (complexKind, assign, more) ->
        let compStr = 
          match complexKind with
            ComplexL -> "ComplexL"
          | ComplexR -> "ComplexR"
          | FPCall -> "FP"
        in
        (dprintf "%s: %s\n" compStr (string_of_assign assign)) ++
          (indent 2 (moreExplain more))
    | Impossible -> text "Impossible"

  let rec d_moreExplain moreExp : doc =
    match moreExp with
      (ptr, targ, None) ->
        dprintf "? %s ptsTo %s\n???"
          (string_of_ptaLv ptr) (string_of_ptaLv targ)
    | (ptr, targ, Some exps) ->
        let listed = exps in
        dprintf "%s ptsTo %s ::\n"
          (string_of_ptaLv ptr) 
          (string_of_ptaLv targ) ++
          if listed = [] then text "[???]\n"
          else seq ~sep:Pretty.line ~doit:(d_explain d_moreExplain) ~elements:listed

  let printExplains expPath =
    if expPath = [] then indent 2 (text "NONE\n")
    else
      List.fold_left
        (fun curDoc exp ->
           let newPart = d_explain d_moreExplain exp in
           curDoc ++ newPart ++ line
        ) Pretty.nil expPath
        
  let printFlows1 ptr targ exp =
    (* TODO: make a dot graph *)
    let header = dprintf "Flows %s  <-- &%s\n"
      (string_of_ptaLv ptr) (string_of_ptaLv targ) in
    let body = printExplains exp in
    writeOutFor2 ptr targ (header ++ (indent 2 (body ++ line)))

  let updateStats flowPath =
    ()

  let explainPointed (ptr:ptaLv) (addr:ptaLv) : unit =
    let flowsToPath = shortestFlow addr ptr EdgeS.empty in
    printFlows1 ptr addr flowsToPath;
    updateStats flowsToPath

  let debugComplexity () =
    let totalNodes, totalEdges, totalComplex = Hashtbl.fold 
      (fun id _ (totNode, totEdge, totComplex) ->
         let nodeInfo = node_of_id id in
         (totNode + 1, 
          totEdge + NS.cardinal nodeInfo.Solver.subsCons,
          totComplex + NS.cardinal nodeInfo.Solver.complexL 
          + NS.cardinal nodeInfo.Solver.complexR)
      ) !st.idToLv (0, 0, 0) in
    logStatusF "Considering %d nodes %d edges %d complex in total\n" 
      totalNodes totalEdges totalComplex
      
  let whyPointsTo (ptrStr, targStr) =
    (* Must have generated or loaded indexes *)
    assert (LvalH.length !st.lvToID > 0);
    debugComplexity ();
    logStatusF "Explaining why ptr %s --> %s\n" ptrStr targStr;
    let ptrs = parseLvStr !st.lvToID ptrStr in
    let targs = parseLvStr !st.lvToID targStr in
    logStatusF "Considering pairs of (%d, %d) lvals\n"
      (List.length ptrs) (List.length targs);
    flushStatus ();
    List_utils.listIterOrderedPairs explainPointed ptrs targs

  let whyAliased (lvStr1, lvStr2) =
    (* Must have generated or loaded indexes *)
    assert (LvalH.length !st.lvToID > 0);
    logStatusF "Explaining aliasing between ptr %s and %s\n" lvStr1 lvStr2;
    let ptrs1 = parseLvStr !st.lvToID lvStr1 in
    let ptrs2 = parseLvStr !st.lvToID lvStr2 in
    logStatusF "Considering pairs of (%d, %d) ptrs\n"
      (List.length ptrs1) (List.length ptrs2);
    flushStatus ();
    failwith "TODO"

  let printStats () =
    ()

  let resetStats () =
    ()

(* TODO: get rid of some unnecessary conversions between id and lval... *)

end


(******************* Bulk testing ******************)

module BulkQuery (AEG:AliasExampleGen) = struct

  let fieldSplit = Str.split (Str.regexp "[ \t\n]+")

  let lvSplit = Str.split (Str.regexp "/")

  type command =
      WhyPoints of string * string
    | WhyAlias of string * string
    | NoOp

  let parseError line =
    failwith ("parseErr: " ^ line)

  let parseLine line =
    let fields = fieldSplit line in
    match fields with
      [] -> NoOp
    | [cmd; wID; lvpair; s1; s2] ->
        let (lv1, lv2) = (match lvSplit lvpair with
                       [lv1; lv2] -> (lv1, lv2)
                     | _ -> parseError line) in
        (match String.lowercase (Strutil.strip cmd) with
           "wp" -> WhyPoints (lv1, lv2)
         | "wa" -> WhyAlias (lv1, lv2)
         | "nc" -> NoOp
         | _ -> parseError line)
    | _ -> parseError line

  let parseAndDispatch in_chan =
    AEG.resetStats ();
    try while true do
      let curLine = input_line in_chan in
      match parseLine curLine with
        WhyPoints (lv1, lv2) -> 
          logStatusF "WhyPoints: %s\n" curLine;
          AEG.whyPointsTo (lv1, lv2)
      | WhyAlias (lv1, lv2) -> 
          logStatusF "WhyAlias: %s\n" curLine;
          AEG.whyAliased (lv1, lv2)
      | NoOp -> ()
    done
    with End_of_file -> begin
      logStatus "\nBulk query done!\n";
      AEG.printStats ();
    end


  (** Read queries from a file, and spit out results / statistics *)
  let bulkQuery fname =
    if fname <> "" then begin
      logStatusF "Answering bulk queries from: %s\n\n" fname;
      Stdutil.open_in_for fname parseAndDispatch
    end
      
end


module BulkSteens = BulkQuery (CheckSteens)
module BulkAnders = BulkQuery (CheckAnders)
module BulkAnders2 = BulkQuery (CheckAndersShortest)
