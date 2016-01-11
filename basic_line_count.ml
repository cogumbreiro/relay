open Cil
open Logging

(************************************************************)
(* Basic line counter that accounts for callgraph pruning *)

class approxLineCountVisitor = object(self)
  inherit nopCilVisitor

  val mutable maxLine = -1

  method processLoc loc =
    if loc.line > maxLine then
      maxLine <- loc.line

  method getMaxLine = maxLine

  method vinst i =
    match i with
      Set (_, _, loc)
    | Asm (_, _, _, _, _, loc) 
    | Call (_,  _, _, loc) ->
        self#processLoc loc
    ;
    DoChildren

  method vstmt s =
    (match s.skind with
       Return (_, loc)
     | Goto (_, loc)
     | Break loc
     | Continue loc
     | If (_, _, _, loc)
     | Switch (_, _, _, loc)
     | Loop (_, loc, _, _)
     | TryFinally (_, _, loc)
     | TryExcept (_, _, _, loc) ->
         self#processLoc loc
     | _ -> ()
    );
    DoChildren
end

let getMaxLine (f:file) : int =
  let obj:approxLineCountVisitor = (new approxLineCountVisitor) in
  (visitCilFileSameGlobals (obj :> cilVisitor) f);
  (obj#getMaxLine)


let doLineCount prune cgDir cg =
  let sccCG = Scc_cg.getSCCGraph cg in
  let newSCCCG = 
    if prune then
      let rooter = new Entry_points.rootGetter cg cgDir in
      let rootSet = rooter#getRootKeys () in
      let reachable = Callg.getReachableFunctions cg rootSet in
      Scc_cg.pruneUnreached sccCG reachable
    else
      sccCG
  in
  let fnames = Scc_cg.filesOfPruned cg newSCCCG in
  let totalLines = ref 0 in
  Scc_cg.iterFiles 
    (fun ast -> 
       let more = getMaxLine ast in
       if more < 0 then logError "getMaxLine return < 0?"
       else totalLines := !totalLines + more
    ) fnames cgDir;
  logStatusF "Total files: %d \t lines processed: %d\n\n"
    (Hashtbl.length fnames) !totalLines
