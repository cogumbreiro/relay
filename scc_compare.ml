
(** Compare distributions of callgraph sccs in Steens vs Anders *)

open Scc
open Fstructs
open Scc_plot

let cgDir = ref ""
let outDir = ref ""

let anonArgFun (arg:string) : unit = ()
let usageMsg = Stdutil.getUsageString "-cg dir -o dir [options]\n"


let argSpecs = 
  [("-cg", Arg.Set_string cgDir, "the call graph directory");
   ("-o", Arg.Set_string outDir, "output directory for plots");]


let plotFor toPlot =
  (* Hardcoded naming convention *)
  let _, _ = List.fold_left 
    (fun (prevS, prevF) (conSens, analName) -> 
       let inFile = Filename.concat !cgDir ("calls." ^ analName) in
       let sccDatFile = Filename.concat !outDir (analName ^ ".scc.dat") in
       let fanOutFile = Filename.concat !outDir (analName ^ ".fanout.dat") in
       if conSens then begin
         let cg = Callg.readCalls inFile in
         let sccGraph = Scc_cg.getSCCGraph cg in
         let prevS = SccPlots.plotSccData sccDatFile sccGraph prevS in
         let prevF = SccPlots.plotFanoutData fanOutFile cg prevF in

         (* Also dump the context-insensitive version *)
         let ciCG = Callg.consSensToInsens cg in
         let ciSccG = Scc_cg.getSCCGraph ciCG in
         let sccDatFile = Filename.concat !outDir (analName ^ "_ci.scc.dat") in
         let fanOutFile = 
           Filename.concat !outDir (analName ^ "_ci.fanout.dat") in
         let prevS = SccPlots.plotSccData sccDatFile ciSccG prevS in
         let prevF = SccPlots.plotFanoutData fanOutFile ciCG prevF in
         
         (prevS, prevF)
       end else begin
         let cg = Callg.readCalls inFile in
         let sccGraph = Scc_cg.getSCCGraph cg in
         let prevS = SccPlots.plotSccData sccDatFile sccGraph prevS in
         let prevF = SccPlots.plotFanoutData fanOutFile cg prevF in
         (prevS, prevF)
       end
    ) (None, None) toPlot in
  ()


let doCompare plotDir =
  (* Hardcoded names ... *)
  plotFor [ (false, "steens"); (false, "anders"); 
            (true, "oic"); (false, "noalias") ]


let printStatistics () = begin
  Gc_stats.printStatistics ();
end

let main () =
  Arg.parse argSpecs anonArgFun usageMsg;
  
  (* Didn't know how to require certain args, so check manually *)
  if (!cgDir = "" || !outDir = "") then begin
    Arg.usage argSpecs usageMsg;
    exit 1
  end else begin
    Pervasives.at_exit printStatistics;
    doCompare !outDir;
    exit 0;
  end

;;
main ()
