
(** Plot statistics / distribution on callgraphs
    (1) functions vs membership in SCCs 
    (2) indirect callsites vs fanout
*)

open Callg
open Scc_cg
open Scc
open Fstructs

module SccPlots = struct

  (***** Some useful generic functions *****)

  let sortFuns funs =
    List.sort (fun (i1, size1) (i2, size2) ->
                 if size1 < size2 then -1
                 else if size1 > size2 then 1
                 else Pervasives.compare i1 i2) funs

  (** Sort a list of funID / size pairs according to the way another 
      list is sorts its funIDs *)
  let sortFunsSame sorted toSort =
    (* Most naive way... *)
    List.sort 
      (fun (i1, _) (i2, _) ->
         try
           let ind1 = List_utils.indexOf (fun (i, _) -> i1 = i) sorted in
           let ind2 = List_utils.indexOf (fun (i, _) -> i2 = i) sorted in
           Pervasives.compare ind1 ind2
         with Not_found ->
           failwith "Scc_plot can't sort indices the same way"
      ) toSort

  let plotSortedFuns datFile heading sorted : unit =
    (* Normalize list index by list length, then print corresponding size *)
    let doPlot oc =
      let len_float = float_of_int (List.length sorted) in
      output_string oc heading;
      let _ = List.fold_left 
        (fun curInd (origID, maxSCC) ->
           let i_float = float_of_int curInd in
           output_string oc 
             (Printf.sprintf "%f\t%d\n" (i_float /. len_float) maxSCC);
           curInd + 1
        ) 0 sorted in
      ()
    in
    Stdutil.open_out_for datFile doPlot

  (***** FunID -> SCC size *****)

  let mergeUpdateMap merger key v map =
    try 
      let old = IntMap.find key map in
      let v2 = merger old v in
      IntMap.add key v2 map
    with Not_found -> IntMap.add key v map

  (** Compute the context-insensitive size of an SCC *)
  let ciSCCSize scc =
    let fkeys = FSet.fold 
      (fun key cur -> IntSet.add (fid_to_fkey key) cur) scc.scc_nodes 
      IntSet.empty in
    IntSet.cardinal fkeys

  (** Compute mapping from function id -> size of its max scc *)
  let funToMaxSCC sccCg =
    let map = IntMap.fold 
      (fun _ scc curMap ->
         let size = ciSCCSize scc in
         FSet.fold
           (fun key curMap ->
              let id = fid_to_fkey key in
              mergeUpdateMap max id size curMap
           ) scc.scc_nodes curMap 
      ) sccCg IntMap.empty in
    Stdutil.mapToList IntMap.fold map

  (** Dump SCC size data *)
  let plotSccData datFile sccCg normalizeTo =
    let funToMax = funToMaxSCC sccCg in
    let sorted, normalizeTo = 
      match normalizeTo with
        None -> let x = sortFuns funToMax in (x, Some x)
      | Some old -> (sortFunsSame old funToMax, normalizeTo)
    in
    plotSortedFuns datFile "#fun ID\tscc size\n" sorted;
    normalizeTo

  (***** Fan out data *****)
      
  let callsiteToFanout cg =
    let calls2Fan = Hashtbl.create 17 in
    let updateFanout (fkey, pp) numOut =
      try 
        let old = Hashtbl.find calls2Fan (fkey, pp) in
        if old < numOut then Hashtbl.replace calls2Fan (fkey, pp) numOut
      with Not_found ->
        Hashtbl.add calls2Fan (fkey, pp) numOut
    in
    FMap.iter
      (fun funID fnode ->
         let fkey = fid_to_fkey funID in
         List.iter 
           (fun call ->
              match call with
                CDirect _ -> ()
              | CIndirect (pp, out) -> updateFanout (fkey, pp) (List.length out)
           ) (calleeDetail fnode)
      ) cg;
    Stdutil.mapToList Hashtbl.fold calls2Fan
    

  let plotFanoutData datFile cg normalizeTo =
    let calls2Fan = callsiteToFanout cg in
    let sorted, normalizeTo = 
      match normalizeTo with
        None -> let x = sortFuns calls2Fan in (x, Some x)
      | Some old -> (sortFunsSame old calls2Fan, normalizeTo)
    in
    plotSortedFuns datFile "#callsite\tfanout\n" sorted;
    normalizeTo


end


