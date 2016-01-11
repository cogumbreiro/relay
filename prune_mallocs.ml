(** Prune functions that are KNOWN malloc-wrappers and the functions that
    are ONLY called by mallocs transitively (and reflexively... ignoring FPs) *)

open Fstructs
open Callg
open Alloc
open Logging

module PruneMallocs = struct

  let pruneMallocFuns addrTk cg =
    let calledByMallocs = ref FSet.empty in
    let calledByOthers = ref FSet.empty in
    let getReachMalloc () =
      let rec visit fk fn =
        if FSet.mem fk !calledByMallocs then ()
        else begin
          calledByMallocs := FSet.add fk !calledByMallocs;
          List.iter 
            (fun call -> 
               match call with
                 CDirect (_, calleeK) ->
                   (try 
                      let calleeN = FMap.find calleeK cg in
                      visit calleeK calleeN
                    with Not_found -> () )
               | CIndirect _ -> ()
            ) (calleeDetail fn)
        end
      in
      FMap.iter 
        (fun fk fn ->
           let callerIsMalloc = isAlloc fn.name in
           if callerIsMalloc then visit fk fn
           else ()
        ) cg;
      logStatusF "Transitively reached mallocs: %d\n" 
        (FSet.cardinal !calledByMallocs);
    in
    let getOthers () =
      FMap.iter 
        (fun fk fn ->
           if not (FSet.mem fk !calledByMallocs) then
             List.iter (fun call -> match call with
                          CDirect (_, calleeK) ->
                            calledByOthers := 
                              FSet.add calleeK !calledByOthers
                        | _ -> ()) (calleeDetail fn)
           else ()) cg
    in
    logStatus "Pruning malloc funs:\n==================================";
    getReachMalloc ();
    getOthers ();
    FMap.fold 
      (fun fk fn cur ->
         if FSet.mem fk !calledByOthers then cur
         else if FSet.mem fk !calledByMallocs then 
           if not (FSet.mem fk addrTk) then 
             (logStatus fn.name;
              FMap.remove fk cur)
           else 
             (logStatusF "not pruning addrTk: %s\n" fn.name;
              cur)
         else cur
      ) cg cg

end
