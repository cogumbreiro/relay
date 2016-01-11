
(** Make a pass to find all global-non-functions w/ their address taken *)

open Cil
open Fstructs


let addGlobAddrTaken tab l =
  match l with
    Var(vi), _ -> Hashtbl.replace tab vi.vid vi.vname
  | _ -> Logging.logError ("skipping addGlobAddrTaken " ^ 
                             (Cildump.string_of_lval l))

let isRelevantGlobal lval =
  match lval with
    Var(vi), _ -> 
      vi.vglob && not (isFunctionType vi.vtype) 
      && not (Trans_alloc.isAllocVar vi.vname)
  | _, _ -> false

module GAddrIn = struct

  let table = Hashtbl.create 10
  let isRelevant l = isRelevantGlobal l
  let addToSet l = addGlobAddrTaken table l

end

module GAddr = Addr_taken.Make(GAddrIn)

let getGlobalAddrTaken root =
  Hashtbl.clear (GAddrIn.table);
  GAddr.searchAddrTaken root;
  GAddrIn.table
    

let printAddrTaken addrTakens =
  Printf.printf "Globals w/ addr taken\n======================\n";
  let count = 
    Hashtbl.fold (fun id name cur -> 
                    Printf.printf "%s(%d)\n" name id;
                    cur + 1
                 ) addrTakens 0 in
  Printf.printf "Total %d\n\n" count

  
let toSet addrTakens =
  Hashtbl.fold (fun id name cur -> IntSet.add id cur) 
    addrTakens IntSet.empty
