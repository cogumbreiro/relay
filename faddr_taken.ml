
(** Make a pass to find all functions w/ their address taken *)

open Cil
open Cildump
open Fstructs
open Callg

type faddrTakenTable = (fKey, string) Hashtbl.t

let addFunAddrTaken (tab : faddrTakenTable) (l : lval) =
  match l with
    Var(vi), NoOffset -> Hashtbl.replace tab vi.vid vi.vname
  | _ -> Logging.logError ("skipping addFunAddrTaken " ^ (string_of_lval l))

let isRelevantLval l =
  isFunctionType (typeOfLval l)

module FAddrIn = struct

  let table : faddrTakenTable = Hashtbl.create 10
  let isRelevant l = isRelevantLval l
  let addToSet l = addFunAddrTaken table l

end

module FAddr = Addr_taken.Make(FAddrIn)

let getFAddrTaken root =
  Hashtbl.clear (FAddrIn.table);
  FAddr.searchAddrTaken root;
  FAddrIn.table

let printAddrTaken addrTakens =
  Printf.printf "Funcs w/ addr taken\n======================\n";
  let count = 
    Hashtbl.fold (fun id name cur -> 
                    Printf.printf "%s (%d)\n" name id;
                    cur + 1
                 ) addrTakens 0 in
  Printf.printf "Total %d\n\n" count
  
let toSet (addrTakens : faddrTakenTable) : FSet.t =
  Hashtbl.fold (fun id name cur -> FSet.add (fkey_to_fid id) cur) 
    addrTakens FSet.empty
