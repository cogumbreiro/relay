

(** Do offline cycle detection prepass *)

module IH = Inthash
open Pta_cycle

module type HCDIn = sig
  
  type id
  val eqID : id -> id -> bool
  val hashID : id -> int    

  val iterSimple : (id -> id -> unit) -> unit
  val iterComplexL : (id -> id -> unit) -> unit
  val iterComplexR : (id -> id -> unit) -> unit

(* Not doing offline stuff w/ funptrs *)
(* Not actually doing offline flow? Well, could include the base assigns too. *)
(* And have an operation to update the ptr set *)
    
end

(** Hardekopf and Lin's HCD *)
module HCDSolver (I:HCDIn) = struct

  (*********** Cycle detection setup *********)

  type offlineNode= 
      ONode of I.id 
    | ORef of I.id

  let eq a b =
    match a, b with
      ONode x, ONode y
    | ORef x, ORef y -> I.eqID x y
    | _, _ -> false

  let hash a =
    match a with 
      ONode x -> I.hashID x
    | ORef x -> 321492 lxor I.hashID x

  module NH = Hashtbl.Make
    (struct 
       type t = offlineNode 
       let equal a b = eq a b
       let hash x = hash x
     end)

  let succNodes = NH.create 17

  let findNeighs n = 
    try NH.find succNodes n with Not_found -> []
      
  let addEdge a b =
    let old = findNeighs a in
    NH.replace succNodes a (List_utils.addOnceP eq old b)

  let makeGraph () =
    I.iterSimple (fun a b ->
                    let nodeA = ONode a in
                    let nodeB = ONode b in
                    addEdge nodeA nodeB
                 );
    I.iterComplexL (fun a b ->
                      let nodeA = ORef a in
                      let nodeB = ONode b in
                     addEdge nodeA nodeB
                   );
    I.iterComplexR (fun a b ->
                      let nodeA = ONode a in
                      let nodeB = ORef b in
                      addEdge nodeA nodeB
                   )

  module SCCNodeInfo = struct

    type id = offlineNode
    let eqID a b = eq a b
    let hashID x = hash x
    let maxStack = max_int

    let clearState () = 
      ()

    let iterNeighs foo id =
      let succs = findNeighs id in
      List.iter foo succs

  end

  module CycleD = CycleDetector (SCCNodeInfo)

  (** Get a set of nodes from which all other nodes are reachable. 
      Just an arbitrary set. Not necessarily the minimum set *)
  let getSeeds () =
    let seeds = NH.create 17 in
    let visited = NH.create 17 in
    let rec visit n =
      List.iter (fun neigh -> 
                   if NH.mem visited neigh then ()
                   else begin
                     NH.add visited neigh ();
                     visit neigh
                   end) (findNeighs n)
    in
    NH.iter (fun n s -> 
               if NH.mem visited n then ()
               else begin
                 NH.add seeds n ();
                 NH.add visited n ();
                 visit n
               end
            ) succNodes;
    seeds
      
  let isRef n =
    match n with ORef _ -> true | _ -> false

  let isNonTrivial l =
    match l with [] | [_] -> false | _ -> true

  let onodeID n =
    match n with ONode id -> id | _ -> failwith "oh no! only onodes"

  let orefID n =
    match n with ORef id -> id | _ -> failwith "expecting oref"

  (** Split cycles into cycles w/ no refs and a collection of pairs of 
      (ref, nonRef) where the ref and nonRef were part of the same cycle *)
  let partitionCycles cycleList : I.id list list * (I.id, I.id) Hashtbl.t =
    let basicCycles = ref [] in
    let refCycles = Hashtbl.create 17 in
    List.iter 
      (fun scc ->
         let refs, nonRefs = List.partition isRef scc in
         match refs, nonRefs with
           [], x when isNonTrivial x -> 
             basicCycles := List.map onodeID x :: !basicCycles
         | [], _ -> ()
         | _, x :: _ -> 
             let nonRefID = onodeID x in
             List.iter 
               (fun r -> Hashtbl.replace refCycles (orefID r) nonRefID) refs
         | _, _ -> failwith "HCD: Cycle w/ ref and no non-refs?"
      ) cycleList;
    (!basicCycles, refCycles)


  (** Return the list of basic cycles, and return the list of cycles
      involving a ref node + a basic node *)
  let detectCycles () =
    makeGraph ();
    let seeds = getSeeds () in
    let cycles = ref [] in
    CycleD.clear ();
    NH.iter 
      (fun r () -> 
         cycles := List.rev_append !cycles (CycleD.detectCycles false r)) seeds;
    partitionCycles !cycles
      
    
end

    
