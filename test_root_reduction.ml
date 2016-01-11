(** Test strategy of divide and memoize for root pair reduction *)

let numRoots = 400

(** Return n sets with random numbers from from 1 to n *)
let randomSets (n:int) chance : int array array =
  let sets = Array.make_matrix n n 0 in
  let setPair i j matrix =
    matrix.(i).(j) <- 1;
    matrix.(j).(i) <- 1;
  in
  for i = 0 to (n - 1) do
    for j = i to (n - 1) do
      if (Random.float 1.0) <= chance then begin
        setPair i j sets
      end
    done;
  done;
  sets


let allSet (n:int) : int array array =
  randomSets n 1.0
    
let allButI (n:int) : int array array =
  let set = allSet n in
  for i = 0 to (n - 1) do
    set.(i).(i) <- 0
  done;
  set

let fullsetJoins n =
  ((n + 1) * n) / 2 

let nonZeroElems array = 
  let _, elems = 
    Array.fold_left 
      (fun (j, ls) on ->
         let ls = if (on <> 0) then (j :: ls) else ls in
         (j + 1, ls)) 
      (0, []) array in
  List.rev elems


(** Only memoize single pairs -- do not try to join them into 
    bigger sets first *)
let tryNaive sets n =
  Printf.printf "Joins for %d roots if full sets: %d\n" n (fullsetJoins n);
  let pairsDone = Hashtbl.create 17 in
  let pairNotDone i j =
    let lower, upper = 
      if i < j then (i, j) else (j, i) in
    try
      ignore (Hashtbl.find pairsDone (lower, upper));
      false
    with Not_found ->
      Hashtbl.add pairsDone (lower, upper) ();
      true
  in 
  let origJoins = ref 0 in

(*
  Printf.printf "Orig Sets:\n";
*)
  for i = 0 to (n - 1) do
    let buff = Buffer.create 80 in
(*    Buffer.add_string buff ("S_" ^ string_of_int i ^ " = { "); *)
    let nonZero = nonZeroElems sets.(i) in
    let cardinal =
      List.fold_left
        (fun card j ->
(*
           Buffer.add_string buff (string_of_int j ^ ", "); *)
           if (pairNotDone i j) then
             incr origJoins  ;
           card + 1
        ) 0 nonZero in
(*
  Buffer.add_string buff ("} (" ^ string_of_int cardinal ^ ")\n");
    print_string (Buffer.contents buff);
*)
    ()
  done;
  Printf.printf "Orig joins for given sets (w/out dupes): %d\n" !origJoins;
  ()

exception SliceTooMuch

let rec sliceFirst left first remaining =
  if left = 0 then
    List.rev first, remaining
  else 
    match remaining with
      h :: t ->
        sliceFirst (left - 1) (h :: first) t
    | [] -> raise SliceTooMuch
        
let divideList ls = 
  let size = List.length ls in
  sliceFirst (size / 2) [] ls

(** Try the memoize technique -- hmm doesn't do too well *)
let tryMemoJoins (sets : int array array) n = 
  let joins = ref 0 in
  let knownSets = Hashtbl.create 13 in
  let addSet (s : int list) =
    Hashtbl.replace knownSets s ()
  in
  let setKnown (s : int list) : bool =
    Hashtbl.mem knownSets s
  in
  let rec makeFromDivision elems = 
    match elems with
      [_] | [] -> () (* don't need to do any joins *)
    | _ -> 
        (* at least two elems *)
        if setKnown elems then () (* no work -- return associated *)
        else begin
          let firstHalf, secondHalf = divideList elems in
          makeFromDivision firstHalf; (* normally would return associated *)
          makeFromDivision secondHalf; (* normally would return associated *)
          (* join the associated thingies *)
          incr joins;
          addSet elems;
          (* return the join *)
        end
  in
  for i = 0 to (n - 1) do
    let nonZero = nonZeroElems sets.(i) in
    makeFromDivision nonZero
  done;
  Printf.printf "Divide and Memoized num joins: %d\n" !joins

    
    
let main () = begin
  Random.self_init ();
  (* Try randoms *)
  Printf.printf "0.25 Random\n===============\n";
  let ss = randomSets numRoots 0.25 in
  tryNaive ss numRoots;
  tryMemoJoins ss numRoots;
  Printf.printf "===============\n\n";
  flush stdout;

  Printf.printf "0.50 Random\n===============\n";
  let ss = randomSets numRoots 0.5 in
  tryNaive ss numRoots;
  tryMemoJoins ss numRoots;
  Printf.printf "===============\n\n";
  flush stdout;

  Printf.printf "0.75 Random\n===============\n";
  let ss = randomSets numRoots 0.75 in
  tryNaive ss numRoots;
  tryMemoJoins ss numRoots;
  Printf.printf "===============\n\n";
  flush stdout;

  (* Try all but 1 *)
  Printf.printf "All but self\n===============\n";
  let allBut = allButI numRoots in
  tryNaive allBut numRoots;
  tryMemoJoins allBut numRoots;  
  Printf.printf "===============\n\n";
  flush stdout;
end ;;

main ();;
