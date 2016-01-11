
(** Get time/mem statistics for certain function applications.
    Adapted around CIL's ocamlutil "Stats". 
    Extended to
    - allow measurement of wall-clock time instead of user-mode time. 
    - properly terminate timer when timed function throws an exception
*)

open Pretty

let doTime = ref false

type t = { name : string;
           mutable time : float; (* In seconds *)
           mutable sub  : t list}

                                        (* Create the top level *)
let top = { name = "TOTAL";
            time = 0.0;
            sub  = []; }

                                        (* The stack of current path through 
                                         * the hierarchy. The first is the 
                                         * leaf. *)
let current : t list ref = ref [top]

let reset = 
  top.sub <- [];
  top.time = 0.0
  
let print chn msg =
  if !doTime then begin
    (* Total up *)
    top.time <- List.fold_left (fun sum f -> sum +. f.time) 0.0 top.sub;
    let rec prTree ind node = 
      (Printf.fprintf chn "%s%-20s          %6.3f s\n" 
         (String.make ind ' ') node.name node.time);
      
      List.iter (prTree (ind + 2)) (List.rev node.sub)
    in
    Printf.fprintf chn "%s" msg; 
    List.iter (prTree 0) [ top ];
    let gc = Gc.quick_stat () in 
    let printM (w: float) : string = 
      Printf.sprintf "%.2fMb" (w *. 4.0 /. 1000000.0)
    in
    Printf.fprintf chn 
      "Memory statistics: total=%s, max=%s, minor=%s, major=%s, promoted=%s\n    minor collections=%d  major collections=%d compactions=%d\n"
      (printM (gc.Gc.minor_words +. gc.Gc.major_words 
               -. gc.Gc.promoted_words))
      (printM (float_of_int gc.Gc.top_heap_words))
      (printM gc.Gc.minor_words)
      (printM gc.Gc.major_words)
      (printM gc.Gc.promoted_words)
      gc.Gc.minor_collections
      gc.Gc.major_collections
      gc.Gc.compactions;
  end
        
  

(* Get the current time from user process's perspective, in seconds *)
let getUserTime () : float = 
  (Unix.times ()).Unix.tms_utime

(* Get the current wall-clock time *)
let getWCTime = Unix.gettimeofday

let repeatTime getTime limit str f arg = 
  (* Find the right stat *)
  let stat : t = 
    let curr = match !current with h :: _ -> h | _ -> assert false in
    let rec loop = function
        h :: _ when h.name = str -> h
      | _ :: rest -> loop rest
      | [] -> 
          let nw = {name = str; time = 0.0; sub = []} in
          curr.sub <- nw :: curr.sub;
          nw
    in
    loop curr.sub
  in
  let oldcurrent = !current in
  current := stat :: oldcurrent;
  let start = getTime () in
  let rec repeatf count = 
    let res = f arg in
    let diff = getTime () -. start in
    if diff < limit then
      repeatf (count + 1)
    else begin
      finishTiming diff count;
      res
    end
  and finishTiming diff count = 
    stat.time <- stat.time +. (diff /. float(count));
    current := oldcurrent;                (* Pop the current stat *)
  in
  try repeatf 1
  with e ->
    let diff = getTime () -. start in
    let count = 1 in (* should have gotten the exception on the first repeat*)
    finishTiming diff count;
    raise e

      
let time str f arg =
  if !doTime then repeatTime getWCTime 0.0 str f arg
  else f arg

let timethis (f: 'a -> 'b) (arg: 'a) : 'b * float =
  let start = getWCTime () in
  let res = f arg in 
  let time = getWCTime () -. start in
  res, time
    
(************************************************************)

module type IndexOps = sig

  type t (* type of index *)

  val hash : t -> int

  val equal : t -> t -> bool

  val to_string : t -> string

  val getTime : unit -> float (* slipped this in there... *)

(* TODO: make output channel a parameter? *)

  val prefix : string

end

module IndexedTimer (I:IndexOps) = struct

  module HI = Hashtbl.Make(I)

  let times = ref (HI.create 4)

  let updateTime idx moreTime =
    let oldTime = try HI.find !times idx with Not_found -> 0.0 in
    HI.replace !times idx (oldTime +. moreTime)

  let time idx foo arg =
    let start = I.getTime () in
    let res = foo arg in
    let newTime = I.getTime () -. start in
    updateTime idx newTime;
    res

  let printTime doc (idx, time) =
    doc ++ text (I.to_string idx ^ " : " ^ string_of_float time ^ "\n")

  let printTimes () =
    print_string (I.prefix ^ "\n");
    let listed = Stdutil.mapToList HI.fold !times in
    (* Sort by time (longest first) *)
    let sorted = List.sort (fun (a, t1) (b, t2) -> 
                              compare (t2, b) (t1, a)) listed in
    let doc = List.fold_left printTime nil sorted in
    print_string (sprint 80 (indent 2 doc));
    let total = List.fold_left (fun tot (_, t) -> tot +. t) 0.0 sorted in
    Printf.printf "TOTAL: %f\n" total
    

  let reset () = 
    times := HI.create 4
      
end

  
