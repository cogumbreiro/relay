(** Track distributions (generically) hashable / comparable types *)

module H = Hashtbl

(** Increment key's count by 1 *)
let updateDistro distro key =
  (try
     let old_freq = H.find distro key in
     H.replace distro key (old_freq + 1)
   with Not_found -> 
     H.add distro key 1
  )

let makeDistro () =
  H.create 17

let distroAsList distro =
  Stdutil.mapToList H.fold distro 


let distroSortKey distro =
  let listed = distroAsList distro in
  (List.sort compare listed)

let distroSortFreq distro =
  let listed = distroAsList distro in
  (List.sort 
     (fun (a', a) (b', b) -> 
        compare (b, b') (a, a')) listed)

let printDistro distro string_of_key name =
  Printf.printf "Distribution of %s: \n" name;
  let total = List.fold_left 
    (fun tot (key, freq) ->
       print_string ((string_of_key key) ^ ": \t" ^ 
                       (string_of_int freq) ^ "\n");
       tot + freq
    ) 0 distro in
  Printf.printf "\nTOTAL: %d\n\n" total

let printDistroSortKey distro string_of_key name =
  printDistro (distroSortKey distro) string_of_key name

let printDistroSortFreq distro string_of_key name =
  printDistro (distroSortFreq distro) string_of_key name
    

(************************************************************)

module type HistogramInput = sig

  type key
  type value 
    
  val string_of_key : key -> string
  val string_of_value : value -> string
  val incr_value : value -> value -> value

end

module Make (I:HistogramInput) = struct

  module H = Hashtbl.Make(Strutil.StringHash)

  let makeDistro () =
    H.create 17

  (** Increment key's count by 1 *)
  let updateDistro distro key v =
    let key = I.string_of_key key in
    (try
       let old_val = H.find distro key in
       H.replace distro key (I.incr_value old_val v)
     with Not_found -> 
       H.add distro key v)

  let distroAsList distro =
    Stdutil.mapToList H.fold distro 

  let lexicalCompare (a, a') (b, b') =
    let c = compare a b in
    if c == 0 then compare a' b'
    else c
      
  let distroSortKey distro =
    let listed = distroAsList distro in
    (List.sort lexicalCompare listed)

  let distroSortFreq distro =
    let listed = distroAsList distro in
    (List.sort 
       (fun (a', a) (b', b) -> 
          lexicalCompare (b, b') (a, a')) listed)

  let printDistro distro name =
    Printf.printf "Distribution of %s: \n" name;
    let total = List.fold_left 
      (fun tot (key, v) ->
         print_string (key ^ ": \t" ^ (I.string_of_value v) ^ "\n");
         match tot with None -> Some v | Some x -> Some (I.incr_value x v)
      ) None distro in
    match total with
      None -> print_string "\n"
    | Some x -> Printf.printf "\nTOTAL: %s\n\n" (I.string_of_value x)
      
  let printDistroSortKey distro name =
    printDistro (distroSortKey distro) name
      
  let printDistroSortFreq distro name =
    printDistro (distroSortFreq distro) name

end


module type IntHistoKey = sig

  type key
  val string_of_key : key -> string

end

module IntHistoInput (K:IntHistoKey) = struct
  include K
    
  type value = int
  let incr_value a b = a + b
  let string_of_value x = string_of_int x

end

