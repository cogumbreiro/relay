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
  H.fold (fun size freq cur -> (size, freq) :: cur) distro []

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

let printDistro distro string_of_key name =
  Printf.printf "Distribution of %s: \n" name;
  let total = List.fold_left 
    (fun tot (size, freq) ->
       print_string ((string_of_key size) ^ ": \t" ^ 
                       (string_of_int freq) ^ "\n");
       tot + freq
    ) 0 distro in
  Printf.printf "\nTOTAL: %d\n\n" total

let printDistroSortKey distro string_of_key name =
  printDistro (distroSortKey distro) string_of_key name

let printDistroSortFreq distro string_of_key name =
  printDistro (distroSortFreq distro) string_of_key name
    
