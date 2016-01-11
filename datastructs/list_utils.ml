
(** Extra operations for polymorphic lists *)

(* TODO: make some of the functions allocate fewer closures ? *)

(** add given [item] if it is not already in the given [lst] *)
let addOnce (lst: 'a list) (item: 'a) =
  if (not (List.mem item lst)) then item :: lst
  else lst


(** add given [item] if it is not already in the given [lst]. Test for
    equality of items with the given [eq] func *)
let addOnceP eq lst item =
  if (List.exists (fun other -> eq item other) lst) then lst
  else item :: lst

let rec insertSortLoop cmp x curH curL =
  match curL with
    h :: t -> 
      let c = cmp x h in
      if c == 0 then List.rev_append curH curL
      else if c < 0 then List.rev_append (x :: curH) curL
      else insertSortLoop cmp x (h :: curH) t
  | [] -> List.rev (x :: curH)

let insertSort cmp x l =
  insertSortLoop cmp x [] l

let rec insertSortCombLoop cmp combine x curH curL =
  match curL with
    h :: t -> 
      let c = cmp x h in
      if c == 0 then 
        let combo = combine x h in 
        List.rev_append curH (combo :: t) 
      else if c < 0 then List.rev_append (x :: curH) curL
      else insertSortCombLoop cmp combine x (h :: curH) t
  | [] -> List.rev (x :: curH)

let insertSortCombine cmp combine x l =
  insertSortCombLoop cmp combine x [] l

(** Take the union of two lists, slowly *)
let union l1 l2 =
  List.fold_left addOnce l1 l2

let unionEq eq l1 l2 =
  List.fold_left (addOnceP eq) l1 l2

let rec mergeUniqLoop cmp cur l1 l2 =
  match l1, l2 with
    [], _ -> List.rev_append cur l2
  | _, [] -> List.rev_append cur l1
  | h1 :: t1, h2 :: t2 ->
      let c = cmp h1 h2 in
      if c == 0 then mergeUniqLoop cmp (h1 :: cur) t1 t2
      else if c < 0 then mergeUniqLoop cmp (h1 :: cur) t1 l2
      else mergeUniqLoop cmp (h2 :: cur) l1 t2

let mergeUnique cmp l1 l2 =
  mergeUniqLoop cmp [] l1 l2

let mergeUniqueCombine cmp comb l1 l2 =
  let rec loop cur l1 l2 =
    match l1, l2 with
      [], _ -> List.rev_append cur l2
    | _, [] -> List.rev_append cur l1
    | h1 :: t1, h2 :: t2 ->
        let c = cmp h1 h2 in
        if c == 0 then 
          let newHead = comb h1 h2 in
          loop (newHead :: cur) t1 t2
        else if c < 0 then loop (h1 :: cur) t1 l2
        else loop (h2 :: cur) l1 t2
  in
  loop [] l1 l2

(** Return elements in l1 that are not in l2 *)
let diff l1 l2 =
  List.fold_left 
    (fun cur x1 ->
       if List.mem x1 l2 then cur
       else x1 :: cur
    ) [] l1

(** Return the index of the first element that satisfies pred *)
let indexOf pred list = 
  let rec indexHelp curIndex = function
      [] -> raise Not_found
    | x :: l -> if pred x then curIndex else indexHelp (curIndex + 1) l
  in
  indexHelp 0 list

(** Remove n elements from the head of the list *)
let rec listSliceHead curL n =
  if n == 0 then curL
  else match curL with
    [] -> raise (Invalid_argument "out of bounds")
  | _ :: t -> 
      listSliceHead t (n - 1)


(** Keep n elements from the list, truncating the rest from the tail.
    The returned list is reversed *)
let rec listSliceTailRev curL rest n =
  if n == 0 then curL
  else match rest with
    [] -> raise (Invalid_argument "out of bounds")
  | h :: t ->
      listSliceTailRev (h :: curL) t (n - 1)
        
(** Get a slice of a list given the bounds. Bounds are 0-based and
    does includes all up to [last - 1]. No fancy negative-indexing.
    @raise Invalid_argument if out of bounds *)
let listSlice lis first last =
  let len = (last - first) in
  if (len < 0) then raise (Invalid_argument "out of bounds")
  else
    let truncH = listSliceHead lis first in
    let truncL = listSliceTailRev [] truncH len in
    List.rev truncL

(** Put the first n elements of the list in one list and the rest in another *)
let listSplit ls n =
  let rec loop front rest n =
    if n = 0 then
      (List.rev front, rest)
    else 
      (match rest with
         [] -> (List.rev front, [])
       | h :: t -> loop (h :: front) t (n - 1))
  in
  loop [] ls n

(** Find an element in [l] equivalent to [x] and replace. 
    Return the new element and the new list, or raise Not_found *)
let listFindReplace (eq: 'a -> 'b -> bool) (replace : 'a -> 'b -> 'a)
    (x:'b) (l: 'a list) : 'a * 'a list =
  let rec findReplace curHead rest =
    match rest with
      [] -> raise Not_found
    | old :: t -> 
        if eq old x then 
          let res = replace old x in
          (res, (List.rev_append curHead (res :: t)))
        else 
          findReplace (old :: curHead) t
  in
  findReplace [] l

let rec compareLoop cmp cur l1tail l2tail =
  if cur != 0 then cur
  else match l1tail, l2tail with
    h1::t1, h2::t2 ->
      compareLoop cmp (cmp h1 h2) t1 t2
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
        
(** Compare 2 lists lexicographically given a comparison function for elems *)
let compare cmp l1 l2 =
  compareLoop cmp 0 l1 l2

let rec eqLoop cmp cur l1tail l2tail =
  if cur then 
    match l1tail, l2tail with
      h1::t1, h2::t2 ->
        eqLoop cmp (cmp h1 h2) t1 t2
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
  else cur

let eq cmp l1 l2 =
  eqLoop cmp true l1 l2


(* DJB2-like *)
let rec hashLoop hElem cur l =
  match l with
    [] -> cur
  | h :: t -> hashLoop hElem ((cur lsl 5) + cur lxor hElem h) t

(** Hash a list given a hash function for individual elements *)
let hash hElem l =
  hashLoop hElem 5381 l

let rec hashSliceLoop hElem cur l n =
  if n <= 0 then (cur lsl 5 + cur lxor List.length l)
  else match l with
    [] -> cur
  | h :: t -> hashSliceLoop hElem ((cur lsl 5) + cur lxor hElem h) t (n -1)

(** Hash only the first n bits *)
let hash_slice hElem l n =
  hashSliceLoop hElem 5381 l n

let pickFromList ls randomize =
  if randomize then begin
    let len = List.length ls in
    if len == 0 then None
    else
      let picked = Random.int len in
      Some (List.nth ls picked)
  end else
    (* Pick first *)
    match ls with
      [] -> None
    | x :: _ -> Some x

let removeNth ls n =
  let rec loop curLs curN curHead =
    match curLs with
      [] -> failwith "removeNth"
    | x :: t ->
        if curN <= 0 then x, t, curHead
        else
          loop t (curN - 1) (x :: curHead)
  in
  let ele, finalTail, revHead = loop ls n [] in
  (ele, List.rev_append revHead finalTail)


(* Removes minimum item, but may shuffle items around *)
let removeMinShuffle cmp ls =
  let rec loop cmp curMin curHead rest =
    match rest with
      [] -> curMin, curHead
    | h :: t ->
        if cmp curMin h <= 0 
        then loop cmp curMin (h :: curHead) t
        else loop cmp h (curMin :: curHead) t
  in
  match ls with
    [] -> failwith "removeMin given empty list"
  | h :: t -> loop cmp h [] t


let stealFromList ls randomize =
  if randomize then begin
    let len = List.length ls in
    if len == 0 then (None, ls)
    else
      let picked = Random.int len in
      let x, newLS = removeNth ls picked in
      (Some x, newLS)
  end else
    (* Pick first *)
    match ls with
      [] -> None, ls
    | x :: t -> (Some x, t)

let pickK n k =
  if n <= 0 || k > n then 
    raise (Invalid_argument "pickK")
  ;
  let rec loop chosen left =
    if left = 0 then chosen
    else
      let next = Random.int n in
      if List.mem next chosen then loop chosen left
      else loop (next :: chosen) (left - 1)
  in
  loop [] k

(* TODO: make a more efficient list shuffle *)
let shuffleList ls = 
  let rec loop remaining result =
    match remaining with 
      [] -> result
    | _ -> 
        let newHd, newRemaining = 
          (match stealFromList remaining true with 
             Some x, l -> x, l
           | None, _ -> failwith "shuffleList should not be empty"
          ) in
        loop newRemaining (newHd :: result)
  in
  loop ls []

(** Return a list of the first [n] elements in list [l] *)
let listPrefix l n =
  let rec helper curL curResult n =
    match curL with
      h :: t -> 
        if n > 0 then helper t (h :: curResult) (n - 1)
        else curResult
    | [] -> curResult
  in
  List.rev (helper l [] n)

(** Break a list down into a list of streaks that match predicate p *)
let findStreaks p ls =
  let addToStreaks newStreak streaks =
    List.rev newStreak :: streaks
  in
  let rec loop prevMatched curList curStreak streaks =
    match curList with
      [] -> addToStreaks curStreak streaks
    | h :: t ->
        let curMatch = p h in
        let newStreak = if curMatch then h :: curStreak else curStreak in
        if prevMatched then begin
          if curMatch then loop curMatch t newStreak streaks
          else loop curMatch t [] (addToStreaks curStreak streaks)
        end else begin
          (* assume curStreak is empty list *)
          loop curMatch t newStreak streaks
        end
  in
  List.rev (loop false ls [] [])

let listIterHelper1 foo l2 x1 =
  List.iter (foo x1) l2

(** Iterate through ordered pairs of [l1] and [l2] *)
let listIterOrderedPairs foo l1 l2 =
  List.iter (listIterHelper1 foo l2) l1

(** Iterate unordered pair combinations within given list [ls] *)
let listIterPairs foo ls =
  let rec iter l1 l2 l2start  =
    match l1, l2, l2start with
      [], _, _
    | _, _, []  -> ()
    | _ :: t1 , [], _ :: t2 ->
        iter t1 t2 t2
    | h1 :: _, h2 :: t2, _ ->
        foo h1 h2;
        iter l1 t2 l2start
  in
  iter ls ls ls

(** Add an element x to a partition, where [x] and other elements of the
    partition should satisfy given [pred]. Order of partitions in the
    given list [parts] may be updated. *)
let addToPartition pred x parts =
  let rec doAdd frontParts rest = 
    match rest with
      [] -> [x] :: frontParts
    | h :: t ->
        (match h with
           x2 :: t2 ->
             if pred x2 then 
               let newH = x :: x2 :: t2 in
               List.rev_append frontParts (newH :: t)
             else
               doAdd (h :: frontParts) t
         | [] -> failwith "had an empty partition?"
        )
  in
  doAdd [] parts
       
let rec mapCrossLoop foo rest cur =
  match rest with
    [] -> cur
  | h :: l -> mapCrossLoop foo l (List.rev_append (foo h) cur)
      
let rev_mapCross (foo : 'a -> 'b list) (l : 'a list) : 'b list =
  mapCrossLoop foo l []

let rec mapCross foo = function
    [] -> []
  | a :: l -> let r = foo a in r @ (mapCross foo l)

