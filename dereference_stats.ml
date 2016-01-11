open Cil
open Fstructs
open Stdutil
open Logging

module NW = Null_warnings
module WarnR = NW.WarnR

module LocHash = Hashtbl.Make(
  struct
    type t = location
    let equal l1 l2 = (compareLoc l1 l2) = 0
    let hash l = Hashtbl.hash l
  end
)

(***************************************************)
(* Commandline handling                            *)

let safe_file = ref ""
let unsafe_file = ref ""
let caption = ref ""

(* Command-line argument parsing *)
    
let argSpecs = 
  [("-safe", Arg.Set_string safe_file, "safe dereference data");
   ("-unsafe", Arg.Set_string unsafe_file, "unsafe dereference data");
   ("-caption", Arg.Set_string caption, "caption for deref report");
  ]

let anonArgFun (arg:string) : unit = 
  ()
    
let usageMsg = getUsageString "-safe filename -unsafe filename\n"



(***************************************************)
(* Computing safe/unsafe ratio                     *)

(* triple contains counts for safe, unsafe, and non-blobby unsafe derefs *)
let locTable : (int * int * int) LocHash.t = LocHash.create 300

let addToTable t loc safe unsafe unsafeNonBlob =
  let a, b, c = 
    try LocHash.find t loc
    with Not_found -> 0, 0, 0
  in
  LocHash.replace t loc (a + safe, b + unsafe, c + unsafeNonBlob)

let notBlobby data =
  match data.NW.nullImprec with
    NW.Blob -> false
  | NW.Shown lvImprec ->
      (match lvImprec with
          Lvals.Syntactic -> true
        | _ -> false
      )
  | NW.NotShown -> true

let combineSafeUnsafe () =
  let safe = WarnR.deserialize !safe_file in
  let unsafe = WarnR.deserialize !unsafe_file in

  let processSafe loc cluster =
    (* No blob count? *)
    let len = List.length cluster in
    addToTable locTable loc len 0 0 in

  let processUnsafe loc cluster =
    (* Assumes no cluster limit... no other way of getting an accurate 
     * blob count w/out changing abstraction of "cluster" *)
    let len = List.length cluster in
    let len2 = List.length
      (List.filter notBlobby cluster) in
    addToTable locTable loc 0 len len2
  in

  WarnR.KH.iter processSafe safe#getData;
  WarnR.KH.iter processUnsafe unsafe#getData

(* computes weighted and unweighted (safe, unsafe) counts *)
let count withBlobs : (float * float) * (int * int) =
  let wSafe, wUnsafe = ref 0.0, ref 0.0 in
  let uSafe, uUnsafe = ref 0, ref 0 in
  let locsWithNoCount = ref 0 in

  (* iterate over all locations *)
  LocHash.iter (fun _ (safeCount, b, c) ->

    let unsafeCount = if withBlobs then b else c in

    if safeCount + unsafeCount > 0 then
    begin

      (* update weighted counts *)
      let x, y = float_of_int safeCount, float_of_int unsafeCount in
      let z = x +. y in
      wSafe := !wSafe +. (x/.z);
      wUnsafe := !wUnsafe +. (y/.z);

      (* update unweighted counts *)
      if unsafeCount = 0 then
        incr uSafe
      else
        incr uUnsafe
    end

    else
      (* this branch can only be taken in the withBlobs = false case.
         need to keep track of the locations that have 0 safe and
         0 non-blobby safe.  these locations shouldn't be counted. *)
      incr locsWithNoCount

  ) locTable;

  let locCount = LocHash.length locTable in
  let locs = locCount - !locsWithNoCount in
  (* sanity checks *)
  let err = !wSafe +. !wUnsafe -. (float_of_int locs) in
  if err > 0.01 then
    logErrorF "weighted counts don't sum to locCount! err=%f\n" err;
  if !uSafe + !uUnsafe <> locs then
    logError "unweighted counts don't sum to locCount|\n";
  if !wSafe < float_of_int !uSafe then
    logError " weighted safe < unweight safe\n";

  (* return counts *)
  (!wSafe, !wUnsafe), (!uSafe, !uUnsafe)


let print ((wS, wU), (uS, uU)) blobCaption =
  NW.printDerefReportF (!caption ^ ", " ^ blobCaption ^ ", weighted") wS wU; 
  logStatus "";
  NW.printDerefReport (!caption ^ ", " ^ blobCaption ^ ", unweighted") uS uU;
  logStatus ""
  
  

(***************************************************)
(* Execution / Program Entry Point                 *)

let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require files, so check manually *)
    if (!safe_file = "" || !unsafe_file = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Stdutil.printCmdline ();
        combineSafeUnsafe ();
        print (count true) "with blobs";
        print (count false) "without blobs";

        (* Debug .. *)



        exit 0;
      end
  with e -> 
    logError ("Exc. in : " ^ (Printexc.to_string e));
    raise e
;;
main () ;;
