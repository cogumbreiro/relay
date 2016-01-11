



(** Get the filename from swapping the current extension for a new one.
    New extension should come with "." as the prefix *)
let changeExtension fname newX =
  let base = try
    Filename.chop_extension fname 
  with Invalid_argument _ ->
    fname
  in
  base ^ newX



(** Load a binary AST file *)
let getFile fname : Cil.file =
  try
    (*
      let newF = Frontc.parse fname () in
    *)
    let newF = Cil.loadBinaryFile fname in
    newF
  with Frontc.ParseError s ->
    (print_string ("Exception in getFile: " ^ s ^ "\n"));
    raise (Frontc.ParseError s)


(** Make sure that the directory / path exists *)
let ensurePath (pathname : string) =
  let checkMake (subdir :string) = 
    if (not (Sys.file_exists subdir)) then
      begin
        Unix.mkdir subdir 0o755
      end
  in
  if (not (Sys.file_exists pathname)) then
    try 
      (* search for file separator characters '/' starting at index 1 
       * since we don't care about the directory "/"
       *)
      let lastSlash = ref 0 in
      let l = String.length pathname in
      while !lastSlash < l do
        lastSlash := String.index_from pathname (!lastSlash+1) '/';
        let subdir = Str.string_before pathname !lastSlash in
        checkMake subdir
      done
    with Not_found -> (* Just means it's the end *)
      (* Still need to check the last dir *)
      checkMake pathname

(** @return a list of subdirectories and files in the given directory *)
let getDirsFiles dirname filefilter =
  let filesInRoot = Sys.readdir dirname in
  
  (* partition array of filenames into list of dirs and reg_files *) 
  let dirs, files =
    Array.fold_left
      (fun (dirs, files) fn ->
         let fullName = Filename.concat dirname fn in
         let fstat = Unix.stat fullName in
         if (fstat.Unix.st_kind == Unix.S_DIR) then 
           (* it's a directory *)
           (fullName :: dirs, files)
         else if filefilter fullName then
           (dirs, fullName :: files)
         else
           (dirs, files)
      ) ([], []) filesInRoot in

  let files = List.sort Pervasives.compare files in
  let dirs = List.sort Pervasives.compare dirs in
  (dirs, files)


let rec walkDir (apply:Cil.file -> string -> unit) (root:string) = 
  let dirs, files = getDirsFiles root 
    (fun fullName -> (Filename.check_suffix fullName ".c")) in
  
  (* Handle files in current directory before descending into other dirs *)
  List.iter
    (fun file ->
       try
         let ast = getFile file in
         apply ast file
       with 
         End_of_file ->
           prerr_string ("walkDir is skipping file: " ^ file ^ "\n");
       | Failure s ->
           Printf.fprintf stderr "walkDir hit error %s w/ file %s\n" s file;
    ) files;
  
  (* Descend into other dirs *)
  List.iter
    (fun dir ->
       walkDir apply dir
    ) dirs



let rec walkDirSimple (apply : string -> unit) (root:string) = 
  let dirs, files = getDirsFiles root (fun _ -> true) in
  
  (* Handle files in current directory before descending into other dirs *)
  List.iter
    (fun file ->
       try
         apply file
       with 
         End_of_file
       | Failure _ ->
           prerr_string ("walkDirSimple is skipping file: " ^ file ^ "\n");
    ) files;
  
  (* Descend into other dirs *)
  List.iter
    (fun dir ->
       walkDirSimple apply dir
    ) dirs


