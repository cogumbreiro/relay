(**
 * Dumps the parsed and pre-processed file.
 *)


open Cil
open Sys
open Pretty
open Filetools

module Dum = Cildump

(* Requires an ENV variable to be defined given *)
let dumpTo = ref ""
let getDumpTo () = 
  (if !dumpTo = "" then
     try
       dumpTo := (Sys.getenv "DUMPROOT");
     with Not_found ->
       prerr_string "Dumpfile: Can't find DUMPROOT environ var\n";
       dumpTo := "/tmp";);
  !dumpTo

let dumpHelp = "path to dump results"

let strip1Slash (s:string) =
  if ((String.get s 0) = '/') then
    Str.string_after s 1
  else 
    s

(* Requires an argument stating the currently processed file *)
let curFile = ref ""
let getCurFile () =
  (if !curFile = "" then
     try
       curFile := (strip1Slash (Sys.getenv "CUR_CIL_FILE"));
     with Not_found ->
       prerr_string "Dumpfile: Can't find CUR_CIL_FILE environ var\n";
       curFile := "/tmp");
  !curFile

let getTargetFile (_:unit) =
  Filename.concat (getDumpTo ()) (getCurFile ())

let makeCFG fundec =
  if fundec.smaxstmtid = None && fundec.sallstmts = [] then begin
    Printf.printf "Making cfg for %s\n" fundec.svar.vname;
(*
    Cil.prepareCFG fundec;
    Cil.computeCFGInfo fundec false;
*)
    Cil.prepareCFG fundec;
    Cfg.clearCFGinfo fundec;
    ignore (Cfg.cfgFun fundec); (* handles "noreturn" attribute... *)
(*    let dotFile = 
      Filename.concat (getDumpTo ()) (fundec.svar.vname ^ ".cfg.dot") in
    Cfg.printCfgFilename dotFile fundec *)
  end else
    Printf.printf "Skipping cfg for %s\n" fundec.svar.vname

(* dumps the pre-processed / parsed file *) 
let dumpFile (f:file) =
  try
    (* Assume first character of f.fileName is '/' -- we skip that *)
    let dumpToFile = getTargetFile () in
    if Sys.file_exists dumpToFile then
      prerr_string ("Dumpfile: File " ^ dumpToFile ^ " already exists\n")      
    ;
    f.fileName <- (getCurFile ()); (* store relative path/file name *)
    ensurePath (Filename.dirname dumpToFile);
    let outFile = (open_out_gen 
                     [Open_creat; Open_wronly] 
                     0o644 
                     dumpToFile) in
    
    (* Xform functions to CFGs first (non-unique stmt ids)... *)
    Cil.iterGlobals f (fun glob -> match glob with
                         Cil.GFun(fundec, _) -> makeCFG fundec
                       | _ -> ());

    (*
      Cil.dumpFile Cil.defaultCilPrinter outFile f;
    *)
    Cil.saveBinaryFileChannel f outFile;
    close_out outFile;
    
  with e -> Printf.printf "Exc. in dumpFile: %s\n"
    (Printexc.to_string e) ; raise e

     
(* Entry point
 * @arg f   a parsed Cil file
 *)      
let dump (f:file) =
  dumpFile f

(* Cil feature descriptor API *)

let doDumpFile = ref false

let feature : featureDescr = 
  { fd_name = "dumpfile";
    fd_enabled = doDumpFile;
    fd_description = "Dumps pre-processed file";
    fd_extraopt = 
      [("--dumproot", Arg.String (fun s -> dumpTo := s), dumpHelp)];
    fd_doit = dump;
    fd_post_check = true
  } 
