(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following 
  conditions are met:
  
  1. Redistributions of source code must retain the above copyright 
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above 
  copyright notice, this list of conditions and the following disclaimer 
  in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the University of California, San Diego, nor 
  the names of its contributors may be used to endorse or promote 
  products derived from this software without specific prior 
  written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
*)


(** Persistent bookkeeping for distributed operation. 
    I.e., write to disk which functions have already been analyzed,
    and where the summary for those functions is stored.

    Beware: the other components (locking sccs, etc.) are deprecated...
*)

open Callg
open Scc_cg
open Fstructs
open Logging
open Summary_keys


let sharedDir = ref ""

let locksDir = ref ""
let sccDoneDir = ref ""
let funDoneDir = ref ""

type sccLock = out_channel


let setLogDir path =
  let _ = Unix.umask 0o000 in
  sharedDir := path;
  Filetools.ensurePath !sharedDir;
  locksDir := Filename.concat !sharedDir "_locks_dir";
  Filetools.ensurePath !locksDir;
  sccDoneDir := Filename.concat !sharedDir "_scc_done_dir";
  Filetools.ensurePath !sccDoneDir;
  funDoneDir := Filename.concat !sharedDir "_fun_done_dir";
  Filetools.ensurePath !funDoneDir
  
let getLogDir () =
  !sharedDir

(* Put the log in the analyzed program's directory *) 
let makeLogDir cgDir =
  Filename.concat cgDir "relay_logs"

(*******************************************************)
(* Update / Store shared function information          *)

let init settings cgDir =
  let disSettings = Config.getGroup settings "DISTRIBUTED" in
  let usePath = ref false in
  let path = ref "" in
  Config.iter 
    (fun fieldName value ->
       let informError () = 
         logError "Corrupt line in distributed settings file:\n";
         logError (fieldName ^ "\n")
       in
       try 
         (match fieldName with
            "LOG_PATH" -> path := value;
          | "CENTRAL" -> usePath := bool_of_string value
          | _ -> informError ()
         ) 
       with e ->
         logError ("initSettings: " ^ (Printexc.to_string e));
         informError ();
         raise e
    ) disSettings;
  if (!usePath && (!path <> "")) then
    setLogDir !path
  else
    setLogDir (makeLogDir cgDir)

let sccPrefix = "s_"

let funPrefix = "f_"

let sccLockPrefix = sccPrefix ^ "l_"

(** get file name of file representing done-ness of an scc *)
let sccDoneName (scc_num:int) =
  Filename.concat !sccDoneDir (sccPrefix ^ (string_of_int scc_num))

(** get file name of file representing locked-ness of an scc *)
let sccLockName (scc_num:int) =
  Filename.concat !locksDir (sccLockPrefix ^ (string_of_int scc_num))
    
(** get file name of file representing done-ness of a file *)
let funDoneName k =
  Filename.concat !funDoneDir (funPrefix ^ (string_of_sumKey k))


(**** Lock items. Should only do these through the Request interface ****)

exception SccLocked

(** Claim given scc. May raise SccLocked *)
let lockScc (scc_num:int) : sccLock =
  try 
    let out_chan = (open_out_gen 
                      [Open_creat; Open_wronly] 
                      0o666
                      (sccLockName scc_num)) in
    try
      let fd = Unix.descr_of_out_channel out_chan in
      Unix.lockf fd Unix.F_TLOCK 0;
      (* wait for unlocker to close channel! *)
      out_chan
    with e ->
      close_out out_chan;
      raise SccLocked
  with Sys_error s ->
    logError ("Distr: lockScc " ^ s);
    raise SccLocked
      
(** Unclaim given scc *)
let unlockScc (scc_num:int) (scc_chan : sccLock) : unit =
  try
    let fd = Unix.descr_of_out_channel scc_chan in
    Unix.lockf fd Unix.F_ULOCK 0;
    close_out scc_chan;
    Sys.remove (sccLockName scc_num)
  with e ->
    logError ("Distr: unlockScc " ^ (Printexc.to_string e));
    ()

(**** Record done items ****)


let ws = "[ \r\n\t]*"

let doneDelim = "$"

let completeSplitter = Str.split_delim 
  (Str.regexp (ws ^ "[" ^ doneDelim ^ "]" ^ ws))

(** Assume scc lock is held! *)
let recordSccDone (scc_num:int) : unit =
  let out_chan = (open_out_gen 
                    [Open_creat; Open_wronly] 
                    0o666 
                    (sccDoneName scc_num)) in
  close_out out_chan

(** true if the scc is done! *)
let isSccDone (scc_num:int) : bool =
  Sys.file_exists (sccDoneName scc_num)


(** convert sockaddr to a string, ignoring port number *)
let string_of_sockaddr = function
    Unix.ADDR_UNIX s ->
      logError "using unix addrs?";
      s
  | Unix.ADDR_INET (ia, port) ->
      Unix.string_of_inet_addr ia

let sockaddr_of_string s =
  Unix.ADDR_INET (Unix.inet_addr_of_string s, -1)

(** Record in the logs that the given function has a complete summary,
    and record where to find the disk image. *)
let recordFunDone key pathAddrs : unit =
  let out_chan = (open_out_gen [Open_creat; Open_wronly] 0o666 
                    (funDoneName key)) in
  (try
     List.iter 
       (fun (a, u, p) ->
          output_string out_chan 
            ((string_of_sockaddr a) ^ doneDelim ^ u ^ doneDelim ^
               p ^ "\n")
       ) pathAddrs;
     close_out out_chan
   with e ->
     logError "couldn't record that function is complete";
     close_out out_chan;
     raise e
  )


(** returns the storage location if the function is done *)
let isFunDone key =
  let fn = funDoneName key in
  if (Sys.file_exists fn) then
    let in_chan = open_in fn in
    let rec loop curPaths = 
      try
        let line = input_line in_chan in
        match completeSplitter line with
          addr :: user :: [path] ->
            loop ((sockaddr_of_string addr, user, path) :: curPaths)
        | _ -> loop curPaths
      with End_of_file -> curPaths
    in
    let result = 
      (try Some (loop [])
       with e ->
         (* May have been asked to check if function was done even
            if it's not... We don't know. That's why we asked! *)
         logError ("couldn't read if a function was done: " ^
                       (Printexc.to_string e));
         None
      ) in
    close_in in_chan;
    result
  else None


(*************** Higher level completion checks *********)

let neighSCCSNotDone (scc) =
  IntSet.exists (fun neighSCCID -> not (isSccDone neighSCCID)) scc.scc_callees

(** Only for bottom up / input-free summaries *)
let sccFuncsDone (scc) =
  FSet.for_all
    (fun sumKey ->
       match isFunDone sumKey with
         None -> false
       | Some (apList) -> true
    ) scc.scc_nodes
    (* TODO: parameterize by CG type *)

(******************* clear on-disk state ****************)

let sccMatch = Str.string_match (Str.regexp (sccPrefix ^ ".+"))

let funMatch = Str.string_match (Str.regexp (funPrefix ^ ".+"))
  
let sccLockMatch = Str.string_match (Str.regexp (sccLockPrefix ^ ".+"))

let genFileName = "gen_num.txt"


let clearState genNum =
  (* check the old generation number -- if same, don't clear *)
  let fname = Filename.concat !sharedDir genFileName in 
  let clearFunc = 
    (fun () ->  
       Stdutil.clearDir !sccDoneDir (fun fname -> (sccMatch fname 0));
       Stdutil.clearDir !funDoneDir (fun fname -> (funMatch fname 0));    
       Stdutil.clearDir !locksDir (fun fname -> (sccLockMatch fname 0))
    ) in
  Stdutil.clearDirGen genNum fname clearFunc
       
