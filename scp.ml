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


(** Interface to exec'ing SCP to acquire summaries, and other files *)

open Unix

(** convert sockaddr to a string, ignoring port number *)
let string_of_sockaddr = function
    ADDR_UNIX s ->
      s
  | ADDR_INET (ia, port) ->
      string_of_inet_addr ia

(** combine a sockaddr and a filesystem path *)
let makeAddrPath addr path =
  (string_of_sockaddr addr) ^ ":" ^ path

(** combine a fully qualified path w/ the login user name *)
let makeUserPath user addrPath =
  user ^ "@" ^ addrPath

(** do scp to copy from src to dest *)
let doScp src dest =
  let dest = Filename.concat dest "." in
  (*  L.logError ("doing scp " ^ src ^ " " ^ dest ^ "\n"); *)
  let pid = 
    create_process "scp" [| "-B"; "-q"; "-c"; "blowfish"; 
                            src; dest |] stdin stderr stderr
  in waitpid [] pid



(** Batch transfer files (by using ssh + tar) 
    Example: 

 Sys.command "ssh -c blowfish jvoung@fwg-cs0.ucsd.edu 'tar cvf - -C /home/jvoung/relay `cd /home/jvoung/relay; ls race_anal.* not_here* server.*`' | `cd /tmp ; tar xf -`"

*)
let batchScp (srcUser, srcAddr) srcPaths dest =
  if srcPaths <> [] then
    let srcCmd = " 'tar cvf - " in
    let srcs = Buffer.create 256 in
    let addNewPath path =
      (Buffer.add_string srcs " -C ";
       Buffer.add_string srcs path;
       Buffer.add_string srcs (" `cd " ^ path ^ ";");
       Buffer.add_string srcs " ls") (* use ls to handle wildcards *)
    in
    let endPath () =
      Buffer.add_string srcs "`"
    in
    let addNewSource src =
      Buffer.add_string srcs (" " ^ src)
    in
    let sortedPaths = List.sort 
      (fun (_, path1) (_, path2) -> String.compare path1 path2) srcPaths in
    (match List.fold_left
       (fun last (src, path) ->
          match last with
            Some (oldp) when oldp = path ->
              (addNewSource src;
               last)
          | None ->
              (addNewPath path;
               addNewSource src;
               Some (path))
          | Some (oldp) ->
              (endPath ();
               addNewPath path;
               addNewSource src;
               Some (path))
       ) None sortedPaths with
         Some _ -> endPath ()
       | None -> ()
    );
    prerr_string ("doing scp from: " ^ (string_of_sockaddr srcAddr) ^"\n");
    let comm = 
      ("ssh -c blowfish " ^ srcUser ^ "@" ^ (string_of_sockaddr srcAddr) ^ 
         srcCmd ^ (Buffer.contents srcs) ^ 
         "' | `cd " ^ dest ^ "; tar xf -`") in
    (* prerr_string (comm ^ "\n"); *)
    (Sys.command comm, comm)
  else
    (0, "skipped")



