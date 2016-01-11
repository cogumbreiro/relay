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

(** Simple file-server process. Includes client-request operations.
    Sends the requested file, client writes file to local store.
    Alternatively, one can use the Scp module (@see scp.ml).
    Binds to port @see serverPort. *)

open Unix
open Stdutil

module L = Logging
module Stat = Mystats

(****************** Make a limited file server *****************)

let listenQueueLen = 96

let buffSize = 8192

let serverPort = ref 13790

(****************** Messages exchanged *****************)

type message =
    MReqData of string * string   (* src filename and src path *)
  | MVariable of int
  | MFail
  | MDone


let writeMessage out_chan msg =
  Marshal.to_channel out_chan msg [Marshal.Closures];
  flush out_chan


let readMessage in_chan =
  (Marshal.from_channel in_chan :  message)


(****************** Client API ******************)

exception ConnError

let getServerAddr portLess =
  match portLess with
    ADDR_INET (oldIP, _) ->
      ADDR_INET (oldIP, !serverPort)
  | _ -> raise ConnError

let connServer sock portLess =
  let servAddr = getServerAddr portLess in
  connect sock servAddr

let num_retries = 1
let retry_wait = 0.5

let requestFiles srcAddr srcPaths dest =
  let saveInput sock_in len srcFile =
    (* Read in LEN number of bytes, writing them to a file *)
    let buf = String.create (buffSize + 1) in
    let doWrite oc =
      let left = ref len in
      L.logError ("saveInput: " ^ srcFile ^ " is " ^
                    (string_of_int !left) ^ " bytes");
      while (!left > 0) do
        let to_read = 
          if (!left > buffSize) then buffSize else !left in
        let read = input sock_in buf 0 to_read in
        (* TODO: what if read == 0? currently loops forever *)
        output oc buf 0 read;
        left := !left - read; 
      done;
      flush oc;
    in
    open_out_bin_for (Filename.concat dest srcFile) doWrite
  in
  let rec loop pathsLeft acquired =
    match pathsLeft with
      [] -> acquired
    | (srcFile, srcPath) :: rest ->
        let rec doRequest (_, sock_in, sock_out) = 
          writeMessage sock_out (MReqData (srcFile, srcPath));
          match readMessage sock_in with    
            MFail ->
              L.logError ("requestFiles: " ^ srcFile ^ " not with peer");
              acquired (* Skip *)
          | MVariable len ->
              saveInput sock_in len srcFile;
              (srcFile :: acquired)
          | _ ->
              L.logError "requestFiles: unexpected reply";
              acquired
        and faultHandler eMsg =
          L.logError ("requestFiles: fault handler with " ^ eMsg);
          let result = ref acquired in
          (Timeout.retry 
             (fun () -> 
                L.logError ("requestFile retrying");
                result := open_conn_for (getServerAddr srcAddr) doRequest)
             (fun () -> L.logError ("requestFile giving up"))
             num_retries retry_wait);
          !result
        in
        let succ' = try open_conn_for (getServerAddr srcAddr) doRequest
        with
          Unix_error (ECONNREFUSED, _, _)
        | Unix_error (ENETDOWN, _, _)
        | Unix_error (ENETUNREACH, _, _)
        | Unix_error (EHOSTDOWN, _, _)
        | Unix_error (EHOSTUNREACH, _, _) -> (* Couldn't connect? *)
            faultHandler ("requestFiles: couldn't connect");
            
        | Unix_error (e, _, _) ->
            faultHandler ("requestFiles doRequest unix_error: " 
                          ^ (error_message e));
        | e ->
            faultHandler ("requestFiles doRequest: " ^ (Printexc.to_string e));
        in
        loop rest succ'  
  in
  L.logError "calling file server";
  loop srcPaths []


(* Don't add retries... use SCP as fallback (may be the right 
   thing to do anyway, if remote worker drops down) *)



(************ Server side... **************)

(** Keep receiving requests from client until says he's done *)
let recvAndReply (sock, sock_in, sock_out) =
  let readFile fullName fchan =
    (try
       (* First tell other end how many bytes to expect *)
       let total = in_channel_length fchan in
       writeMessage sock_out (MVariable total);
       
       (* Then start reading in the requested file and writing *)
       let buf = String.create (buffSize + 1) in
       let total_read = ref 0 in
       (try 
          let bytes_read = ref 0 in
          bytes_read := input fchan buf 0 buffSize;
          total_read := !total_read + !bytes_read;
          while (!bytes_read != 0) do
            (* TODO: send by tagging packets and end w/ a "DONE" packet? *)
            output sock_out buf 0 !bytes_read;
            bytes_read := input fchan buf 0 buffSize;
            total_read := !total_read + !bytes_read;
         done
        with End_of_file ->
          ()
       );
       if (!total_read != total) then
         L.logError ("Didn't send whole file! " ^ fullName ^ " " ^ 
                       (string_of_int !total_read) ^ "/" ^
                       (string_of_int total))
       ;
       flush sock_out;
     with e ->
       L.logError ("File_serv readFile: " ^ (Printexc.to_string e));
       flush sock_out;
    );
  in
  let msg = readMessage sock_in in
  (match msg with
     MReqData (fname, path) ->
       let fullName = Filename.concat path fname in
       open_in_bin_for fullName (readFile fullName)
   | _ ->
       L.logError "File_serv: received an unexpected request"
  );
  try
    shutdown sock SHUTDOWN_ALL;
    close sock;
  with e ->
    L.logError ("File_serv: shutdown conn failed? " ^ 
                  (Printexc.to_string e))



(** Do the actual serving *)
let serv listenSock () =
  bind listenSock (ADDR_INET (inet_addr_any, !serverPort));
  while (true) do
    listen listenSock listenQueueLen;
    let (clientSock, clientAddr) = accept listenSock in
    (* single-threaded version *)
    let (sock_in,sock_out) = 
      (in_channel_of_descr clientSock, out_channel_of_descr clientSock) in
    (try
       recvAndReply (clientSock, sock_in, sock_out);
     with 
       Unix.Unix_error (e, _, _) ->
         L.logError ("child thread died - " ^ (error_message e))
     | e ->
         L.logError ("child thread died - " ^ (Printexc.to_string e))
    )
  done    

(** Start up the "file server" *)
let startServer () =
  let listenSock = socket PF_INET SOCK_STREAM 0 in (* TODO use ipv6 instead? *)
  setsockopt listenSock SO_REUSEADDR true;
  (try serv listenSock ()
   with 
     Unix.Unix_error (EADDRINUSE, _, _) ->
       (Timeout.retry 
          (serv listenSock)
          (fun () -> L.logError "FS: tried to serv several times -- failed")
          2 1.0)
   | Unix.Unix_error (e, _, _) ->
       L.logError ("file_serv main thread: failed - " ^ 
                     (error_message e));
  )

(** initialize settings and start a thread with the server *)
let init settings =
  let localSettings = Config.getGroup settings "FILE_SERVER" in
  Config.iter
    (fun fieldName values ->
       let informError () = 
         L.logError "Corrupt value in file server config:";
         L.logError (values);
       in
       match fieldName with
         "PORT" ->
           let p = (int_of_string values) in
           serverPort := p;
           L.logStatus ("file server will use: " ^ values);
       | _ ->
           informError ()
    ) localSettings;
  let _ = Thread.create startServer () in
  ()

