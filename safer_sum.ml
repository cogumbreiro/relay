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


(** Version of Backed_summary where operations now "retry" on failures
    for a bounded number of times. Other safety features also added. *)

open Stdutil
open Fstructs

module BS = Backed_summary
module Req = Request
module L = Logging

module Make (I:BS.Summarizeable) = struct
  
  module Basic = BS.Make(I)

  type sum = I.t

  let checkSep = ","

  let checkPointK = "deserial " ^ (BS.string_of_sumType I.id)

  class data : [sum] BS.base = object(self)
    inherit Basic.data as super

    val numRetries = 6
      
    val retryTime = 10.0

    (** Check if the checkpoint is clear. If not, remove the summary.
        Assumes the summary can be re-obtained *)
    method cleanup () =
      L.logStatus ("Checking left-over state in " ^ 
                     (BS.string_of_sumType I.id));
      L.flushStatus ();
      match Checkpoint.getPrevCheck checkPointK with
        Some (info) ->
          let splitter = Str.regexp checkSep in
          (match Str.split splitter info with
             [keyStr; tokStr] -> 
               let fkey, token = 
                 (fkey_of_string keyStr, BS.token_of_string tokStr) in
               self#err ("Recovering from deserialization. Nuking: " ^ keyStr);
               BS.removeSummary fkey I.id token;
               Checkpoint.clearCheck checkPointK;
           | _ ->
               self#err "Corrupt checkpoint info";
               failwith "Corrupt checkpoint info"
          )
      | None -> ()


    (** Extended version of deserialize, which will retry when files
        are corrupt, etc. *)
    method private deserialize fkey token : sum * BS.dbToken =
      let doDeserial fkey token =
        (* Hmm... checkpoints on retry too? 
           Also, what if the token is not the same the next time around? *)
        let checkpointInfo = (string_of_fkey fkey) ^ checkSep ^ 
          (BS.string_of_token token) in
        Checkpoint.addCheck checkPointK checkpointInfo;
        let result = (super#deserialize fkey token) in
        Checkpoint.clearCheck checkPointK;
        result
      in try 
        doDeserial fkey token
      with 
        Out_of_memory ->
          (* The file is not corrupt *)
          let result = ref (I.initVal, token) in
          Timeout.retry 
            (fun () ->
               super#serializeAndFlush;
               Gc.full_major ();
               result := doDeserial fkey token;
            )
            (fun () ->
               L.logError ~prior:0 "deserialization giving up";
               Checkpoint.clearCheck checkPointK; (* Don't nuke on reboot *)
               quit 1;
               (* maybe re-raise exception, but we don't have the latest exc. *)
            ) numRetries retryTime;
          !result
      | Stack_overflow as e ->
          (* The file is not corrupt... can't do anything =( *)
          raise e
            
      (* TODO: Check for other non-file-corruption exceptions *)
      | Sys_error _
      | Failure _ 
      | End_of_file as e ->
          (* File is corrupt. Try to reacquire the file, then re-read it *)
          let result = ref (I.initVal, token) in
          let redo = 
            (fun () ->
               let keyToks = Req.requestSumm [(fkey, self#typ)] in
               match keyToks with
                 [(k, _, tok)] when k = fkey ->
                   result := doDeserial k tok
               | _ ->
                   L.logError ~prior:0 
                     "deserialization retry couldn't get sums";
                   raise e
            )
          in
          Timeout.retry 
            redo 
            (fun () -> 
               L.logError ~prior:0 "deserialization giving up";
               Checkpoint.clearCheck checkPointK; (* Don't nuke on reboot *)
               quit 1; (* fail fast *)
            ) numRetries retryTime;
          !result       
  end (* End class: base *)

end 
