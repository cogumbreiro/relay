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

(** Messages and low-level message transfer for the distributed/grid mode
    (communication between a server/directory and worker clients) *)


open Cil
open Fstructs
open Scc_cg
open Summary_keys
open Warn_reports

module RP = Race_reports

(***** Message types ******)

type root = 
    MEntry of Callg.funID
  | MThread of Callg.funID
              
type warnKey = root * root
    
type userName = string
    
type path = string

(* TODO: make work w/ context-sens. callgraph *)    

(** Message clients send to the server -- 
    "Noti" messages represent clients notifying the server of an event
    "Req"  messages represent a client requesting service 
           from the server 
    "Lock" messages are a special client lock request *)
type message =
    MInit
  | MInitReply of int

  | MReqSCCWork 
  | MSCCReady of scc 
  | MNotiSCCDone of scc * userName * Unix.sockaddr * (sumKey * path) list

  | MLockWarn of warnKey
  | MUnlockWarn of warnKey

  | MDone
  | MLocked

  | MWarnBarrier of int
 
  | MReqSum of sumKey list
  | MReplySum of (userName * Unix.sockaddr, (sumKey * path) list) Hashtbl.t 

  (* Different warning tag for different kinds of warning reports *)
  | MNotiRace of RP.raceTable

  | MReqData of string * path

  | MSuccess
  | MFail



(**************** higher level operations *****************)

let writeMessage out_chan msg =
  Marshal.to_channel out_chan msg [Marshal.Closures];
  flush out_chan


let readMessage in_chan =
  Marshal.from_channel in_chan
