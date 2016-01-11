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


(** Handle timer/alarm related operations. Try not to set up a 
    conflicting SIGALRM handler outside of here.
    Also, provide a do-sleep-retry mechanism *)


(************************************************************
      Setting timeout handlers, setting up retries, etc.
 ************************************************************)

module L = Logging

(*** User should get unique timer ids before using ***)
type timerID = int

let nilTimer = -1

(* Assume handlers have curried any needed args, and result is an effect.
   Reason: don't know how to support multiple polymorphic handlers, 
   each w/ different parametric types! *)
type handler = unit -> unit


(** Map of timer id -> exp time * timer handler *)
module TM = Map.Make (
  struct
    type t = timerID
    let compare = Pervasives.compare
  end
)


(***** Timer state *****)
let (timerMap : (float * handler) TM.t ref) = ref TM.empty

let firstTime = -1.0

let lastTime = ref firstTime

let prevAlrmHandler = ref Sys.Signal_default

let curTimerID = ref 0

(************************************************************
                     Helper routines...
 ************************************************************)

let rec myAlarmHandler sigtype =
  if(sigtype == Sys.sigalrm) then
    (checkUpdateTimers ())
  else
    L.logError "We get signal -- other than sigalrm?!\n"

and setAlarm time =
  let _ = Unix.alarm time in ()

and initAlarm time =
  prevAlrmHandler := Sys.signal Sys.sigalrm (Sys.Signal_handle myAlarmHandler);
  lastTime := time
    (* Doesn't actually set the alarm! Caller does so! *)

and cancelAlarm () = 
  setAlarm 0;
  Sys.set_signal Sys.sigalrm !prevAlrmHandler;
  lastTime := firstTime

and updateAlarm newTime =
  let diff = newTime -. (Unix.time ()) in (* check if negative? *)
  if (!lastTime == firstTime) then
    (initAlarm newTime;
     setAlarm (int_of_float diff))
  else if (!lastTime > newTime) then
    (lastTime := newTime;
     setAlarm (int_of_float diff))
  else () (* timer will expire earlier anyway *)

and checkUpdateTimers () =
  (* 1) Signal expired timers and remove them. 
     2) Get the next-up timer and set alarm for that *)
  let now = Unix.time () in
  let next, newMap = TM.fold 
    (fun k (t, h) (next, curTs) ->
       if (t <= now) then
         (h ();
          next, TM.remove k curTs)
       else
         ((min next t), curTs)
    ) !timerMap (max_float, !timerMap) in
  timerMap := newMap;
  if (TM.is_empty newMap) then cancelAlarm () (* no more timers *)
  else updateAlarm next
    

(************************************************************
                      Interface
 ************************************************************)

(** Returns a unique timer ID *)
let newTimerID () =
  let ret = !curTimerID in
  incr curTimerID;
  ret
  

(** Set a new timer for the given [timerID]. Expires after [secs]
    and invokes [handler]. Overwrites any old timers. *)
let set timerID secs handler =
  let now = Unix.time () in
  let expiresAt = now +. secs in
  (* Search for existing timer and update expiration time *)
  let temp = try
    TM.remove timerID !timerMap
  with Not_found ->
    !timerMap
  in
  timerMap := TM.add timerID (expiresAt, handler) temp


(** Remove timer under given name *)
let cancel timerID =
  try timerMap := TM.remove timerID !timerMap
  with Not_found -> ()




(** Retry given function [foo] up to [n] times. 
    Wait for [period] seconds before each retry.
    If it still fails, then call [stillNo]
*)
let retry foo stillNo n period =
  (* Synchronous (block and waits before retrying) *)
  let setTimer handler : unit =
    Unix.sleep (int_of_float period);
    handler ()
  in
  let rec handler n () =
    if n <= 0 then stillNo () 
    else (try foo (); L.logError "retry succeeded!\n"
          with e ->
            (L.logError ("retry failed with " ^ (Printexc.to_string e) ^ 
                           " ... " ^ (string_of_int n) ^ " tries left\n");
             setTimer (handler (n-1)))
         )
  in
  setTimer (handler n)

