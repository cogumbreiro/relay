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


(** Count occurrences of lock_kernel and unlock_kernel for Linux *)

open Cil
open Printf
open Gc_stats
open Stdutil
open Logging

(******** ARG MANAGEMENT ********)
let cgFile = ref ""

let cgDir = ref ""

let configFile = ref "client.cfg"

let argSpecs = 
  [("-cg", Arg.Set_string cgFile, "call graph directory");]

let anonArgFun (arg:string) : unit = 
  ()

let usageMsg = getUsageString "-cg fname [options]\n"


(*********)

type instStats = {
  mutable matched : int;
  mutable outOfOrder : int;
  mutable lockOnly : int;
  mutable unlockOnly : int;
}


let freshStats () =
  {
    matched = 0;
    outOfOrder = 0;
    lockOnly = 0;
    unlockOnly = 0;
  }


let combineStats s1 s2 =
  s1.matched <- s1.matched + s2.matched;
  s1.outOfOrder <- s1.outOfOrder + s2.outOfOrder;
  s1.lockOnly <- s1.lockOnly + s2.lockOnly;
  s1.unlockOnly <- s1.unlockOnly + s2.unlockOnly


let printStats s =
  logStatusF "Lock nested: %d\n" s.matched;
  logStatusF "Lock-Unlock not nested: %d\n" s.outOfOrder;
  logStatusF "Lock only: %d\n" s.lockOnly;
  logStatusF "Unlock only: %d\n" s.unlockOnly;
  logStatus "\n"


let lockCounts = Hashtbl.create 10

let printLockCounts () =
  let both = ref 0 in
  let l = ref 0 in
  let u = ref 0 in
  Hashtbl.iter 
    (fun fid (lc, uc) ->
       Printf.printf "fid %d (%d, %d)\n" fid lc uc;
       if lc > 0 then
         if uc > 0 then incr both
         else incr l
       else if uc > 0 then incr u
       else ()
    ) lockCounts;
  print_newline ();
  Printf.printf "both: %d\n" !both;
  Printf.printf "lockOnly: %d\n" !l;
  Printf.printf "unlockOnly: %d\n" !u
           
class statGatherer = object(self)
  inherit nopCilVisitor

  val stats = freshStats ()
  val mutable didLock = false
  val mutable didUnlock = false
  val mutable locks = 0
  val mutable unlocks = 0

  val mutable curFunc = dummyFunDec

  method getStats = stats

  method vfunc f =
    didLock <- false; didUnlock <- false;
    curFunc <- f;
    DoChildren

  method private isLock s = s = "lock_kernel"
  method private isUnlock s = s = "unlock_kernel"

  method private incrCount table (fid:int) lock =
    let (oldL, oldU) = try Hashtbl.find table fid with Not_found -> (0, 0) in
    if lock then
      Hashtbl.replace table fid (oldL + 1, oldU)
    else
      Hashtbl.replace table fid (oldL, oldU + 1)

  method private handleLock () =
    self#incrCount lockCounts curFunc.svar.vid true

  method private handleUnlock () =
    self#incrCount lockCounts curFunc.svar.vid false

  method vinst (i:instr) : instr list visitAction =
    (match i with
       Asm (_) -> ()
     | Set(_, _, _)  -> ()
     | Call(_,callexp,_,_) -> 
         (match callexp with
          | Lval(Var(vi),NoOffset) ->
              if self#isLock(vi.vname) then
                self#handleLock ()
              else if self#isUnlock(vi.vname) then
                self#handleUnlock ()

          (* Other indirect call *)
          | _ -> ()
         );
    );
    DoChildren

end

(** Get statistics on each instruction *)
let getStats (f:file) : instStats =
  let obj:statGatherer = (new statGatherer) in
  (* visit the whole file *)
  (visitCilFileSameGlobals (obj :> cilVisitor) f);
  (obj#getStats)

(** Make call graph for all files starting at root *)
let statFiles root : unit =
  let stats = freshStats () in
  Filetools.walkDir 
    (fun ast filename ->
       let newStats = getStats ast in
       combineStats stats newStats
    ) root;
  printLockCounts ()


let initSettings () = begin
(*  Cilinfos.reloadRanges !cgDir;
  let clientSet = Config.initSettings !configFile in
  Default_cache.makeLCaches (!cgDir);
  Alias.initSettings clientSet !cgDir;
  Threads.initSettings clientSet;
  Entry_points.initSettings clientSet;
*)
end


(** Entry point *)
let main () = 
  try
    Arg.parse argSpecs anonArgFun usageMsg;
    
    (* Didn't know how to require the -cg file, etc., so check manually *)
    if (!cgFile = "") then
      begin
        Arg.usage argSpecs usageMsg;
        exit 1
      end
    else
      begin
        Cil.initCIL ();
        setGCConfig ();
        
        cgDir := Filename.dirname !cgFile;
        initSettings ();
        logStatus "\nGathering stats";
        logStatus "-----\n";
        statFiles (Filename.dirname !cgFile);
        
        printStatistics ();
        exit 0;
      end
  with e -> 
    logStatus ("Exc. in lock_stats: " ^
                   (Printexc.to_string e)) ;
    printStatistics ();
    flushStatus ();
    raise e
;;

main () ;;
