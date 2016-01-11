
(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Ravi Chugh
  
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


open Cil
open Logging
open Fstructs
open Ciltools
open Racesummary

let debug = false

module LocSet = Set.Make(
  struct
    type t = location
    let compare = compareLoc
  end)

module LSHash = Hashtbl.Make(
  struct
    type t = lockState
    let equal a b = Lockset.LS.equal a b
    let hash = Lockset.LS.hash
  end)

type ls2ppTable = PPSet.t LSHash.t

type pp2intTable = int PPHash.t

type int2lsTable = lockState Inthash.t


let computeTables (data : ls2ppTable) : pp2intTable * int2lsTable =
  let count = ref 0 in
  let pp2int = PPHash.create 20 in
  let int2ls = Inthash.create 20 in

  let addPP pp i =
    PPHash.add pp2int pp i in

  let processPart ls pps =
    Inthash.add int2ls !count ls;
    PPSet.iter (fun pp -> addPP pp !count) pps;
    count := !count + 1 in

  LSHash.iter processPart data;
  pp2int, int2ls


class lsPartitioner (getLocks : prog_point -> lockState) = object(self)
  inherit Pp_visitor.ppVisitor

  val data : ls2ppTable =
    LSHash.create 20 
      
  method add ls pp =
    if LSHash.mem data ls then
      let pps = LSHash.find data ls in
      LSHash.replace data ls (PPSet.add pp pps)
    else
      LSHash.replace data ls (PPSet.singleton pp)
        
  method vinst instr =
    self#setInstrPP instr;
    let pp = getCurrentPP () in
    let ls = getLocks pp in
    self#add ls pp;
    self#bumpInstr 1;
    DoChildren
            
  method vstmt stmt =
    self#setStmtPP stmt;
    let pp = getCurrentPP () in
    let ls = getLocks pp in
    self#add ls pp;
    DoChildren

  method compute fundec = 
    LSHash.clear data;
    ignore (visitCilFunction (self :> cilVisitor) fundec);
    computeTables data

end


type summary = pp2intTable * int2lsTable

let emptySumm () : summary =
  PPHash.create 0, Inthash.create 0

let printSumm fkey ((pp2int, int2ls) : summary) =
  logStatus ("LSP fkey " ^ string_of_int fkey);
  Inthash.iter
    (fun i ls ->
       logStatus ("lockset assigned id " ^ (string_of_int i));
       printLockset ls;
    ) int2ls;
  PPHash.iter
    (fun pp i ->
       logStatus ("pp "
         ^ Cildump.string_of_pp pp
         ^ " has ls id "
         ^ string_of_int i)
    ) pp2int
  

exception LsAtPP

let getLocksetAtPP pp ((pp2int, int2ls) : summary) =
  try let i = PPHash.find pp2int pp in
  try let ls = Inthash.find int2ls i in
    ls

  with Not_found -> (* assignment of ls *)
    logError ~prior:1 ("getLocksetAtPP: i not found");
    raise LsAtPP

  with Not_found -> (* assignment of i *)
    logError ~prior:1 ("getLocksetAtPP: pp not found");
    raise LsAtPP

module LSPartSum = struct

  type t = summary
  type simpleSum = t
  let simplify s = s
  let desimplify s = s
  let initVal = emptySumm ()
  let unknownSummary = emptySumm ()

end

module LSPartitionSummary = Safer_sum.Make (LSPartSum)

let sums = new LSPartitionSummary.data (Backed_summary.makeSumType "lsp")

let _ = Backed_summary.registerType sums


(** Package up the Lockset partitioning analysis *)
class lspAnalysis
  (lockAnalysisSkips : Summary_keys.sumKey -> bool) 
  (getLocks : prog_point ->  lockState) : IntraDataflow.analysis = 
object (self)
    
  val mutable summary = emptySumm ()
    
  method setInspect (yesno:bool) = 
    ()

  (* Skip the same functions as the Lockset analysis skips *)
  method isFinal fkey =
    lockAnalysisSkips fkey
      
  method compute funID cfg =
    logStatus "doing lockset partitions";
    flushStatus ();
    let lsp = new lsPartitioner getLocks in
    summary <- lsp#compute cfg

  method summarize fkey (_:fundec) : bool =
    if self#isFinal fkey then sums#addReplace fkey (emptySumm ())
    else sums#addReplace fkey summary;
    false

  method flushSummaries () =
    sums#serializeAndFlush
      

end
