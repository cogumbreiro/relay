
(** Index which top-level variables are used / defined by which instrs 
    Coarse -- not partitioning them temporally like SSA / use-def chains *)

(* Not touching the Cil version because it doesn't check uses
   for arguments at function calls! 

   Use this to determine which instrs / stmts to reanalyze if
   a top-level variable changes. 

   How about non-top-level vars? *)

open Cil

module VidS = (struct
               type t = int
               let compare x y = x - y
             end)

module PPS = (struct
                type t = prog_point
                let compare x y = Pervasives.compare x y
              end)

module VIH = Inthash
module PPH = Ciltools.PPHash

(** Per-function use-def maps *)
type funUseDefMaps =
    { ppToVars : VidS.t PPH.t; 
      varToPPs : PPS.t VIH.t; }


