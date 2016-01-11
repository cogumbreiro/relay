
(** Detect direct exit functions *)

open Cil
open Logging

let exitFNs = ["exit"; ]

let isExitFun fvar =
  let isExit = List.mem fvar.vname exitFNs in
  (match unrollType fvar.vtype with
     TFun (_, _, _, att) ->
       if hasAttribute "noreturn" att then
         logStatusF "Detected noreturn %s %b\n" fvar.vname isExit;
   | _ -> ());
  isExit


(* TODO add interface for reading in more... *)

