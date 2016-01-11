open Messages
open Scc_cg

open Summary_keys
open Backed_summary


(* setup *)

val init : Config.settings -> unit

val localDir : string ref

val setUser : string -> unit

val clearState : int -> unit

(* actual requests *)

val initServer : unit -> int

val getSCCWork : unit -> message

val sccDone : scc -> (sumKey * string) list -> unit

exception SummariesNotFound

val requestSumm : (sumKey * sumType) list -> (sumKey * sumType * dbToken) list

val requestData : string -> string -> unit

val reqWarnBarrier : int -> unit

val lockWarn : root -> root -> message

val unlockWarn : root -> root -> unit

(*
val notifyWarn : #warnReports -> unit
*)

val notifyRace : Race_reports.raceTable -> unit

