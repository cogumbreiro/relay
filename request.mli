open Messages
open Fstructs
open Scc

open Warn_reports
open Race_reports

open Backed_summary

(* setup *)

val init : Config.settings -> unit

val localDir : string ref

val setUser : string -> unit

val clearState : int -> unit

(* actual requests *)

val initServer : unit -> int

val getSCCWork : unit -> message

val sccDone : scc -> (fKey * string) list -> unit

exception SummariesNotFound

val requestSumm : (fKey * sumType) list -> (fKey * sumType * dbToken) list

val requestData : string -> string -> unit

val reqWarnBarrier : int -> unit

val lockWarn : fKey -> fKey -> message

val unlockWarn : fKey -> fKey -> unit

(*
val notifyWarn : #warnReports -> unit
*)

val notifyRace : raceTable -> unit

