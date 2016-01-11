
open Fstructs
open Summary_keys
open Scc_cg

type sccLock

exception SccLocked

(*************** Settings *********)

val init : Config.settings -> string -> unit

val setLogDir : string -> unit

val getLogDir : unit -> string

(*************** Completion checks *********)

val lockScc : int -> sccLock

val unlockScc : int -> sccLock -> unit

val recordSccDone : int -> unit

val isSccDone : int -> bool

val recordFunDone : sumKey -> (Unix.sockaddr * string * string) list -> unit

val isFunDone : sumKey -> (Unix.sockaddr * string * string) list option

(*************** Higher level completion checks *********)

val neighSCCSNotDone : scc -> bool

val sccFuncsDone : scc -> bool

(*************** Cleanup *********)

val clearState : int -> unit
