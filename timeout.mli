(************************************************************
      Setting timeout handlers, setting up retries, etc. 
 ************************************************************)

type timerID

type handler = unit -> unit

val nilTimer : timerID

val newTimerID : unit -> timerID

val set : timerID -> float -> handler -> unit

val cancel : timerID -> unit

val retry : handler -> handler -> int -> float -> unit
