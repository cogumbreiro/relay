

module type PARDB  = sig
  val parL : Pseudo_access.PARsummary.data 
  val parNL : Pseudo_access.PARsummary.data 
end

module type PASS3DRIVER = sig
  val main : unit -> unit 
end

module MakePseudoRacePass (P : PARDB) : PASS3DRIVER
