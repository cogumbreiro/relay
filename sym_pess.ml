
module SPTA = Radar_symstate

module PessAna = struct
  module Transfer = SPTA.Radarayed.PessTransfer
  module Dataflow = SPTA.Radarayed.PessDataflow
  let beforeDataflow = SPTA.doBefore
  let afterDataflow = SPTA.doAfter
end

module Ana = Radar_anal.MakeRadarAnal (PessAna);;

Ana.main ();;

