
module SPTA = Radar_symstate

module AdjNLAna = struct
  module Transfer = SPTA.Radarayed.AdjNLTransfer
  module Dataflow = SPTA.Radarayed.AdjNLDataflow
  let beforeDataflow = SPTA.doBefore
  let afterDataflow = SPTA.doAfter
end

module Ana = Radar_anal.MakeRadarAnal (AdjNLAna);;

Ana.main ();;
