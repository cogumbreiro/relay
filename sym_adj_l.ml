
module SPTA = Radar_symstate

module AdjLAna = struct
  module Transfer = SPTA.Radarayed.AdjLTransfer
  module Dataflow = SPTA.Radarayed.AdjLDataflow
  let beforeDataflow = SPTA.doBefore
  let afterDataflow = SPTA.doAfter
end

module Ana = Radar_anal.MakeRadarAnal (AdjLAna);;

Ana.main ();;
