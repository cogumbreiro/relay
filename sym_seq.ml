
module SPTA = Radar_symstate


module SeqAna = struct
  module Transfer = SPTA.Radarayed.SeqTransfer
  module Dataflow = SPTA.Radarayed.SeqDataflow
  let beforeDataflow = SPTA.doBefore
  let afterDataflow = SPTA.doAfter
end

module Ana = Radar_anal.MakeRadarAnal (SeqAna);;

Ana.main ();;
