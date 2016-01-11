open Radar
open Radar_anal

module SeqAna = struct
  class anaClass = object
    method initStats = Nullset.Radarayed.SeqTransfer.initStats
    method computeAll = Nullset.Radarayed.SeqDataflow.compute
    method beforeDataflow = Nullset.warnDoBefore SEQ LOCKS (* dummy *)
    method afterDataflow = Nullset.warnDoAfter SEQ LOCKS (* dummy *)
  end
end

module PessAna = struct
  class anaClass = object
    method initStats = Nullset.Radarayed.PessTransfer.initStats
    method computeAll = Nullset.Radarayed.PessDataflow.compute
    method beforeDataflow = Nullset.warnDoBefore PESS LOCKS (* dummy *)
    method afterDataflow = Nullset.warnDoAfter PESS LOCKS (* dummy *)
  end
end

module AdjLAna = struct
  class anaClass = object
    method initStats = Nullset.Radarayed.AdjLTransfer.initStats
    method computeAll = Nullset.Radarayed.AdjLDataflow.compute
    method beforeDataflow = Nullset.warnDoBefore ADJ LOCKS (* dummy *)
    method afterDataflow = Nullset.warnDoAfter ADJ LOCKS (* dummy *)
  end
end

module AdjNLAna = struct
  class anaClass = object
    method initStats = Nullset.Radarayed.AdjNLTransfer.initStats
    method computeAll = Nullset.Radarayed.AdjNLDataflow.compute
    method beforeDataflow = Nullset.warnDoBefore ADJ NOLOCKS (* dummy *)
    method afterDataflow = Nullset.warnDoAfter ADJ NOLOCKS (* dummy *)
  end
end

module A = MakeRadarAnal (SeqAna) (AdjLAna) (AdjNLAna) (PessAna);;

A.main ();;

