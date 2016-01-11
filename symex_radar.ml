(** Driver for running radar version of symex *)

module SPTA = Radar_symstate

module SeqAna = struct
  class anaClass = object
    method initStats cg sccCG = SPTA.Radarayed.SeqTransfer.initStats cg sccCG
    method computeAll cg sccCG = SPTA.Radarayed.SeqDataflow.compute cg sccCG
    method beforeDataflow = SPTA.doBefore
    method afterDataflow = SPTA.doAfter
  end
end

module AdjLAna = struct
  class anaClass = object
    method initStats cg sccCG = SPTA.Radarayed.AdjLTransfer.initStats cg sccCG
    method computeAll cg sccCG = SPTA.Radarayed.AdjLDataflow.compute cg sccCG
    method beforeDataflow = SPTA.doBefore
    method afterDataflow = SPTA.doAfter
  end
end

module AdjNLAna = struct
  class anaClass = object
    method initStats cg sccCG = SPTA.Radarayed.AdjNLTransfer.initStats cg sccCG
    method computeAll cg sccCG = SPTA.Radarayed.AdjNLDataflow.compute cg sccCG
    method beforeDataflow = SPTA.doBefore
    method afterDataflow = SPTA.doAfter
  end
end

module PessAna = struct
  class anaClass = object
    method initStats cg sccCG = SPTA.Radarayed.PessTransfer.initStats cg sccCG
    method computeAll cg sccCG = SPTA.Radarayed.PessDataflow.compute cg sccCG
    method beforeDataflow = SPTA.doBefore
    method afterDataflow = SPTA.doAfter
  end
end

module Ana = Radar_anal.MakeRadarAnal (SeqAna) (AdjLAna) (AdjNLAna) (PessAna);;

Ana.main ();;
