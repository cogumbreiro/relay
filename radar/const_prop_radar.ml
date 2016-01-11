open Const_prop
open Radar_anal

module SeqAna = struct
  class anaClass = object
    method initStats cg sccCG = Radarayed.SeqTransfer.initStats cg sccCG
    method computeAll cg sccCG = Radarayed.SeqDataflow.compute cg sccCG
    method beforeDataflow = dummyBefore
    method afterDataflow = dummyAfter 
  end
end

module PessAna = struct
  class anaClass = object
    method initStats cg sccCG = Radarayed.PessTransfer.initStats cg sccCG
    method computeAll cg sccCG = Radarayed.PessDataflow.compute cg sccCG
    method beforeDataflow = dummyBefore
    method afterDataflow = dummyAfter 
  end
end

module PseudoRacePass = Radarayed.PseudoRacePass

module AdjLAna = struct
  class anaClass = object
    method initStats cg sccCG = Radarayed.AdjLTransfer.initStats cg sccCG
    method computeAll cg sccCG = Radarayed.AdjLDataflow.compute cg sccCG
    method beforeDataflow = dummyBefore
    method afterDataflow = dummyAfter 
  end
end

module AdjNLAna = struct
  class anaClass = object
    method initStats cg sccCG = Radarayed.AdjNLTransfer.initStats cg sccCG
    method computeAll cg sccCG = Radarayed.AdjNLDataflow.compute cg sccCG
    method beforeDataflow = dummyBefore
    method afterDataflow = dummyAfter 
  end
end

module A = MakeRadarAnal (SeqAna) (AdjLAna) (AdjNLAna) (PessAna);;

A.main ();;

