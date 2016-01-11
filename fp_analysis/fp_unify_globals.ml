
open Fp_types

let globalUnifTable = makeMappings 17 

let unifyGlobal gv1 gv2 : unit =
  updateMap globalUnifTable gv1 gv2 

let gRep gv : fvar = 
  findMappingNonOpt globalUnifTable gv
