

(** Generic handler for using modsums at function calls *)
class virtual ['st] modAtCallTF (modSumRef: Modsummaryi.modSum ref) = 
object(self)
  
  method virtual private smashAliasesALv : 'st -> Lvals.aLval -> 'st

  method private aggregateMods targFuns =
    List.fold_left
      (fun (gMods, lMods) sumKey ->
         try
           let gMods = 
             Lvals.LvalSet.union gMods (!modSumRef#getGlobalMods sumKey) in
           let lMods = (!modSumRef#getLocalMods sumKey) :: lMods in
           gMods, lMods
         with Modsummaryi.BottomSummary ->
           gMods, lMods
      ) (Lvals.LvalSet.empty, []) targFuns

  method private smashGlobal (lv: Lvals.aLval) (st:'st) =
    self#smashAliasesALv st lv

  method private handleCallMods actuals (inState:'st) 
    (gMods: Lvals.LvalSet.t) (lMods: (Lvals.aLval * Scope.scope) list list) : 'st =
    let postG = Lvals.LvalSet.fold self#smashGlobal gMods inState in
    let postL = 
      List.fold_left
        (fun st lModL ->
           List.fold_left
             (fun st (lv, sumScope) -> match sumScope with
                Scope.SFormal n -> begin
                  let arg = List.nth actuals n in
                  let absArg = Lvals.abs_of_exp arg in
                  try
                    let substLv = Lvals.substActForm absArg lv in
                    self#smashAliasesALv st substLv
                  with Cil_lvals.SubstInvalidArg ->
                    st
                end
              | Scope.SGlobal ->
                  (* May contain a global if global has misc local attribs *)
                  self#smashGlobal lv st
              | _ ->
                  failwith "modsums: given local"
             ) st lModL
        ) postG lMods in
    postL

  method private handleCallModsFuns actuals (inState:'st) funs =
    let gMods, lMods = self#aggregateMods funs in
    self#handleCallMods actuals inState gMods lMods


end
