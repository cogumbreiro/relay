
(** Generic handler for using modsums at function calls *)
class virtual ['st] modAtCallTF : Modsummaryi.modSum ref -> object
  
  method virtual private smashAliasesALv : 'st -> Lvals.aLval -> 'st

  method private handleCallModsFuns : Cil.exp list ->  'st -> Summary_keys.sumKey list -> 'st
    
end
