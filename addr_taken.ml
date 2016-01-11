

(** Track address taken *)


open Cil
open Cildump
open Fstructs

module type AddrTakenIn = sig

  val isRelevant : lval -> bool
  val addToSet : lval -> unit

end

module Make (A:AddrTakenIn) = struct

  let rec checkAddrTaken e =
    match e with
      Lval l -> 
        checkAddrTakenLv l
    | AddrOf l
    | StartOf l -> 
        if A.isRelevant l then
          A.addToSet l;
        checkAddrTakenLv l
    | Const (CStr str) -> ()
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _ -> ()
    | AlignOfE e1
    | SizeOfE e1 -> checkAddrTaken e1
    | UnOp (op, e1, t) -> checkAddrTaken e1
    | BinOp (op, e1, e2, t) -> checkAddrTaken e1; checkAddrTaken e2
    | CastE (t, e1) -> checkAddrTaken e1

  and checkAddrTakenLv l =
    match l with
      Var _, _ -> ()
    | Mem e, _ -> checkAddrTaken e
        
        
  class addrTracker = object(self)
    inherit nopCilVisitor
      
    method private checkExp exp =
      checkAddrTaken exp

    (* visit an instruction; *)
    method vinst (i:instr) : instr list visitAction =
      (match i with
         Asm (_) -> ()
       | Call(_,callexp,args,_) -> 
           List.iter self#checkExp args;
           self#checkExp callexp;
       | Set(lv, exp, _)  ->
           self#checkExp exp;
      );
      DoChildren

    method vstmt (s:stmt) : stmt visitAction =
      (match s.skind with
         Instr _ -> () 
       | Goto _  
       | Break _
       | Continue _
       | Loop _ -> ()
       | Block _
       | TryFinally _
       | TryExcept _ 
       | Return (None, _) -> ()
       | Return (Some e, _) -> 
           self#checkExp e
       | Switch (e, _, _, _)
       | If (e, _, _, _) -> 
           self#checkExp e
      ); DoChildren

    method vinit var off init =
      let rec checkInit (innerOff, init) =
        match init with
          SingleInit exp -> 
            self#checkExp exp
        | CompoundInit (t, moreInit) ->
            List.iter checkInit moreInit
      in
      checkInit (off, init);
      DoChildren

  end

  let searchAddrTaken root =
    let vis = new addrTracker in
    Filetools.walkDir 
      (fun ast file ->
         (visitCilFileSameGlobals (vis :> cilVisitor) ast)
      ) root

end

