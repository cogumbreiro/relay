(** Detect when an argument is a void * and map it to something more
    concrete if that is known. *)

open Cil
open Cildump
open Logging
open Type_reach
open Type_utils

class voidMapper = object(self)
  inherit nopCilVisitor

  val vidToType = Inthash.create 17
  method getVoidMapping () = vidToType

  (* Temporary set of var ids we are currently interested in *)
  val curSeek = Inthash.create 17
  method clearSeek () = Inthash.clear curSeek

  method vfunc f : fundec visitAction =
    let foundVoid = List.fold_left 
      (fun cur var -> 
         if isPoly var.vtype then begin
           if cur = false then self#clearSeek (); (* Clear on first time *)
           Inthash.add curSeek var.vid ();
           true
         end else cur
      ) false f.sformals in
    if foundVoid then DoChildren
    else SkipChildren
      
  method private pickWidest t1 t2 =
    if isPoly t1 then t2
    else if isPoly t2 then t1
    else 
      if structSubtype t1 t2 then t1
      else if structSubtype t2 t1 then t2
      else pickWidest t1 t2
          
  method private mapType var t =
    try 
      let oldT = Inthash.find vidToType var.vid in
      if Ciltools.compare_type oldT t <> 0 then begin
        logErrorF "Found 2 types for formal %s(%d): %s, %s\n"
          var.vname var.vid (string_of_type oldT) (string_of_type t);
        let widest = self#pickWidest oldT t in
        logErrorF "Picked: %s\n" (string_of_type widest);
        Inthash.replace vidToType var.vid widest
      end
    with Not_found ->
      logStatusF "Found that %s is of type %s instead of %s\n"
        var.vname (string_of_type t) (string_of_type var.vtype);
      Inthash.add vidToType var.vid t

        
  (* Look for casts on the formal itself *)      
  method private checkExp exp =
    let rec seekCastFormal exp tOpt =
      match exp with
        (* Main pattern we look for *)
        CastE (t, Lval(Var(vi), NoOffset)) -> 
          if Inthash.mem curSeek vi.vid then
            self#mapType vi t
      | CastE (t, e) ->
          seekCastFormal e (Some t)
      | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AddrOf _ | StartOf _
      | SizeOfE _ | AlignOfE _ ->
          ()
      | UnOp (_, e, _) ->
          seekCastFormal e None
      | BinOp (_, e1, e2, _) ->
          seekCastFormal e1 None; seekCastFormal e2 None
      | Lval (Var vi, NoOffset) ->
          (* Warn if we got to formal w/ random intermediate cast? *)
          if Inthash.mem curSeek vi.vid then
            (match tOpt with
               Some (t) -> failwith "Hit formal w/ other random cast?"
             | None -> ())
      | Lval (Var vi, _) -> ()
      | Lval (Mem (e), _) ->
          seekCastFormal e None
    in
    seekCastFormal exp None

  method vinst (i:instr) : instr list visitAction =
    (match i with
       Asm (_) -> ()
     | Call(_,callexp,args,_) ->
         List.iter self#checkExp args;
         self#checkExp callexp;
     | Set(lv, exp, _)  ->
         self#checkExp exp;
    ); DoChildren

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

end
