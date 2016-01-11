(** Simplified canonical represenation of an lval *)

open Cil
open Pretty
open Cildump

exception ExpNotLval (* of string *)

(* String describing the lval (or exp that may be an lval) and the 
   location lval was declared, if known (may not know if exp is not an lval) *)
type canon_lvalexp = {
  canon_desc : string;
  canon_loc  : location option;
}


let rec findTopLval (e:exp) : lval =
  match e with
    Lval lv -> lv
  | StartOf lv -> lv
  | AddrOf lv -> lv
  | CastE (t, exp) -> findTopLval exp
  | BinOp (PlusPI, ptrexp, _, _) 
  | BinOp (MinusPI, ptrexp, _, _)
  | BinOp (IndexPI, ptrexp, _, _) -> findTopLval ptrexp
  | _ -> raise (ExpNotLval) (* (string_of_exp e)) *)


let rec findTopVinfo (lv:lval): varinfo =
  match lv with
    (Var vinfo, _) ->
      vinfo
  | (Mem exp, _) ->
      findTopVinfo (findTopLval exp)
  
      
let rec findTopLoc (lv:lval): location =
  match lv with
    (Var vinfo, _) ->
      vinfo.vdecl
  | (Mem exp, _) ->
      findTopLoc (findTopLval exp)
          
          
(* TODO simplify lvals/exps (e.g., &*x = x) *)

let canonicize_lvexp (e:exp) : canon_lvalexp =
  {canon_desc = string_of_exp e;
   canon_loc = 
      try 
        Some (findTopLoc (findTopLval e));
      with ExpNotLval -> (* _ -> *)
        None
  }

let canonicize_lval (lv:lval) : canon_lvalexp =
  {canon_desc = string_of_lval lv;
   canon_loc = try 
     Some (findTopLoc lv);
   with ExpNotLval -> 
     None
(* Saw in httpd_comb.:
   "((void * )0))->auth_pwfile)"
   as an lval... WHY people?!
*)
    (* raise (ExpNotLval (string_of_lval lv)) *)
  }

