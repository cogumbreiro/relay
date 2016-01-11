open Cil

(* Contributed by Nathan Cooprider *)

let isOne e = 
  isInteger e = Some Int64.one


(* written by Zach *)
let is_volatile_tp tp =
  List.exists (function (Attr("volatile",_)) -> true 
    | _ -> false) (typeAttrs tp) 
    
(* written by Zach *)
let is_volatile_vi vi =
  let vi_vol =
    List.exists (function (Attr("volatile",_)) -> true 
      | _ -> false) vi.vattr in
  let typ_vol = is_volatile_tp vi.vtype in
  vi_vol || typ_vol

(*****************************************************************************
 * A collection of useful functions that were not already in CIL as far as I 
 * could tell. However, I have been surprised before . . . 
 ****************************************************************************)

type sign = Signed | Unsigned 

exception Not_an_integer

(*****************************************************************************
 * A bunch of functions for accessing integers. Originally written for 
 * somebody who didn't know CIL and just wanted to mess with it at the 
 * OCaml level. 
 ****************************************************************************)

let unbox_int_type (ye : typ) : (int * sign) =
  let tp = unrollType ye in
  let s = 
    match tp with 
      TInt (i, _) -> 
	if (isSigned i) then
	  Signed
	else
	  Unsigned
    | _ -> raise Not_an_integer
  in
  (bitsSizeOf tp), s
  
(* depricated. Use isInteger directly instead *)
let unbox_int_exp (e : exp) : int64 = 
  match isInteger e with 
    None -> raise Not_an_integer
  | Some (x) -> x
  
let box_int_to_exp (n : int64) (ye : typ) : exp =
  let tp = unrollType ye in
  match tp with 
    TInt (i, _) -> 
      kinteger64 i n 
  | _ -> raise Not_an_integer

let cil_to_ocaml_int (e : exp) : (int64 * int * sign) = 
  let v, s = unbox_int_type (typeOf e) in
  unbox_int_exp (e), v, s

exception Weird_bitwidth

(* (int64 * int * sign) : exp *)
let ocaml_int_to_cil v n s =
  let char_size = bitsSizeOf charType in 
  let int_size = bitsSizeOf intType in
  let short_size = bitsSizeOf (TInt(IShort,[]))in 
  let long_size = bitsSizeOf longType in
  let longlong_size = bitsSizeOf (TInt(ILongLong,[])) in
  let i = 
    match s with
      Signed ->
	if (n = char_size) then 
	  ISChar
	else if (n = int_size) then
	  IInt
	else if (n = short_size) then
	  IShort
	else if (n = long_size) then
	  ILong
	else if (n = longlong_size) then
	  ILongLong
	else
	  raise Weird_bitwidth
    | Unsigned ->
	if (n = char_size) then 
	  IUChar
	else if (n = int_size) then
	  IUInt
	else if (n = short_size) then
	  IUShort
	else if (n = long_size) then
	  IULong
	else if (n = longlong_size) then
	  IULongLong
	else
	  raise Weird_bitwidth
  in
  kinteger64 i v

(*****************************************************************************
 * a couple of type functions that I thought would be useful:
 ****************************************************************************)

let rec isCompositeType tp =
  match tp with
    TComp _  -> true
  | TPtr(x, _) -> isCompositeType x
  | TArray(x,_,_) -> isCompositeType x
  | TFun(x,_,_,_) -> isCompositeType x
  | TNamed (x,_) -> isCompositeType x.ttype
  | _ -> false

(** START OF deepHasAttribute ************************************************)
let visited = ref [] 
class attribute_checker target rflag = object (self)
  inherit nopCilVisitor
  method vtype t =
    match t with 
      TComp(cinfo, a) ->
	if(not (List.exists (fun x -> cinfo.cname = x) !visited )) then begin
	  visited := cinfo.cname :: !visited;
	  List.iter 
	    (fun f -> 
	      if (hasAttribute target f.fattr) then 
		rflag := true
	      else
		ignore(visitCilType (new attribute_checker target rflag) 
			 f.ftype)) cinfo.cfields;
	end;
	DoChildren	
    | TNamed(t1, a) ->
	if(not (List.exists (fun x -> t1.tname = x) !visited )) then begin
	  visited := t1.tname :: !visited;
	  ignore(visitCilType (new attribute_checker target rflag) t1.ttype);
	end;
	DoChildren
    | _ ->
	DoChildren
  method vattr (Attr(name,params)) =
    if (name = target) then rflag := true;
    DoChildren
end

let deepHasAttribute s t =
  let found = ref false in
  visited := [];
  ignore(visitCilType (new attribute_checker s found) t);
  !found
(** END OF deepHasAttribute **************************************************)

(** Stuff from ptranal, slightly modified ************************************)

(*****************************************************************************
 * A transformation to make every instruction be in its own statement.  
 ****************************************************************************)

class callBBVisitor = object
  inherit nopCilVisitor 

  method vstmt s =
    match s.skind with
      Instr(il) -> begin
	if (List.length il > 1) then 
          let list_of_stmts = List.map (fun one_inst -> 
            mkStmtOneInstr one_inst) il in
          let block = mkBlock list_of_stmts in
	  s.skind <- Block block;
	  ChangeTo(s)
	else
	  SkipChildren
      end
    | _ -> DoChildren

  method vvdec _ = SkipChildren
  method vexpr _ = SkipChildren
  method vlval _ = SkipChildren
  method vtype _ = SkipChildren
end 

let one_instruction_per_statement f =
  let thisVisitor = new callBBVisitor in
  visitCilFileSameGlobals thisVisitor f  

(*****************************************************************************
 * A transformation that gives each variable a unique identifier. 
 ****************************************************************************)

class vidVisitor = object
  inherit nopCilVisitor 
  val count = ref 0 

  method vvdec vi = 
    vi.vid <- !count ;
    incr count ; SkipChildren
end 

let globally_unique_vids f =
  let thisVisitor = new vidVisitor in
  visitCilFileSameGlobals thisVisitor f 

(** End of stuff from ptranal ************************************************)

class sidVisitor = object
  inherit nopCilVisitor 
  val count = ref 0 

  method vstmt s = 
    s.sid <- !count ;
    incr count ;
    DoChildren
end 

let globally_unique_sids f =
  let thisVisitor = new sidVisitor in
  visitCilFileSameGlobals thisVisitor f 


(** Comparing expressions without a Out_of_memory error **********************)
 
let rec compare_exp x y =
 
   match x,y with
     Const(CEnum(e1,s1,ei1)),Const(CEnum(e2,s2,ei2)) -> 
       let s_comp = compare s1 s2 in 
       if (s_comp <> 0) then 
         s_comp
       else 
         let e_comp = compare_exp e1 e2 in
         if (e_comp <> 0) then
           e_comp
         else
           compare ei1.ename ei2.ename
 
   | Lval(lv1), Lval(lv2) | AddrOf(lv1),AddrOf(lv2) 
   | StartOf(lv1),StartOf(lv2)  ->
       compare_lval lv1 lv2
   | SizeOf(t1),SizeOf(t2) | AlignOf(t1),AlignOf(t2) -> 
       compare_type t1 t2
   | AlignOfE(e1), AlignOfE(e2)  
   | SizeOfE(e1), SizeOfE(e2) ->
       compare_exp e1 e2 
   | UnOp(o1,e1,t1),UnOp(o2,e2,t2) ->
       let o_comp = compare o1 o2 in
       if (o_comp <> 0) then 
         o_comp
       else
         let e_comp = compare_exp e1 e2 in
         if( e_comp <> 0) then
           e_comp
         else
           compare_type t1 t2
   | BinOp(o1,l1,r1,t1),BinOp(o2,l2,r2,t2) ->
       let o_comp = compare o1 o2 in
       if (o_comp <> 0) then
         o_comp
       else
         let l_comp = compare_exp l1 l2 in
         if( l_comp <> 0) then
           l_comp
         else
           let r_comp = compare_exp r1 r2 in
           if ( r_comp<> 0) then 
             r_comp
           else
             compare_type t1 t2
   | CastE(t1,e1),CastE(t2,e2) ->
       let t_comp = compare_type t1 t2 in
       if (t_comp <> 0) then 
         t_comp
       else 
         compare_exp e1 e2 
   | _, _ -> compare x y    (* Should be okay, since the tags are different *)
 
and compare_lval x y =  
   match x,y with
     (Var v1, o1), (Var v2, o2) ->
       let v_comp = compare_var v1 v2 in
       if (v_comp <> 0) then
         v_comp
       else
         compare_offset o1 o2 
   | (Mem p1, o1), (Mem p2, o2) ->
       let e_comp = compare_exp p1 p2 in
       if (e_comp <> 0) then
         e_comp
       else
         compare_offset o1 o2 
   | _, _ -> compare x y
 
and compare_lhost x y =
  match x, y with
    Var v1, Var v2 ->
      compare_var v1 v2
  | Mem p1, Mem p2 ->
      compare_exp p1 p2
  | _, _ -> 
      compare x y

and compare_type x y =
  compare (typeSig x) (typeSig y) 

and compare_var x y =
(*
  let s_comp = compare x.vglob y.vglob in
  if (s_comp <> 0) then
    s_comp
  else
    let n_comp = compare x.vname y.vname in
    if (n_comp <> 0) then
      n_comp
    else
      compare_type x.vtype y.vtype
*)
  (* trust vid for now... *)
  compare x.vid y.vid 

and compare_fundec x y =
  compare_var x.svar y.svar
 
and compare_offset x y = 
   match x, y with
     Field(fi1,o1),Field(fi2,o2)->
       let f_comp = compare fi1.fname fi2.fname in
       if (f_comp <> 0) then
         f_comp 
       else
         let c_comp = compare_compi fi1.fcomp fi2.fcomp in
         if (c_comp <> 0) then c_comp
         else compare_offset o1 o2

   | Index(e1,o1),Index(e2,o2) ->
       let e_comp = compare_exp e1 e2 in
       if (e_comp <> 0) then
         e_comp
       else
         compare_offset o1 o2 
   | _, _ -> compare x y

(*--------------------*)
(* Contributed by Jan *)

and compare_compi (c:compinfo) (c':compinfo) =
    let c_comp_su = compare c.cstruct c'.cstruct in
    if (c_comp_su <> 0) then c_comp_su
    else compare c.cname c'.cname
      (*          (* Assumes ckeys are fixed! *)
                  compare c.ckey c'.ckey
      *)
         
and compare_enumi (e:enuminfo) (e':enuminfo) =
  let n_comp = compare e.ename e'.ename in
  if (n_comp <> 0) then n_comp
  else 
    let len_diff = (List.length e.eitems) - (List.length e'.eitems) in
    if (len_diff <> 0) then len_diff
    else List.fold_left2 
      (fun c (s1, _, _) (s2, _, _) -> 
         if (c <> 0) then c
         else compare s1 s2) 0 e.eitems e'.eitems


let compare_instr (i1:instr) (i2:instr) : int =
  match i1, i2 with
    (Set (_, _, loc1)),
    (Set (_, _, loc2))
  | (Call (_, _, _, loc1)),
    (Call (_, _, _, loc2))
  | (Asm (_, _, _, _, _, loc1)),
    (Asm (_, _, _, _, _,loc2)) ->
      compareLoc loc1 loc2
  | (Set _), (Call _)
  | (Call _), (Asm _)
  | (Set _), (Asm _) ->
      1
  | (Call _), (Set _)
  | (Asm _), (Call _)
  | (Asm _), (Set _) ->
      -1

let hash_instr (i:instr) =
  match i with
    (Set (_, _, loc))
  | (Call (_, _, _, loc))
  | (Asm (_, _, _, _, _, loc)) ->
      Hashtbl.hash loc


let rec hash_exp x = 
   match x with
     Const(CEnum(e1,s1,ei1)) ->
       let s_hash = Hashtbl.hash s1 in 
       let e_hash = Hashtbl.hash e1 in
       let ei_hash = hash_enumi ei1 in
       s_hash lxor e_hash lxor ei_hash
   | Const(CReal(f,_,_)) ->
       Hashtbl.hash f
   | Const(CChr(c)) ->
       Hashtbl.hash c
   | Const(CStr(s)) ->
       Hashtbl.hash s

   | Lval(lv1) -> 
       0x25873541 lxor (hash_lval lv1)
   | AddrOf(lv1) ->
       0x34973258 lxor (hash_lval lv1)
   | StartOf(lv1) ->
       0x10030251 lxor (hash_lval lv1)
   | SizeOf(t1) ->
       0x22198721 lxor (hash_type t1)
   | AlignOf(t1) ->
       0x04358225 lxor (hash_type t1)
   | SizeOfE e ->
       0x11237639 lxor (hash_exp e)
   | AlignOfE(e1)  -> 
       0x19857434 lxor (hash_exp e1)

   | UnOp(o1,e1,t1) ->
       let o_hash = Hashtbl.hash o1 in
       let e_hash = hash_exp e1 in
       let t_hash = hash_type t1 in
       o_hash lxor e_hash lxor t_hash

   | BinOp(o1,l1,r1,t1) ->
       let o_hash = Hashtbl.hash o1 in
       let l_hash = hash_exp l1 in
       let r_hash = hash_exp r1 in
       let t_hash = hash_type t1 in
       o_hash lxor l_hash lxor r_hash lxor t_hash

   | CastE(t1,e1) ->
       let t_hash = hash_type t1 in
       let e_hash = hash_exp e1 in
       0x33982173 lxor t_hash lxor e_hash
   | SizeOfStr s ->
       Hashtbl.hash s
   | _ ->
       Hashtbl.hash x

and hash_lval x =  
   match x with
     (Var v1, o1) ->
       let v_hash = hash_var v1 in
       let o_hash = hash_offset o1 in
       v_hash lxor o_hash
   | (Mem p1, o1) ->
       let e_hash = hash_exp p1 in
       let o_hash = hash_offset o1 in
       e_hash lxor o_hash
 
and hash_type x =
  Hashtbl.hash (typeSig x)

and hash_var x = 
  Hashtbl.hash x.vid (* TODO: convert this to not rely on vids? *)

and hash_lhost x = 
  match x with
    Var v1 ->
      hash_var v1
  | Mem p1 ->
      hash_exp p1
 
and hash_offset x = 
   match x with
     Field(fi1,o1) ->
       let n_hash = Hashtbl.hash fi1.fname in
       let c_hash = hash_compi fi1.fcomp in
       let o_hash = hash_offset o1  in
       n_hash lxor c_hash lxor o_hash
   | Index(e1,o1) ->
       let e_hash = hash_exp e1 in
       let o_hash = hash_offset o1 in
       e_hash lxor o_hash
   | NoOffset ->
       0x13474137

and hash_fundec x =
  0x24788113 lxor (hash_var x.svar)

and hash_compi (c:compinfo) =
  0x32849 lxor Hashtbl.hash c.cname 
    (*          (* Assumes ckeys are fixed! *)
                Hashtbl.hash c.ckey
    *)

and hash_enumi (e:enuminfo) =
  Hashtbl.hash e.ename


(** Compare two chains of offsets
    @return Some(0)  if they are equal
            Some(-1) if off1 is a subchain of off2
            Some(1)  if off2 is a subchain of off1
            None     if incomparable
*)
let rec isSubchainOffset (off1:offset) (off2:offset) : int option =
  match off1, off2 with
    NoOffset, NoOffset -> Some(0)
  | Field (fi1,rest1), Field (fi2,rest2) -> 
      let c = compare fi1.fname fi2.fname in
      if(c == 0) then 
        (* If field names are the same, compare types as well *)
        if ((compare_type fi1.ftype fi2.ftype) == 0) then
          isSubchainOffset rest1 rest2
        else
          None
      else
        None
  | Index (iexp1, rest1), Index (iexp2, rest2) -> begin
      match compare_exp iexp1 iexp2 with 
        0 -> isSubchainOffset rest1 rest2
      | _ -> None
    end
  | _, _ -> (* Must be different *)
      None




(** Compare the current node of the two given offsets 
    (not further offsets in the chain).
    @return 0 if they are equal, 
              or a non-zero value if they are not 
*)
let compareCurOffset (off1:offset) (off2:offset) : int =
  match off1, off2 with
    NoOffset, NoOffset -> 0
  | Field (fi1, _), Field (fi2,_) -> 
      let c = compare fi1.fname fi2.fname in
      if(c != 0) then 
        c
      else 
        (* If field names are the same, compare types as well *)
        compare_type fi1.ftype fi2.ftype
  | Index (iexp1, _), Index (iexp2, _) -> 
      compare_exp iexp1 iexp2
 
  | _, _ -> (* Must be different *)
      1




(** Add nodes from a chain of offsets to a given stack *)
let rec offsetToStack (off:offset) (curStack:offset Stack.t) = 
  match off with
    NoOffset -> ()
  | Field (fi, nextOff) -> 
      Stack.push (Field (fi, NoOffset)) curStack; (* Just this node *)
      offsetToStack nextOff curStack
  | Index (iexp, nextOff) ->
      Stack.push (Index (iexp, NoOffset)) curStack; (* Just this node *)
        offsetToStack nextOff curStack


(** Take the offset nodes from the stack and put them back together as 
    an offset chain
*)
let rec stackToOffset (curStack:offset Stack.t) (offSuffix:offset) =
  try
    let topOff = Stack.pop curStack in
    match topOff with
      NoOffset -> prerr_string 
        "Error in stackToOffset: found NoOffset in stack";
        stackToOffset curStack offSuffix
    | Field (fi, _) ->
        stackToOffset curStack (Field (fi, offSuffix))
    | Index (iexp, _) ->
        stackToOffset curStack (Index (iexp, offSuffix))
  with Stack.Empty ->
    offSuffix




(** Pop the top portion of the stack if the elems. are equivalent *)
let rec popEQ (s1:'a Stack.t) (s2:'a Stack.t) (comp) : unit =
  if (Stack.is_empty s1 || Stack.is_empty s2) then
    ()
  else
    let o1 = Stack.top s1 in
    let o2 = Stack.top s2 in
    if ((comp o1 o2) == 0) then
      let _ = Stack.pop s1 in
      let _ = Stack.pop s2 in
      popEQ s1 s2 comp
    else
      ()



(** Remove suffix of offsets if they are equal (by converting to NoOffset) *)
let cutEQSuffixOffsets (off1:offset) (off2:offset)
    : (offset * offset) =
  let stack1 = Stack.create () in
  offsetToStack off1 stack1;
  let stack2 = Stack.create () in
  offsetToStack off2 stack2;
  (* Pop EQ parts of suffix *)
  popEQ stack1 stack2 compareCurOffset;
  (* Chain all back together *)
  (stackToOffset stack1 NoOffset,
   stackToOffset stack2 NoOffset)        


    
(** Calculate the type of an expression after dereference, given the
    type before dereference *)
let rec typeAfterDeref (typ:typ) =
  match typ with
    TPtr (t, _) ->
      t
  | TArray (t, _, _) -> 
      t
  | TFun _ -> (* Assume dereferencing a pointer to a func is the same
                 as just the func *)
      typ 
  | TNamed (tinfo, _) -> (* unroll the type a step *)
      typeAfterDeref tinfo.ttype
  | _ -> failwith "typeAfterDeref: given a non-pointer typ to deref"


(** true if the tail of the offsets are the "same" *)
let eq_offset_tail o1 o2 =
  let s1 = Stack.create () in
  let s2 = Stack.create () in
  offsetToStack o1 s1;
  offsetToStack o2 s2;
  if (Stack.is_empty s1) then
    if (Stack.is_empty s2) then
      true
    else
      false
  else
    if(Stack.is_empty s2) then
      false
    else
      let last1 = Stack.pop s1 in
      let last2 = Stack.pop s2 in
      match last1, last2 with
        NoOffset, NoOffset -> 
          true
      | Field (fi1, _), Field (fi2, _) ->
          let f_comp = compare fi1.fname fi2.fname in
          
          if (f_comp <> 0) then
            false
          else
            
            let c_comp_su = compare 
              fi1.fcomp.cstruct fi2.fcomp.cstruct in
            if (c_comp_su <> 0) then
              false
            else 
              let c_comp_n = compare
                fi1.fcomp.cname fi2.fcomp.cname in
              if (c_comp_n <> 0) then
                false
              else
                true
            
            (*             (* Assumes ckeys are fixed! *)
            let c_comp = compare fi1.fcomp.ckey fi2.fcomp.ckey in
            c_comp == 0
            *)
      | Index (_, _), Index (_, _) ->
          true
      | _ -> 
          false
