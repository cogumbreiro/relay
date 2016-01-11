(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(* JAN: modified to heapify locals into SEPARATE cells instead of all
   sharing the same heap cell. Removed stack guard clone... *)

(* 
 * Heapify: a program transform that looks over functions, finds those
 * that have local (stack) variables that contain arrays, puts all such
 * local variables into a single heap allocated structure, changes all
 * accesses to such variables into accesses to fields of that structure
 * and frees the structure on return. 
 *)
open Cil

(* actual Heapify begins *)

let heapifyNonArrays = ref false

(* Does this local var contain an array? *)
let rec containsArray (t:typ) : bool =  (* does this type contain an array? *)
  match unrollType t with
    TArray _ -> true
  | TComp(ci, _) -> (* look at the types of the fields *)
      List.exists (fun fi -> containsArray fi.ftype) (!getCfields ci)
  | _ -> 
    (* Ignore other types, including TInt and TPtr.  We don't care whether
       there are arrays in the base types of pointers; only about whether
       this local variable itself needs to be moved to the heap. *)
   false


class heapifyModifyVisitor var_to_ptr free (currentFunction: fundec) = object(self)
  inherit nopCilVisitor  (* visit lvalues and statements *)

  method vlval l = match l with (* should we change this one? *)
    Var(vi),vi_offset when List.mem_assoc vi var_to_ptr ->
      let new_ptr = List.assoc vi var_to_ptr in (* find new ptr var *)
      let new_lval = mkMem (Lval(new_ptr, NoOffset)) vi_offset in
      ChangeDoChildrenPost(new_lval, (fun l -> l))
  | _ -> DoChildren (* ignore other lvalues *)

  method private make_free_instrs loc =
    List.map (fun (_, new_ptr) ->
                Call(None,free,[Lval(new_ptr, NoOffset)],loc)
             ) var_to_ptr


  method vstmt s = match s.skind with (* also rewrite the return *)
    Return(None,loc) -> 
      (* Free each of the new ptrs *)
      let free_instrs = self#make_free_instrs loc in
      self#queueInstr free_instrs; (* insert free_instrs before the return *)
      DoChildren

  | Return(Some exp ,loc) ->
      (* exp may depend on big_struct, so evaluate it before calling free. 
       * This becomes:  tmp = exp; free(big_struct); return tmp; *)
      let exp_new = visitCilExpr (self :> cilVisitor) exp in
      let ret_tmp = makeTempVar currentFunction (typeOf exp_new) in
      let eval_ret_instr = Set(var ret_tmp, exp_new, loc) in
      (* insert the instructions before the return *)
      self#queueInstr (eval_ret_instr :: self#make_free_instrs loc);
      s.skind <- (Return(Some(Lval(var ret_tmp)), loc));
      DoChildren
  | _ -> DoChildren (* ignore other statements *)

end
    
class heapifyAnalyzeVisitor f alloc free = object (self)
  inherit nopCilVisitor (* only look at function bodies *)

  method vglob gl = match gl with
    GFun(fundec,funloc) -> 
      let counter = ref 0 in (* the number of local vars containing arrays *)
      let varlist = ref [] in  (* a list of (var,id) pairs, in reverse order *)
      List.iter (fun vi ->  
         (* find all local vars with arrays.  If the user requests it,
            we also look for non-array vars whose address is taken. *)
        if (containsArray vi.vtype) || (vi.vaddrof && !heapifyNonArrays)
        then begin
          varlist := (vi,!counter) :: !varlist ; (* add it to the list *)
          incr counter (* put the next such var in the next slot *)
        end
        ) fundec.slocals ; 
      if (!varlist <> []) then begin (* some local vars contain arrays *)
        self#make_new_ptrs !varlist fundec funloc
      end else
	    DoChildren	(* ignore everything else *)
  | _ -> DoChildren
      
      
  method private make_new_ptrs varlist fundec funloc =
    let list_with_new_vars = 
      List.map 
        (fun (var, id) ->
           let ptrVi = makeLocalVar fundec 
             (var.vname ^ "__heapify__" ^ string_of_int id)
             (TPtr(var.vtype,[])) in
           (var, Var ptrVi)
        ) varlist
    in
    (* rewrite accesses to local vars *)
    let modify = new heapifyModifyVisitor list_with_new_vars free fundec in
    fundec.sbody <- visitCilBlock modify fundec.sbody ;
    let alloc_stmt = self#make_allocs list_with_new_vars funloc in
    fundec.sbody.bstmts <- alloc_stmt :: fundec.sbody.bstmts ; 
	fundec.slocals <- 
      List.filter (fun vi -> (* remove local vars *)
	                 not (List.mem_assoc vi varlist)) fundec.slocals ; 
    ChangeTo([GFun(fundec,funloc)])  (* done! *)

      
  method private make_allocs var_to_ptr funloc =
    let allocs = List.map 
      (fun (var, ptr) ->
         Call(Some (ptr, NoOffset), alloc, [SizeOf (var.vtype)], funloc)
      ) var_to_ptr in
    mkStmt (Instr allocs)


end
    
let heapify (f : file) (alloc : exp) (free : exp)  =
  visitCilFile (new heapifyAnalyzeVisitor f alloc free) f;
  f

(* heapify code ends here *)
    

let default_heapify (f : file) =
(*  let alloc_fun = emptyFunction "malloc" in
  let free_fun = emptyFunction "free" in *)
  let alloc_type = TFun (voidPtrType, Some [("", uintType, [])], false, []) in  
  let alloc_fun = findOrCreateFunc f "malloc" alloc_type in
  let alloc_exp = (Lval((Var(alloc_fun)),NoOffset)) in
  let free_type = TFun (voidType, Some [("", voidPtrType, [])], false, []) in
  let free_fun = findOrCreateFunc f "free" free_type in
  let free_exp = (Lval((Var(free_fun)),NoOffset)) in
  ignore (heapify f alloc_exp free_exp)    

let feature : featureDescr = 
  { fd_name = "heapify_sep";
    fd_enabled = Cilutil.doHeapifySep;
    fd_description = "move stack-allocated arrays separate the heap cells" ;
    fd_extraopt = [
      "--heapify_locals", Arg.Set heapifyNonArrays,
      "When using heapify, move all local vars whose address is taken, not just arrays.";
    ];
    fd_doit = (function (f: file) -> default_heapify f);
    fd_post_check = true;
  } 
      





