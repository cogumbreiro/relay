

(** Translate assertions in the form of CIL expressions and make 
    a theorem prover query (for Simplify).

    Borrowed from Westley Weimer.

    Currently not used.
*)


open Cil
open Pretty


(*************************************************************************** 
 * These are some global configuration settings 
 ***************************************************************************)

let be_quiet = ref false (* when true, we only print out error messages *) 
let debug_tp = ref false (* when true, we print out theorem prover calls *)


(* 
 * This "debug" function works like printf, but only emits output if
 * !be_quiet is not set. 
 *) 
let debug fmt =  
  let k result = 
    if not !be_quiet then begin
      output_string stdout result ;
      flush stdout 
    end 
  in 
  Printf.kprintf k fmt 

(*************************************************************************** 
 *
 * We will make calls to Simplify theorem prover to decide if a certain
 * symbolic expression must be true or not. For example, given that we know
 * "x0 = 55", we should be able to reason that "y = x0 ; y > 100" is false. 
 * The theorem prover is part of that. 
 *
 * The theorem prover interface also encapsulates some basic knowledge
 * about C programs. For example:
 *
 * (1) String constants like "hello" are never zero.
 * (2) Character constants besides '\000' are not zero.
 * (3) In C, "if (x)" really means "if (x != 0)".
 * (4) &variable is never zero. 
 * (5) &v1 != &v2 
 *
 * This sounds cool, but in reality the Theorem prover will not be able to
 * solve everything for you. It will often return "Unknown" -- for example,
 * in questions involving multiplication or division. Also, it doesn't
 * understand bitwise operations. 
 ***************************************************************************)

(* theorem provers are incomplete, so often "Unknown" is the answer *) 
type tp_result = 
  | Must_Be_True
  | Must_Be_False
  | Unknown

(* Uses an external theorem prover to try to decide if "e" is necessarily
 * true. *) 
let valid_regexp = Str.regexp ".*Valid\\." 
let invalid_regexp = Str.regexp ".*Invalid\\." 
let bad_regexp = Str.regexp ".*Bad input" 

(* This powerful procedure asks the Theorem Prover to decide if "e" 
 * must be true. If anything goes wrong, we return Unknown. *) 
let decideHelp (assumptions:Cil.exp list) (e:Cil.exp): tp_result = 
  match e with
  (* for constants, we don't even need to ask the theorem prover --
   * we can decide it locally *) 
  | Const(CInt64(x,_,_)) -> 
    if x = Int64.zero then Must_Be_False else Must_Be_True
  | Const(CStr(_)) 
  | Const(CWStr(_)) -> Must_Be_True
  | Const(CChr(c)) -> if c = '\000' then Must_Be_False else Must_Be_True
  | _ -> begin 

  (* if the expression is already a relational operator, we don't need
   * to coerce on the implicit "!= 0" suffix *) 
  let is_relop bop = match bop with
  | Lt                                  (** <  (arithmetic comparison) *)
  | Gt                                  (** >  (arithmetic comparison) *)  
  | Le                                  (** <= (arithmetic comparison) *)
  | Ge                                  (** >  (arithmetic comparison) *)
  | Eq                                  (** == (arithmetic comparison) *)
  | Ne                                  (** != (arithmetic comparison) *)
  | LAnd                                (** logical and. *) 
  | LOr                                 (** logical or. *) 
  -> true
  | _ -> false 
  in 
  let must_coerce e = match e with
  | BinOp(bop, _, _, _) when is_relop bop -> false
  | UnOp(LNot,_,_) -> false 
  | _ -> true 
  in

  (* Now we want to convert the CIL AST expression into a Simplify Query
   * String. Simplify expects queries like
   *
   * (IMPLIES (= X 5) (>= (+ X 2) 7))
   *
   * However, you cannot just use program variables directly -- you might
   * have a C program variable named "IMPLIES", but that would confuse
   * Simplify. So we rename all of the variables involved to v0, v1, ...
   *)
  let vht = Hashtbl.create 255 in
  let vht_counter = ref 0 in 
  let handle_va va =
    try 
      Hashtbl.find vht va.vname
    with Not_found -> begin
      let res = Printf.sprintf "v%d" !vht_counter in
      incr vht_counter ;
      Hashtbl.add vht va.vname res ;
      res 
    end 
  in 
  (* We also introduce a special symbolic expression representing
   * the address of a variable. We know that addresses are all distinct and
   * they are all non-null. *) 
  let addrs_mentioned = Hashtbl.create 5 in 
  let handle_addrof va = 
    let res = "a" ^ (handle_va va) in 
    Hashtbl.replace addrs_mentioned res () ; 
    res
  in 

  (* This bit does a recursive walk to structurally convert a CIL
   * expression into a Simplify query. Note that C has implicit conversions
   * between bools and ints but that Simplify does not. Thus we insert 
   * " != 0" for int-like C expressions. *) 
  let rec to_str e want_bool = 
    let result = match e with
    | Const(CInt64(i64,_,_)) -> Int64.to_string i64
    | Const(CStr(_)) 
    | Const(CWStr(_)) 
    | Const(CChr(_)) 
    | Const(CReal(_,_,_)) -> failwith "don't know how ask simplify about this" 
    | AddrOf(Var(va),NoOffset) -> handle_addrof va 
    | AddrOf(lv) -> failwith "fixme -- I should handle &lval" 
    | Lval(Var(va),NoOffset) -> handle_va va
    | Lval(_) -> failwith "fixme -- I should handle other lvalues" 
    | UnOp(Neg,e,_) -> Printf.sprintf "(- 0 %s)" (to_str e false) 
    | UnOp(LNot,e,_) -> Printf.sprintf "(NOT %s)" (to_str e true) 
    | BinOp(bop,e1,e2,_) -> begin
      let bop_str, want_bool = match bop with
      | PlusA | PlusPI -> "+", false 
      | MinusA | MinusPI | MinusPP -> "-", false 
      | Mult -> "*", false 
      | Div -> "/", false 
      | Mod -> "%", false 
      | Lt -> "<", false 
      | Gt -> ">", false 
      | Le -> "<=" , false 
      | Ge -> ">=", false 
      | Ne -> "NEQ", false
      | Eq -> "EQ", false

      | LAnd -> "AND", true
      | LOr -> "OR", true
      | _ -> failwith "don't know how to ask simplify about this" 
      in
      Printf.sprintf "(%s %s %s)" bop_str (to_str e1 want_bool) 
        (to_str e2 want_bool) 
    end 
    | _ -> failwith "don't know how to ask simplify about this" 
    in 
    if want_bool && must_coerce e then 
      Printf.sprintf "(NEQ %s 0)" result 
    else
      result 
  in

  if not !be_quiet && !debug_tp then begin
    ignore (Pretty.printf "Theorem Prover Query: @[%a@]@!" 
      defaultCilPrinter#pExp e) ;
  end ; 

  try
    let str = to_str e true in
    (* we get to use all of the assumptions we have built up along this
     * path in order to help prove the query *) 
    let assumption_string_list = List.map (fun assumption -> 
      try (to_str assumption false) with _ -> "TRUE" 
    ) assumptions in
    (* we also get to assume that addresses are distinct and non-null *) 
    let distinct_assumption = 
      (Hashtbl.fold (fun name () str -> 
        str ^ name ^ " "
      ) addrs_mentioned "(DISTINCT 0 ") ^ ")"
    in 
    let make_query str = match 
      distinct_assumption :: assumption_string_list with
    | [] -> str
    | big_list -> 
      let b = Buffer.create 255 in
      Printf.bprintf b "(IMPLIES (AND" ;
      List.iter (fun elt -> Printf.bprintf b " %s" elt) big_list ;
      Printf.bprintf b ") %s)" str ;
      Buffer.contents b 
    in 
    let query str = 
      (if !debug_tp then debug "Theorem Prover Asking: %s\n" str) ; 
      try 
        (* in real life you do *NOT* want to spawn a new process every time
         * you make a theorem prover query -- however, for this homework
         * it inspires you not to make too many theorem prover calls *)
        let inchan, outchan = Unix.open_process "./Simplify" in 
        output_string outchan (str ^ "\n") ; flush outchan ; 
        let finished = ref None in 
        let rec scan () = match !finished with
        | None -> 
          let reply = input_line inchan in 
          (if !debug_tp then debug "Theorem Prover Reply: %s\n" reply) ; 
          (if Str.string_match valid_regexp reply 0 then
            finished := Some(Must_Be_True)
          else if Str.string_match invalid_regexp reply 0 then
            finished := Some(Must_Be_False)
          else if Str.string_match bad_regexp reply 0 then begin 
            finished := Some(Unknown)
          end 
          ) ;
          scan () 
        | Some(x) -> 
          let _ = Unix.close_process (inchan,outchan) in 
          x
        in 
        scan () 
      with _ -> Unknown
    in 
    (* because Simplify is incomplete, it may say that both PREDICATE
     * and (NOT PREDICATE) can or cannot be proved. We only trust it when
     * it really knows. 
     *
     * As a concrete example (try it!) Simplify will say that both:
     * (IMPLIES (EQ X 5) (EQ ( * X X) 25))
     * and 
     * (IMPLIES (EQ X 5) (NOT (EQ ( * X X) 25)))
     * are Invalid (= not provable by Simplify).
     *)
    let q1 = make_query str in 
    let q2 = make_query (Printf.sprintf "(NOT %s)" str) in
    (match query q1, query q2 with
    | Must_Be_True, Must_Be_False -> Must_Be_True
    | Must_Be_False, Must_Be_True -> Must_Be_False
    | _, _ -> Unknown)
  with _ -> Unknown
  end 
  
let decide (assumptions:Cil.exp list) (e:Cil.exp) : tp_result = 
  let result = decideHelp assumptions e in 
  if not !be_quiet && !debug_tp then begin
    ignore (Pretty.printf "Theorem Prover Decide: @[%a@] = %s@!" 
      defaultCilPrinter#pExp e
      (match result with
      | Must_Be_True -> "true"
      | Must_Be_False -> "false"
      | _ -> "???")
      ) ;
  end ; 
  result 


let setQuiet (q:bool) =
  be_quiet := q

let setDebug (d:bool) =
  debug_tp := d
