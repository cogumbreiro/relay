(** Collection of "to string" functions for various Cil types *)

open Cil
open Pretty

let buffsize = 80


(******************************)
(* Lval-Expressions and Lvals *)

class lvalPrinter = object 
  inherit defaultCilPrinterClass as super
    
  method pAttr (a:attribute) =
    (Pretty.nil, false)
      
  method pAttrParam (_:unit) (a:attrparam) =
    Pretty.nil
      
  method pAttrs (_:unit) (a:attributes) =
    Pretty.nil
      
  method pLineDirective ?(forcefile:bool option) (l:location) =
    Pretty.nil
    
end

let lvPrinter = new lvalPrinter


let string_of_exp (e:exp) : string =
  sprint buffsize (printExp lvPrinter () e)
  
let string_of_lval (lv:lval) : string =
  sprint buffsize (printLval lvPrinter () lv)
    
let string_of_loc (loc:location) : string =
  sprint buffsize (d_loc () loc)

let string_of_pp (pp:prog_point) : string =
  sprint buffsize (d_pp () pp)

let string_of_stmt (s:stmt) : string =
  sprint buffsize (d_stmt () s)

let string_of_instr (i:instr) : string =
  sprint buffsize (d_instr () i)

let string_of_type (t:typ) : string =
  sprint buffsize (d_type () t)

let string_of_offset (off:offset) : string =
  sprint buffsize (d_offset Pretty.nil () off)        

(***************************************)
(* Function types and call expressions *)

class noAttPrinter = object 
  inherit defaultCilPrinterClass

  method pAttr (a:attribute) =
    (Pretty.nil, false)
      
  method pAttrParam (_:unit) (a:attrparam) =
    Pretty.nil

  method pAttrs (_:unit) (a:attributes) =
    Pretty.nil

  method pLineDirective ?(forcefile:bool option) (l:location) =
    Pretty.nil

end

let noAP = new noAttPrinter

(* Strip whitespace *)
let stripWSRE = Str.regexp "[\n\r\t]"

let stripSPRE = Str.regexp "[ ]+"

(* Remove most whitespace, leaving at most one space *)
let stripWS s =
  Str.global_replace stripSPRE " " (Str.global_replace stripWSRE " " s)

let unrollType x = x (* Cil.unrollType x *)

(* Strip the name parts of a function's arg list entry
 * How to strip comments? yes for some reason it prints comments
 *)
let simplifyArgs ((name, typ, attrib) : string * typ * attributes) =
  ("", unrollType typ, attrib)  


(* Make a string out of a function type, stripping names from
 * the arg list entries *)
let rec string_of_ftype (t:typ) =
  match t with
    TFun (resT, argL, varArgP, atts) -> 
      begin
        match argL with
          None -> 
            let modTFun = TFun 
              (unrollType resT, argL, varArgP, atts) in
            stripWS (sprint buffsize (printType noAP () modTFun))
        | Some l ->
            let argNoName = List.map simplifyArgs l in
            (* strip function attributes here too *)
            let modTFun = TFun 
              (unrollType resT, Some argNoName, varArgP, atts) in
            stripWS (sprint buffsize (printType noAP () modTFun))
      end
  | TPtr (t,_) ->
      string_of_ftype t
  | TArray (t,_,_) ->
      string_of_ftype t
  | TNamed (tinfo,_) ->
      string_of_ftype tinfo.ttype
  | _ ->
      let normalTypString = (sprint buffsize (printType noAP () t)) in
      normalTypString


(* Make a string out of a call expression *)
let string_of_cexp (callexp:exp) = 
  let fType = Cil.typeOf callexp in
  string_of_ftype fType


