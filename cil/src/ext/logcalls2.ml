
(*
 *
 * Copyright (c) 2009, 
 *  Jan Voung           <jvoung@cs.ucsd.edu>
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

(** Log function name of each reached function into a file 
    (name of file based on pid -- vs logcalls.ml which just calls printf) *)

open Pretty
open Cil
open Trace
module E = Errormsg

let i = ref 0
let name = ref ""
let usePthreads = ref false (** True if compiled code uses pthreads *)

(* TODO: be able to pick a fixed directory for these log files (e.g., /tmp/) *)
let log_file_prefix = ref "_logged_calls_"


(** When we insert code we will need the Cil varinfo(s), typ(s), etc. 
    The following structures keep track of what already exists in the file. *)
  
type logCompinfo = {
  mutable logCompname : string;
  mutable logCompinfo : compinfo option;
}

type logTypedef = {
  mutable logTypename : string;
  mutable logTypedefT : typeinfo option;
}

type logVarInfo = {
  mutable logVname : string;
  mutable logVarinfo : varinfo option;
  mutable logProto : typ;
}

(* required varinfos (mostly functions) *)

let myPrinter = {
  logVname = "__my_log_calls__";
  logVarinfo = None;
  logProto = TFun (voidType, Some [("str", charConstPtrType, [])], false, []);
}

let myFileDescriptor = {
  logVname = "_my_log_file__";
  logVarinfo = None;
  logProto = voidType; (* Change later -- depends on FILE * *)
}

let printfInfo = {
  logVname = "fprintf";
  logVarinfo = None;
  logProto = voidType; (* Change later -- depends on FILE * *)
}

let sprintfInfo = {
  logVname = "sprintf";
  logVarinfo = None;
  logProto = TFun (intType, Some [("str", charPtrType, []);
                                  ("format", charConstPtrType, [])],
                   true, []);
}

let strcpyInfo = {
  logVname = "strcpy";
  logVarinfo = None;
  logProto = TFun (charPtrType, Some [("dest", charPtrType, []);
                                      ("src", charConstPtrType, [])],
                   false, []);
}

let strcatInfo = {
  logVname = "strcat";
  logVarinfo = None;
  logProto = TFun (charPtrType, Some [("dest", charPtrType, []);
                                      ("src", charConstPtrType, [])],
                   false, []);
}

let pidInfo = {
  logVname = "getpid"; 
  logVarinfo = None;
  logProto = voidType; (* Change later -- depends on pid_t *)
}

let fopenInfo = {
  logVname = "fopen";
  logVarinfo = None;
  logProto = voidType; (* Change later -- depends on FILE * *)
}

(* required typedefs *)

let pid_t_typedef = {
  logTypename = "pid_t";
  logTypedefT = None;
}

let pid2_t_typedef = {     (* Gah, why the indirection in typedefs?! *)
  logTypename = "__pid_t";
  logTypedefT = None;
}

let file_io_typedef = {
  logTypename = "FILE";
  logTypedefT = None;
}

(* required compinfos *)

let file_io_comp = {
  logCompname = "_IO_FILE";
  logCompinfo = None;
}

(************************************************************)

let funcsNeedingVarinfos = [myPrinter; printfInfo; pidInfo; 
                            fopenInfo; sprintfInfo; strcpyInfo; strcatInfo]
let compinfosNeeded = [file_io_comp]
let typeDefsNeeded = [pid_t_typedef; pid2_t_typedef; file_io_typedef]

(*********)

let searchForDelcs file =
  let mainInFile = ref false in
  let searchForDecls = function
    | GType (tinfo, _) ->
        List.iter 
          (fun typedefInfo ->
             if typedefInfo.logTypename = tinfo.tname then
               typedefInfo.logTypedefT <- Some tinfo
          ) typeDefsNeeded
          
    | GCompTag (cinfo, _)
    | GCompTagDecl (cinfo, _) ->
        List.iter 
          (fun compInfo ->
             if compInfo.logCompname = cinfo.cname then
               compInfo.logCompinfo <- Some cinfo
          ) compinfosNeeded
          
    | GVarDecl (vinfo, _) ->
        List.iter 
          (fun info ->
             if info.logVname = vinfo.vname then
               info.logVarinfo <- (Some vinfo)
          ) funcsNeedingVarinfos
          
    | GFun (fdec, loc) ->
        if fdec.svar.vname = "main" then mainInFile := true
    | GText _ | GPragma _ | GAsm _ | GVar _ | GEnumTag _ | GEnumTagDecl _ ->
        ()
  in
  iterGlobals file searchForDecls;
  !mainInFile


let getFVarinfo finfo : varinfo = 
  match finfo.logVarinfo with 
    Some v -> v
  | None -> begin 
      let v = makeGlobalVar finfo.logVname finfo.logProto in
      finfo.logVarinfo <- Some v;
      v
    end

let getTypeinfo tinfo defaultType : typeinfo =
  match tinfo.logTypedefT with
    Some ti -> ti
  | None -> begin
      let ti = { tname = tinfo.logTypename;
                 ttype = defaultType;
                 treferenced = true; } in
      tinfo.logTypedefT <- Some ti;
      ti
    end

let getCompinfo cinfo : compinfo =
  match cinfo.logCompinfo with
    Some ci -> ci
  | None -> begin
      let ci = mkCompInfo true cinfo.logCompname (fun ci -> []) [] in
      ci.creferenced <- true;
      cinfo.logCompinfo <- Some ci;
      ci
    end

(** Injects the body of the logger (not thread safe) *)
let makeMyLogger myprint myFile 
    fopen fprintf getpid strcat strcpy sprintf pidType =
  let fundec = { svar = myprint;
                 sformals = [];
                 slocals = [];
                 smaxid = 0;
                 sbody = mkBlock []; 
                 smaxstmtid = None;
                 sallstmts = [];
               } in 
  (******* code will be:

     void myLogger(const char * str) {
       pid_t tempPID;
       char tempFName[120];
       char tempITOA[120];
       char *mode = "a+";

       if (myFile == 0) {
         tempPID = getpid();
         strcpy(tempFName, "_logged_calls_");
         sprintf(tempITOA, "%d", tempPID);
         strcat(tempFName, tempITOA);
         myFile = fopen (tempFName, mode);
       }
       fprintf(myFile, "%s", str);
     }

  *******)

  (** Actually, we want to add the formal "str". makeFormalVar will modify
      the function var's vtype, but that is okay because it will only
      update the type based on the new formals list (which will include
      the formal "str" as it did already in the type *)
  let strFormal = makeFormalVar fundec "str" charConstPtrType in
  let tempPID = makeLocalVar fundec "tempPID" pidType in
  let charBuffType = TArray(charType, Some (integer 120), []) in
  let tempITOA = makeLocalVar fundec "tempITOA" charBuffType in
  let tempFName = makeLocalVar fundec "tempFName" charBuffType in
  let mode = makeLocalVar fundec "mode" charPtrType in
  
  let assignMode = mkStmtOneInstr 
    (Set ((Var mode, NoOffset), mkString "a+", locUnknown)) in
  let guard = BinOp (Eq, Lval (Var myFile, NoOffset), zero, boolType) in
  let thenBlock = mkBlock [
    mkStmt (Instr 
              [
                Call (Some (Var tempPID, NoOffset), 
                      (Lval (Var getpid, NoOffset)),
                      [], locUnknown);
                Call (None, 
                      (Lval (Var strcpy, NoOffset)),
                      [StartOf (Var tempFName, NoOffset);
                       mkString !log_file_prefix], locUnknown);
                Call (None, 
                      (Lval (Var sprintf, NoOffset)),
                      [StartOf (Var tempITOA, NoOffset);
                       mkString "%d";
                       Lval (Var tempPID, NoOffset)], locUnknown);
                Call (None, 
                      (Lval (Var strcat, NoOffset)),
                      [StartOf (Var tempFName, NoOffset);
                       StartOf (Var tempITOA, NoOffset)], locUnknown);
                Call (Some (Var myFile, NoOffset), 
                      (Lval (Var fopen, NoOffset)),
                      [StartOf (Var tempFName, NoOffset);
                       Lval (Var mode, NoOffset)], locUnknown);
                
              ]
           )
  ]
  in
  let elseBlock = mkBlock [] in
  let ifStmt = mkStmt (If (guard, thenBlock, elseBlock, locUnknown)) in
  let fprintfStmt = mkStmt (Instr
                              [
                                Call (None, 
                                      (Lval (Var fprintf, NoOffset)),
                                      [Lval (Var myFile, NoOffset);
                                       mkString "%s";
                                       Lval (Var strFormal, NoOffset)], 
                                      locUnknown);
                              ]) in
  let body = mkBlock [assignMode; ifStmt; fprintfStmt] in
  fundec.sbody <- body;
  fundec


let addTail listRef ele : unit = 
  listRef := !listRef @ [ele]

(** Add element into a list after all of its dependents *)
let rec addAfterDependents testEq elt deps lis =
  (* Don't add duplicates either *)
  if List.exists (testEq elt) lis then
    lis
  else
    addAfterDependentsHelp testEq elt deps [] lis

and addAfterDependentsHelp testEq elt deps curhd rest =
  if deps = [] then
    addAfterDepsDoAdd elt curhd rest
  else
    (match rest with
       [] -> failwith "not all dependencies in list"
     | h :: t ->
         let newdeps = findRemove testEq h deps in
         addAfterDependentsHelp testEq elt newdeps (h :: curhd) t
    )
      
and addAfterDepsDoAdd elt curhd rest =
  List.rev_append curhd (elt :: rest)

and findRemove test x xs =
  findRemoveHelp test x [] xs

and findRemoveHelp test x curhd rest =
  match rest with
    [] -> (* not there, just return the original *) 
      List.rev curhd
  | h :: t ->
      if test x h then List.rev_append curhd t
      else findRemoveHelp test x (h :: curhd) t


(** true if the two globals are equal modulo location
    (because we put locUnknown) *)
let eqGlobals g1 g2 =
  match g1, g2 with
    GType (ti1, _), GType (ti2, _) -> ti1.tname = ti2.tname
  | GCompTag (ci1, _), GCompTag (ci2, _) 
  | GCompTagDecl (ci1, _), GCompTagDecl (ci2, _) ->  ci1.ckey = ci2.ckey
  | GEnumTag (ei1, _), GEnumTag (ei2, _) 
  | GEnumTagDecl (ei1, _), GEnumTagDecl (ei2, _) -> ei1.ename = ei2.ename
  | GVarDecl (vi1, _), GVarDecl (vi2, _) 
  | GVar (vi1, _, _), GVar (vi2, _, _) -> vi1.vid = vi2.vid
  | GFun (f1, _), GFun (f2, _) -> f1.svar.vid = f2.svar.vid
  | GAsm (x1, _), GAsm (x2, _) -> x1 = x2
  | GPragma (x1, _), GPragma (x2, _) -> x1 = x2
  | _, _ -> g1 = g2


(** Nasty version that injects the logger definition too *)
let addDeclsAndDefs mainInFile (file : file) : unit =
  let globs = file.globals in
  
  (* Fix up prototype for myFileDescriptor, which depends on FILE *, 
     which depends on struct _IO_FILE *)
  let ioFile = getCompinfo file_io_comp in
  let ioFileDecl = GCompTagDecl (ioFile, locUnknown) in
  let globs = addAfterDependents eqGlobals ioFileDecl [] globs in

  let ioFileType = TComp (ioFile, []) in
  let fileTI = getTypeinfo file_io_typedef ioFileType in
  let fileTIDecl = GType (fileTI, locUnknown) in
  let globs = addAfterDependents eqGlobals fileTIDecl [ioFileDecl] globs in 

  let fileType = TNamed (fileTI, []) in
  let filePtrType = TPtr (fileType, []) in
  
  myFileDescriptor.logProto <- filePtrType;
  let myFile = getFVarinfo myFileDescriptor in
  let myFileDecl = GVarDecl (myFile, locUnknown) in
  let globs = addAfterDependents eqGlobals myFileDecl [fileTIDecl] globs in

  (* Fix up prototype for fopen, which also depends on FILE *... *)
  fopenInfo.logProto <- TFun (filePtrType, 
                              Some [("filename", charConstPtrType, []);
                                    ("mode", charConstPtrType, [])],
                              false, []);
  let fopen = getFVarinfo fopenInfo in
  let fopenDecl = GVarDecl (fopen, locUnknown) in
  let globs = addAfterDependents eqGlobals fopenDecl [fileTIDecl] globs in
  
  (* Fix up fprintf, which depends on FILE * also *)
  printfInfo.logProto <- TFun (intType,
                               Some [("stream", filePtrType, []);
                                     ("format", charConstPtrType, [])],
                               true, []);
  let fprintf = getFVarinfo printfInfo in
  let fprintfDecl = GVarDecl (fprintf, locUnknown) in
  let globs = addAfterDependents eqGlobals fprintfDecl [fileTIDecl] globs in


  (* Fix up getpid, which depends on pid, which depends on __pid *)
  let pid2 = getTypeinfo pid2_t_typedef intType in
  let pid2Type = TNamed (pid2, []) in
  let pid2Decl = GType (pid2, locUnknown) in
  let globs = addAfterDependents eqGlobals pid2Decl [] globs in

  let pid = getTypeinfo pid_t_typedef pid2Type in
  let pidType = TNamed (pid, []) in
  let pidDecl = GType (pid, locUnknown) in
  let globs = addAfterDependents eqGlobals pidDecl [pid2Decl] globs in

  pidInfo.logProto <- TFun (pidType, Some [], false, []);
  let getpid = getFVarinfo pidInfo in
  let getpidDecl = GVarDecl (getpid, locUnknown) in
  let globs = addAfterDependents eqGlobals getpidDecl [pidDecl] globs in

  (* Make sure my log declarations exist too *)
  let myprint = getFVarinfo myPrinter in
  let myprintDecl = GVarDecl (myprint, locUnknown) in
  let globs = addAfterDependents eqGlobals myprintDecl [] globs in

  (* If this is the main file, make sure the logger and log file 
     are actually defined / initialized *)
  let globs = if mainInFile then begin
    let strcat = getFVarinfo strcatInfo in
    let strcatDecl = GVarDecl (strcat, locUnknown) in
    let globs = addAfterDependents eqGlobals strcatDecl [] globs in

    let strcpy = getFVarinfo strcpyInfo in
    let strcpyDecl = GVarDecl (strcpy, locUnknown) in
    let globs = addAfterDependents eqGlobals strcpyDecl [] globs in

    let sprintf = getFVarinfo sprintfInfo in
    let sprintfDecl = GVarDecl (sprintf, locUnknown) in
    let globs = addAfterDependents eqGlobals sprintfDecl [] globs in
    
    let myFileDefn = GVar (myFile, 
                           { init = Some (SingleInit (Cil.zero)); },
                           locUnknown) in
    let globs = addAfterDependents eqGlobals myFileDefn [myFileDecl] globs in
    
    let myLogFundec = makeMyLogger myprint myFile
      fopen fprintf getpid strcat strcpy sprintf pidType
    in

    let myLogDefn = GFun (myLogFundec, locUnknown) in
    addAfterDependents eqGlobals myLogDefn 
      [myprintDecl; myFileDecl; fopenDecl; fprintfDecl; getpidDecl; 
       strcatDecl; strcpyDecl; sprintfDecl] globs
  end else globs in
  
  file.globals <- globs


(** Simpler version that expects you to link the logger library later.
    Only adds the declaration of the logger function in each file. *)
let addDeclsAndDefs2 mainInFile (file : file) : unit =
  let globs = file.globals in
  
  (* Make sure my logger declaration exists *)
  let myprint = getFVarinfo myPrinter in
  let myprintDecl = GVarDecl (myprint, locUnknown) in
  let globs = addAfterDependents eqGlobals myprintDecl [] globs in

  file.globals <- globs
    


(************************************************************)

let mkPrint (str: string) : instr = 
  let p: varinfo = getFVarinfo myPrinter in 
  Call(None, Lval(var p), [(mkString str)], !currentLoc)


let d_string (fmt : ('a,unit,doc,string) format4) : 'a = 
  let f (d: doc) : string = 
    Pretty.sprint 200 d
  in
  Pretty.gprintf f fmt 

let extraPthreadFunctions = ["pthread_mutex_init";
                             "pthread_mutex_lock";
                             "pthread_mutex_unlock";
                             "pthread_self";]


(* Avoid recursion into helper functions!!! *)
let doNotLog fname =
  List.exists (fun finfo -> finfo.logVname = fname) funcsNeedingVarinfos ||
    List.exists (fun n -> n = fname) extraPthreadFunctions

(************************************************************)

class gotoReplacer newStmt oldStmt = object (self)
  inherit nopCilVisitor
    
  method vstmt (s : stmt) = begin
    match s.skind with
      Goto (sref, _) ->
        if !sref = oldStmt then
          sref := newStmt
        ;
        DoChildren
    | _ -> DoChildren
  end

end


let currentFunc: string ref = ref ""

class logCallsVisitorClass = object (self)
  inherit nopCilVisitor

  val mutable curFunc = dummyFunDec

  method vfunc fdec =
    curFunc <- fdec;
    DoChildren

  method vinst i = begin
    match i with
    | Call(lo,e,al,l) ->
        if doNotLog !currentFunc then
          DoChildren
        else
          let pre = mkPrint (d_string "call %a\n" d_exp e) in
          let post = mkPrint (d_string "return from %a\n" d_exp e) in
          ChangeTo [ pre; i; post ] 
            
    | _ -> DoChildren
  end

  method vstmt (s : stmt) = begin
    match s.skind with
      Return _ -> 
        if doNotLog !currentFunc then DoChildren
        else begin
          let pre = mkPrint (d_string "EXIT: %s\n" !currentFunc) in 
          let preStmt = mkStmtOneInstr pre in
          let newBlock = (mkStmt (Block (mkBlock [ preStmt; s ]))) in          
          self#shiftLabels newBlock s;
          ChangeTo newBlock
        end
    | _ -> DoChildren
  end

  (** If we insert stmts before others, we may need to shift labels to the
      new "first" stmt in the sequence  *)
  method private shiftLabels pre stmt =
    if stmt.labels <> [] then begin
      if stmt.succs <> [] || stmt.preds <> [] then
        E.error "shiftLabels given stmt w/ succs and preds\n"
      ;
      pre.labels <- stmt.labels;
      stmt.labels <- [];
      (* Also, scan through all stmts in the current function and update
         the references for the "GOTO" stmt *)
      let gotoReplacer = new gotoReplacer pre stmt in
      ignore (visitCilFunction gotoReplacer curFunc)
    end

end

let logCallsVisitor = new logCallsVisitorClass

let logCalls2 (f: file) : unit =

  (** Make one pre-pass to find any interesting function declarations *)
  let mainInFile = searchForDelcs f in

  (** Add prototypes for any missing functions, etc. *)
  addDeclsAndDefs2 mainInFile f;

  (** Now, do the actual rewriting for the calls / returns / enter / exit *)
  let doGlobal = function
      GFun (fdec, loc) ->
        currentFunc := fdec.svar.vname;
        (* do the body *)
        ignore (visitCilFunction logCallsVisitor fdec);
        (* Now add the entry instruction *)
        if doNotLog !currentFunc then ()
        else begin
          let pre = mkPrint (d_string "ENTER: %s\n" !currentFunc) in 
          (* If the first stmt had a label, don't need to shift it to "pre" *)
          fdec.sbody <- 
            mkBlock [ mkStmtOneInstr pre;
                      mkStmt (Block fdec.sbody) ]
        end
    | _ -> ()
  in
  iterGlobals f doGlobal  


let feature : featureDescr = 
  { fd_name = "logcalls2";
    fd_enabled = Cilutil.logCalls2;
    fd_description = "generation of code to log function calls to a file";
    fd_extraopt = [
      ("--logcallpthread", Arg.Set usePthreads, 
       "client uses pthreads, prefix log msgs w/ thread ids");
    ];
    fd_doit = logCalls2;
    fd_post_check = true
  } 
