(*
  Copyright (c) 2006-2007, Regents of the University of California

  Authors: Jan Voung
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following 
  conditions are met:
  
  1. Redistributions of source code must retain the above copyright 
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above 
  copyright notice, this list of conditions and the following disclaimer 
  in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the University of California, San Diego, nor 
  the names of its contributors may be used to endorse or promote 
  products derived from this software without specific prior 
  written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
*)


(** Utility to ensure uniqueness and consistency of varinfo VIDs
    and compinfo CKeys across different CIL AST object files. Reassigns IDs.

    WARNING: Make a copy of files beforehand. A crash could leave
    IDs in an unstable state  *)

open Cil
open Pretty
open Filetools
open Intrange
open Cildump
open Logging

module CF = Cilfiles


(**************** "Recovery" "log" **************)

let setFile (fname:string) =
  let out_chan = open_out fname in
  output_char out_chan '!';
  close_out out_chan

(** dummy file that is dumped into the directory to indicate that 
    id fixing is in progress. Its existence after completion or a crash
    indicates that the files are now corrupt *)
let inProgressFile = ".doing_id_fixins"

let setInProgress (root:string) =
  setFile (Filename.concat root inProgressFile)

let clearInProgress (root:string) =
  Sys.remove (Filename.concat root inProgressFile)



(******* VID / CKEY range stuff *******)


(** file that is dumped into the directory to indicate that the
    ids have been fixed. Also contains ID/Key range -> file mapping *)
let doneFile = ".vids_ckeys_fixed"

(** make a file to indicate ids are fixed. piggyback id ranges
    in this file as well *)
let setDone (root:string) =
  logStatusF "Saving ranges to %s\n" doneFile;
  CF.saveRanges (Filename.concat root doneFile)

let loadRanges (root:string) = 
  logStatusF "Loading ranges from %s\n" doneFile;
  CF.loadRanges (Filename.concat root doneFile)


(**************** Visitor to do the work **************)
  
let isInlineAttr att =
  match att with
    Attr (name, _) -> name = "inline" || name = "always_inline"
  
let isInline vi =
  vi.vinline || List.exists isInlineAttr vi.vattr

let declHeader vi =
  let result = Strutil.containsSub vi.vdecl.file "\\.h" in
  result

(* 
   What's going to happen:
   
   All vinfos and compinfos that are supposed to be equal across AST files
   will be assigned the same key. Sometimes the vinfo/compinfo will
   need to be merged (e.g., if only one of them has all the cfields).

   Option (1) Update the vinfo/compinfo in all ASTs that reference them.
   Since within a file, all references to the compinfos share that
   compinfo, we only need to update the compinfo from the declaration.

   Option (2) Keep one index from the key to the extra vinfo/compinfo 
   data stored externally, and only ensure that those vinfo/compinfos
   are complete (in terms of fields) but ensure that all vinfo/compinfo
   have the right key so that they can reference the complete version.
   
   Problem with option (1) is we might need to do Option (2) anyway
   to get the "best" info, before making another pass to update all
   references to use that "best" info. Problems w/ circular references
   going across files?

   Problem with option (2) is there's lots of places that use cfields
   and crap in Cil itself that we would need to change to use indirection

   Option 1 is probably best because the effect is local, but uh,
   it uses more memory!

*)


(** Recursively go through type finding all compinfos that match ckey 
    of parentCi and changing them to use the parentCi itself *)
let redirectType t parentCi = 
  let visitedCi = Inthash.create 17 in
  let rec helpRedirectType curType =
    match curType with
      TVoid _ | TInt _ | TFloat _ | TBuiltin_va_list _ -> curType

    | TNamed (ti, a) ->
        ti.ttype <- helpRedirectType ti.ttype;
        curType
          
    | TPtr (t, a) ->
        let newT = helpRedirectType t in
        if newT == t then curType
        else TPtr (newT, a)
          
    | TArray (bt, sizeExpOpt, a) ->
        let bt2 = helpRedirectType bt in
        if bt2 == bt then curType
        else TArray (bt2, sizeExpOpt, a)
          (* Not redirecting the size expression for now --
             should be a constant anyway *)

    | TFun (retT, argsOpt, vaP, a) ->
        let retT2 = helpRedirectType retT in
        (match argsOpt with
           None -> 
             if retT2 == retT then curType
             else TFun (retT2, argsOpt, vaP, a)
         | Some args ->
             let args2 = List.map 
               (fun (s, t, a) -> (s, helpRedirectType t, a)) args in
             TFun (retT2, Some (args2), vaP, a) )
          
    | TEnum (ei, a) -> 
        (* Not redirecting the enum expressions -- should be const anyway? *)
        curType

    | TComp (ci, a) ->
        if ci.ckey == parentCi.ckey then
          if ci == parentCi then curType
          else TComp (parentCi, a)
        else begin
          if Inthash.mem visitedCi ci.ckey then curType
          else begin
            Inthash.add visitedCi ci.ckey ();
            List.iter 
              (fun fi -> fi.ftype <- helpRedirectType fi.ftype) ci.cfields;
            curType
          end
        end
  in
  helpRedirectType t

(** Create a new version of the field where it's fcomp is redirected
    to the given parent compinfo *)
let redirectFieldInfo parentCi fi =
  if fi.fcomp == parentCi then fi
  else 
    { fi with
        fcomp = parentCi;
        ftype = redirectType fi.ftype parentCi; }
      

(** Merge basic info from fi2 into fi1 *)
let mergeFieldinfo fi1 fi2 =
  if compareLoc locUnknown fi1.floc == 0 
  then fi1.floc <- fi2.floc
  else if compareLoc fi1.floc fi2.floc <> 0 then begin
(*    if fi1.floc.line = fi2.floc.line && fi1.floc.file = fi2.floc.file then
      logStatusF "Field %s vs %s different floc bytes %d vs %d\n"
        fi1.fname fi2.fname fi1.floc.byte fi2.floc.byte
    else
      logStatusF "Field %s vs %s different floc? (%s <> %s)\n"
        fi1.fname fi2.fname (string_of_loc fi1.floc) (string_of_loc fi2.floc)
*)
    (* whatever.. it's just the declaration location *)
  end

let d_fields fields =
  (seq_to_doc (text ", ") List.iter 
     (fun f -> text 
        (f.fname ^ "(" ^ string_of_type (unrollType f.ftype) ^ ")"))
     fields nil)
    
(** Merge info from ci2 into ci1 *)
let mergeCompinfo ci1 ci2 = begin
  (* Merge some parts of the fields *)
  let newFields = 
    List.fold_left 
      (fun cur fi2 ->
         try 
           let fi1 = List.find (fun fi -> fi.fname = fi2.fname) cur in
           mergeFieldinfo fi1 fi2;
           cur
         with Not_found -> fi2 :: cur
      ) ci1.cfields ci2.cfields in
  (* Finally redirect the "fcomp" to the same compinfo *)
  if List.length newFields == List.length ci1.cfields then ()
  else begin
    let newFields = List.map (redirectFieldInfo ci1) newFields in
    ci1.cfields <- newFields;
    logStatusF "Got new fields for %s(%d)\n" ci1.cname ci1.ckey;
    logStatusD (d_fields ci1.cfields ++ line);
  end;
  ci1.cdefined <- ci1.cdefined || ci2.cdefined;
  ci1.creferenced <- ci1.creferenced || ci2.creferenced;
end

    
(************************************************************)

let nextGlobVID = ref 1
let getNextVID () =
  let result = !nextGlobVID in
  incr nextGlobVID;
  result

let nextCompKey = ref 1
let getNextCKey () =
  let result = !nextCompKey in
  incr nextCompKey;
  result

type varMapping = ((string * string), int) Hashtbl.t 

(* Consistently reassign vids to each global var. 
   For local var, just make sure it doesn't clash *)
let (seenGlobalVars : varMapping)  = Hashtbl.create 173

(* Also consistently reassign locals (based on what function it belongs to) 
   funID -> varMapping *)
let (seenLocalVars : (int, varMapping) Hashtbl.t) = Hashtbl.create 173

(* Consistently reassign struct indentifiers to each struct *)
let (compKeys : (string * bool * string, int) Hashtbl.t) = Hashtbl.create 173

(* Most complete version of each struct *)
let goldenCompinfos = Hashtbl.create 173

let initState () =
  CF.initRanges ();
  Hashtbl.clear seenGlobalVars;
  Hashtbl.clear seenLocalVars;
  Hashtbl.clear compKeys;
  Hashtbl.clear goldenCompinfos


(** A visitor that searches for varinfo IDs and compinfo IDs to make 
    sure that if we make another info, it will not clash w/ any of 
    the IDs in the visited file (or previously loaded files) *)
class canonicizeIDs curFile = object (self)
  inherit nopCilVisitor 
    
  val mutable curFunc = dummyFunDec
  val mutable currentLocalMap : varMapping = Hashtbl.create 7
  val mutable staticsMap : varMapping = Hashtbl.create 7
    (* New staticsMap per file *)


  method vfunc fdec =
    curFunc <- fdec;
    self#handleVI fdec.svar;
    let localMap = 
      try Hashtbl.find seenLocalVars fdec.svar.vid
      with Not_found -> 
        let map = Hashtbl.create 7 in
        Hashtbl.add seenLocalVars fdec.svar.vid map;
        map
    in
    currentLocalMap <- localMap;
    DoChildren

  method private varinfoToKey vi =
    let name = vi.vname in
(*    let typs = string_of_ftype vi.vtype in
    (name, typs)
*)
(*    let loc = string_of_loc vi.vdecl in *)
    (name, "")

  method private changeVID vi newID =
    if vi.vid <> newID then begin
(*      logStatusF "Changing vid: %s from %d to %d\n" vi.vname vi.vid newID; *)
      vi.vid <- newID
    end

  method private findInMap vi map =
    let name, typs = self#varinfoToKey vi in
    try
      let oldID = Hashtbl.find map (name, typs) in
      self#changeVID vi oldID
    with Not_found ->
      let newID = getNextVID () in
      self#changeVID vi newID;
      Hashtbl.add map (name, typs) newID
        
  (** Check if the vi found in this file should be linked w/ old ones *)
  method private handleVI (vi:varinfo) =
    if vi.vglob then begin
      (* Okay, so linux uses "static inline" for kmalloc even though kmalloc
       * is imported through the .h file... treat "static inline" as global
       * symbol if it is defined in a .h file... 
       * (and treat as simply static otherwise) *)
      if ((vi.vstorage <> Static && vi.vname <> "main") ||
            (vi.vstorage = Static && isInline vi && declHeader vi )) then begin
        self#findInMap vi seenGlobalVars;
      end else begin
        self#findInMap vi staticsMap;
      end
    end else begin
      self#findInMap vi currentLocalMap;
    end

  method vvdec (vi:varinfo) =
    self#handleVI vi;
    DoChildren

  method vvrbl (vi:varinfo) =
(*    self#handleVI vi; *)
    DoChildren

      
  (** nameSpaces:

      - A global one for normal structs and unions
      
      - A "local" one for __anonstruct__XYZ and __anonunion__XYZ. 

      OK: made Cil base the __anon name on the parent structure so
      the name is the smae when it is the same / not when it is different ?
  *)
  method private getNamespace ci =
(*
    if Strutil.prefix "__anonstruct" ci.cname ||
      Strutil.prefix "__anonunion" ci.cname then curFile
    else 
*)
    ""

  method private changeCKey ci ckey =
(*    logStatusF "Changing ckey: %s from %d to %d\n" ci.cname ci.ckey ckey; *)
    ci.ckey <- ckey
      
  (** See if the compinfo found in this file should be linked w/ old ones *)
  method private handleCompinfo ci =
(*
    logStatusF "handleCi %s [%s]\n" ci.cname 
      (sprint 80 (d_fields ci.cfields));
*)
    let nameSpace = self#getNamespace ci in
    let finalCKey = 
      try
        (* Some structures don't have globally unique names.
           Example: "net_local" is found in over 10 files in Linux,
           each instance having a different definition. 
           With this scheme we will end up making one uber net_local
           structure with all of the fields. 
           Hopefully there is no clash in field types...
           How does the CIL merger handle this, especially when
           some files don't have fields for a struct -- how do they
           match such empty ones up with the existing structs ?
           I guess they have some discipline where they don't have
           empty versions of the struct when it is redefined ?
        *)
        let oldCKey = Hashtbl.find compKeys (nameSpace, ci.cstruct, ci.cname) in
        self#changeCKey ci oldCKey;
        oldCKey
      with Not_found ->
        let newCKey = getNextCKey () in
        self#changeCKey ci newCKey;
        Hashtbl.add compKeys (nameSpace, ci.cstruct, ci.cname) newCKey;
        newCKey
    in
    try
      let goldCi = Hashtbl.find goldenCompinfos finalCKey in
      mergeCompinfo goldCi ci
    with Not_found ->
      Hashtbl.add goldenCompinfos finalCKey ci


  method vglob = function
      GCompTag (ci, _)
    | GCompTagDecl (ci, _) ->
        self#handleCompinfo ci;
        DoChildren
    | _ ->
        DoChildren

end

(************************************************************)


(** Take the info golden compinfos, etc. and push it back into the 
    individual files (if it is the file that is responsible for the gold info)
*)
class compinfoMerger curFile = object (self)
  inherit nopCilVisitor

  method private handleCompinfo ci =
    if CF.ownsCKey ci.ckey curFile then begin
      let goldCi = Hashtbl.find goldenCompinfos ci.ckey in
      assert (ci.cname = goldCi.cname);
      ci.cdefined <- goldCi.cdefined;
      ci.creferenced <- goldCi.creferenced;
      let ciLen = List.length ci.cfields in
      let goldLen = List.length goldCi.cfields in
      if ciLen == goldLen then ()
      else begin
        assert (ciLen < goldLen);
        logStatusF "Merging cfield info for %s\n" ci.cname;
        ci.cfields <- List.map (redirectFieldInfo ci) goldCi.cfields 
          (*      ci.cfields <- goldCi.cfields *)
      end 
    end else begin
      (* Okay... for now do something more like Option 2 above,
         and only get the golden fields for structs in the file
         that supposedly holds the golden struct.
         I have changed all direct references to cfields to go
         through the golden copy the struct for the cfields *)

      (* Okay, changed my mind... back to option 1 because stuff in
         Cil really does depend on the cfields (e.g., bitsOffset) *)

(*      logStatusF "Not merging cfields for %s(%d) in %s -- nulling\n" 
        ci.cname ci.ckey curFile; *)
(*      ci.cfields <- []; *)
      let goldCi = Hashtbl.find goldenCompinfos ci.ckey in
      assert (ci.cname = goldCi.cname);
      ci.cdefined <- goldCi.cdefined;
      let ciLen = List.length ci.cfields in
      let goldLen = List.length goldCi.cfields in
      if ciLen == goldLen then ()
      else begin
        assert (ciLen < goldLen);
        logStatusF "cfields left alone for %s in %s (%d vs %d)\n" 
          ci.cname curFile ciLen goldLen;
(*        ci.cfields <- List.map (redirectFieldInfo ci) goldCi.cfields  *)
      end
    end
      
  method vglob = function
      GCompTag (ci, _)
    | GCompTagDecl (ci, _) ->
        self#handleCompinfo ci;
        DoChildren
    | _ -> DoChildren
        
end

(******* ID -> info index stuff *******)

let addIndex map id info =
  Inthash.replace map id info


class indexVarCompInfos curFile = object (self)
  inherit nopCilVisitor 
    
  (* map from id -> varinfo / compinfo (valid for current file) *)
  val seenVinfos = Inthash.create 173
  val seenCinfos = Inthash.create 173
  
  method private addVarIndex id vi =
    addIndex seenVinfos id vi

  method private addCompIndex id ci =
    addIndex seenCinfos id ci

  (** Check if the vi found in this file should be linked w/ old ones *)
  method private handleVI (vi:varinfo) =
    self#addVarIndex vi.vid vi

  method vvdec (vi:varinfo) =
    self#handleVI vi;
    DoChildren

  method vvrbl (vi:varinfo) =
    self#handleVI vi;
    DoChildren

      
  (** See if the compinfo found in this file should be linked w/ old ones *)
  method private handleCompinfo ci =
(*    logStatusF "Indexing ckey %s %d\n" ci.cname ci.ckey; *)
    self#addCompIndex ci.ckey ci

  method vglob = function
      GCompTag (ci, _)
    | GCompTagDecl (ci, _) ->
        self#handleCompinfo ci;
        DoChildren
    | _ ->
        DoChildren

  method flushIndexes () =
    (* Write just the varinfos/cinfos to a separate file *)
    CF.writeIndexes seenVinfos seenCinfos curFile

end


(**************** Interface **************)

let debugCompinfos () =
  logStatusF "\n\nGolden compinfos\n====================\n";
  Hashtbl.iter 
    (fun ckey ci -> 
       assert (ci.ckey = ckey);
       logStatusF "%s(%d): [%s]\n" ci.cname ci.ckey
         (sprint 80 (d_fields ci.cfields))
    ) goldenCompinfos;
  logStatus "========================\n\n"
            
    


let doEnsureUniqueIDs (root : string) = begin
  logStatus "Fixing IDs";
  flushStatus ();

  (* First, choose a canonical ID and start grabbing all the info *)
  Filetools.walkDir 
    (fun ast file ->
       logStatusF "Fixing file %s\n" file;
       flushStatus ();
       let startVid = !nextGlobVID in
       let startCkey = !nextCompKey in

       let vis = new canonicizeIDs file in (* must be fresh for every file *)
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       (* Write back the result! *)
       Cil.saveBinaryFile ast file;

       (* Record the vid / ckey range *)
       let endVid = !nextGlobVID - 1 in
       let endCkey = !nextCompKey - 1 in

       logStatusF "File %s has VID: [%d, %d] and CKEY: [%d, %d]\n"
         ast.fileName startVid endVid startCkey endCkey;

       CF.addRanges startVid endVid startCkey endCkey ast.fileName;
    ) root;

  logStatus "Write out fixed infos";
  flushStatus ();

  (* Now use the new IDs and merge compinfos in the ASTs *)
  Filetools.walkDir 
    (fun ast file ->
       logStatusF "Merging compinfos for %s\n" file;
       flushStatus ();
       let vis = new compinfoMerger ast.fileName in 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       (* Write back the result! *)
       Cil.saveBinaryFile ast file;
    ) root;
  
  logStatus "Indexing var/comp infos";
  flushStatus ();

  Filetools.walkDir 
    (fun ast file ->
       logStatusF "Indexing file: %s\n" file;
       flushStatus ();
       let vis = new indexVarCompInfos file in 
       visitCilFileSameGlobals (vis :> cilVisitor) ast;
       vis#flushIndexes ();
    ) root;
  
  logStatus "IDs are now fixed / compinfos merged";
  flushStatus ();

end

  

(** Apply ID fixer to each file rooted at the given path. *)
let ensureUniqueIDs (root : string) : unit =
  (* Maybe it's already done *)
  if (Sys.file_exists (Filename.concat root doneFile)) then
    logStatus "IDs are already fixed"
  else begin
    setInProgress root;
    initState ();
    doEnsureUniqueIDs root;
    setDone root;
    clearInProgress root;

    (* Don't need state anymore, so clear *)
    initState ();
  end
    
