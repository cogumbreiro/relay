(** Track memcpy functions *)

type memcpy_info = 
    { memcpy_fun : string;
      memcpy_dest : int;
      memcpy_src : int; }

let (memcopiers : (string, memcpy_info) Hashtbl.t) = Hashtbl.create 17

let clearMemcopiers () =
  Hashtbl.clear memcopiers

let addMemcopier funname destind srcind =
  try
    let old = Hashtbl.find memcopiers funname in
    assert (old.memcpy_src = srcind && old.memcpy_dest = destind)
  with Not_found ->
    let toAdd = { memcpy_fun = funname;
                  memcpy_dest = destind;
                  memcpy_src = srcind; } in
    Hashtbl.add memcopiers funname toAdd

let isMemcopier funname = 
  try Some (Hashtbl.find memcopiers funname) with Not_found -> None

(************************************************************)

let ws = "[ \r\n\t]*"

(* Top level split, between the function name, type, lockset, correlations *)
let topSplitter = Str.split_delim (Str.regexp (ws ^ "[$]" ^ ws))

let initSettings settings =
  let memcpySet = Config.getGroup settings "BUILTIN_FUNCS" in
  clearMemcopiers ();
  Config.iter 
    (fun funcName details ->
       let fields = topSplitter details in
       (match fields with
          [typString ; destIndexStr ; srcIndexStr] ->
            if Strutil.strip typString = "MEMCPY" then
              let destInd = int_of_string destIndexStr in
              let srcInd = int_of_string srcIndexStr in
              addMemcopier funcName destInd srcInd
        | _ ->
            (* skip it -- another kind of builtin? *) ()
       )
    ) memcpySet

  
