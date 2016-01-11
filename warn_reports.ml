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

open Stdutil
open Pretty


(** Basic interface for warning reports *)
class type warnReports = object ('a)

  method joinReports : 'a -> unit

  method printWarnings : unit

  method serialize : string -> unit

  method deserialize : string -> unit

  method saveToXML : string -> unit

  method clear : unit

end


(** Expected interface for a single warning *)
module type WarnCluster = sig

  type key (* Cluster by key *)

  type data (* Data associated with key *)

  val equalKey : key -> key -> bool
    
  val hashKey : key -> int

  val hashConsKey : key -> key
    
  val equalData : data -> data -> bool
    
  val hashConsData : data -> data
    
  val pDataXML : data -> Pretty.doc
    
end


(** Generate a warn-report class for the given warning clustering *)
module MakeRep (W:WarnCluster) = struct

  (* TODO: switch to mapset instead? Hash-consing will be 
     less convenient though *)

  (* Hash by key, for clustering *)
  module KeyHash = struct
    type t = W.key
    let equal = W.equalKey
    let hash = W.hashKey
  end

  module KH = Hashtbl.Make(KeyHash)

  type warnCluster = W.data list

  let emptyCluster = []

  (** Categorize a difference between two clusters *)
  type diffKind =
      OnlyFirst of warnCluster
    | OnlySecond of warnCluster
    | BothButDiff of warnCluster * warnCluster

  (** Identify new vs duplicate warnings *)
  type dupeKind = 
      OldCluster | MaxWarnings | DupeWarning

  type warnData = warnCluster KH.t

  class report 
    ?(maxWarningsPerKey = 20) ?(limitWarnings = false)
    ?(initialData = KH.create 17) () = object (self : 'self)

    val mutable data : warnData = initialData
    val mutable limitWarnings = limitWarnings
    val mutable maxWarningsPerKey = maxWarningsPerKey

    (* 
       TODO: count num warnings in case of limiting? 
       Sort elements within clusters to make it easier to DIFF?
       Give each cluster a unique ID and add that to the XML output? *)

    method getData = data

    method setData d =
      data <- d

    method clear () = 
      KH.clear data

    method hashConsCluster cluster : warnCluster =
      List.map (fun d -> W.hashConsData d) cluster
        (* Not keeping one instance of the list, only one instance
           of each member of each list *)


    method joinCluster cluster1 cluster2 : warnCluster =
      (* TODO: Make a more generic "union" function for hashtables
         and a union for the lists that is its data? *)
      let rec from2 l2 left =
        if (left <= 0) then []
        else match l2 with
          [] -> l2
        | newR :: t ->
            if (List.exists (fun oldR -> W.equalData newR oldR) cluster1) then
              from2 t left
            else
              newR :: from2 t (left - 1)
      in
      let limit = if limitWarnings 
      then maxWarningsPerKey - List.length cluster1
      else List.length cluster2 in
      cluster1 @ (from2 cluster2 limit)


    method joinReports (other : 'self) : unit =
      (* a bit restrictive? *)
      let otherData = other#getData in
      KH.iter
        (fun k otherClust ->
           try
             let ownClust = KH.find data k in
             let newClust = self#joinCluster ownClust otherClust in
             KH.replace data k newClust
           with Not_found ->
             let newK = W.hashConsKey k in
             let newCluster = self#hashConsCluster otherClust in
             KH.add data newK newCluster
        ) otherData


    (** Diff the clusters, returning a cluster of the items only in
        [c1] and a cluster of items only in [c2] *)
    method diffCluster c1 c2 : warnCluster * warnCluster =
      (* Assume the clusters are small *)
      let onlyOne, onlyTwo = 
        List.fold_left 
          (fun (one, two) d1 ->
             if not (List.exists (fun d2 -> W.equalData d1 d2) two) then 
               (d1 :: one, two)
             else
               (one, List.filter (fun d2 -> not (W.equalData d1 d2)) two)
          ) ([], c2) c1 in
      (onlyOne, onlyTwo)

    method diffReport (other : 'self) =
      (* a bit restrictive? *)
      let diff = KH.create 7 in
      let otherData = other#getData in
      let onlyMe = KH.copy data in
      (* Find bindings only in other table, or in both but different *)
      KH.iter
        (fun k otherClust ->
           try
             let ownClust = KH.find onlyMe k in
             let oneClust, twoClust = self#diffCluster ownClust otherClust in
             if oneClust <> [] || twoClust <> [] then 
               KH.add diff k (BothButDiff (oneClust, twoClust))
             ;
             KH.remove onlyMe k; 
           with Not_found ->
             KH.add diff k (OnlySecond otherClust)
        ) otherData;
      (* Add bindings that are only in own table *)
      KH.iter 
        (fun k myClust ->
           KH.add diff k (OnlyFirst myClust)
        ) onlyMe;
      diff

        
    (** Add a report to the current set (and cluster "duplicates").
        @return None if a new cluster was started
        or return Some dupeKind if the warning is a duplicate / max warnings
        already accumulated, etc.  *)
    method addWarning k d : dupeKind option =
      try
        let oldCluster = KH.find data k in
        if limitWarnings && List.length oldCluster >= maxWarningsPerKey then 
          Some MaxWarnings
        else if (List.exists (fun oldD -> W.equalData d oldD) oldCluster) then 
          Some DupeWarning
        else
          let newD = W.hashConsData d in
          KH.replace data k (newD :: oldCluster);
          Some OldCluster
      with Not_found ->
        let newK = W.hashConsKey k in
        let newD = W.hashConsData d in
        KH.add data newK [ newD ];
        None
          
    (** Make a pretty print document of the clusters out as XML *)
    method pXML oc =
      fprint oc 80 
        (text "<?xml version=\"1.0\"?>" ++ line ++ text "<run>" ++ line);
      KH.iter
        (fun key cluster ->
           fprint oc 80 
             (text "<cluster>" ++ line ++
                indent 1 (List.fold_left
                            (fun doc info -> 
                               (doc ++ W.pDataXML info ++ line)) nil cluster)
              ++ text "</cluster>" ++ line)
        ) data;
      fprint oc 80 (text "</run>" ++ line ++ text "</xml>")
        
    method private serializeChan oc : unit =
      Marshal.to_channel oc 
        (maxWarningsPerKey, limitWarnings, data) [Marshal.Closures]
      
    method serialize (fname:string) : unit =
      open_out_bin_for fname self#serializeChan

    method private deserializeChan ic =
      let (maxWarns, limitWarns, d) = Marshal.from_channel ic in
      maxWarningsPerKey <- maxWarns;
      limitWarnings <- limitWarns;
      data <- d
      
    method deserialize (fname:string) =
      open_in_bin_for fname self#deserializeChan

    method private writeXMLTo oc =
      self#pXML oc
        
    method saveToXML (fname:string) = 
      open_out_for fname self#writeXMLTo
        
    end
    
  (** Deserializers that return new objects *)
  let deserializeChan ic =
    let (maxWarns, limitWarn, d) = Marshal.from_channel ic in
    (new report 
       ~maxWarningsPerKey:maxWarns ~limitWarnings:limitWarn ~initialData:d ())
      
  let deserialize (fname:string) =
    open_in_bin_for fname deserializeChan
      

end
