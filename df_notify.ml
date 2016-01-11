

open Cil

(** At a RETURN statement or other locations, indicate that DF should
    notify caller of a new summary or any other known location of new data *)
type 'id toNotify = 
    { notifyPP : ('id * prog_point) list;
      notifyCaller : bool; }

(** At a CALL statement, *)
type ('id, 'state) callFlows = 
    { addCallees : ('id * 'state) list;
      removeCallees : 'id list; }


let emptyFlows = { addCallees = []; removeCallees = []; }

let addCalleeFlow (id, st) cur = 
  { cur with addCallees = (id, st) :: cur.addCallees; }

let removeCalleeFlow id cur =
  { cur with removeCallees = id :: cur.removeCallees; }

let combineCalleeFlows c1 c2 =
  if c2 == emptyFlows then c1
  else 
    { addCallees = List.rev_append c1.addCallees c2.addCallees;
      removeCallees = List.rev_append c1.removeCallees c2.removeCallees; }
