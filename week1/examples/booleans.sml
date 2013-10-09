val z = 3;
val x = if (z = 2) andalso (4 + 5 < 10)
  then 5
  else 0

(* style with booleans: *)

(* e1 andalso e2 *)
val a1 = if e1 then e2 else false;

(* e1 orelse e2 *)
val a2 = if e1 then true else e2;

(* not e1 *)
val a3 = if e1 then false else true;

(* just say e (!!!) *)
val a4 = if e then true else false;

(* can't compare int with reals and cannot use = and <> with reals. *)
