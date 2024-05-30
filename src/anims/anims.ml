module Easing = Easing

type anim = {
  easing   : float -> float;
  pt_start : int;
  pt_end   : int;
  vector   : float;
  mutable ticks_start : float;
  ticks_span  : float;
  at_update   : int -> unit;
  at_end      : unit -> unit;
}

let anim_empty : anim = {
  easing = (fun _ -> 0.);
  pt_start = 0;
  pt_end = 0;
  vector = 0.;
  ticks_start = 0.;
  ticks_span = 0.;
  at_update = (fun _ -> ());
  at_end    = (fun () -> ());
}

let queue : anim list ref = ref []

let length () = List.length !queue

let create
  ~pt_start
  ~pt_end
  ~span
  ?(at_update = (fun _ -> ()))
  ?(at_end    = (fun () -> ())) curve =
  {
    easing = Easing.get_anim curve;
    pt_start = pt_start;
    pt_end = pt_end;
    vector = Float.of_int (pt_end - pt_start);
    ticks_start = 0.;
    ticks_span = (Float.of_int span);
    at_update = at_update;
    at_end = at_end;
  }

let start anim ticks =
  anim.ticks_start <- Float.of_int ticks;
  anim.at_update anim.pt_start;
  queue := anim :: !queue

(*let animate anim int_ticks =*)
let rec process ticks = function
  | [] -> ()
  | a :: tail ->
    let prog = (ticks -. a.ticks_start) /. a.ticks_span in
    if prog > 1.0 then (
      queue := List.filter (fun a -> a != a) !queue;
      a.at_update a.pt_end;
      a.at_end ()
    ) else (
      let dist = a.easing prog in
      let curr = a.pt_start + (Float.to_int (dist *. a.vector)) in
      a.at_update curr
    );
    process ticks tail

let update int_ticks =
    process (Float.of_int int_ticks) !queue

