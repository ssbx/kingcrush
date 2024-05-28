module Easing = Easing

type anim = {
  anim_fun : float -> float;
  mutable pt_curr  : int;
  pt_start : int;
  pt_end   : int;
  vector   : float;
  mutable ticks_start : float;
  ticks_span  : float;
  at_update   : int -> unit;
  at_end      : unit -> unit;
}

let anim_empty : anim = {
  anim_fun = (fun _ -> 0.);
  pt_curr = 0;
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
    anim_fun = Easing.get_anim curve;
    pt_curr = pt_start;
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
let rec update_all ticks = function
  | [] -> ()
  | anim :: tail ->
    let prog = (ticks -. anim.ticks_start) /. anim.ticks_span in
    if prog > 1.0 then (
      queue := List.filter (fun a -> a != anim) !queue;
      anim.pt_curr <- anim.pt_end;
      anim.at_update anim.pt_end;
      anim.at_end ()
    ) else (
      let dist = anim.anim_fun prog in
      let pt_curr = anim.pt_start + (Float.to_int (dist *. anim.vector)) in
      anim.pt_curr <- pt_curr;
      anim.at_update pt_curr
    );
    update_all ticks tail

let update int_ticks =
    update_all (Float.of_int int_ticks) !queue
