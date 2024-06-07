open Tsdl
open Tsdl_mixer

let sdl_try = function | Ok _ -> () | Error (`Msg e) -> failwith e
let sdl_get_ok = function | Ok v -> v | Error (`Msg e) -> failwith e
let sdl_ignore _ = ()
let log msg = Printf.printf msg

let sdl_get_ticks () = Int32.to_int (Tsdl.Sdl.get_ticks ())
let sdl_get_evt_typ e = Sdl.Event.enum Sdl.Event.(get e typ)
let sdl_get_evt_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode)
let some_or_fail = function | Some v -> v | None -> failwith "error some or fail"

let ms_wait_60fps = Int32.div 1000l 60l
let ticks : int ref = ref 0
let delta : int ref = ref 0

module Easing = struct

  type easing_func_t =
    | Linear
    | Quadratic_in
    | Quadratic_out
    | Quadratic_inout
    | Cubic_in
    | Cubic_out
    | Cubic_inout
    | Quartic_in
    | Quartic_out
    | Quartic_inout
    | Quintic_in
    | Quintic_out
    | Quintic_inout
    | Sin_in
    | Sin_out
    | Sin_inout
    | Circular_in
    | Circular_out
    | Circular_inout
    | Exponential_in
    | Exponential_out
    | Exponential_inout
    | Elastic_in
    | Elastic_out
    | Elastic_inout
    | Back_in
    | Back_out
    | Back_inout
    | Bounce_out
    | Bounce_in
    | Bounce_inout

  type point_t = { x : int; y : int }

  type anim_t = {
    func : float -> float;
    start_point : point_t;
    vector : point_t;
    start_tick : int;
    duration_ticks : int;
    mutable ended : bool;
    start_fun : int -> int -> unit;
    update_fun : int -> int -> unit;
    ended_fun : unit -> unit;
  }

  type anim_return_t = AnimActive of (int * int) | AnimEnded of (int * int)

  let pi = Float.pi
  let half_pi = pi /. 2.
  let linear f = f
  let quadratic_in f = f *. f
  let quadratic_out f = -.(f *. (f -. 2.))

  let quadratic_inout f =
    if f < 0.5 then (
      2. *. f *. f
    ) else (
      (-2. *. f *. f) +. (4. *. f) -. 1.
    )

  let cubic_in f = f *. f *. f

  let cubic_out f =
    let t = f -. 1. in
    (t *. t *. t) +. 1.

  let cubic_inout f =
    if f < 0.5 then (
      4. *. f *. f *. f
    ) else (
      let t = (2. *. f) -. 2. in
      (0.5 *. t *. t *. t) +. 1.
    )

  let quartic_in f = f *. f *. f *. f

  let quartic_out f =
    let t = f -. 1. in
    (t *. t *. t *. (1. -. f)) +. 1.

  let quartic_inout f =
    if f < 0.5 then (
      8. *. f *. f *. f *. f
    ) else (
      let t = f -. 1. in
      (-8. *. t *. t *. t *. t) +. 1.
    )

  let quintic_in f = f *. f *. f *. f *. f

  let quintic_out f =
    let t = f -. 1. in
    (t *. t *. t *. t *. t) +. 1.

  let quintic_inout f =
    if f < 0.5 then (
      16. *. f *. f *. f *. f *. f
    ) else (
      let t = (2. *. f) -. 2. in
      (0.5 *. t *. t *. t *. t *. t) +. 1.
    )

  let sin_in f = sin ((f -. 1.) *. half_pi) +. 1.
  let sin_out f = sin (f *. half_pi)
  let sin_inout f = 0.5 *. (1. -. cos (f *. pi))
  let circular_in f = 1. -. sqrt (1. -. (f *. f))
  let circular_out f = sqrt ((2. -. f) *. f)

  let circular_inout f =
    if f < 0.5 then (
      0.5 *. (1. -. sqrt (1. -. (4. *. (f *. f))))
    ) else (
      0.5 *. (sqrt (-.((2. *. f) -. 3.) *. ((2. *. f) -. 1.)) +. 1.)
    )

  let exponential_in f =
    if f = 0.0 then (
      f
    ) else (
      Float.pow 2. (10. *. (f -. 1.))
    )

  let exponential_out f =
    if f = 1.0 then (
      f
    ) else (
      1. -. Float.pow 2. (-10. *. f)
    )

  let exponential_inout f =
    if f = 0.0 || f = 1.0 then (
      f
    ) else (
      if f < 0.5 then (
        0.5 *. Float.pow 2. ((20. *. f) -. 10.)
      ) else (
        (-0.5 *. Float.pow 2. ((-20. *. f) +. 10.)) +. 1.
      )
    )

  let elastic_in f = sin (13. *. half_pi *. f) *. Float.pow 2. (10. *. (f -. 1.))

  let elastic_out f =
    (sin (-13. *. half_pi *. (f +. 1.)) *. Float.pow 2. (-10. *. f)) +. 1.

  let elastic_inout f =
    if f < 0.5 then (
        0.5
        *. sin (13. *. half_pi *. (2. *. f))
        *. Float.pow 2. (10. *. ((2. *. f) -. 1.))
    ) else (
        0.5
        *. (sin (-13. *. half_pi *. ((2. *. f) -. 1. +. 1.))
            *. Float.pow 2. (-10. *. ((2. *. f) -. 1.))
           +. 2.)
    )

  let back_in f = (f *. f *. f) -. (f *. sin (f *. pi))

  let back_out f =
    let t = 1. -. f in
    1. -. ((t *. t *. t) -. (t *. sin (t *. pi)))

  let back_inout f =
    if f < 0.5 then (
      let t = 2. *. f in
      0.5 *. ((t *. t *. t) -. (t *. sin (t *. pi)))
    ) else (
      let t = 1. -. ((2. *. f) -. 1.) in
      (0.5 *. (1. -. ((t *. t *. t) -. (t *. sin (t *. pi))))) +. 0.5
    )

  let bounce_out f =
    if f < 4. /. 11. then (
      121. *. f *. f /. 16.
    ) else if f < 8. /. 11. then (
      (363. /. 40. *. f *. f) -. (99. /. 10. *. f) +. (17. /. 5.)
    ) else if f < 9. /. 10. then (
      (4356. /. 361. *. f *. f) -. (35442. /. 1805. *. f) +. (16061. /. 1805.)
    ) else (
      (54. /. 5. *. f *. f) -. (513. /. 25. *. f) +. (268. /. 25.)
    )

  let bounce_in f = 1. -. bounce_out (1. -. f)

  let bounce_inout f =
    if f < 0.5 then (
      0.5 *. bounce_in (f *. 2.)
    ) else (
      (0.5 *. bounce_out ((f *. 2.) -. 1.)) +. 0.5
    )

  let rec easing_test func step f =
    match f with
    | x when x < 0. || x > 1. -> ()
    | x ->
        let v = func x in
        let nindent = Float.to_int (v *. 58.) + 22 in
        let indent = String.make nindent ' ' in
        Printf.printf "%s*\n" indent;
        easing_test func step (f +. step)


  let get_anim = function
    | Linear -> linear
    | Quadratic_in -> quadratic_in
    | Quadratic_out -> quadratic_out
    | Quadratic_inout -> quadratic_inout
    | Cubic_in -> cubic_in
    | Cubic_out -> cubic_out
    | Cubic_inout -> cubic_inout
    | Quartic_in -> quartic_in
    | Quartic_out -> quartic_out
    | Quartic_inout -> quartic_inout
    | Quintic_in -> quintic_in
    | Quintic_out -> quintic_out
    | Quintic_inout -> quintic_inout
    | Sin_in -> sin_in
    | Sin_out -> sin_out
    | Sin_inout -> sin_inout
    | Circular_in -> circular_in
    | Circular_out -> circular_out
    | Circular_inout -> circular_inout
    | Exponential_in -> exponential_in
    | Exponential_out -> exponential_out
    | Exponential_inout -> exponential_inout
    | Elastic_in -> elastic_in
    | Elastic_out -> elastic_out
    | Elastic_inout -> elastic_inout
    | Back_in -> back_in
    | Back_out -> back_out
    | Back_inout -> back_inout
    | Bounce_out -> bounce_out
    | Bounce_in -> bounce_in
    | Bounce_inout -> bounce_inout


  let demo () =
    let step = 1. /. 30. in
    print_endline "linear";
    easing_test linear step 0.;
    print_endline "quadratic_in";
    easing_test quadratic_in step 0.;
    print_endline "quadratic_out";
    easing_test quadratic_out step 0.;
    print_endline "quadratic_inout";
    easing_test quadratic_inout step 0.;
    print_endline "cubic_in";
    easing_test cubic_in step 0.;
    print_endline "cubic_out";
    easing_test cubic_out step 0.;
    print_endline "cubic_inout";
    easing_test cubic_inout step 0.;
    print_endline "quartic_in";
    easing_test quartic_in step 0.;
    print_endline "quartic_out";
    easing_test quartic_out step 0.;
    print_endline "quartic_inout";
    easing_test quartic_inout step 0.;
    print_endline "quintic_in";
    easing_test quintic_in step 0.;
    print_endline "quintic_out";
    easing_test quintic_out step 0.;
    print_endline "quintic_inout";
    easing_test quintic_inout step 0.;
    print_endline "sin_in";
    easing_test sin_in step 0.;
    print_endline "sin_out";
    easing_test sin_out step 0.;
    print_endline "sin_inout";
    easing_test sin_inout step 0.;
    print_endline "circular_in";
    easing_test circular_in step 0.;
    print_endline "circular_out";
    easing_test circular_out step 0.;
    print_endline "circular_inout";
    easing_test circular_inout step 0.;
    print_endline "exponential_in";
    easing_test exponential_in step 0.;
    print_endline "exponential_out";
    easing_test exponential_out step 0.;
    print_endline "exponential_inout";
    easing_test exponential_inout step 0.;
    print_endline "elastic_in";
    easing_test elastic_in step 0.;
    print_endline "elastic_out";
    easing_test elastic_out step 0.;
    print_endline "elastic_inout";
    easing_test elastic_inout step 0.;
    print_endline "back_in";
    easing_test back_in step 0.;
    print_endline "back_out";
    easing_test back_out step 0.;
    print_endline "back_inout";
    easing_test back_inout step 0.;
    print_endline "bounce_out";
    easing_test bounce_out step 0.;
    print_endline "bounce_in";
    easing_test bounce_in step 0.;
    print_endline "bounce_inout";
    easing_test bounce_inout step 0.;
    print_endline ""


  (* ========================================================================= *)
  (* high level API                                                            *)
  (* ========================================================================= *)

  let anims : anim_t list ref = ref []
  let create ~anim_type ~start_point ~end_point ~duration_ms ~fun_start
        ~fun_update =
    {
      func = get_anim anim_type;
      start_point;
      vector =
        { x = end_point.x - start_point.x; y = end_point.y - start_point.y };
      start_tick = 0;
      duration_ticks = duration_ms;
      ended = false;
      start_fun = fun_start;
      update_fun = fun_update;
      ended_fun = (fun () -> ());
    }

  let start anim =
    let a = { anim with start_tick = Int32.to_int (Tsdl.Sdl.get_ticks ()) } in
    a.start_fun a.start_point.x a.start_point.y;
    anims := a :: !anims;
    a


  let animate anim now =
    let elapsed = now - anim.start_tick in
    let elapsed_f = Float.of_int elapsed
    and duration_f = Float.of_int anim.duration_ticks in
    let progress_f = elapsed_f /. duration_f in
    match progress_f > 1.0 with
    | true ->
        let nx = anim.start_point.x + anim.vector.x
        and ny = anim.start_point.y + anim.vector.y in
        AnimEnded (nx, ny)
    | false ->
        let pos_anim = anim.func progress_f in
        let prog_x_f = Float.of_int anim.vector.x *. pos_anim
        and prog_y_f = Float.of_int anim.vector.y *. pos_anim in
        let nx = anim.start_point.x + Float.to_int prog_x_f
        and ny = anim.start_point.y + Float.to_int prog_y_f in
        AnimActive (nx, ny)

  let print_anim anim =
    Printf.printf "\nanim startp x:%i y:%i\n" anim.start_point.x
      anim.start_point.y;
    Printf.printf "anim vect x:%i y:%i\n" anim.vector.x anim.vector.y;
    Printf.printf "anim duration %i ms\n" anim.duration_ticks;
    print_endline ""

end

(* ========================================================================= *)
(* ========================================================================= *)
(* ========================================================================= *)
(* ANIMS MODULE                                                              *)
(* ========================================================================= *)
(* ========================================================================= *)
(* ========================================================================= *)
module Anims = struct
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

  let anims_queue : anim list ref = ref []
  let anims_wait_queue : anim list ref = ref []
  let anims_waiting : bool ref = ref false

  let length () = (List.length !anims_queue) + (List.length !anims_wait_queue)

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

  let start anim =
    anim.ticks_start <- Float.of_int (sdl_get_ticks ());
    anim.at_update anim.pt_start;
    anims_wait_queue := anim :: !anims_wait_queue;
    anims_waiting := true

  (*let animate anim int_ticks =*)
  let rec update_all ticks del = function
    | [] -> del
    | a :: tail ->
      let prog = (ticks -. a.ticks_start) /. a.ticks_span in
      if prog > 1.0 then (
        a.at_update a.pt_end;
        a.at_end ();
        update_all ticks (a :: del) tail
      ) else (
        let dist = a.easing prog in
        let curr = a.pt_start + (Float.to_int (dist *. a.vector)) in
        a.at_update curr;
        update_all ticks del tail
      )

  let update int_ticks =
    if !anims_waiting then (
      anims_queue := !anims_queue @ !anims_wait_queue;
      anims_wait_queue := [];
      anims_waiting := false
    );
    if (List.length !anims_queue) > 0 then (
      let del = update_all (Float.of_int int_ticks) [] !anims_queue in
      anims_queue := List.filter (fun a ->
        (List.exists (fun d -> a == d) del) = false
      ) !anims_queue
    )

end

(* ========================================================================= *)
(* ========================================================================= *)
(* TIMER MODULE                                                              *)
(* ========================================================================= *)
(* ========================================================================= *)

module Timer = struct

  type job_t = {
    at : int;
    fn  : unit -> unit;
  }

  let get_ticks () = Int32.to_int (Sdl.get_ticks())

  let jobs_queue : job_t list ref = ref []
  let jobs_wait_queue : job_t list ref = ref []
  let jobs_waiting : bool ref = ref false

  let fire_at ms f =
    jobs_wait_queue := {at = ms; fn = f} :: !jobs_wait_queue;
    jobs_waiting := true

  let fire_in ms f =
    fire_at ((get_ticks ()) + ms) f

  let rec update_all ticks del = function
    | [] -> del
    | j :: tail ->
        if ticks > j.at then (
          j.fn ();
          update_all ticks (j :: del) tail
        ) else (
          update_all ticks del tail
        )

  let length () = (List.length !jobs_queue) + (List.length !jobs_wait_queue)

  let update ticks =
    if !jobs_waiting then (
      jobs_queue := !jobs_queue @ !jobs_wait_queue;
      jobs_wait_queue := [];
      jobs_waiting := false
    );
    if (List.length !jobs_queue) > 0 then (
      let del = update_all ticks [] !jobs_queue in
      jobs_queue := List.filter (fun a ->
        (List.exists (fun d -> a == d) del) = false
      ) !jobs_queue
    )

end

(* ========================================================================= *)
(* ========================================================================= *)
(* MAIN LOOP                                                                 *)
(* ========================================================================= *)
(* ========================================================================= *)

let emit_events ~event ~handle_event ~wait =
  let rec consume_events () =
    handle_event ~event;
    if Sdl.poll_event (Some event) = true then consume_events () else ()
  in
  if wait then (
    sdl_try (Sdl.wait_event (Some event));
    consume_events ()
  ) else if Sdl.poll_event (Some event) = true then (
    consume_events ()
  )

let rec loop ~renderer ~vsync ~event ~wait_for_events ~needs_redraw ~quit_requested
    ~handle_update ~handle_event ~handle_draw =
  if vsync <> true then Sdl.delay ms_wait_60fps;
  let new_ticks = sdl_get_ticks () in
  delta := new_ticks - !ticks;
  ticks := new_ticks;
  let wait =
    (Timer.length ()) = 0 &&
    (Anims.length ()) = 0 &&
    !needs_redraw = false &&
    !wait_for_events = true in
  Timer.update !ticks;
  Anims.update !ticks;
  emit_events ~event ~handle_event ~wait;
  handle_update ~ticks:!ticks;

  if !needs_redraw then (
    sdl_ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
    sdl_ignore (Sdl.render_clear renderer);
    handle_draw ~renderer;
    Sdl.render_present renderer
  );
  if !quit_requested = true then
    print_endline "bye!"
  else
    loop ~renderer ~vsync ~event ~wait_for_events ~needs_redraw ~quit_requested
    ~handle_update ~handle_event ~handle_draw


let init ~w ~h ~logical_w ~logical_h ~name =
  sdl_try (Sdl.init Sdl.Init.(video + events + audio));
  let audio_chunk_size = 2048 in
  sdl_try (Mixer.open_audio Mixer.default_frequency Mixer.default_format
           Mixer.default_channels audio_chunk_size);

  sdl_ignore (Sdl.set_hint Sdl.Hint.render_scale_quality "2");
  sdl_ignore (Sdl.set_hint Sdl.Hint.render_vsync "1");

  let w_flags = Sdl.Window.(opengl + resizable) in
  let win = sdl_get_ok (Sdl.create_window ~w ~h name w_flags) in
  let r_flags = Sdl.Renderer.(presentvsync + accelerated + targettexture) in
  let renderer = sdl_get_ok (Sdl.create_renderer ~flags:r_flags win) in

  sdl_try (Sdl.render_set_scale renderer 4. 3.);
  sdl_try (Sdl.render_set_logical_size renderer logical_w logical_h);
  ticks := sdl_get_ticks ();
  delta := 0;
  (win, renderer)

let release (w,r) =
  Mixer.close_audio ();
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Sdl.quit ()
