open Gamekit
open Ressources

type state_t = {
  mutable fun_update : int -> unit;
  mutable fun_draw : Tsdl.Sdl.renderer -> unit;
  mutable fun_event : Tsdl.Sdl.event -> unit;
}

let curr_state : state_t = {
  fun_update = (fun _ -> ());
  fun_draw = (fun _ -> ());
  fun_event = (fun _ -> ());
}

let to_play () =
  Audio.music_stop ();
  Audio.play Audio.LevelStart;
  Gm_streak_controller.new_game 1;
  curr_state.fun_update <- (fun _ -> Brd_position.update ());
  curr_state.fun_event <- (fun e -> Brd_position.handle_sdl_event ~event:e);
  curr_state.fun_draw <- (fun rdr ->
      Scr_bg.draw ~renderer:rdr;
      Brd_squares.draw ~renderer:rdr;
      Brd_hints.draw ~renderer:rdr;
      Brd_position.draw ~renderer:rdr)

let to_map () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Scr_map.draw ~renderer;
    Scr_fade.draw ~renderer);
  Audio.music_play Audio.Calm;
  Scr_fade.fade_in (fun () ->
    curr_state.fun_event <- (fun e ->
      if sdl_get_evt_typ e = `Mouse_button_down then (
        curr_state.fun_event <- (fun _ -> ());
        Scr_fade.fade_out (fun () -> to_play ())
      )
    )
  )

let to_level_details () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Scr_bg.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_position.draw ~renderer;
    Osd_level_details.draw ~renderer;
    Scr_fade.draw ~renderer);
  Scr_fade.alpha := 0;
  Osd_level_details.start_anim_in (fun () ->
    curr_state.fun_event <- (fun e ->
      if sdl_get_evt_typ e = `Mouse_button_down then (
        curr_state.fun_event <- (fun _ -> ());
        Osd_level_details.start_anim_out (fun () -> () );
        Scr_fade.fade_out (fun () ->  Audio.music_stop (); to_map ());
      )
    )
  )

let to_level_info () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Scr_bg.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_position.draw ~renderer;
    Osd_level_info.draw ~renderer);
  Osd_level_info.start_anim_in (fun () ->
    Timer.fire_in 1000 (fun () ->
      Osd_level_info.start_anim_out (fun () -> to_level_details ()))
  )

let to_level_over () =
  curr_state.fun_draw <- (fun renderer ->
    Scr_bg.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_position.draw ~renderer;
    Osd_level_over.draw ~renderer);
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  Timer.fire_in 2300 (fun () -> Audio.music_play Audio.Groove);
  Osd_level_over.start_anim_in (fun () ->
    Timer.fire_in 1000 (fun () ->
      Osd_level_over.start_anim_out
        (fun () -> to_level_info ())))



let handle_game_event = function
  | Gm_streak_model.GameOver ->
    Game_info.wait_for_events := false;
    Game_info.needs_redraw := true;
    to_level_over ();
    Audio.play Audio.GameOver
  | Gm_streak_model.LevelComplete ->
    to_level_over ();
    Audio.play Audio.LevelComplete
  | e ->
    Brd_position.handle_game_event e;
    Gm_streak_score.handle_game_event e

let handle_sdl_event ~event =
  match sdl_get_evt_typ event with
  | `Key_down -> if (sdl_get_evt_scancode event) = `Escape then
    Gm_streak_controller.quit ()
  | `Quit ->
    Gm_streak_controller.quit ()
  | `Window_event ->
    Game_info.needs_redraw := true
  | _ ->
    curr_state.fun_event event

let update ~ticks =
  curr_state.fun_update ticks

let draw ~renderer =
  curr_state.fun_draw renderer


    (*
  let at_anim_ended = (fun () ->
    Timer.fire_in 2000 (fun () -> Audio.music_play Audio.Groove;
      Osd_level_over.start_anim_out to_level_info));


let to_level_details () =
  Osd_level_details.start_anim_in (fun () -> to_level_details_wait ());
  view_state := LevelDetails

let to_level_info () =
  Osd_level_info.start_anim_in (fun () ->
    Timer.fire_in 3000 (fun () ->
      Osd_level_info.start_anim_out to_level_details));
  view_state := LevelInfo
  Osd_level_over.start_anim_in (fun () ->
    Timer.fire_in 2000 (fun () ->
      Audio.music_play Audio.Groove;
      Osd_level_over.start_anim_out to_level_info));

*)
(* ================================================================== *)
(* model events ===================================================== *)
(* ================================================================== *)

  (*
let draw2 ~renderer = function
  | LevelOver | LevelInfo | LevelDetails
  | LevelDetailsWait | Map ->
    Scr_bg.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_hints.draw ~renderer;
    Brd_position.draw ~renderer;
    Osd_level_over.draw ~renderer;
    Osd_level_info.draw ~renderer;
    Osd_level_details.draw ~renderer;
    Osd_map_select.draw ~renderer;
    Scr_fade.draw ~renderer
  | Playing ->
    Scr_bg.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_hints.draw ~renderer;
    Brd_position.draw ~renderer
  | PlayStartIn | PlayStart | PlayStartOut ->
    Scr_bg.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_position.draw ~renderer;
    Osd_level_start.draw ~renderer;
    Scr_fade.draw ~renderer
  | PlayInitAnimOut | PlayInit | PlayInitAnimIn ->
    Scr_bg.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_position.draw ~renderer;
    Osd_level_start.draw ~renderer;
    Scr_fade.draw ~renderer
  | MapWait | MapIn | SoonPlay ->
    Scr_map.draw ~renderer;
    Scr_fade.draw ~renderer
  | MapSelectAnimIn | MapSelect | MapSelectOkAnimOut ->
    Scr_map.draw ~renderer;
    Scr_fade.draw ~renderer;
    Osd_map_select.draw ~renderer

let draw ~renderer =
  draw2 ~renderer !view_state
*)
  (*
let to_soon_play () =
  Audio.music_stop ();
  Timer.fire_in 1000 (fun () -> to_play ());
  view_state := SoonPlay

let to_map_select () =
  view_state := MapSelect

let to_map_select_anim_in () =
  (* todo animate fade out map bg *)
  Osd_map_select.start_anim_in (fun () -> to_map_select ());
  view_state := MapSelectAnimIn

let to_play_start_out () =
  Osd_level_start.start_anim_out (fun () -> ());
  to_play ()

let to_play_start () =
  view_state := PlayStart

let to_play_start_in () =
  Osd_level_start.start_anim_in (fun () -> ());
  Scr_fade.fade_in (fun () -> to_play_start ());
  view_state := PlayStartIn

let to_play_init_anim_out () =
  Osd_map_select.start_anim_out (fun () -> ());
  Scr_fade.fade_out (fun () -> to_play_start_in ());
  view_state := PlayInitAnimOut

let to_play_init () =
  view_state := PlayInit

let to_play_init_anim_in () =
  Audio.music_stop ();
  Scr_fade.fade_in (fun () ->
    Timer.fire_in 300 (fun () ->
      Osd_level_start.start_anim_in (fun () -> to_play_init ())
    )
  );
  view_state := PlayInitAnimIn

let to_map_select_ok_anim_out () =
  Osd_map_select.start_anim_out (fun () -> ());
  Scr_fade.fade_out (fun () -> to_play_init_anim_in ());
  view_state := MapSelectOkAnimOut

let handle_sdl_event2 ~event = function
  | LevelOver -> ()
  | LevelInfo -> ()
  | LevelDetails -> ()
  | Map -> ()
  | MapIn -> ()
  | SoonPlay -> ()
  | PlayInitAnimIn -> ()
  | PlayInitAnimOut -> ()
  | PlayStartIn -> ()
  | PlayStartOut -> ()
  | MapSelectAnimIn -> ()
  | MapSelectOkAnimOut -> ()
  | LevelDetailsWait ->
      if sdl_get_evt_typ event = `Mouse_button_down then to_map ()
  | Playing ->
      Brd_position.handle_sdl_event ~event
  | MapWait ->
      if sdl_get_evt_typ event = `Mouse_button_down then to_map_select_anim_in ()
  | MapSelect ->
      if sdl_get_evt_typ event = `Mouse_button_down then to_map_select_ok_anim_out ()
  | PlayInit ->
      if sdl_get_evt_typ event = `Mouse_button_down then to_play ()
  | PlayStart ->
      (*if sdl_get_evt_typ event = `Mouse_button_down then to_play_start_out ()*)
      if sdl_get_evt_typ event = `Mouse_button_down then to_play ()
*)

(*
let to_map_wait () =
  view_state := MapWait

let to_map_in () =
  Audio.music_play Audio.Calm;
  Timer.fire_in 1000 (fun () -> Scr_fade.fade_in (fun () -> to_map_wait ()));
  view_state := MapIn

let to_map () =
  Audio.music_stop ();
  Osd_level_details.start_anim_out (fun () -> ());
  Scr_fade.fade_out (fun () -> to_map_in ());
  view_state := Map

let to_level_details_wait () =
  view_state := LevelDetailsWait

let to_level_details () =
  Osd_level_details.start_anim_in (fun () -> to_level_details_wait ());
  view_state := LevelDetails

let to_level_info () =
  Osd_level_info.start_anim_in (fun () ->
    Timer.fire_in 3000 (fun () ->
      Osd_level_info.start_anim_out to_level_details));
  view_state := LevelInfo

let to_level_over () =
  Osd_level_over.start_anim_in (fun () ->
    Timer.fire_in 2000 (fun () ->
      Audio.music_play Audio.Groove;
      Osd_level_over.start_anim_out to_level_info));
  view_state := LevelOver
*)

