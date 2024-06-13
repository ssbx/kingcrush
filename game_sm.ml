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
    Scr_map.draw ~renderer:rdr;
    Brd_squares.draw ~renderer:rdr;
    Brd_hints.draw ~renderer:rdr;
    Brd_position.draw ~renderer:rdr)

let to_menu () =
  Scr_fade.alpha := 255;
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Scr_map.draw ~renderer;
    Gm_streak_menu2.draw ~renderer;
    Scr_fade.draw ~renderer);
  Audio.music_play Audio.Calm;
  Timer.fire_in 1000 (fun () -> Scr_fade.fade_in
    (fun () ->
      curr_state.fun_event <- (fun e ->
        if sdl_get_evt_typ e = `Mouse_button_down then (
          if Gm_streak_menu2.handle_sdl_button_down e then
            curr_state.fun_event <- (fun _ -> ());
            Scr_fade.fade_out (fun () -> to_play ())
        ) else (
          Gm_streak_menu2.handle_sdl_event e
        )
      )
    )
  )

let to_level_details () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Scr_map.draw ~renderer;
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
        Timer.fire_in 1500 (fun () ->
          Scr_fade.fade_out (fun () ->  Audio.music_stop (); to_menu ());
        )
        )
      )
    )

let to_level_info () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Scr_map.draw ~renderer;
    Brd_squares.draw ~renderer;
    Brd_position.draw ~renderer;
    Osd_level_info.draw ~renderer);
  Osd_level_info.start_anim_in (fun () ->
    Timer.fire_in 1000 (fun () ->
      Osd_level_info.start_anim_out (fun () -> to_level_details ()))
    )

let to_level_over () =
  curr_state.fun_draw <- (fun renderer ->
    Scr_map.draw ~renderer;
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

