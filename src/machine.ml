open Gamekit

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
  Streak_controller.new_game 1;
  curr_state.fun_update <- (fun _ -> Board_position.update ());
  curr_state.fun_event <- (fun e -> Board_position.handle_sdl_event ~event:e);
  curr_state.fun_draw <- (fun renderer ->
    Background.draw ~renderer;
    Board_squares.draw ~renderer;
    Board_hints.draw ~renderer;
    Board_position.draw ~renderer;
    Streak_hud.draw ~renderer
  )

let to_menu () =
  Fade.alpha := 255;
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Background.draw ~renderer;
    Menu.draw ~renderer;
    Fade.draw ~renderer);
  Audio.music_fade_out 700;
  Timer.fire_in 500 (fun () ->
    Audio.music_play Audio.MusicCalm;
    Fade.fade_in
      (fun () ->
       curr_state.fun_event <- (fun e ->
         if sdl_get_evt_typ e = `Mouse_button_down then (
           if (Menu.handle_sdl_button_down e) = true then (
             curr_state.fun_event <- (fun _ -> ());
             Fade.fade_out (fun () -> to_play ()))
         ) else (
           Menu.handle_sdl_event e
         )
       )
      )
    )

let to_level_details () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Background.draw ~renderer;
    Board_squares.draw ~renderer;
    Board_position.draw ~renderer;
    Osd.Level_details.draw ~renderer;
    Fade.draw ~renderer);
  Fade.alpha := 0;
  Osd.Level_details.start_anim_in (fun () ->
    curr_state.fun_event <- (fun e ->
      if sdl_get_evt_typ e = `Mouse_button_down then (
        curr_state.fun_event <- (fun _ -> ());
        Osd.Level_details.start_anim_out (fun () -> () );
        Timer.fire_in 500 (fun () ->
          Fade.fade_out (fun () ->  Audio.music_fade_out 1000; to_menu ());
        )
        )
      )
    )

let to_level_info () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw <- (fun renderer ->
    Background.draw ~renderer;
    Board_squares.draw ~renderer;
    Board_position.draw ~renderer;
    Osd.Level_info.draw ~renderer);
  Osd.Level_info.start_anim_in (fun () ->
    Timer.fire_in 1000 (fun () ->
      Osd.Level_info.start_anim_out (fun () -> to_level_details ()))
    )

let to_level_over () =
  curr_state.fun_draw <- (fun renderer ->
    Background.draw ~renderer;
    Board_squares.draw ~renderer;
    Board_position.draw ~renderer;
    Osd.Level_over.draw ~renderer);
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  Timer.fire_in 2300 (fun () -> Audio.music_play Audio.MusicGroove);
  Osd.Level_over.start_anim_in (fun () ->
    Timer.fire_in 1000 (fun () ->
      Osd.Level_over.start_anim_out
        (fun () -> to_level_info ())))

let handle_game_event = function
  | Streak_model.GameOver ->
      Info.wait_for_events := false;
    Info.needs_redraw := true;
    to_level_over ();
    Audio.play Audio.GameOver
  | Streak_model.LevelComplete ->
      to_level_over ();
    Audio.play Audio.LevelComplete
  | e ->
      Board_position.handle_game_event e;
    Streak_hud.handle_game_event e


let handle_sdl_event ~event =
  match sdl_get_evt_typ event with
  | `Key_down ->
      if (sdl_get_evt_scancode event) = `Escape then
        Streak_controller.quit ()
  | `Quit ->
      Streak_controller.quit ()
  | `Window_event ->
      Info.needs_redraw := true
  | _ ->
      curr_state.fun_event event

let update ~ticks =
  curr_state.fun_update ticks

let draw ~renderer =
  curr_state.fun_draw renderer

