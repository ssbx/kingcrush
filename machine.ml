open CamlSDL2
open Gamekit

type state_t =
  { mutable fun_update : int -> unit
  ; mutable fun_draw : Sdl.Renderer.t -> unit
  ; mutable fun_event : Sdl.Event.t -> unit
  }

let curr_state : state_t =
  { fun_update = (fun _ -> ()); fun_draw = (fun _ -> ()); fun_event = (fun _ -> ()) }
;;

let to_streak_play () =
  Audio.Music.fade_out ~ms:500;
  Audio.Sample.play Audio.Sample.LevelStart;
  Info.ctrl_set Streak_controller.interface;
  Info.model_set Streak_model.interface;
  Streak_controller.new_game 1;
  curr_state.fun_update <- (fun _ -> Board_position.update ());
  curr_state.fun_event <- (fun e -> Board_position.handle_sdl_event e);
  curr_state.fun_draw
  <- (fun renderer ->
       Background.draw ~renderer;
       Board_squares.draw ~renderer;
       Board_hints.draw ~renderer;
       Board_position.draw ~renderer;
       Streak_hud.draw ~renderer)
;;

let to_streak_menu () =
  Fade.alpha := 255;
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw
  <- (fun renderer ->
       Background.draw ~renderer;
       Menu.draw ~renderer;
       Fade.draw ~renderer);
  Audio.Music.fade_out ~ms:300;
  Timer.fire_in 500 (fun () ->
    Audio.Music.play Audio.Music.Calm;
    Fade.fade_in (fun () ->
      curr_state.fun_event
      <- (fun event ->
           match event with
           | Sdl.Event.SDL_MOUSEBUTTONDOWN mb_evt ->
             if Menu.handle_sdl_button_down mb_evt = true
             then (
               curr_state.fun_event <- (fun _ -> ());
               Fade.fade_out (fun () -> to_streak_play ()))
           | other -> Menu.handle_sdl_event other)))
;;

let to_level_details () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw
  <- (fun renderer ->
       Background.draw ~renderer;
       Board_squares.draw ~renderer;
       Board_position.draw ~renderer;
       Osd.Level_details.draw ~renderer;
       Fade.draw ~renderer);
  Fade.alpha := 0;
  Osd.Level_details.start_anim_in (fun () ->
    curr_state.fun_event
    <- (fun event ->
         match event with
         | SDL_MOUSEBUTTONDOWN _ ->
           curr_state.fun_event <- (fun _ -> ());
           Osd.Level_details.start_anim_out (fun () -> ());
           Timer.fire_in 500 (fun () ->
             Fade.fade_out (fun () ->
               Audio.Music.fade_out ~ms:1000;
               to_streak_menu ()))
         | _ -> ()))
;;

let to_level_info () =
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  curr_state.fun_draw
  <- (fun renderer ->
       Background.draw ~renderer;
       Board_squares.draw ~renderer;
       Board_position.draw ~renderer;
       Osd.Level_info.draw ~renderer);
  Osd.Level_info.start_anim_in (fun () ->
    Timer.fire_in 1000 (fun () ->
      Osd.Level_info.start_anim_out (fun () -> to_level_details ())))
;;

let to_level_over () =
  curr_state.fun_draw
  <- (fun renderer ->
       Background.draw ~renderer;
       Board_squares.draw ~renderer;
       Board_position.draw ~renderer;
       Osd.Level_over.draw ~renderer);
  curr_state.fun_update <- (fun _ -> ());
  curr_state.fun_event <- (fun _ -> ());
  Timer.fire_in 2300 (fun () -> Audio.Music.play Audio.Music.Groove);
  Osd.Level_over.start_anim_in (fun () ->
    Timer.fire_in 1000 (fun () ->
      Osd.Level_over.start_anim_out (fun () -> to_level_info ())))
;;

let handle_streak_event = function
  | Events.GameOver ->
    to_level_over ();
    Audio.Sample.play Audio.Sample.GameOver
  | Events.LevelComplete ->
    to_level_over ();
    Audio.Sample.play Audio.Sample.LevelComplete
  | e ->
    Board_position.handle_game_event e;
    Streak_hud.handle_game_event e
;;

let handle_sdl_event = function
  | Sdl.Event.SDL_QUIT _ -> Streak_controller.quit ()
  | Sdl.Event.SDL_WINDOWEVENT _ -> Info.needs_redraw := true
  | Sdl.Event.SDL_KEYDOWN e ->
    if e.scancode = Sdl.Scancode.ESCAPE then Streak_controller.quit ()
  | e -> curr_state.fun_event e
;;

let update ~ticks = curr_state.fun_update ticks
let draw ~renderer = curr_state.fun_draw renderer
