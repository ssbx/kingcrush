module State = State
open Utils

(* ================================================================== *)
(* transition states between phases ================================= *)
(* ================================================================== *)
type view_state_t =
    Playing    (* inputs enabled, position receive all events -> to_level_over *)
  | LevelOver  (* inputs disabled, anim in/out Level_over -> to_level_info *)
  | LevelInfo  (* inputs disabled, anim in/out Level_info -> to_level_details *)
  | LevelDetails (* inputs disabled, anim in Level_details -> to_details_wait *)
  | LevelDetailsWait (* wait click to move forward -> to_map *)
  | Map (* inputs disabled, anim in/out map -> to_map_wait *)
  | MapWait (*inputs enabled, wait_click to move forward -> to_play *)

let view_state : view_state_t ref = ref Playing

let to_map_wait () =
  Audio.music_play Audio.Calm;
  Timer.fire_in 1000 (fun () -> Fade.fade_in (fun () -> ()));
  view_state := MapWait

let to_map () =
  Audio.music_stop ();
  Level_details.start_anim_out (fun () -> ());
  Fade.fade_out (fun () -> to_map_wait ());
  view_state := Map

let to_level_details_wait () =
  view_state := LevelDetailsWait

let to_level_details () =
  Level_details.start_anim_in (fun () -> to_level_details_wait ());
  view_state := LevelDetails

let to_level_info () =
  Level_info.start_anim_in (fun () ->
    Timer.fire_in 3000 (fun () ->
      Level_info.start_anim_out to_level_details));
  view_state := LevelInfo

let to_level_over () =
  Level_over.start_anim_in (fun () ->
    Timer.fire_in 2000 (fun () ->
      Audio.music_play Audio.Groove;
      Level_over.start_anim_out to_level_info));
  view_state := LevelOver

let to_play () =
  Controller.new_game 1;
  view_state := Playing

let handle_sdl_event2 ~event = function
  | Playing ->
      Position.handle_sdl_event ~event
  | LevelOver -> ()
  | LevelInfo -> ()
  | LevelDetails -> ()
  | LevelDetailsWait ->
      if sdl_get_evt_typ event = `Mouse_button_down then to_map ()
  | Map -> ()
  | MapWait ->
      if sdl_get_evt_typ event = `Mouse_button_down then to_play ()

(* ================================================================== *)
(* model events ===================================================== *)
(* ================================================================== *)

let handle_game_event = function
  | Model.GameOver ->
    State.wait_for_events := false;
    State.needs_redraw := true;
    to_level_over ();
    Sounds.play Audio.GameOver
  | Model.LevelComplete ->
    to_level_over ();
    Sounds.play Audio.LevelComplete
  | e ->
    Position.handle_game_event e;
    Score.handle_game_event e

(* ================================================================== *)
(* main.ml calls ==================================================== *)
(* ================================================================== *)
let handle_sdl_event ~event =
  match sdl_get_evt_typ event with
  | `Key_down -> if (sdl_get_evt_scancode event) = `Escape then
    Controller.quit ()
  | `Quit ->
    Controller.quit ()
  | `Window_event ->
    State.needs_redraw := true
  | _ ->
    handle_sdl_event2 ~event !view_state

let update ~ticks =
  State.needs_redraw := true;
  State.delta := ticks - !State.ticks;
  State.ticks := ticks;
  Timer.update !State.ticks;
  Anims.update !State.ticks;
  Position.update ()

let draw ~renderer =
  Board.draw ~renderer;
  Hints.draw ~renderer;
  Position.draw ~renderer;
  Level_over.draw ~renderer;
  Level_info.draw ~renderer;
  Level_details.draw ~renderer;
  Fade.draw ~renderer

let init ~renderer ~with_audio ~with_anims =
  State.ticks := 0;
  State.delta := 0;
  State.with_audio := with_audio;
  State.with_anims := with_anims;
  Sounds.init ();
  Fonts.init ();
  Model.init ();
  Pieces.init ~renderer;
  Background.init ~renderer;
  Score.init ~renderer;
  Board.init ~renderer;
  Hints.init ~renderer;
  Fade.init ~renderer;
  Level_over.init ~renderer;
  Level_info.init ~renderer;
  Level_details.init ~renderer;
  Position.init ~renderer;
  Model.listen handle_game_event;
  Controller.new_game 1 ~timeout:false

let release () =
  Position.release ();
  Level_details.release ();
  Level_info.release ();
  Level_over.release ();
  Fade.release ();
  Hints.release ();
  Board.release ();
  Score.release ();
  Background.release ();
  Pieces.release ();
  (*Fonts.release ();*)
  Sounds.release ();
  Model.release ()

