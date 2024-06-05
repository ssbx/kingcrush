open Gamekit
open Gamekit.Utils
open Ressources

(* ================================================================== *)
(* transition states between phases ================================= *)
(* ================================================================== *)
type view_state_t =
    Playing    (* inputs enabled, position receive all events -> to_level_over *)
  | LevelOver  (* inputs disabled, anim in/out Osd_level_over -> to_level_info *)
  | LevelInfo  (* inputs disabled, anim in/out Osd_level_info -> to_level_details *)
  | LevelDetails (* inputs disabled, anim in Osd_level_details -> to_details_wait *)
  | LevelDetailsWait (* wait click to move forward -> to_map *)
  | Map (* inputs disabled, anim in/out map -> to_map_wait *)
  | MapWait (*inputs enabled, wait_click to move forward -> to_play *)

let view_state : view_state_t ref = ref Playing

let to_map_wait () =
  Audio.music_play Audio.Calm;
  Timer.fire_in 1000 (fun () -> Scr_fade.fade_in (fun () -> ()));
  view_state := MapWait

let to_map () =
  Audio.music_stop ();
  Osd_level_details.start_anim_out (fun () -> ());
  Scr_fade.fade_out (fun () -> to_map_wait ());
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

let to_play () =
  Controller.new_game 1;
  view_state := Playing

let handle_sdl_event2 ~event = function
  | Playing ->
      Brd_position.handle_sdl_event ~event
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
    Game_state.wait_for_events := false;
    Game_state.needs_redraw := true;
    to_level_over ();
    Audio.play Audio.GameOver
  | Model.LevelComplete ->
    to_level_over ();
    Audio.play Audio.LevelComplete
  | e ->
    Brd_position.handle_game_event e;
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
    Game_state.needs_redraw := true
  | _ ->
    handle_sdl_event2 ~event !view_state

let update ~ticks =
  Game_state.needs_redraw := true;
  Game_state.delta := ticks - !Game_state.ticks;
  Game_state.ticks := ticks;
  Timer.update !Game_state.ticks;
  Anims.update !Game_state.ticks;
  Brd_position.update ()

let draw ~renderer =
  Brd_squares.draw ~renderer;
  Brd_hints.draw ~renderer;
  Brd_position.draw ~renderer;
  Osd_level_over.draw ~renderer;
  Osd_level_info.draw ~renderer;
  Osd_level_details.draw ~renderer;
  Scr_fade.draw ~renderer

let init ~renderer ~with_audio ~with_anims =
  Game_state.ticks := 0;
  Game_state.delta := 0;
  Game_state.with_audio := with_audio;
  Game_state.with_anims := with_anims;
  Audio.init ();
  Fonts.init ();
  Model.init ();
  Pieces.init ~renderer;
  Scr_bg.init ~renderer;
  Score.init ~renderer;
  Brd_squares.init ~renderer;
  Brd_hints.init ~renderer;
  Scr_fade.init ~renderer;
  Osd_level_over.init ~renderer;
  Osd_level_info.init ~renderer;
  Osd_level_details.init ~renderer;
  Brd_position.init ~renderer;
  Model.listen handle_game_event;
  Controller.new_game 1 ~timeout:false

let release () =
  Brd_position.release ();
  Osd_level_details.release ();
  Osd_level_info.release ();
  Osd_level_over.release ();
  Scr_fade.release ();
  Brd_hints.release ();
  Brd_squares.release ();
  Score.release ();
  Scr_bg.release ();
  Pieces.release ();
  (*Fonts.release ();*)
  Audio.release ();
  Model.release ()

