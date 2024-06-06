open Gamekit
open Gamekit.Utils
open Ressources

(* ================================================================== *)
(* transition states between phases ================================= *)
(* ================================================================== *)
type view_state_t =
    SoonPlay   (* inputs disabled soon playing *)
  | PlayInitAnimIn
  | PlayInit
  | PlayInitAnimOut
  | PlayStartIn
  | PlayStart
  | PlayStartOut
  | Playing    (* inputs enabled, position receive all events -> to_level_over *)
  | LevelOver  (* inputs disabled, anim in/out Osd_level_over -> to_level_info *)
  | LevelInfo  (* inputs disabled, anim in/out Osd_level_info -> to_level_details *)
  | LevelDetails (* inputs disabled, anim in Osd_level_details -> to_details_wait *)
  | LevelDetailsWait (* wait click to move forward -> to_map *)
  | Map (* inputs disabled, anim in/out map -> to_map_wait *)
  | MapIn
  | MapWait (*inputs enabled, wait_click to move forward -> to_play *)
  | MapSelectAnimIn
  | MapSelectOkAnimOut
  | MapSelect

let view_state : view_state_t ref = ref Playing

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

let to_play () =
  Audio.play Audio.LevelStart;
  Gm_streak_controller.new_game 1;
  view_state := Playing

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

(* ================================================================== *)
(* model events ===================================================== *)
(* ================================================================== *)

let handle_game_event = function
  | Gm_streak_model.GameOver ->
    Game_state.wait_for_events := false;
    Game_state.needs_redraw := true;
    to_level_over ();
    Audio.play Audio.GameOver
  | Gm_streak_model.LevelComplete ->
    to_level_over ();
    Audio.play Audio.LevelComplete
  | e ->
    Brd_position.handle_game_event e;
    Gm_streak_score.handle_game_event e

(* ================================================================== *)
(* main.ml calls ==================================================== *)
(* ================================================================== *)
let handle_sdl_event ~event =
  match sdl_get_evt_typ event with
  | `Key_down -> if (sdl_get_evt_scancode event) = `Escape then
    Gm_streak_controller.quit ()
  | `Quit ->
    Gm_streak_controller.quit ()
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

let init ~renderer ~with_audio ~with_anims =
  Game_state.ticks := 0;
  Game_state.delta := 0;
  Game_state.with_audio := with_audio;
  Game_state.with_anims := with_anims;
  Audio.init ();
  Fonts.init ();
  Gm_streak_model.init ();
  Pieces.init ~renderer;
  Scr_bg.init ~renderer;
  Scr_map.init ~renderer;
  Gm_streak_score.init ~renderer;
  Brd_squares.init ~renderer;
  Brd_hints.init ~renderer;
  Scr_fade.init ~renderer;
  Osd_level_over.init ~renderer;
  Osd_level_info.init ~renderer;
  Osd_level_details.init ~renderer;
  Osd_level_start.init ~renderer;
  Osd_map_select.init ~renderer;
  Brd_position.init ~renderer;
  Gm_streak_model.listen handle_game_event;
  to_play ()

let release () =
  Brd_position.release ();
  Osd_level_details.release ();
  Osd_level_info.release ();
  Osd_level_over.release ();
  Osd_level_start.release ();
  Osd_map_select.release ();
  Scr_fade.release ();
  Brd_hints.release ();
  Brd_squares.release ();
  Gm_streak_score.release ();
  Scr_bg.release ();
  Scr_map.release ();
  Pieces.release ();
  (*Fonts.release ();*)
  Audio.release ();
  Gm_streak_model.release ()

