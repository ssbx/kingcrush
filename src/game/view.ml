open Tsdl

let map_mode : bool ref = ref false

let handle_game_event e =
  Position.handle_game_event e;
  Score.handle_game_event e

let forward_sdl_event ~event = Position.handle_sdl_event ~event

let to_map_scene2 () =
  Audio.music_play Audio.Calm

let to_map_scene () =
  Audio.music_stop ();
  Fade.fade_out (fun () -> to_map_scene2 ())

let handle_sdl_event ~event =
  match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
  | `Key_down ->
        begin match Sdl.Scancode.enum Sdl.Event.(get event keyboard_scancode) with
        | `Escape -> State.quit_requested := true
        | `T ->
            if !State.pause_redraw then State.pause_redraw := false
            else State.pause_redraw := true
        | _ -> ()
        end
  | `Window_event -> State.needs_redraw := true
  | `Quit -> State.quit_requested := true
  | `Mouse_button_up ->
      if !State.game_over then ()
      else forward_sdl_event ~event
  | `Mouse_button_down ->
      if !State.game_over && !State.game_over_ready then (
        Level_details.anim_out (fun () -> to_map_scene ())
      ) else forward_sdl_event ~event
  | _ -> forward_sdl_event ~event

let update () =
  State.needs_redraw := true;
  Anims.update !State.ticks;
  Position.update ()


let over_started = ref false

let draw ~renderer =
  Board.draw ~renderer;
  Hints.draw ~renderer;
  Position.draw ~renderer;
  Level_over.draw ~renderer;
  Level_info.draw ~renderer;
  Level_details.draw ~renderer;
  Fade.draw ~renderer

let init ~renderer =
  Background.init ~renderer;
  Score.init ~renderer;
  Board.init ~renderer;
  Hints.init ~renderer;
  Fade.init ~renderer;
  Level_over.init ~renderer;
  Level_info.init ~renderer;
  Level_details.init ~renderer;
  Position.init ~renderer;
  Model.listen handle_game_event

let release () =
  Background.release ();
  Score.release ();
  Level_over.release ();
  Level_info.release ();
  Level_details.release ();
  Board.release ();
  Hints.release ();
  Position.release ();
  Fade.release ()
