module State = State
open Tsdl

let handle_game_event = function
  | Model.GameOver ->
      State.game_over := true;
      State.wait_for_events := false;
      State.needs_redraw := true;
      Sounds.play Audio.GameOver
  | Model.LevelComplete ->
      State.game_over := true;
      Sounds.play Audio.LevelComplete
  | _ -> ()

let init ~renderer ~with_audio ~with_anims =
  State.ticks := 0;
  State.delta := 0;
  State.with_audio := with_audio;
  State.with_anims := with_anims;
  Sounds.init ();
  Fonts.init ();
  Model.init ();
  Pieces.init ~renderer;
  View.init ~renderer;
  Model.listen handle_game_event;
  Controller.new_game !State.game_len ~timeout:false


let release () =
  Pieces.release ();
  Model.release ();
  View.release ()

let handle_sdl_event ~event =
  match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
  | `Quit -> Controller.quit ()
  | `Key_down -> (
      match Sdl.Scancode.enum Sdl.Event.(get event keyboard_scancode) with
      | `Escape -> Controller.quit ()
      | _ -> View.handle_sdl_event ~event)
  | _ -> View.handle_sdl_event ~event

let update ~ticks =
  State.delta := ticks - !State.ticks;
  State.ticks := ticks;
  (*Anims.update !State.ticks;*)
  Timer.update !State.ticks;
  View.update ()

let draw ~renderer = View.draw ~renderer
