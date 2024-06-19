open Tsdl
open Gamekit

#include "log.cppo"

let usage_msg = "kingcrush [--disable-anims] [--disable-audio] [--verbose]"
let with_anims = ref true
let with_audio = ref true
let generate_themes = ref false
let verbose = ref false

let speclist =
  [
    ( "--generate-themes",
      Arg.Set generate_themes,
      "Generate themes.txt file and quit" );
    ("--disable-anims", Arg.Clear with_anims, "Disable animations");
    ("--disable-audio", Arg.Clear Audio.enabled, "Disable audio");
    ("--verbose", Arg.Set verbose, "For debugging purpose only");
  ]

let () =

  let _prefs = sdl_get_ok (Sdl.get_pref_path ~org:"seb" ~app:"kingcrush") in
  Arg.parse speclist (fun _ -> ()) usage_msg;

  let (window, renderer) = Gamekit.init
    ~w:1200
    ~h:800
    ~logical_w:Info.Display.logical_w
    ~logical_h:Info.Display.logical_h
    ~name:"kingcrush" in

  Info.with_audio := !with_audio;
  Info.with_anims := !with_anims;
  Audio.init ();
  Fonts.init ();
  Figures.init ~renderer;
  Streak_model.listen Machine.handle_game_event;
  Streak_model.init ();
  Streak_hud.init ~renderer;
  Streak_menu.init ~renderer;
  Background.init ~renderer;
  Fade.init ~renderer;
  Board_position.init ~renderer;
  Board_squares.init ~renderer;
  Board_hints.init ~renderer;
  Osd.Level_over.init ~renderer;
  Osd.Level_info.init ~renderer;
  Osd.Level_details.init ~renderer;
  Osd.Level_confirm.init ~renderer;
  Machine.to_play ();

  Gamekit.loop
    ~renderer ~vsync:false ~event:(Sdl.Event.create ())
    ~wait_for_events:Info.wait_for_events
    ~needs_redraw:Info.needs_redraw
    ~quit_loop:Info.quit_loop
    ~handle_event:Machine.handle_sdl_event
    ~handle_update:Machine.update
    ~handle_draw:Machine.draw;

  Board_position.release ();
  Osd.Level_details.release ();
  Osd.Level_info.release ();
  Osd.Level_over.release ();
  Osd.Level_confirm.release ();
  Fade.release ();
  Board_hints.release ();
  Board_squares.release ();
  Streak_model.release ();
  Streak_hud.release ();
  Streak_menu.release ();
  Background.release ();
  Figures.release ();
  (*Fonts.release ();*)
  Audio.release ();

  Gamekit.release (window,renderer);
  exit 0


