open Tsdl
open Gamekit

#include "log.cppo"

let usage_msg = "kingcrush [--disable-anims] [--disable-audio] [--verbose]"
let with_anims = ref true
let with_audio = ref true
let generate_themes = ref false
let verbose = ref false

let with_datadir : string ref = ref ""

let speclist =
  [
    ("--generate-themes", Arg.Set generate_themes, "Generate themes.txt file and quit" );
    ("--with-datadir", Arg.Set_string with_datadir, "Overhide default datadir search");
    ("--disable-anims", Arg.Clear with_anims, "Disable animations");
    ("--disable-audio", Arg.Clear Audio.enabled, "Disable audio");
    ("--verbose", Arg.Set verbose, "For debugging purpose only");
  ]

let () =


  Info.pref_dir := sdl_get_ok (Sdl.get_pref_path ~org:"seb" ~app:"kingcrush");
  Info.base_dir := sdl_get_ok (Sdl.get_base_path ());

  Arg.parse speclist (fun _ -> ()) usage_msg;
  if String.length !with_datadir > 0 then Info.base_dir := !with_datadir;

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
  Menu.init ~renderer;
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
  Menu.release ();
  Background.release ();
  Figures.release ();
  (*Fonts.release ();*)
  Audio.release ();

  Gamekit.release (window,renderer);
  exit 0


