open Tsdl
open Ressources
open Gamekit

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

  let prefs = sdl_get_ok (Sdl.get_pref_path ~org:"seb" ~app:"kingcrush") in
  Printf.printf "prefs are %s\n" prefs;
  Arg.parse speclist (fun _ -> ()) usage_msg;

  let (window, renderer) = Gamekit.init
    ~w:1200
    ~h:800
    ~logical_w:Game_info.Screen.logical_w
    ~logical_h:Game_info.Screen.logical_h
    ~name:"kingcrush" in

  Game_info.with_audio := !with_audio;
  Game_info.with_anims := !with_anims;
  Audio.init ();
  Fonts.init ();
  Pieces.init ~renderer;
  Gm_streak_model.listen Game_sm.handle_game_event;
  Gm_streak_model.init ();
  Gm_streak_score.init ~renderer;
  Gm_streak_menu.init ~renderer;
  Gm_streak_menu2.init ~renderer;
  Scr_bg.init ~renderer;
  Scr_map.init ~renderer;
  Scr_fade.init ~renderer;
  Brd_position.init ~renderer;
  Brd_squares.init ~renderer;
  Brd_hints.init ~renderer;
  Osd_level_over.init ~renderer;
  Osd_level_info.init ~renderer;
  Osd_level_details.init ~renderer;
  Osd_level_start.init ~renderer;
  Osd_map_select.init ~renderer;
  Game_sm.to_menu ();

  Gamekit.loop
    ~renderer ~vsync:false ~event:(Sdl.Event.create ())
    ~wait_for_events:Game_info.wait_for_events
    ~needs_redraw:Game_info.needs_redraw
    ~quit_loop:Game_info.quit_loop
    ~handle_event:Game_sm.handle_sdl_event
    ~handle_update:Game_sm.update
    ~handle_draw:Game_sm.draw;

  Brd_position.release ();
  Osd_level_details.release ();
  Osd_level_info.release ();
  Osd_level_over.release ();
  Osd_level_start.release ();
  Osd_map_select.release ();
  Scr_fade.release ();
  Brd_hints.release ();
  Brd_squares.release ();
  Gm_streak_model.release ();
  Gm_streak_score.release ();
  Gm_streak_menu.release ();
  Gm_streak_menu2.release ();
  Scr_bg.release ();
  Scr_map.release ();
  Pieces.release ();
  (*Fonts.release ();*)
  Audio.release ();

  Gamekit.release (window,renderer);
  exit 0

