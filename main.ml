open CamlSDL2

let usage_msg = "kingcrush [--disable-anims] [--disable-audio] [--verbose]"
let with_audio = ref true
let verbose = ref false
let test_uci = ref false

let generate_themes_dir : string ref = ref ""
let with_datadir        : string ref = ref ""

let speclist =
  [
    ("--generate-themes-in", Arg.Set_string generate_themes_dir,
      "Generate themes.txt and theme_groups.txt in directory argument and quit" );
    ("--with-datadir", Arg.Set_string with_datadir, "Overhide default datadir search");
    ("--verbose", Arg.Set verbose, "For debugging purpose only");
    ("--test-uci", Arg.Set test_uci, "For debugging purpose only");
  ]

let () =

  Info.pref_dir := Sdl.get_pref_path ~org:"seb" ~app:"kingcrush";
  Info.base_dir := Sdl.get_base_path ();

  Arg.parse speclist (fun _ -> ()) usage_msg;
  if !test_uci then (
    Chesslib.Uci.test ();
    exit 0
  );

  if String.length !with_datadir > 0 then Info.base_dir := !with_datadir;
  if String.length !generate_themes_dir > 0 then (
    let tfile  = Filename.concat !generate_themes_dir "themes.txt"
    and tgfile = Filename.concat !generate_themes_dir "theme_groups.txt" in
    Streak_model.generate_themes ~themes_file:tfile ~theme_groups_file:tgfile;
    exit 0;
  );


  let (window, renderer) = Gamekit.init
    ~w:1200
    ~h:800
    ~logical_w:Info.Display.logical_w
    ~logical_h:Info.Display.logical_h
    ~name:"kingcrush"
    ~font_dir:(Filename.concat !Info.base_dir "fonts")
  in

  Info.with_audio := !with_audio;
  Audio.init ();
  Figures.init ~renderer;
  Streak_model.init ();
  Streak_model.register_callback Machine.handle_streak_event;
  Streak_hud.init ~renderer;
  Info.ctrl_set Streak_controller.interface;
  Info.model_set Streak_model.interface;
  Versus_model.init ();
  Versus_model.register_callback Machine.handle_versus_event;
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
  Levels.init ();
  Machine.to_streak_menu ();

  (*
  Gamekit.loop
    ~renderer ~vsync:false ~event:(Sdl.Event.create ())
    ~wait_for_events:Info.wait_for_events
    ~needs_redraw:Info.needs_redraw
    ~quit_loop:Info.quit_loop
    ~handle_event:Machine.handle_sdl_event
    ~handle_update:Machine.update
    ~handle_draw:Machine.draw;
    *)
  Gamekit.loop
    ~renderer ~vsync:false
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


