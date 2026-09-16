
open Tsdl
open Gamekit
open Ressources

module Machine = struct

  let num_puzzles = ref 5

  let set_num_puzzles n =
       num_puzzles := n


  type state_t = {
    mutable fun_update : int -> unit;
    mutable fun_draw : Tsdl.Sdl.renderer -> unit;
    mutable fun_event : Tsdl.Sdl.event -> unit;
  }

  let curr_state : state_t = {
    fun_update = (fun _ -> ());
    fun_draw = (fun _ -> ());
    fun_event = (fun _ -> ());
  }

  let to_streak_play () =
    Audio.music_stop ();
    Audio.play Audio.LevelStart;
    Streak.Controller.new_game !num_puzzles;
    curr_state.fun_update <- (fun _ -> Board.Position.update ());
    curr_state.fun_event <- (fun e -> Board.Position.handle_sdl_event ~event:e);
    curr_state.fun_draw <- (fun renderer ->
      Utils.Background.draw ~renderer;
      Board.Squares.draw ~renderer;
      Board.Hints.draw ~renderer;
      Board.Position.draw ~renderer;
      Streak.Hud.draw ~renderer
    )

  let to_streak_menu () =
    Utils.Fade.alpha := 255;
    curr_state.fun_update <- (fun _ -> ());
    curr_state.fun_event <- (fun _ -> ());
    curr_state.fun_draw <- (fun renderer ->
      Utils.Background.draw ~renderer;
      Ui.Menu.draw ~renderer;
      Utils.Fade.draw ~renderer);
    Audio.music_fade_out 700;
    Timer.fire_in 500 (fun () ->
      Audio.music_play Audio.MusicCalm;
      Utils.Fade.fade_in
        (fun () ->
         curr_state.fun_event <- (fun e ->
           if sdl_get_evt_typ e = `Mouse_button_down then (
             if (Ui.Menu.handle_sdl_button_down e) = true then (
               curr_state.fun_event <- (fun _ -> ());
               Utils.Fade.fade_out (fun () -> to_streak_play ()))
           ) else (
             Ui.Menu.handle_sdl_event e
           )
         )
        )
      )

  let to_level_details () =
    curr_state.fun_update <- (fun _ -> ());
    curr_state.fun_event <- (fun _ -> ());
    curr_state.fun_draw <- (fun renderer ->
      Utils.Background.draw ~renderer;
      Board.Squares.draw ~renderer;
      Board.Position.draw ~renderer;
      Ui.Level_details.draw ~renderer;
      Utils.Fade.draw ~renderer);
    Utils.Fade.alpha := 0;
    Ui.Level_details.start_anim_in (fun () ->
      curr_state.fun_event <- (fun e ->
        if sdl_get_evt_typ e = `Mouse_button_down then (
          curr_state.fun_event <- (fun _ -> ());
          Ui.Level_details.start_anim_out (fun () -> () );
          Timer.fire_in 500 (fun () ->
            Utils.Fade.fade_out (fun () ->  Audio.music_fade_out 1000; to_streak_menu ());
          )
        )
      )
    )

  let to_level_info () =
    curr_state.fun_update <- (fun _ -> ());
    curr_state.fun_event <- (fun _ -> ());
    curr_state.fun_draw <- (fun renderer ->
      Utils.Background.draw ~renderer;
      Board.Squares.draw ~renderer;
      Board.Position.draw ~renderer;
      Ui.Level_info.draw ~renderer);
    Ui.Level_info.start_anim_in (fun () ->
      Timer.fire_in 1000 (fun () ->
        Ui.Level_info.start_anim_out (fun () -> to_level_details ()))
      )

  let to_level_over () =
    curr_state.fun_draw <- (fun renderer ->
      Utils.Background.draw ~renderer;
      Board.Squares.draw ~renderer;
      Board.Position.draw ~renderer;
      Ui.Level_over.draw ~renderer);
    curr_state.fun_update <- (fun _ -> ());
    curr_state.fun_event <- (fun _ -> ());
    Timer.fire_in 2300 (fun () -> Audio.music_play Audio.MusicGroove);
    Ui.Level_over.start_anim_in (fun () ->
      Timer.fire_in 1000 (fun () ->
        Ui.Level_over.start_anim_out
          (fun () -> to_level_info ())))

  let handle_streak_event = function
    | Streak.Model.GameOver ->
        Conf.wait_for_events := false;
      Conf.needs_redraw := true;
      to_level_over ();
      Audio.play Audio.GameOver
    | Streak.Model.LevelComplete ->
        to_level_over ();
      Audio.play Audio.LevelComplete
    | e ->
      Board.Position.handle_game_event e;
      Streak.Hud.handle_game_event e

  let handle_sdl_event ~event =
    match sdl_get_evt_typ event with
    | `Key_down ->
        if (sdl_get_evt_scancode event) = `Escape then
          Streak.Controller.quit ()
    | `Quit ->
        Streak.Controller.quit ()
    | `Window_event ->
        Conf.needs_redraw := true
    | _ ->
        curr_state.fun_event event

  let update ~ticks =
    curr_state.fun_update ticks

  let draw ~renderer =
    curr_state.fun_draw renderer

end


let usage_msg = "kingcrush [--disable-anims] [--disable-audio] [--verbose]"
let with_audio = ref true
let verbose = ref false

let generate_themes_dir : string ref = ref ""
let with_datadir        : string ref = ref ""

let speclist =
  [
    ("--generate-themes-in", Arg.Set_string generate_themes_dir,
      "Generate themes.txt and theme_groups.txt in directory argument and quit" );
    ("--with-datadir", Arg.Set_string with_datadir, "Overhide default datadir search");
    ("--disable-audio", Arg.Clear Audio.enabled, "Disable audio");
    ("--verbose", Arg.Set verbose, "For debugging purpose only");
  ]

let () =

  Conf.pref_dir := sdl_get_ok (Sdl.get_pref_path ~org:"seb" ~app:"kingcrush");
  Conf.base_dir :=
    Filename.concat (
      Filename.concat
        (sdl_get_ok (Sdl.get_base_path ()) |> Filename.dirname )
        "share" )
    "kingcrush";

  Arg.parse speclist (fun _ -> ()) usage_msg;

  if String.length !with_datadir > 0 then Conf.base_dir := !with_datadir;
  if String.length !generate_themes_dir > 0 then (
    let tfile  = Filename.concat !generate_themes_dir "themes.txt"
    and tgfile = Filename.concat !generate_themes_dir "theme_groups.txt" in
    Streak.Model.generate_themes ~themes_file:tfile ~theme_groups_file:tgfile;
    exit 0;
  );


  let (window, renderer) = Gamekit.init
    ~w:1200
    ~h:800
    ~logical_w:Conf.Display.logical_w
    ~logical_h:Conf.Display.logical_h
    ~name:"kingcrush" in

  Conf.with_audio := !with_audio;
  Audio.init ();
  Fonts.init ();
  Figures.init ~renderer;
  Streak.Model.init ();
  Streak.Model.register_callback Machine.handle_streak_event;
  Streak.Hud.init ~renderer;
  Conf.ctrl_set Streak.Controller.interface;
  Conf.model_set Streak.Model.interface;
  Ui.Menu.init ~renderer;
  Utils.Background.init ~renderer;
  Utils.Fade.init ~renderer;
  Board.Position.init ~renderer;
  Board.Squares.init ~renderer;
  Board.Hints.init ~renderer;
  Ui.Level_over.init ~renderer;
  Ui.Level_info.init ~renderer;
  Ui.Level_details.init ~renderer;
  Ui.Level_confirm.init ~renderer;
  Conf.Levels.init ();
  Machine.set_num_puzzles 5;
  Machine.to_streak_menu ();

  Gamekit.loop
    ~renderer ~vsync:false ~event:(Sdl.Event.create ())
    ~wait_for_events:Conf.wait_for_events
    ~needs_redraw:Conf.needs_redraw
    ~quit_loop:Conf.quit_loop
    ~handle_event:Machine.handle_sdl_event
    ~handle_update:Machine.update
    ~handle_draw:Machine.draw;

  Board.Position.release ();
  Ui.Level_details.release ();
  Ui.Level_info.release ();
  Ui.Level_over.release ();
  Ui.Level_confirm.release ();
  Utils.Fade.release ();
  Board.Hints.release ();
  Board.Squares.release ();
  Streak.Model.release ();
  Streak.Hud.release ();
  Ui.Menu.release ();
  Utils.Background.release ();
  Figures.release ();
  (*Fonts.release ();*)
  Audio.release ();

  Gamekit.release (window,renderer);
  exit 0


