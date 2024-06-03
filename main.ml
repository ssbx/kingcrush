open Tsdl
open Tsdl_mixer
open Utils

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

let ms_wait_60fps = Int32.div 1000l 60l

let emit_events ~event =
  let rec consume_events () =
    Game.handle_sdl_event ~event;
    if Sdl.poll_event (Some event) = true then consume_events () else ()
  in

  if !Game.State.wait_for_events = true then (
    sdl_try (Sdl.wait_event (Some event));
    consume_events ())
  else if Sdl.poll_event (Some event) = true then consume_events ()

let rec loop ~renderer ~vsync ~event =
  if vsync <> true then Sdl.delay ms_wait_60fps;
  emit_events ~event;
  sdl_ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
  sdl_ignore (Sdl.render_clear renderer);
  Game.update ~ticks:(Int32.to_int (Sdl.get_ticks ()));
  if !Game.State.needs_redraw then (
    Game.draw ~renderer;
    Sdl.render_present renderer);
  if !Game.State.quit_requested = true then print_endline "bye!"
  else loop ~renderer ~vsync ~event

(* ====================================================================================*)
(* MAIN ===============================================================================*)
(* ====================================================================================*)
let () =

  Arg.parse speclist (fun _ -> ()) usage_msg;
  sdl_try (Sdl.init Sdl.Init.(video + events + audio));
  if !with_audio then (
    let audio_chunk_size = 2048 in
    sdl_try
      (Mixer.open_audio Mixer.default_frequency Mixer.default_format
         Mixer.default_channels audio_chunk_size)
  );

  sdl_ignore (Sdl.set_hint Sdl.Hint.render_scale_quality "2");
  sdl_ignore (Sdl.set_hint Sdl.Hint.render_vsync "1");

  let w_flags = Sdl.Window.(opengl + resizable) in
  let win =
    sdl_get_ok (Sdl.create_window ~w:640 ~h:480 "kingcrush" w_flags)
  in
  let r_flags = Sdl.Renderer.(presentvsync + accelerated + targettexture) in
  let renderer = sdl_get_ok (Sdl.create_renderer ~flags:r_flags win) in
  let info = sdl_get_ok (Sdl.get_renderer_info renderer) in

  if !verbose then (
    if Sdl.Renderer.test info.ri_flags Sdl.Renderer.accelerated then
      Printf.printf "RendererInfo accelerated: yes\n"
    else Printf.printf "RendererInfo accelerated: no\n";

    if Sdl.Renderer.test info.ri_flags Sdl.Renderer.software then
      Printf.printf "RendererInfo software: yes\n"
    else Printf.printf "RendererInfo software no\n";

    if Sdl.Renderer.test info.ri_flags Sdl.Renderer.targettexture then
      Printf.printf "RendererInfo targettexture: yes\n"
    else Printf.printf "RendererInfo targettexture: no\n";

    if Sdl.Renderer.test info.ri_flags Sdl.Renderer.presentvsync then
      Printf.printf "RendererInfo vsync: yes\n"
    else Printf.printf "RendererInfo vsync: no\n");

  let vsync = Sdl.Renderer.test info.ri_flags Sdl.Renderer.presentvsync in
  sdl_try (Sdl.render_set_scale renderer 4. 3.);
  sdl_try
    (Sdl.render_set_logical_size renderer Game.State.Screen.logical_w
       Game.State.Screen.logical_h);

  Game.init ~renderer ~with_anims:!with_anims ~with_audio:!with_audio;
  loop ~renderer ~vsync ~event:(Sdl.Event.create ());
  Game.release ();

  if !with_audio then Mixer.close_audio ();
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window win;
  Sdl.quit ();
  print_endline
    (sdl_get_ok (Sdl.get_pref_path ~org:"ssbx" ~app:"kingcrush"));
  exit 0

