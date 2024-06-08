open Tsdl
open Tsdl_mixer

module Anims = Anims
module Timer = Timer

include Utils

let ms_wait_60fps = Int32.div 1000l 60l
let ticks : int ref = ref 0
let delta : int ref = ref 0


let emit_events ~event ~handle_event ~wait =
  let rec consume_events () =
    handle_event ~event;
    if Sdl.poll_event (Some event) = true then consume_events () else ()
  in
  if wait then (
    sdl_try (Sdl.wait_event (Some event));
    consume_events ()
  ) else if Sdl.poll_event (Some event) = true then (
    consume_events ()
  )

let rec loop ~renderer ~vsync ~event ~wait_for_events ~needs_redraw ~quit_requested
    ~handle_update ~handle_event ~handle_draw =
  if vsync <> true then Sdl.delay ms_wait_60fps;
  let new_ticks = sdl_get_ticks () in
  delta := new_ticks - !ticks;
  ticks := new_ticks;
  let wait =
    (Timer.length ()) = 0 &&
    (Anims.length ()) = 0 &&
    !needs_redraw = false &&
    !wait_for_events = true in
  Timer.update !ticks;
  Anims.update !ticks;
  emit_events ~event ~handle_event ~wait;
  handle_update ~ticks:!ticks;

  if !needs_redraw then (
    sdl_ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
    sdl_ignore (Sdl.render_clear renderer);
    handle_draw ~renderer;
    Sdl.render_present renderer
  );
  if !quit_requested = true then
    print_endline "bye!"
  else
    loop ~renderer ~vsync ~event ~wait_for_events ~needs_redraw ~quit_requested
    ~handle_update ~handle_event ~handle_draw


let init ~w ~h ~logical_w ~logical_h ~name =
  sdl_try (Sdl.init Sdl.Init.(video + events + audio));
  let audio_chunk_size = 2048 in
  sdl_try (Mixer.open_audio Mixer.default_frequency Mixer.default_format
           Mixer.default_channels audio_chunk_size);

  sdl_ignore (Sdl.set_hint Sdl.Hint.render_scale_quality "2");
  sdl_ignore (Sdl.set_hint Sdl.Hint.render_vsync "1");

  let w_flags = Sdl.Window.(opengl + resizable) in
  let win = sdl_get_ok (Sdl.create_window ~w ~h name w_flags) in
  let r_flags = Sdl.Renderer.(presentvsync + accelerated + targettexture) in
  let renderer = sdl_get_ok (Sdl.create_renderer ~flags:r_flags win) in

  sdl_try (Sdl.render_set_scale renderer 4. 3.);
  sdl_try (Sdl.render_set_logical_size renderer logical_w logical_h);
  ticks := sdl_get_ticks ();
  delta := 0;
  (win, renderer)

let release (w,r) =
  Mixer.close_audio ();
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Sdl.quit ()
