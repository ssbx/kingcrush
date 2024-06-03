open Tsdl
open Gamekit
open Gamekit.Utils
open Ressources


let rdr : Sdl.renderer option ref = ref None
let bg_tex : Sdl.texture option ref = ref None
let rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
let get_rdr () = match !rdr with Some v -> v | None -> assert false
let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

let enabled : bool ref = ref false

let orig_x = ref 0
let orig_y = ref 0

let gen_text ~renderer ~text =
  let surf = Fonts.get_surface text in
  let w, h = Sdl.get_surface_size surf in
  let tex = sdl_get_ok (Sdl.create_texture_from_surface renderer surf) in
  Sdl.free_surface surf;
  (tex, w, h)

let init ~renderer =
  let otex, over_w, over_h = gen_text ~renderer ~text:"SCORE BOARD" in

  let rtex, retry_w, retry_h = gen_text ~renderer ~text:"Click to continue ..." in

  Sdl.Rect.set_w rect (Stdlib.max retry_w over_w + (10 * 2));
  Sdl.Rect.set_h rect (retry_h + over_h + (10 * 2));
  let bg_w = Sdl.Rect.w rect and bg_h = Sdl.Rect.h rect in

  bg_tex :=
    Some
      (sdl_get_ok
         (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bg_w ~h:bg_h
            Sdl.Texture.access_target));
  sdl_try (Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.Blend.mode_blend);

  sdl_try (Sdl.set_render_target renderer (Some (get_bg_tex ())));
  sdl_try (Sdl.set_render_draw_color renderer 0 0 0 100);
  sdl_try (Sdl.render_clear renderer);
  let orect = Sdl.Rect.create ~x:10 ~y:10 ~w:over_w ~h:over_h in
  sdl_try (Sdl.render_copy ~dst:orect renderer otex);
  let rrect =
    Sdl.Rect.create ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h
  in
  sdl_try (Sdl.render_copy ~dst:rrect renderer rtex);

  sdl_try (Sdl.set_render_target renderer None);

  Sdl.destroy_texture otex;
  Sdl.destroy_texture rtex;

  Sdl.Rect.set_w rect (bg_w * 2);
  Sdl.Rect.set_h rect (bg_h * 2);

  let w = State.Screen.logical_w
  and h = State.Screen.logical_h
  and over_w = Sdl.Rect.w rect
  and over_h = Sdl.Rect.h rect in
  orig_x := ((w / 2) - (over_w / 2));
  orig_y := ((h / 2) - over_h);
  Sdl.Rect.set_x rect !orig_x;
  Sdl.Rect.set_y rect !orig_y;

  rdr := Some renderer


let start_anim_out f =
  State.wait_for_events := false;
  let anim = Anims.create
    ~pt_start:(!orig_x)
    ~pt_end:(-1000)
    ~span:400
    ~at_update:(fun v -> Sdl.Rect.set_x rect v)
    ~at_end:(fun () -> enabled := false; f ())
    Anims.Easing.Quadratic_in in
  Anims.start anim !State.ticks

let start_anim_in f =
  State.wait_for_events := false;
  Sdl.Rect.set_y rect !orig_y;
  Sdl.Rect.set_x rect !orig_x;
  enabled := true;
  let anim = Anims.create
    ~pt_start:(-1000)
    ~pt_end:(!orig_y)
    ~span:400
    ~at_update:(fun v -> Sdl.Rect.set_y rect v)
    ~at_end:f
    Anims.Easing.Quadratic_in in
  Anims.start anim !State.ticks

let draw ~renderer =
  if !enabled then (
    sdl_try (Sdl.render_copy ~dst:rect renderer (get_bg_tex ()))
  )

let release () =
  if !bg_tex <> None then (
    Sdl.destroy_texture (get_bg_tex ());
    bg_tex := None)
