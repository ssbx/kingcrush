open Tsdl
open Tsdl_image
open Gamekit

module Background = struct

  let bg_tex : Sdl.texture option ref = ref None
  let get_tex () = match !bg_tex with Some v -> v | None -> failwith "background get_tex"
  let bg_rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0

  let init ~renderer =
    let filename =
      Filename.(concat (concat !Conf.base_dir "images") "background1.png")
    in
    let tex = sdl_get_ok (Image.load_texture renderer filename) in
    let _, _, (w, h) = sdl_get_ok (Sdl.query_texture tex) in

    let ratio = Float.of_int Conf.Display.logical_h /. Float.of_int h in
    let logic_h = Conf.Display.logical_h
    and logic_w = Float.to_int (ratio *. Float.of_int w) in
    Sdl.Rect.set_h bg_rect logic_h;
    Sdl.Rect.set_w bg_rect logic_w;
    Sdl.Rect.set_y bg_rect 0;
    (* centered horizontal *)
    let logic_x_center = Conf.Display.logical_w / 2
    and logic_bg_center = logic_w / 2 in
    Sdl.Rect.set_x bg_rect (logic_x_center - logic_bg_center);
    bg_tex := Some tex

  let release () =
    Sdl.destroy_texture (get_tex ());
    bg_tex := None

  let draw ~renderer =
    (*Sdl.Rect.set_h bg_rect rh;
      Sdl.Rect.set_w bg_rect (Int.of_float ((Float.of_int rh) *. !bg_ratio));*)
    sdl_try (Sdl.render_copy ~dst:bg_rect renderer (get_tex ()))

  let set_image _ = ()
end

module Fade = struct
  let tex : Sdl.texture option ref = ref None
  let get_tex () = match !tex with Some v -> v | None -> assert false
  let rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0
    ~w:Conf.Display.logical_w ~h:Conf.Display.logical_w

  let alpha : int ref = ref 0

  let init ~renderer =
    let texture = sdl_get_ok (Sdl.create_texture renderer
      Sdl.Pixel.format_rgba8888
      ~w:Conf.Display.logical_w ~h:Conf.Display.logical_h
           Sdl.Texture.access_target) in

    sdl_try (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
    sdl_try (Sdl.set_render_target renderer (Some texture));
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 255);
    sdl_try (Sdl.render_clear renderer);
    sdl_try (Sdl.set_render_target renderer None);
    tex := Some texture

  let release () =
    Sdl.destroy_texture (get_tex ());
    tex := None

  let fade_in f =
    alpha := 255;
    let anim = Anims.create
      ~pt_start:(255)
      ~pt_end:(0)
      ~span:1000
      ~at_update:(fun v -> alpha := v)
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let fade_out f =
    alpha := 0;
    let anim = Anims.create
      ~pt_start:(0)
      ~pt_end:(255)
      ~span:1000
      ~at_update:(fun v -> alpha := v)
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let draw ~renderer =
    let t = get_tex () in
    sdl_try (Sdl.set_texture_alpha_mod t !alpha);
    sdl_try (Sdl.render_copy ~dst:rect renderer t)
end
