open Tsdl
open Gamekit

let tex : Sdl.texture option ref = ref None
let get_tex () = match !tex with Some v -> v | None -> assert false
let rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0
  ~w:Game_info.Screen.logical_w ~h:Game_info.Screen.logical_w

let alpha : int ref = ref 0
let enabled : bool ref = ref false

let init ~renderer =
  let texture = sdl_get_ok (Sdl.create_texture renderer
    Sdl.Pixel.format_rgba8888
    ~w:Game_info.Screen.logical_w ~h:Game_info.Screen.logical_h
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
  let anim = Anims.create
    ~pt_start:255
    ~pt_end:0
    ~span:3000
    ~at_update:(fun v -> alpha := v)
    ~at_end:(fun () -> enabled := false; f ())
    Anims.Easing.Quadratic_in in
  Anims.start anim


let fade_out f =
  enabled := true;
  let anim = Anims.create
    ~pt_start:(0)
    ~pt_end:(255)
    ~span:400
    ~at_update:(fun v -> alpha := v)
    ~at_end:(fun () -> f ())
    Anims.Easing.Quadratic_in in
  Anims.start anim


let draw ~renderer =
  if !enabled then (
    let t = get_tex () in
    sdl_try (Sdl.set_texture_alpha_mod t !alpha);
    sdl_try (Sdl.render_copy ~dst:rect renderer t)
  )
