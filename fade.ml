open CamlSDL2
open Gamekit

let tex : Sdl.Texture.t option ref = ref None
let get_tex () = match !tex with Some v -> v | None -> assert false
let rect : Sdl.Rect.t = Sdl.Rect.make
                          ~x:0
                          ~y:0
                          ~w:Info.Display.logical_w
                          ~h:Info.Display.logical_w

let alpha : int ref = ref 0

let init ~renderer =
  let texture = Sdl.create_texture renderer
    ~fmt:Sdl.PixelFormat.RGBA8888
    ~access:Sdl.TextureAccess.Target
    ~width:Info.Display.logical_w
    ~height:Info.Display.logical_h in

  Sdl.set_texture_blend_mode texture Sdl.BlendMode.SDL_BLENDMODE_BLEND;
  Sdl.set_render_target renderer (Some texture);
  Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:255;
  Sdl.render_clear renderer;
  Sdl.set_render_target renderer None;
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
  let texture = get_tex () in
  Sdl.set_texture_alpha_mod texture ~alpha:!alpha;
  Sdl.render_copy renderer ~texture ~srcrect:None ~dstrect:(Some rect)
