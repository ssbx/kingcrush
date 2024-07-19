open CamlSDL2
open CamlSDL2_image

#include "log.cppo"

let bg_tex  : Sdl.Texture.t option ref = ref None
let bg_rect : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)

let get_tex () = match !bg_tex with Some v -> v | None -> LOG_CRASH()

let init ~renderer =
  let filename =
    Filename.(concat (concat !Info.base_dir "images") "background1.png")
  in
  let tex = Img.load_texture renderer ~filename:filename in
  let _, _, w, h = Sdl.query_texture tex in

  let ratio = Float.of_int Info.Display.logical_h /. Float.of_int h in
  let logic_h = Info.Display.logical_h
  and logic_w = Float.to_int (ratio *. Float.of_int w) in
  (* centered horizontal *)
  let logic_x_center = Info.Display.logical_w / 2
  and logic_bg_center = logic_w / 2 in

  bg_rect := Sdl.Rect.make
    ~x:(logic_x_center - logic_bg_center)
    ~y:0
    ~w:logic_w
    ~h:logic_h;
  bg_tex := Some tex

let release () =
  Sdl.destroy_texture (get_tex ());
  bg_tex := None

let draw ~renderer =
  Sdl.render_copy renderer
    ~texture:(get_tex ())
    ~srcrect:None
    ~dstrect:(Some !bg_rect)

let set_image _ = ()
