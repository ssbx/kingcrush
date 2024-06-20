open Tsdl
open Tsdl_image
open Gamekit

#include "log.cppo"

let bg_tex : Sdl.texture option ref = ref None
let get_tex () = match !bg_tex with Some v -> v | None -> LOG_CRASH()
let bg_rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0

let init ~renderer =
  let filename =
    Filename.(concat (concat !Info.data_dir "images") "background1.png")
  in
  let tex = sdl_get_ok (Image.load_texture renderer filename) in
  let _, _, (w, h) = sdl_get_ok (Sdl.query_texture tex) in

  let ratio = Float.of_int Info.Display.logical_h /. Float.of_int h in
  let logic_h = Info.Display.logical_h
  and logic_w = Float.to_int (ratio *. Float.of_int w) in
  Sdl.Rect.set_h bg_rect logic_h;
  Sdl.Rect.set_w bg_rect logic_w;
  Sdl.Rect.set_y bg_rect 0;
  (* centered horizontal *)
  let logic_x_center = Info.Display.logical_w / 2
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
