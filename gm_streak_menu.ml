open Tsdl
open Tsdl_image
open Gamekit

let button_1_rect : Sdl.rect = Sdl.Rect.create ~w:100  ~h:100 ~x:10 ~y:10
let button_2_rect : Sdl.rect = Sdl.Rect.create ~w:100  ~h:100 ~x:10 ~y:10
let button_3_rect : Sdl.rect = Sdl.Rect.create ~w:100  ~h:100 ~x:10 ~y:10
let button_4_rect : Sdl.rect = Sdl.Rect.create ~w:100  ~h:100 ~x:10 ~y:10
let button_5_rect : Sdl.rect = Sdl.Rect.create ~w:100  ~h:100 ~x:10 ~y:10
let button_6_rect : Sdl.rect = Sdl.Rect.create ~w:100  ~h:100 ~x:10 ~y:10

type menu_textures_t = {
  button : Sdl.texture;
  one : Sdl.texture;
}

let textures : menu_textures_t option ref = ref None
let get_texts () = match !textures with Some v -> v | None -> assert false

let gen_text ~renderer ~text =
  let surf = Fonts.get_surface text in
  let w, h = Sdl.get_surface_size surf in
  let tex = sdl_get_ok (Sdl.create_texture_from_surface renderer surf) in
  Sdl.free_surface surf;
  (tex, w, h)


let init ~renderer =
  let filename = Filename.concat (List.nth Data.Sites.images 0) "button.png" in
  let btext = sdl_get_ok (Image.load_texture renderer filename) in
  let (t,_,_) = (gen_text ~renderer ~text:"l1" ) in
  textures := Some {
    button = btext;
    one = t
  };
  let vspan = Game_info.Screen.logical_w / 10
  and hspan = Game_info.Screen.logical_h / 10 in
  let button_width = (Game_info.Screen.logical_w - 4 * hspan) / 3
  and button_height = (Game_info.Screen.logical_h - 3 * vspan) / 2 in
  Sdl.Rect.set_w button_1_rect button_width;
  Sdl.Rect.set_w button_2_rect button_width;
  Sdl.Rect.set_w button_3_rect button_width;
  Sdl.Rect.set_w button_4_rect button_width;
  Sdl.Rect.set_w button_5_rect button_width;
  Sdl.Rect.set_w button_6_rect button_width;
  Sdl.Rect.set_h button_1_rect button_height;
  Sdl.Rect.set_h button_2_rect button_height;
  Sdl.Rect.set_h button_3_rect button_height;
  Sdl.Rect.set_h button_4_rect button_height;
  Sdl.Rect.set_h button_5_rect button_height;
  Sdl.Rect.set_h button_6_rect button_height;

  (* first line *)
  let x_pos = hspan
  and y_pos = vspan in
  Sdl.Rect.set_y button_1_rect y_pos;
  Sdl.Rect.set_y button_2_rect y_pos;
  Sdl.Rect.set_y button_3_rect y_pos;
  Sdl.Rect.set_x button_1_rect x_pos;
  Sdl.Rect.set_x button_2_rect (x_pos + hspan + button_width);
  Sdl.Rect.set_x button_3_rect (x_pos + 2 * hspan + 2 * button_width);

  (* second line *)
  Sdl.Rect.set_y button_4_rect (y_pos + vspan + button_height);
  Sdl.Rect.set_y button_5_rect (y_pos + vspan + button_height);
  Sdl.Rect.set_y button_6_rect (y_pos + vspan + button_height);
  Sdl.Rect.set_x button_4_rect x_pos;
  Sdl.Rect.set_x button_5_rect (x_pos + hspan + button_width);
  Sdl.Rect.set_x button_6_rect (x_pos + 2 * hspan + 2 * button_width)

let release () =
  let ts = get_texts () in
  Sdl.destroy_texture ts.button;
  Sdl.destroy_texture ts.one

let draw ~renderer =
  let ts = get_texts () in
  sdl_try (Sdl.render_copy ~dst:button_1_rect renderer ts.button);
  sdl_try (Sdl.render_copy ~dst:button_1_rect renderer ts.one);

  sdl_try (Sdl.render_copy ~dst:button_2_rect renderer ts.button);
  sdl_try (Sdl.render_copy ~dst:button_2_rect renderer ts.one);

  sdl_try (Sdl.render_copy ~dst:button_3_rect renderer ts.button);
  sdl_try (Sdl.render_copy ~dst:button_3_rect renderer ts.one);

  sdl_try (Sdl.render_copy ~dst:button_4_rect renderer ts.button);
  sdl_try (Sdl.render_copy ~dst:button_4_rect renderer ts.one);

  sdl_try (Sdl.render_copy ~dst:button_5_rect renderer ts.button);
  sdl_try (Sdl.render_copy ~dst:button_5_rect renderer ts.one);

  sdl_try (Sdl.render_copy ~dst:button_6_rect renderer ts.button);
  sdl_try (Sdl.render_copy ~dst:button_6_rect renderer ts.one)

