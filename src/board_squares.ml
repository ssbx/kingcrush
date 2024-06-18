open Tsdl
open Gamekit

(*
let black_square_color = (114, 74, 140, 255)
let white_square_color = (150, 142, 178, 255)
*)

let blend = 255
let black_square_color = (181, 136, 99, blend)
let white_square_color = (240, 217, 181, blend)
let board_tex : Sdl.texture option ref = ref None
let get_tex () = match !board_tex with Some v -> v | None -> assert false

let init ~renderer =
  let psize = Figures.piece_width in
  let bsize = psize * 8 in
  let texture =
    sdl_get_ok
      (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bsize ~h:bsize
         Sdl.Texture.access_target)
  and rectangle = Sdl.Rect.create ~x:0 ~y:0 ~w:psize ~h:psize
  and br, bg, bb, ba = black_square_color
  and wr, wg, wb, wa = white_square_color in
  sdl_try (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
  sdl_try (Sdl.set_render_target renderer (Some texture));
  sdl_try (Sdl.set_render_draw_color renderer br bg bb ba);
  sdl_try (Sdl.render_clear renderer);

  sdl_try (Sdl.set_render_draw_color renderer wr wg wb wa);
  List.iter
    (fun x ->
      Sdl.Rect.set_x rectangle (x * psize);
      let flip = x mod 2 in
      List.iter
        (fun y ->
          Sdl.Rect.set_y rectangle ((y + flip) * psize);
          sdl_try (Sdl.render_fill_rect renderer (Some rectangle)))
        [ 0; 2; 4; 6 ])
    [ 0; 1; 2; 3; 4; 5; 6; 7 ];

  sdl_try (Sdl.set_render_target renderer None);
  board_tex := Some texture

let release () =
  Sdl.destroy_texture (get_tex ());
  board_tex := None

let draw ~renderer =
  sdl_try (Sdl.render_copy ~dst:Info.Screen.board_rect renderer (get_tex ()))
