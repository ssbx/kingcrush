open Tsdl
open Ressources
open Gamekit


open Tsdl_image
open Chess

module Squares = struct

  let blend = 240
  let black_square_color = (181, 136, 99, blend)
  let white_square_color = (240, 217, 181, blend)

  let black_square_color2 = (114, 74, 140, blend)
  let white_square_color2 = (150, 142, 178, blend)

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
    sdl_try (Sdl.render_copy ~dst:Info.Display.board_rect renderer (get_tex ()))
end


(*
module Hints = struct

  let img_texture : Sdl.texture option ref = ref None
  let img_rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let texture : Sdl.texture option ref = ref None
  let rdr : Sdl.renderer option ref = ref None

  let get_img_tex () =
    match !img_texture with Some v -> v | None -> failwith "error: board_hints get_img_tex"

  let get_tex () = match !texture with Some v -> v | None -> failwith "board_hints get_tex"
  let get_rdr () = match !rdr with Some v -> v | None -> failwith "board_hints get_rdr"
  let psize = ref 0

  let init ~renderer =
    let img = Filename.(concat (concat !Info.base_dir "images") "hint.png") in
    rdr := Some renderer;
    img_texture := Some (sdl_get_ok (Image.load_texture renderer img));
    psize := Figures.piece_width;

    let bsize = !psize * 8 in
    texture :=
      Some
        (sdl_get_ok
           (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bsize
              ~h:bsize Sdl.Texture.access_target));

    sdl_try (Sdl.set_texture_blend_mode (get_tex ()) Sdl.Blend.mode_blend);
    Sdl.Rect.set_w img_rect !psize;
    Sdl.Rect.set_h img_rect !psize;
    Sdl.Rect.set_x img_rect !psize;
    sdl_try (Sdl.set_render_target renderer !texture);
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 0);
    sdl_try (Sdl.render_clear renderer);
    sdl_try (Sdl.set_render_target renderer None)

  let release () =
    match !img_texture with
    | None -> ()
    | Some t -> (
        Sdl.destroy_texture t;
        match !texture with
        | None -> ()
        | Some t ->
            Sdl.destroy_texture t;
            img_texture := None;
            texture := None)

  let show pos from_x from_y =
    let mat =
      Chess.Move.get_move_matrix pos.board pos.active_player from_x from_y
    in
    let renderer = get_rdr () in
    let img = get_img_tex () in
    sdl_try (Sdl.set_render_target renderer !texture);
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 0);
    sdl_try (Sdl.render_clear renderer);
    Array.iteri
      (fun y l ->
        Array.iteri
          (fun x v ->
            if v then (
              Sdl.Rect.set_x img_rect (!psize * x);
              Sdl.Rect.set_y img_rect (!psize * y);
              sdl_try (Sdl.render_copy ~dst:img_rect renderer img)))
          l)
      mat;
    sdl_try (Sdl.set_render_target renderer None)

  let clear () =
    let renderer = get_rdr () in
    sdl_try (Sdl.set_render_target renderer !texture);
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 0);
    sdl_try (Sdl.render_clear renderer);
    sdl_try (Sdl.set_render_target renderer None)

  let draw ~renderer =
    sdl_try (Sdl.render_copy ~dst:Info.Display.board_rect renderer (get_tex ()))

end
*)

module Hints = struct
  let img_texture : Sdl.texture option ref = ref None
  let img_rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let texture : Sdl.texture option ref = ref None
  let rdr : Sdl.renderer option ref = ref None

  let get_img_tex () =
    match !img_texture with Some v -> v | None -> failwith "error: board_hints get_img_tex"

  let get_tex () = match !texture with Some v -> v | None -> failwith "board_hints get_tex"
  let get_rdr () = match !rdr with Some v -> v | None -> failwith "board_hints get_rdr"
  let psize = ref 0

  let init ~renderer =
    let img = Filename.(concat (concat !Info.base_dir "images") "hint.png") in
    rdr := Some renderer;
    img_texture := Some (sdl_get_ok (Image.load_texture renderer img));
    psize := Figures.piece_width;

    let bsize = !psize * 8 in
    texture :=
      Some
        (sdl_get_ok
           (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bsize
              ~h:bsize Sdl.Texture.access_target));

    sdl_try (Sdl.set_texture_blend_mode (get_tex ()) Sdl.Blend.mode_blend);
    Sdl.Rect.set_w img_rect !psize;
    Sdl.Rect.set_h img_rect !psize;
    Sdl.Rect.set_x img_rect !psize;
    sdl_try (Sdl.set_render_target renderer !texture);
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 0);
    sdl_try (Sdl.render_clear renderer);
    sdl_try (Sdl.set_render_target renderer None)

  let release () =
    match !img_texture with
    | None -> ()
    | Some t -> (
        Sdl.destroy_texture t;
        match !texture with
        | None -> ()
        | Some t ->
            Sdl.destroy_texture t;
            img_texture := None;
            texture := None)

  let show pos from_x from_y =
    let mat =
      Chess.Move.get_move_matrix pos.board pos.active_player from_x from_y
    in
    let renderer = get_rdr () in
    let img = get_img_tex () in
    sdl_try (Sdl.set_render_target renderer !texture);
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 0);
    sdl_try (Sdl.render_clear renderer);
    Array.iteri
      (fun y l ->
        Array.iteri
          (fun x v ->
            if v then (
              Sdl.Rect.set_x img_rect (!psize * x);
              Sdl.Rect.set_y img_rect (!psize * y);
              sdl_try (Sdl.render_copy ~dst:img_rect renderer img)))
          l)
      mat;
    sdl_try (Sdl.set_render_target renderer None)

  let clear () =
    let renderer = get_rdr () in
    sdl_try (Sdl.set_render_target renderer !texture);
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 0);
    sdl_try (Sdl.render_clear renderer);
    sdl_try (Sdl.set_render_target renderer None)

  let draw ~renderer =
    sdl_try (Sdl.render_copy ~dst:Info.Display.board_rect renderer (get_tex ()))

end
