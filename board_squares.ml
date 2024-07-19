open CamlSDL2

let blend = 240
let black_square_color = 181, 136, 99, blend
let white_square_color = 240, 217, 181, blend
let black_square_color2 = 114, 74, 140, blend
let white_square_color2 = 150, 142, 178, blend
let board_tex : Sdl.Texture.t option ref = ref None

let get_tex () =
  match !board_tex with
  | Some v -> v
  | None -> assert false
;;

let init ~renderer =
  let psize = Figures.piece_width in
  let bsize = psize * 8 in
  let texture =
    Sdl.create_texture
      renderer
      ~fmt:Sdl.PixelFormat.RGBA8888
      ~access:Sdl.TextureAccess.Target
      ~width:bsize
      ~height:bsize
  and rectangle = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:psize ~h:psize)
  and br, bg, bb, ba = black_square_color
  and wr, wg, wb, wa = white_square_color in
  Sdl.set_texture_blend_mode texture Sdl.BlendMode.SDL_BLENDMODE_BLEND;
  Sdl.set_render_target renderer (Some texture);
  Sdl.set_render_draw_color renderer ~r:br ~g:bg ~b:bb ~a:ba;
  Sdl.render_clear renderer;
  Sdl.set_render_draw_color renderer ~r:wr ~g:wg ~b:wb ~a:wa;
  List.iter
    (fun x ->
      rectangle := { !rectangle with x = x * psize };
      let flip = x mod 2 in
      List.iter
        (fun y ->
          rectangle := { !rectangle with y = (y + flip) * psize };
          Sdl.render_fill_rect renderer !rectangle)
        [ 0; 2; 4; 6 ])
    [ 0; 1; 2; 3; 4; 5; 6; 7 ];
  Sdl.set_render_target renderer None;
  board_tex := Some texture
;;

let release () =
  Sdl.destroy_texture (get_tex ());
  board_tex := None
;;

let draw ~renderer =
  Sdl.render_copy
    renderer
    ~texture:(get_tex ())
    ~srcrect:None
    ~dstrect:(Some Info.Display.board_rect)
;;
