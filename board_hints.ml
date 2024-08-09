open CamlSDL2
open CamlSDL2_image
open Chesslib

let img_texture : Sdl.Texture.t option ref = ref None
let img_rect : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
let texture : Sdl.Texture.t option ref = ref None
let rdr : Sdl.Renderer.t option ref = ref None

let get_img_tex () =
  match !img_texture with
  | Some v -> v
  | None -> failwith "get_img_text"
;;

let get_tex () =
  match !texture with
  | Some v -> v
  | None -> failwith "get_tex"
;;

let get_rdr () =
  match !rdr with
  | Some v -> v
  | None -> failwith "get_rdr"
;;

let psize = ref 0

let init ~renderer =
  let img = Filename.(concat (concat !Info.base_dir "images") "hint.png") in
  rdr := Some renderer;
  img_texture := Some (Img.load_texture renderer ~filename:img);
  psize := Figures.piece_width;
  let bsize = !psize * 8 in
  texture
  := Some
       (Sdl.create_texture
          renderer
          ~fmt:Sdl.PixelFormat.RGBA8888
          ~access:Sdl.TextureAccess.Target
          ~width:bsize
          ~height:bsize);
  Sdl.set_texture_blend_mode (get_tex ()) Sdl.BlendMode.BLEND;
  img_rect := { !img_rect with w = !psize; h = !psize; x = !psize };
  Sdl.set_render_target renderer !texture;
  Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:0;
  Sdl.render_clear renderer;
  Sdl.set_render_target renderer None
;;

let release () =
  match !img_texture with
  | None -> ()
  | Some t ->
    Sdl.destroy_texture t;
    (match !texture with
     | None -> ()
     | Some t ->
       Sdl.destroy_texture t;
       img_texture := None;
       texture := None)
;;

let show pos from_x from_y =
  let mat = Chesslib.Move.get_move_matrix pos.board pos.active_player from_x from_y in
  let renderer = get_rdr () in
  let img = get_img_tex () in
  Sdl.set_render_target renderer !texture;
  Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:0;
  Sdl.render_clear renderer;
  Array.iteri
    (fun y l ->
      Array.iteri
        (fun x v ->
          if v
          then (
            img_rect := { !img_rect with x = !psize * x; y = !psize * y };
            Sdl.render_copy renderer ~texture:img ~srcrect:None ~dstrect:(Some !img_rect)))
        l)
    mat;
  Sdl.set_render_target renderer None
;;

let clear () =
  let renderer = get_rdr () in
  Sdl.set_render_target renderer !texture;
  Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:0;
  Sdl.render_clear renderer;
  Sdl.set_render_target renderer None
;;

let draw ~renderer =
  Sdl.render_copy
    renderer
    ~texture:(get_tex ())
    ~srcrect:None
    ~dstrect:(Some Info.Display.board_rect)
;;
