open CamlSDL2
open Gamekit

let rdr : Sdl.Renderer.t option ref = ref None
let bg_tex : Sdl.Texture.t option ref = ref None
let score_tex : Sdl.Texture.t option ref = ref None
let rating_tex : Sdl.Texture.t option ref = ref None
let score_rect = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
let rating_rect = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)

let get_rdr () =
  match !rdr with
  | Some v -> v
  | None -> assert false
;;

let get_tex () =
  match !bg_tex with
  | Some v -> v
  | None -> assert false
;;

let get_score_tex () =
  match !score_tex with
  | Some v -> v
  | None -> assert false
;;

let get_rating_tex () =
  match !rating_tex with
  | Some v -> v
  | None -> assert false
;;

let generate_msg ~renderer ~score ~rating =
  if Option.is_some !score_tex
  then (
    Sdl.destroy_texture (get_score_tex ());
    score_tex := None);
  if Option.is_some !rating_tex
  then (
    Sdl.destroy_texture (get_rating_tex ());
    score_tex := None);
  (* bizare *)
  let score_texture =
    let surf = Fonts.get_surface score in
    let w = Sdl.get_surface_width surf in
    let h = Sdl.get_surface_height surf in
    score_rect := { !score_rect with w; h };
    let t = Sdl.create_texture_from_surface renderer surf in
    Sdl.free_surface surf;
    t
  in
  let rating_texture =
    let surf = Fonts.get_surface rating in
    let w = Sdl.get_surface_width surf in
    let h = Sdl.get_surface_height surf in
    rating_rect := { !rating_rect with w; h };
    let t = Sdl.create_texture_from_surface renderer surf in
    Sdl.free_surface surf;
    t
  in
  score_tex := Some score_texture;
  rating_tex := Some rating_texture
;;

let init ~renderer =
  let texture =
    Sdl.create_texture
      renderer
      ~fmt:Sdl.PixelFormat.RGBA8888
      ~access:Sdl.TextureAccess.Target
      ~width:100
      ~height:100
  in
  Sdl.set_texture_blend_mode texture Sdl.BlendMode.BLEND;
  Sdl.set_render_target renderer (Some texture);
  Sdl.set_render_draw_color renderer ~r:100 ~g:100 ~b:100 ~a:155;
  Sdl.render_clear renderer;
  Sdl.set_render_target renderer None;
  bg_tex := Some texture;
  rdr := Some renderer
;;

let release () =
  if Option.is_some !score_tex then Sdl.destroy_texture (get_score_tex ());
  if Option.is_some !bg_tex then Sdl.destroy_texture (get_tex ());
  bg_tex := None;
  score_tex := None
;;

let draw ~renderer =
  Sdl.render_copy
    renderer
    ~texture:(get_tex ())
    ~srcrect:None
    ~dstrect:(Some Info.Display.score_rect);
  score_rect
  := { !score_rect with
       x = Info.Display.score_rect.x + 10
     ; y = Info.Display.score_rect.y + 10
     };
  Sdl.render_copy
    renderer
    ~texture:(get_score_tex ())
    ~srcrect:None
    ~dstrect:(Some !score_rect);
  rating_rect
  := { !rating_rect with
       x = Info.Display.score_rect.x + 10
     ; y = Info.Display.score_rect.y + 10 + !score_rect.h
     };
  Sdl.render_copy
    renderer
    ~texture:(get_rating_tex ())
    ~srcrect:None
    ~dstrect:(Some !rating_rect)
;;

let handle_game_event = function
  | Events.NewPuzzle ->
    let rating = Printf.sprintf "rating: %s" (Streak_model.get_rating ())
    and score = Printf.sprintf "streak: %i" (Streak_model.get_streak ()) in
    generate_msg ~renderer:(get_rdr ()) ~score ~rating
  | _ -> ()
;;
