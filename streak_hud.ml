open Tsdl
open Gamekit

let rdr : Sdl.renderer option ref = ref None
let bg_tex : Sdl.texture option ref = ref None
let score_tex : Sdl.texture option ref = ref None
let rating_tex : Sdl.texture option ref = ref None
let score_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
let rating_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
let get_rdr () = match !rdr with Some v -> v | None -> assert false
let get_tex () = match !bg_tex with Some v -> v | None -> assert false

let get_score_tex () =
  match !score_tex with Some v -> v | None -> assert false

let get_rating_tex () =
  match !rating_tex with Some v -> v | None -> assert false

let generate_msg ~renderer ~score ~rating =
  if Option.is_some !score_tex then (
    Sdl.destroy_texture (get_score_tex ());
    score_tex := None);
  if Option.is_some !rating_tex then (
    Sdl.destroy_texture (get_rating_tex ());
    score_tex := None);
  let score_texture =
    let surf = Fonts.get_surface score in
    let w, h = Sdl.get_surface_size surf in
    Sdl.Rect.set_w score_rect w;
    Sdl.Rect.set_h score_rect h;
    match Sdl.create_texture_from_surface renderer surf with
    | Error (`Msg e) -> failwith e
    | Ok t ->
        Sdl.free_surface surf;
        t
  in
  let rating_texture =
    let surf = Fonts.get_surface rating in
    let w, h = Sdl.get_surface_size surf in
    Sdl.Rect.set_w rating_rect w;
    Sdl.Rect.set_h rating_rect h;
    match Sdl.create_texture_from_surface renderer surf with
    | Error (`Msg e) -> failwith e
    | Ok t ->
        Sdl.free_surface surf;
        t
  in
  score_tex := Some score_texture;
  rating_tex := Some rating_texture

let init ~renderer =
  let texture =
    sdl_get_ok
      (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:100 ~h:100
         Sdl.Texture.access_target)
  in
  sdl_try (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
  sdl_try (Sdl.set_render_target renderer (Some texture));
  sdl_try (Sdl.set_render_draw_color renderer 100 100 100 155);
  sdl_try (Sdl.render_clear renderer);
  sdl_try (Sdl.set_render_target renderer None);
  bg_tex := Some texture;
  rdr := Some renderer

let release () =
  if Option.is_some !score_tex then Sdl.destroy_texture (get_score_tex ());
  if Option.is_some !bg_tex then Sdl.destroy_texture (get_tex ());
  bg_tex := None;
  score_tex := None

let draw ~renderer =
  sdl_try (Sdl.render_copy ~dst:Info.Display.score_rect renderer (get_tex ()));

  Sdl.Rect.set_x score_rect (Sdl.Rect.x Info.Display.score_rect + 10);
  Sdl.Rect.set_y score_rect (Sdl.Rect.y Info.Display.score_rect + 10);
  sdl_try (Sdl.render_copy ~dst:score_rect renderer (get_score_tex ()));
  Sdl.Rect.set_x rating_rect (Sdl.Rect.x Info.Display.score_rect + 10);
  Sdl.Rect.set_y rating_rect
    (Sdl.Rect.y Info.Display.score_rect + 10 + Sdl.Rect.h score_rect);
  sdl_try (Sdl.render_copy ~dst:rating_rect renderer (get_rating_tex ()))

let handle_game_event = function
  | Events.NewPuzzle ->
      let rating = Printf.sprintf "rating: %s" (Streak_model.get_rating ())
      and score = Printf.sprintf "streak: %i" (Streak_model.get_streak ()) in
      generate_msg ~renderer:(get_rdr ()) ~score ~rating
  | _ -> ()
