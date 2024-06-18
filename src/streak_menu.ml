open Tsdl
open Gamekit
open Chess

let gen_text ~renderer ~text =
  let surf = Fonts.get_surface text in
  let w, h = Sdl.get_surface_size surf in
  let tex = sdl_get_ok (Sdl.create_texture_from_surface renderer surf) in
  Sdl.free_surface surf;
  (tex, w, h)

let rect = Sdl.Rect.create ~w:0 ~h:0 ~x:0 ~y:0
let tex : Sdl.texture option ref = ref None
let get_tex () = match !tex with Some v -> v | None -> assert false

let hl_rect = Sdl.Rect.create ~w:0 ~h:0 ~x:0 ~y:0
let hl_tex : Sdl.texture option ref = ref None
let get_hl_tex () = match !hl_tex with Some v -> v | None -> assert false

type problem = {
  descr : string;
  theme : Puzzles.theme_t;
  rank : int}

let problems : problem list = [
  {descr = "Mate in one"; theme = Puzzles.Theme "mateIn1" ; rank = 1000; };
  {descr = "Mate in two"; theme = Puzzles.Theme "mateIn2" ; rank = 1000; };
  {descr = "Mate in three"; theme = Puzzles.Theme "mateIn3" ; rank = 1000; };
  {descr = "Pawn endgames"; theme = Puzzles.Theme "pawnEndgame" ; rank = 1000; };
  {descr = "Rook endgames"; theme = Puzzles.Theme "rookEndgame" ; rank = 1000; };
  {descr = "Back rank mate"; theme = Puzzles.Theme "backRankMate" ; rank = 1000; };
  {descr = "Any"; theme = Puzzles.AnyTheme; rank = 1000; }
]

let mouse_areas : (int * int * (Puzzles.theme_t * int)) list ref = ref []
let span = 100
let span2 = 20

let list_width : int ref = ref 0
let list_height : int ref = ref 0
let hl_enabled : bool ref = ref false
let hl_y       : int ref = ref 0
let hl_h  : int ref = ref 0

let update_hl mx my =
  List.exists (fun (x,y, (theme,rank)) ->
    if my > y && my < (y + !hl_h) && mx > x && mx < (x + !list_width) then (
      Sdl.Rect.set_y hl_rect y;
      Info.streak_theme := theme;
      Info.streak_rank := rank;
      true
    ) else (
      false
    )
  ) !mouse_areas


let init ~renderer =
  list_width  := Info.Screen.logical_w - (2 * span);
  list_height := Info.Screen.logical_h - (2 * span);
  let t =
    (sdl_get_ok
      (Sdl.create_texture renderer
        Sdl.Pixel.format_rgba8888
            ~w:!list_width
            ~h:!list_height
            Sdl.Texture.access_target)) in

  sdl_try (Sdl.set_texture_blend_mode t Sdl.Blend.mode_blend);
  Sdl.Rect.set_x rect span;
  Sdl.Rect.set_y rect span;
  Sdl.Rect.set_w rect !list_width;
  Sdl.Rect.set_h rect !list_height;
  sdl_try (Sdl.set_render_target renderer (Some t));
  sdl_try (Sdl.set_render_draw_color renderer 0 0 0 50);
  sdl_try (Sdl.render_clear renderer);


  let mate1, _, th = gen_text ~renderer ~text:"Test" in
  hl_h := th;
  Sdl.destroy_texture mate1;
  let hl =
    (sdl_get_ok
      (Sdl.create_texture renderer
        Sdl.Pixel.format_rgba8888
            ~w:!list_width
            ~h:th
            Sdl.Texture.access_target)) in

  Sdl.Rect.set_x hl_rect span;
  Sdl.Rect.set_y hl_rect (span + span2);
  Sdl.Rect.set_w hl_rect !list_width;
  Sdl.Rect.set_h hl_rect th;

  sdl_try (Sdl.set_texture_blend_mode hl Sdl.Blend.mode_blend);
  sdl_try (Sdl.set_render_target renderer (Some hl));
  sdl_try (Sdl.set_render_draw_color renderer 255 0 0 50);
  sdl_try (Sdl.render_clear renderer);


  sdl_try (Sdl.set_render_target renderer (Some t));
  let txt_rect = Sdl.Rect.create ~w:!list_width ~h:th ~x:span2 ~y:span2 in
  let rec draw_txt = function
    | [] -> ()
    | head :: tail ->
      let txt , w, _ = gen_text ~renderer ~text:head.descr in
      Sdl.Rect.set_w txt_rect w;
      sdl_try (Sdl.render_copy ~dst:txt_rect renderer txt);
      Sdl.destroy_texture txt;
      (* set click area *)
      let scr_x = span
      and scr_y = (span + (Sdl.Rect.y txt_rect))
      and game_conf = (head.theme, head.rank) in
      mouse_areas := (scr_x,scr_y, game_conf) :: !mouse_areas;
      let y1 = Sdl.Rect.y txt_rect in
      Sdl.Rect.set_y txt_rect (y1 + th);
      draw_txt tail
  in
  draw_txt problems;

  sdl_try (Sdl.set_render_target renderer None);
  tex := Some t;
  hl_tex := Some hl

let release () =
  Sdl.destroy_texture (get_tex ())

let handle_sdl_button_down event =
  if Sdl.Event.(get event mouse_button_button) = 1 then (
    let x = Sdl.Event.(get event mouse_button_x)
    and y = Sdl.Event.(get event mouse_button_y) in
    update_hl x y
  ) else (
    false
  )


let handle_sdl_event event =
  match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
  | `Mouse_motion ->
    let cursor_x = Sdl.Event.(get event mouse_motion_x)
    and cursor_y = Sdl.Event.(get event mouse_motion_y) in
    hl_enabled := update_hl cursor_x cursor_y
  | _ -> ()

let draw ~renderer =
  sdl_try (Sdl.render_copy ~dst:rect renderer (get_tex ()));
  if !hl_enabled then
    sdl_try (Sdl.render_copy ~dst:hl_rect renderer (get_hl_tex ()));
