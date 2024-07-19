open CamlSDL2
open Gamekit
open Chesslib

let gen_text ~renderer ~text =
  let surf = Fonts.get_surface text in
  let w = Sdl.get_surface_width surf in
  let h = Sdl.get_surface_height surf in
  let tex = Sdl.create_texture_from_surface renderer surf in
  Sdl.free_surface surf;
  tex, w, h
;;

let rect = ref (Sdl.Rect.make ~w:0 ~h:0 ~x:0 ~y:0)
let tex : Sdl.Texture.t option ref = ref None

let get_tex () =
  match !tex with
  | Some v -> v
  | None -> assert false
;;

let hl_rect = ref (Sdl.Rect.make ~w:0 ~h:0 ~x:0 ~y:0)
let hl_tex : Sdl.Texture.t option ref = ref None

let get_hl_tex () =
  match !hl_tex with
  | Some v -> v
  | None -> assert false
;;

type problem =
  { descr : string
  ; theme : Puzzles.theme_t
  ; rank : int
  }

let problems : problem list =
  [ { descr = "Mate in one"; theme = Puzzles.Theme "mateIn1"; rank = 1000 }
  ; { descr = "Mate in two"; theme = Puzzles.Theme "mateIn2"; rank = 1000 }
  ; { descr = "Mate in three"; theme = Puzzles.Theme "mateIn3"; rank = 1000 }
  ; { descr = "Pawn endgames"; theme = Puzzles.Theme "pawnEndgame"; rank = 1000 }
  ; { descr = "Rook endgames"; theme = Puzzles.Theme "rookEndgame"; rank = 1000 }
  ; { descr = "Back rank mate"; theme = Puzzles.Theme "backRankMate"; rank = 1000 }
  ; { descr = "Any"; theme = Puzzles.AnyTheme; rank = 1000 }
  ]
;;

let mouse_areas : (int * int * (Puzzles.theme_t * int)) list ref = ref []
let span = 100
let span2 = 20
let list_width : int ref = ref 0
let list_height : int ref = ref 0
let hl_enabled : bool ref = ref false
let hl_y : int ref = ref 0
let hl_h : int ref = ref 0

let update_hl mx my =
  List.exists
    (fun (x, y, (theme, rank)) ->
      if my > y && my < y + !hl_h && mx > x && mx < x + !list_width
      then (
        hl_rect := { !hl_rect with y };
        Info.streak_theme := theme;
        Info.streak_rank := rank;
        true)
      else false)
    !mouse_areas
;;

let init ~renderer =
  list_width := Info.Display.logical_w - (2 * span);
  list_height := Info.Display.logical_h - (2 * span);
  let t =
    Sdl.create_texture
      renderer
      ~fmt:Sdl.PixelFormat.RGBA8888
      ~access:Sdl.TextureAccess.Target
      ~width:!list_width
      ~height:!list_height
  in
  Sdl.set_texture_blend_mode t Sdl.BlendMode.SDL_BLENDMODE_BLEND;
  rect := Sdl.Rect.make ~x:span ~y:span ~w:!list_width ~h:!list_height;
  Sdl.set_render_target renderer (Some t);
  Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:50;
  Sdl.render_clear renderer;
  let mate1, _, th = gen_text ~renderer ~text:"Test" in
  hl_h := th;
  Sdl.destroy_texture mate1;
  let hl =
    Sdl.create_texture
      renderer
      ~fmt:Sdl.PixelFormat.RGBA8888
      ~access:Sdl.TextureAccess.Target
      ~width:!list_width
      ~height:th
  in
  hl_rect := Sdl.Rect.make ~x:span ~y:(span + span2) ~w:!list_width ~h:th;
  Sdl.set_texture_blend_mode hl Sdl.BlendMode.SDL_BLENDMODE_BLEND;
  Sdl.set_render_target renderer (Some hl);
  Sdl.set_render_draw_color renderer ~r:255 ~g:0 ~b:0 ~a:50;
  Sdl.render_clear renderer;
  Sdl.set_render_target renderer (Some t);
  let txt_rect = ref (Sdl.Rect.make ~w:!list_width ~h:th ~x:span2 ~y:span2) in
  let rec draw_txt = function
    | [] -> ()
    | head :: tail ->
      let txt, w, _ = gen_text ~renderer ~text:head.descr in
      txt_rect := { !txt_rect with w };
      Sdl.render_copy renderer ~texture:txt ~srcrect:None ~dstrect:(Some !txt_rect);
      Sdl.destroy_texture txt;
      (* set click area *)
      let scr_x = span
      and scr_y = span + !txt_rect.y
      and game_conf = head.theme, head.rank in
      mouse_areas := (scr_x, scr_y, game_conf) :: !mouse_areas;
      let y1 = !txt_rect.y in
      txt_rect := { !txt_rect with y = y1 + th };
      draw_txt tail
  in
  draw_txt problems;
  Sdl.set_render_target renderer None;
  tex := Some t;
  hl_tex := Some hl
;;

let release () = Sdl.destroy_texture (get_tex ())

let handle_sdl_button_down (evt : Sdl.MouseButtonEvent.t) =
  if evt.mb_button = 1 then update_hl evt.mb_x evt.mb_y else false
;;

let handle_sdl_event = function
  | Sdl.Event.SDL_MOUSEMOTION mm -> hl_enabled := update_hl mm.mm_x mm.mm_y
  | _ -> ()
;;

let draw ~renderer =
  Sdl.render_copy renderer ~texture:(get_tex ()) ~srcrect:None ~dstrect:(Some !rect);
  if !hl_enabled
  then
    Sdl.render_copy
      renderer
      ~texture:(get_hl_tex ())
      ~srcrect:None
      ~dstrect:(Some !hl_rect)
;;
