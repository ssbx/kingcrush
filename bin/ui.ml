open Tsdl
open Gamekit
open Ressources
open Chess

module Level_over = struct

  let rdr : Sdl.renderer option ref = ref None
  let bg_tex : Sdl.texture option ref = ref None
  let rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w, h = Sdl.get_surface_size surf in
    let tex = sdl_get_ok (Sdl.create_texture_from_surface renderer surf) in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"LEVEL COMPLETE BRAVO!!!" in

    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"..." in

    Sdl.Rect.set_w rect (Stdlib.max retry_w over_w + (10 * 2));
    Sdl.Rect.set_h rect (retry_h + over_h + (10 * 2));
    let bg_w = Sdl.Rect.w rect and bg_h = Sdl.Rect.h rect in

    bg_tex :=
      Some
        (sdl_get_ok
           (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bg_w ~h:bg_h
              Sdl.Texture.access_target));
    sdl_try (Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.Blend.mode_blend);

    sdl_try (Sdl.set_render_target renderer (Some (get_bg_tex ())));
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 100);
    sdl_try (Sdl.render_clear renderer);
    let orect = Sdl.Rect.create ~x:10 ~y:10 ~w:over_w ~h:over_h in
    sdl_try (Sdl.render_copy ~dst:orect renderer otex);
    let rrect =
      Sdl.Rect.create ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h
    in
    sdl_try (Sdl.render_copy ~dst:rrect renderer rtex);

    sdl_try (Sdl.set_render_target renderer None);

    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    Sdl.Rect.set_w rect (bg_w * 2);
    Sdl.Rect.set_h rect (bg_h * 2);

    let w = Conf.Display.logical_w
    and h = Conf.Display.logical_h
    and over_w = Sdl.Rect.w rect
    and over_h = Sdl.Rect.h rect in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    Sdl.Rect.set_x rect !orig_x;
    Sdl.Rect.set_y rect !orig_y;

    rdr := Some renderer


  let start_anim_out f =
    Conf.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_x rect v)
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    Conf.wait_for_events := false;
    Sdl.Rect.set_y rect !orig_y;
    Sdl.Rect.set_x rect !orig_x;
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_y rect v)
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim


  let draw ~renderer =
    sdl_try (Sdl.render_copy ~dst:rect renderer (get_bg_tex ()))

  let release () =
    if Option.is_some !bg_tex then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None)
end


module Level_info = struct
  let rdr : Sdl.renderer option ref = ref None
  let bg_tex : Sdl.texture option ref = ref None
  let rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w, h = Sdl.get_surface_size surf in
    let tex = sdl_get_ok (Sdl.create_texture_from_surface renderer surf) in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"LEVEL INFO" in

    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"Completion status ..." in

    Sdl.Rect.set_w rect (Stdlib.max retry_w over_w + (10 * 2));
    Sdl.Rect.set_h rect (retry_h + over_h + (10 * 2));
    let bg_w = Sdl.Rect.w rect and bg_h = Sdl.Rect.h rect in

    bg_tex :=
      Some
        (sdl_get_ok
           (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bg_w ~h:bg_h
              Sdl.Texture.access_target));
    sdl_try (Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.Blend.mode_blend);

    sdl_try (Sdl.set_render_target renderer (Some (get_bg_tex ())));
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 100);
    sdl_try (Sdl.render_clear renderer);
    let orect = Sdl.Rect.create ~x:10 ~y:10 ~w:over_w ~h:over_h in
    sdl_try (Sdl.render_copy ~dst:orect renderer otex);
    let rrect =
      Sdl.Rect.create ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h
    in
    sdl_try (Sdl.render_copy ~dst:rrect renderer rtex);

    sdl_try (Sdl.set_render_target renderer None);

    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    Sdl.Rect.set_w rect (bg_w * 2);
    Sdl.Rect.set_h rect (bg_h * 2);

    let w = Conf.Display.logical_w
    and h = Conf.Display.logical_h
    and over_w = Sdl.Rect.w rect
    and over_h = Sdl.Rect.h rect in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    Sdl.Rect.set_x rect !orig_x;
    Sdl.Rect.set_y rect !orig_y;

    rdr := Some renderer


  let start_anim_out f =
    Conf.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_x rect v)
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    Conf.wait_for_events := false;
    Sdl.Rect.set_y rect !orig_y;
    Sdl.Rect.set_x rect !orig_x;
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_y rect v)
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim


  let draw ~renderer =
    sdl_try (Sdl.render_copy ~dst:rect renderer (get_bg_tex ()))

  let release () =
    if Option.is_some !bg_tex then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None)
end


module Level_details = struct
  let rdr : Sdl.renderer option ref = ref None
  let bg_tex : Sdl.texture option ref = ref None
  let rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w, h = Sdl.get_surface_size surf in
    let tex = sdl_get_ok (Sdl.create_texture_from_surface renderer surf) in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"SCORE BOARD" in

    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"Click to continue ..." in

    Sdl.Rect.set_w rect (Stdlib.max retry_w over_w + (10 * 2));
    Sdl.Rect.set_h rect (retry_h + over_h + (10 * 2));
    let bg_w = Sdl.Rect.w rect and bg_h = Sdl.Rect.h rect in

    bg_tex :=
      Some
        (sdl_get_ok
           (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bg_w ~h:bg_h
              Sdl.Texture.access_target));
    sdl_try (Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.Blend.mode_blend);

    sdl_try (Sdl.set_render_target renderer (Some (get_bg_tex ())));
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 100);
    sdl_try (Sdl.render_clear renderer);
    let orect = Sdl.Rect.create ~x:10 ~y:10 ~w:over_w ~h:over_h in
    sdl_try (Sdl.render_copy ~dst:orect renderer otex);
    let rrect =
      Sdl.Rect.create ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h
    in
    sdl_try (Sdl.render_copy ~dst:rrect renderer rtex);

    sdl_try (Sdl.set_render_target renderer None);

    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    Sdl.Rect.set_w rect (bg_w * 2);
    Sdl.Rect.set_h rect (bg_h * 2);

    let w = Conf.Display.logical_w
    and h = Conf.Display.logical_h
    and over_w = Sdl.Rect.w rect
    and over_h = Sdl.Rect.h rect in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    Sdl.Rect.set_x rect !orig_x;
    Sdl.Rect.set_y rect !orig_y;

    rdr := Some renderer


  let start_anim_out f =
    Conf.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_x rect v)
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    Conf.wait_for_events := false;
    Sdl.Rect.set_y rect !orig_y;
    Sdl.Rect.set_x rect !orig_x;
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_y rect v)
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let draw ~renderer =
    sdl_try (Sdl.render_copy ~dst:rect renderer (get_bg_tex ()))

  let release () =
    if Option.is_some !bg_tex then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None)
end


module Level_confirm = struct
  let rdr : Sdl.renderer option ref = ref None
  let bg_tex : Sdl.texture option ref = ref None
  let rect : Sdl.rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let enabled : bool ref = ref false

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w, h = Sdl.get_surface_size surf in
    let tex = sdl_get_ok (Sdl.create_texture_from_surface renderer surf) in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"Ready to play this: select" in

    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"..." in

    Sdl.Rect.set_w rect (Stdlib.max retry_w over_w + (10 * 2));
    Sdl.Rect.set_h rect (retry_h + over_h + (10 * 2));
    let bg_w = Sdl.Rect.w rect and bg_h = Sdl.Rect.h rect in

    bg_tex :=
      Some
        (sdl_get_ok
           (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:bg_w ~h:bg_h
              Sdl.Texture.access_target));
    sdl_try (Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.Blend.mode_blend);

    sdl_try (Sdl.set_render_target renderer (Some (get_bg_tex ())));
    sdl_try (Sdl.set_render_draw_color renderer 0 0 0 100);
    sdl_try (Sdl.render_clear renderer);
    let orect = Sdl.Rect.create ~x:10 ~y:10 ~w:over_w ~h:over_h in
    sdl_try (Sdl.render_copy ~dst:orect renderer otex);
    let rrect =
      Sdl.Rect.create ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h
    in
    sdl_try (Sdl.render_copy ~dst:rrect renderer rtex);

    sdl_try (Sdl.set_render_target renderer None);

    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    Sdl.Rect.set_w rect (bg_w * 2);
    Sdl.Rect.set_h rect (bg_h * 2);

    let w = Conf.Display.logical_w
    and h = Conf.Display.logical_h
    and over_w = Sdl.Rect.w rect
    and over_h = Sdl.Rect.h rect in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    Sdl.Rect.set_x rect !orig_x;
    Sdl.Rect.set_y rect !orig_y;

    rdr := Some renderer


  let start_anim_out f =
    Conf.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_x rect v)
      ~at_end:(fun () -> enabled := false; f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    enabled := true;
    Conf.wait_for_events := false;
    Sdl.Rect.set_y rect !orig_y;
    Sdl.Rect.set_x rect !orig_x;
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> Sdl.Rect.set_y rect v)
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim


  let draw ~renderer =
    if !enabled then (
      sdl_try (Sdl.render_copy ~dst:rect renderer (get_bg_tex ()))
    )

  let release () =
    if !bg_tex <> None then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None)
end


module Menu = struct

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
        Conf.streak_theme := theme;
        Conf.streak_rank := rank;
        true
      ) else (
        false
      )
    ) !mouse_areas


  let init ~renderer =
    list_width  := Conf.Display.logical_w - (2 * span);
    list_height := Conf.Display.logical_h - (2 * span);
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
end

