open CamlSDL2
open Gamekit

module Level_over = struct

  let rdr : Sdl.Renderer.t option ref = ref None
  let bg_tex : Sdl.Texture.t option ref = ref None
  let rect : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w = Sdl.get_surface_width surf
    and h = Sdl.get_surface_height surf in
    let tex = Sdl.create_texture_from_surface renderer surf in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"LEVEL COMPLETE BRAVO!!!" in

    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"..." in

    rect := {!rect with
      w = (Stdlib.max retry_w over_w + (10 * 2));
      h = (retry_h + over_h + (10 * 2)) };

    let bg_w = !rect.w and bg_h = !rect.h in

    bg_tex := Some (
      Sdl.create_texture renderer
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:bg_w
        ~height:bg_h);
    Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.BlendMode.SDL_BLENDMODE_BLEND;

    Sdl.set_render_target renderer (Some (get_bg_tex ()));
    Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:100;
    Sdl.render_clear renderer;
    let orect = Sdl.Rect.make ~x:10 ~y:10 ~w:over_w ~h:over_h in
    Sdl.render_copy renderer ~texture:otex ~srcrect:None ~dstrect:(Some orect);
    let rrect = Sdl.Rect.make
      ~x:10
      ~y:((10 * 2) + over_h)
      ~w:retry_w
      ~h:retry_h
    in
    Sdl.render_copy renderer ~texture:rtex ~srcrect:None ~dstrect:(Some rrect);
    Sdl.set_render_target renderer None;
    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    rect := {!rect with
      w = (bg_w * 2);
      h = (bg_h * 2)};

    let w = Info.Display.logical_w
    and h = Info.Display.logical_h
    and over_w = !rect.w
    and over_h = !rect.h in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    rect := {!rect with
      x = !orig_x;
      y = !orig_y};
    rdr := Some renderer


  let start_anim_out f =
    Info.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with x = v})
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    Info.wait_for_events := false;
    rect := {!rect with
      x = !orig_x;
      y = !orig_y};
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with y = v})
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim


  let draw ~renderer =
    Sdl.render_copy renderer ~texture:(get_bg_tex ()) ~srcrect:None ~dstrect:(Some !rect)

  let release () =
    if (Option.is_some !bg_tex) then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None
    )

end


module Level_info = struct
  let rdr : Sdl.Renderer.t option ref = ref None
  let bg_tex : Sdl.Texture.t option ref = ref None
  let rect : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w = Sdl.get_surface_width surf
    and h = Sdl.get_surface_height surf in
    let tex = Sdl.create_texture_from_surface renderer surf in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"LEVEL INFO" in
    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"Completion status ..." in

    rect := { !rect with
      w = (Stdlib.max retry_w over_w + (10 * 2));
      h = (retry_h + over_h + (10 * 2))};
    let bg_w = (!rect).w and bg_h = (!rect).h in

    bg_tex := Some (
      Sdl.create_texture renderer
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:bg_w ~height:bg_h);
    Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.BlendMode.SDL_BLENDMODE_BLEND;
    Sdl.set_render_target renderer (Some (get_bg_tex ()));
    Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:100;
    Sdl.render_clear renderer;
    let orect = Sdl.Rect.make ~x:10 ~y:10 ~w:over_w ~h:over_h in
    Sdl.render_copy renderer ~texture:otex ~srcrect:None ~dstrect:(Some orect);
    let rrect = Sdl.Rect.make ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h in
    Sdl.render_copy renderer ~texture:rtex ~srcrect:None ~dstrect:(Some rrect);
    Sdl.set_render_target renderer None;
    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    rect := {!rect with
      w = (bg_w * 2);
      h = (bg_h * 2)};

    let w = Info.Display.logical_w
    and h = Info.Display.logical_h
    and over_w = !rect.w
    and over_h = !rect.h in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    rect := {!rect with
      x = !orig_x;
      y = !orig_y};

    rdr := Some renderer

  let start_anim_out f =
    Info.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with x = v} )
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    Info.wait_for_events := false;
    rect := {!rect with
      y = !orig_y;
      x = !orig_x};
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with y = v} )
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let draw ~renderer =
    Sdl.render_copy renderer ~texture:(get_bg_tex ()) ~srcrect:None ~dstrect:(Some !rect)

  let release () =
    if Option.is_some !bg_tex then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None)
end


module Level_details = struct
  let rdr : Sdl.Renderer.t option ref = ref None
  let bg_tex : Sdl.Texture.t option ref = ref None
  let rect : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w = Sdl.get_surface_width surf
    and h = Sdl.get_surface_height surf in
    let tex = Sdl.create_texture_from_surface renderer surf in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"SCORE BOARD" in

    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"Click to continue ..." in

    rect := {!rect with
    w = (Stdlib.max retry_w over_w + (10 * 2));
    h = (retry_h + over_h + (10 * 2))};
    let bg_w = !rect.w and bg_h = !rect.h in

    bg_tex := Some
           (Sdl.create_texture renderer
            ~fmt:Sdl.PixelFormat.RGBA8888
            ~access:Sdl.TextureAccess.Target
            ~width:bg_w ~height:bg_h);
    Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.BlendMode.SDL_BLENDMODE_BLEND;
    Sdl.set_render_target renderer (Some (get_bg_tex ()));
    Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:100;
    Sdl.render_clear renderer;
    let orect = Sdl.Rect.make ~x:10 ~y:10 ~w:over_w ~h:over_h in
    Sdl.render_copy renderer ~texture:otex ~srcrect:None ~dstrect:(Some orect);
    let rrect = Sdl.Rect.make ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h in
    Sdl.render_copy renderer ~texture:rtex ~srcrect:None ~dstrect:(Some rrect);
    Sdl.set_render_target renderer None;
    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    rect := { !rect with
    w = (bg_w * 2);
    h = (bg_h * 2)};

    let w = Info.Display.logical_w
    and h = Info.Display.logical_h
    and over_w = !rect.w
    and over_h = !rect.h in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    rect := {!rect with
    x = !orig_x;
    y = !orig_y};

    rdr := Some renderer


  let start_anim_out f =
    Info.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with x = v })
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    Info.wait_for_events := false;
    rect := {!rect with
      x = !orig_x;
      y = !orig_y};
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with y = v })
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let draw ~renderer =
    Sdl.render_copy renderer ~texture:(get_bg_tex ()) ~srcrect:None ~dstrect:(Some !rect)

  let release () =
    if Option.is_some !bg_tex then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None)
end


module Level_confirm = struct
  let rdr : Sdl.Renderer.t option ref = ref None
  let bg_tex : Sdl.Texture.t option ref = ref None
  let rect : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_bg_tex () = match !bg_tex with Some v -> v | None -> assert false

  let orig_x = ref 0
  let orig_y = ref 0

  let enabled : bool ref = ref false

  let gen_text ~renderer ~text =
    let surf = Fonts.get_surface text in
    let w = Sdl.get_surface_width surf in
    let h = Sdl.get_surface_height surf in
    let tex = Sdl.create_texture_from_surface renderer surf in
    Sdl.free_surface surf;
    (tex, w, h)

  let init ~renderer =
    let otex, over_w, over_h = gen_text ~renderer ~text:"Ready to play this: select" in

    let rtex, retry_w, retry_h = gen_text ~renderer ~text:"..." in

    rect := {!rect with
    w = (Stdlib.max retry_w over_w + (10 * 2));
    h = (retry_h + over_h + (10 * 2))};
    let bg_w = !rect.w and bg_h = !rect.h in

    bg_tex :=
      Some
           (Sdl.create_texture renderer
            ~fmt:Sdl.PixelFormat.RGBA8888
            ~access:Sdl.TextureAccess.Target
            ~width:bg_w
            ~height:bg_h);
    Sdl.set_texture_blend_mode (get_bg_tex ()) Sdl.BlendMode.SDL_BLENDMODE_BLEND;
    Sdl.set_render_target renderer (Some (get_bg_tex ()));
    Sdl.set_render_draw_color renderer ~r:0 ~g:0 ~b:0 ~a:100;
    Sdl.render_clear renderer;
    let orect = Sdl.Rect.make ~x:10 ~y:10 ~w:over_w ~h:over_h in
    Sdl.render_copy renderer ~texture:otex ~srcrect:None ~dstrect:(Some orect);
    let rrect = Sdl.Rect.make ~x:10 ~y:((10 * 2) + over_h) ~w:retry_w ~h:retry_h in
    Sdl.render_copy renderer ~texture:rtex ~srcrect:None ~dstrect:(Some rrect);
    Sdl.set_render_target renderer None;

    Sdl.destroy_texture otex;
    Sdl.destroy_texture rtex;

    rect := {!rect with
    w = (bg_w * 2);
    h = (bg_h * 2)};

    let w = Info.Display.logical_w
    and h = Info.Display.logical_h
    and over_w = !rect.w
    and over_h = !rect.h in
    orig_x := ((w / 2) - (over_w / 2));
    orig_y := ((h / 2) - over_h);
    rect := {!rect with
    x = !orig_x;
    y = !orig_y};

    rdr := Some renderer


  let start_anim_out f =
    Info.wait_for_events := false;
    let anim = Anims.create
      ~pt_start:(!orig_x)
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with x = v } )
      ~at_end:(fun () -> enabled := false; f ())
      Anims.Easing.Quadratic_in in
    Anims.start anim

  let start_anim_in f =
    enabled := true;
    Info.wait_for_events := false;
    rect := {!rect with
    x = !orig_x;
    y = !orig_y};
    let anim = Anims.create
      ~pt_start:(-1000)
      ~pt_end:(!orig_y)
      ~span:400
      ~at_update:(fun v -> rect := {!rect with y = v })
      ~at_end:f
      Anims.Easing.Quadratic_in in
    Anims.start anim


  let draw ~renderer =
    if !enabled then (
      Sdl.render_copy renderer ~texture:(get_bg_tex ()) ~srcrect:None ~dstrect:(Some !rect)
    )

  let release () =
    if !bg_tex <> None then (
      Sdl.destroy_texture (get_bg_tex ());
      bg_tex := None)
end
