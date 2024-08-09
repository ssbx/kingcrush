open CamlSDL2
open Gamekit

type t =
  { mutable t2d : Texture2D.t option
  ; mutable t2d_dst : Sdl.Rect.t
  ; mutable orig_x : int
  ; mutable orig_y : int
  }

let empty () =
  { t2d = None; t2d_dst = Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0; orig_x = 0; orig_y = 0 }
;;

let gen_osd text1 text2 osd_t =
  let line1 = Fonts.gen_text text1 in
  let line1_rect = Sdl.Rect.make ~x:10 ~y:10 ~w:line1.width ~h:line1.height
  and line2 = Fonts.gen_text text2 in
  let line2_rect =
    Sdl.Rect.make ~x:10 ~y:((10 * 2) + line1.height) ~w:line2.width ~h:line2.height
  in
  let rect =
    Sdl.Rect.make
      ~x:0
      ~y:0
      ~w:(Stdlib.max line1.width line2.width + (10 * 2))
      ~h:(line2.height + line1.height + (10 * 2))
  in
  let texture = Texture2D.create rect.w rect.h in
  Texture2D.draw_begin texture;
  Texture2D.clear_with_color texture ~r:0 ~g:0 ~b:0 ~a:100;
  Texture2D.copy line1 line1_rect;
  Texture2D.copy line2 line2_rect;
  Texture2D.destroy line1;
  Texture2D.destroy line2;
  Texture2D.draw_end texture;
  let dstrect =
    Sdl.Rect.make
      ~x:((Info.Display.logical_w / 2) - rect.w)
      ~y:((Info.Display.logical_h / 2) - (rect.h * 2))
      ~w:(rect.w * 2)
      ~h:(rect.h * 2)
  in
  osd_t.orig_x <- dstrect.x;
  osd_t.orig_y <- dstrect.y;
  osd_t.t2d_dst <- dstrect;
  osd_t.t2d <- Some texture
;;

let draw osd_t = Texture2D.copy (Option.get osd_t.t2d) osd_t.t2d_dst

let release osd_t =
  Texture2D.destroy (Option.get osd_t.t2d);
  osd_t.t2d <- None
;;

module Level_over = struct
  let osd : t = empty ()
  let init ~renderer:_ = gen_osd "LEVEL COMPLETE BRAVO!!!" "..." osd
  let draw ~renderer:_ = draw osd
  let release () = release osd

  let start_anim_out f =
    Anims.create_start
      ~pt_start:osd.orig_x
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with x = v })
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in
  ;;

  let start_anim_in f =
    osd.t2d_dst <- { osd.t2d_dst with x = osd.orig_x; y = osd.orig_y };
    Anims.create_start
      ~pt_start:(-1000)
      ~pt_end:osd.orig_y
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with y = v })
      ~at_end:f
      Anims.Easing.Quadratic_in
  ;;
end

module Level_info = struct
  let osd : t = empty ()
  let init ~renderer:_ = gen_osd "LEVEL INFO" "Completion status ..." osd
  let draw ~renderer:_ = draw osd
  let release () = release osd

  let start_anim_out f =
    Anims.create_start
      ~pt_start:osd.orig_x
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with x = v })
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in
  ;;

  let start_anim_in f =
    osd.t2d_dst <- { osd.t2d_dst with x = osd.orig_x; y = osd.orig_y };
    Anims.create_start
      ~pt_start:(-1000)
      ~pt_end:osd.orig_y
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with y = v })
      ~at_end:f
      Anims.Easing.Quadratic_in
  ;;
end

module Level_details = struct
  let osd : t = empty ()
  let init ~renderer:_ = gen_osd "SOCRE BOARD" " Click to continue ..." osd
  let draw ~renderer:_ = draw osd
  let release () = release osd

  let start_anim_out f =
    Anims.create_start
      ~pt_start:osd.orig_x
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with x = v })
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in
  ;;

  let start_anim_in f =
    osd.t2d_dst <- { osd.t2d_dst with x = osd.orig_x; y = osd.orig_y };
    Anims.create_start
      ~pt_start:(-1000)
      ~pt_end:osd.orig_y
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with y = v })
      ~at_end:f
      Anims.Easing.Quadratic_in
  ;;
end

module Level_confirm = struct
  let osd : t = empty ()
  let init ~renderer:_ = gen_osd "Quit?" " Click to quit ..." osd
  let draw ~renderer:_ = draw osd
  let release () = release osd

  let start_anim_out f =
    Anims.create_start
      ~pt_start:osd.orig_x
      ~pt_end:(-1000)
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with x = v })
      ~at_end:(fun () -> f ())
      Anims.Easing.Quadratic_in
  ;;

  let start_anim_in f =
    osd.t2d_dst <- { osd.t2d_dst with x = osd.orig_x; y = osd.orig_y };
    Anims.create_start
      ~pt_start:(-1000)
      ~pt_end:osd.orig_y
      ~span:400
      ~at_update:(fun v -> osd.t2d_dst <- { osd.t2d_dst with y = v })
      ~at_end:f
      Anims.Easing.Quadratic_in
  ;;
end
