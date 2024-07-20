open CamlSDL2
open Gamekit


(* factorised init code , TODO factorise anim in/out most of it *)
let gen_osd text1 text2 =
  let line1 = Fonts.gen_text text1 in
  let line1_rect = Sdl.Rect.make
    ~x:10
    ~y:10
    ~w:line1.width
    ~h:line1.height
  and line2 = Fonts.gen_text text2 in
  let line2_rect = Sdl.Rect.make
    ~x:10
    ~y:((10 * 2) + line1.height)
    ~w:line2.width
    ~h:line2.height in
  let rect  = Sdl.Rect.make
    ~x:0
    ~y:0
    ~w:((Stdlib.max line1.width line2.width) + (10 * 2))
    ~h:(line2.height + line1.height + (10 * 2)) in

  let texture = Texture2D.create rect.w rect.h in

  Texture2D.draw_begin texture;
  Texture2D.clear_with_color texture ~r:0 ~g:0 ~b:0 ~a:100;
  Texture2D.copy line1 line1_rect;
  Texture2D.copy line2 line2_rect;
  Texture2D.destroy line1;
  Texture2D.destroy line2;
  Texture2D.draw_end texture;

  let dstrect = Sdl.Rect.make
    ~x:((Info.Display.logical_w / 2) - rect.w)
    ~y:((Info.Display.logical_h / 2) - (rect.h * 2))
    ~w:(rect.w * 2)
    ~h:(rect.h * 2) in
  (dstrect, texture)


module Level_over = struct
  let t2d : Texture2D.t option ref = ref None
  let t2d_dst : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let orig_x : int ref = ref 0
  let orig_y : int ref = ref 0

  let init ~renderer:_ =
    let (dst, tex) = gen_osd "LEVEL COMPLETE BRAVO!!!" "..." in
    orig_x := dst.x;
    orig_y := dst.y;
    t2d_dst := dst;
    t2d := Some tex

  let draw ~renderer:_ =
    Texture2D.copy (Option.get !t2d) !t2d_dst

  let release () =
    Texture2D.destroy (Option.get !t2d); t2d := None

  let start_anim_out f =
    Info.wait_for_events := false;
    let anim =
      Anims.create
        ~pt_start:!orig_x
        ~pt_end:(-1000)
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with x = v })
        ~at_end:(fun () -> f ())
        Anims.Easing.Quadratic_in
    in
    Anims.start anim

  let start_anim_in f =
    Info.wait_for_events := false;
    t2d_dst := { !t2d_dst with x = !orig_x; y = !orig_y };
    let anim =
      Anims.create
        ~pt_start:(-1000)
        ~pt_end:!orig_y
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with y = v })
        ~at_end:f
        Anims.Easing.Quadratic_in
    in
    Anims.start anim
end

module Level_info = struct
  let t2d : Texture2D.t option ref = ref None
  let t2d_dst : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let orig_x : int ref = ref 0
  let orig_y : int ref = ref 0

  let init ~renderer:_ =
    let (dst, tex) = gen_osd "LEVEL INFO" "Completion status ..." in
    orig_x := dst.x;
    orig_y := dst.y;
    t2d_dst := dst;
    t2d := Some tex

  let draw ~renderer:_ =
    Texture2D.copy (Option.get !t2d) !t2d_dst

  let release () =
    Texture2D.destroy (Option.get !t2d); t2d := None

  let start_anim_out f =
    Info.wait_for_events := false;
    let anim =
      Anims.create
        ~pt_start:!orig_x
        ~pt_end:(-1000)
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with x = v })
        ~at_end:(fun () -> f ())
        Anims.Easing.Quadratic_in
    in
    Anims.start anim

  let start_anim_in f =
    Info.wait_for_events := false;
    t2d_dst := { !t2d_dst with x = !orig_x; y = !orig_y };
    let anim =
      Anims.create
        ~pt_start:(-1000)
        ~pt_end:!orig_y
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with y = v })
        ~at_end:f
        Anims.Easing.Quadratic_in
    in
    Anims.start anim
end


module Level_details = struct
  let t2d : Texture2D.t option ref = ref None
  let t2d_dst : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let orig_x : int ref = ref 0
  let orig_y : int ref = ref 0

  let init ~renderer:_ =
    let (dst, tex) = gen_osd "SOCRE BOARD" " Click to continue ..." in
    orig_x := dst.x;
    orig_y := dst.y;
    t2d_dst := dst;
    t2d := Some tex

  let draw ~renderer:_ =
    Texture2D.copy (Option.get !t2d) !t2d_dst

  let release () =
    Texture2D.destroy (Option.get !t2d); t2d := None

  let start_anim_out f =
    Info.wait_for_events := false;
    let anim =
      Anims.create
        ~pt_start:!orig_x
        ~pt_end:(-1000)
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with x = v })
        ~at_end:(fun () -> f ())
        Anims.Easing.Quadratic_in
    in
    Anims.start anim

  let start_anim_in f =
    Info.wait_for_events := false;
    t2d_dst := { !t2d_dst with x = !orig_x; y = !orig_y };
    let anim =
      Anims.create
        ~pt_start:(-1000)
        ~pt_end:!orig_y
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with y = v })
        ~at_end:f
        Anims.Easing.Quadratic_in
    in
    Anims.start anim
end


module Level_confirm = struct
  let t2d : Texture2D.t option ref = ref None
  let t2d_dst : Sdl.Rect.t ref = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
  let orig_x : int ref = ref 0
  let orig_y : int ref = ref 0

  let init ~renderer:_ =
    let (dst, tex) = gen_osd "Quit?" " Click to quit ..." in
    orig_x := dst.x;
    orig_y := dst.y;
    t2d_dst := dst;
    t2d := Some tex

  let draw ~renderer:_ =
    Texture2D.copy (Option.get !t2d) !t2d_dst

  let release () =
    Texture2D.destroy (Option.get !t2d); t2d := None

  let start_anim_out f =
    Info.wait_for_events := false;
    let anim =
      Anims.create
        ~pt_start:!orig_x
        ~pt_end:(-1000)
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with x = v })
        ~at_end:(fun () -> f ())
        Anims.Easing.Quadratic_in
    in
    Anims.start anim

  let start_anim_in f =
    Info.wait_for_events := false;
    t2d_dst := { !t2d_dst with x = !orig_x; y = !orig_y };
    let anim =
      Anims.create
        ~pt_start:(-1000)
        ~pt_end:!orig_y
        ~span:400
        ~at_update:(fun v -> t2d_dst := { !t2d_dst with y = v })
        ~at_end:f
        Anims.Easing.Quadratic_in
    in
    Anims.start anim
end

