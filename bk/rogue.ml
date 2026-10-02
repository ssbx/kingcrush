open CamlSDL2
open CamlSDL2_image

open Rogue

let render_table_switch = false
let screen_width = 1240
let screen_height = 780
let view_border = 30
let view_side_border = 90
let view_ui_width = screen_width - 2 * view_side_border
let view_ui_height = screen_height - 2 * view_border

let rdr = ref (Sdl.Renderer.null ())
let window = ref (Sdl.Window.null ())

let cartouche_line_width = 6

let view_ui_dstrect = Sdl.Rect.make
    ~x:view_side_border
    ~y:view_border
    ~w:view_ui_width
    ~h:view_ui_height

let view_img_dstrect = Sdl.Rect.make
    ~x:(view_side_border + cartouche_line_width / 2)
    ~y:(view_border + cartouche_line_width / 2)
    ~w:(view_ui_width - cartouche_line_width)
    ~h:(view_ui_height - cartouche_line_width)

let combat_ui_span = 16
let combat_ui_height = 250
let combat_ui_width = screen_width
let combat_ui_dstrect = Sdl.Rect.make
    ~x:combat_ui_span
    ~y:(screen_height - combat_ui_height)
    ~w:(combat_ui_width - 2 * combat_ui_span)
    ~h:(combat_ui_height - combat_ui_span)

let combat_ui_inner_top_span = 5
let combat_ui_inner_inter_span = 10
let combat_ui_inner_border_span = 30
let combat_ui_line_width =
    combat_ui_width - ((combat_ui_span + combat_ui_inner_border_span) * 2)
let combat_ui_inner_height = combat_ui_height -
    (combat_ui_span * 2) - combat_ui_inner_top_span


let combat_ui_inner_x = combat_ui_span + combat_ui_inner_border_span
let combat_ui_inner_y = combat_ui_span + combat_ui_inner_top_span

let combat_ui_row_height =
    (combat_ui_inner_height - (4 * combat_ui_inner_inter_span)) / 4

let p1_rect = Sdl.Rect.make
    ~x:combat_ui_inner_x
    ~y:combat_ui_inner_y
    ~w:combat_ui_line_width
    ~h:combat_ui_row_height

let p2_rect = Sdl.Rect.make
    ~x:combat_ui_inner_x
    ~y:(combat_ui_inner_y + combat_ui_row_height + combat_ui_inner_inter_span)
    ~w:combat_ui_line_width
    ~h:combat_ui_row_height

let p3_rect = Sdl.Rect.make
    ~x:combat_ui_inner_x
    ~y:(combat_ui_inner_y + (combat_ui_row_height + combat_ui_inner_inter_span) * 2)
    ~w:combat_ui_line_width
    ~h:combat_ui_row_height

let p4_rect = Sdl.Rect.make
    ~x:combat_ui_inner_x
    ~y:(combat_ui_inner_y + (combat_ui_row_height + combat_ui_inner_inter_span) * 3)
    ~w:combat_ui_line_width
    ~h:combat_ui_row_height

let decode_char = function
| 'A' -> 64
| 'B' -> 65
| 'C' -> 66
| 'D' -> 67
| 'E' -> 68
| 'F' -> 69
| 'G' -> 70
| 'H' -> 71
| 'I' -> 72
| 'J' -> 73
| 'K' -> 74
| 'L' -> 75
| 'M' -> 76
| 'N' -> 77
| 'O' -> 78
| 'P' -> 79
| 'Q' -> 80
| 'R' -> 81
| 'S' -> 82
| 'T' -> 83
| 'U' -> 84
| 'V' -> 85
| 'W' -> 86
| 'X' -> 87
| 'Y' -> 88
| 'Z' -> 89
| 'a' -> 96
| 'b' -> 97
| 'c' -> 98
| 'd' -> 99
| 'e' -> 100
| 'f' -> 101
| 'g' -> 102
| 'h' -> 103
| 'i' -> 104
| 'j' -> 105
| 'k' -> 106
| 'l' -> 107
| 'm' -> 108
| 'n' -> 109
| 'o' -> 110
| 'p' -> 111
| 'q' -> 112
| 'r' -> 113
| 's' -> 114
| 't' -> 115
| 'u' -> 116
| 'v' -> 117
| 'w' -> 118
| 'x' -> 119
| 'y' -> 120
| 'z' -> 121
| '!' -> 32
| '"' -> 33
| '\'' -> 38
| '(' -> 39
| ')' -> 40
| '/' -> 46
| '0' -> 47
| '1' -> 48
| '2' -> 49
| '3' -> 50
| '4' -> 51
| '5' -> 52
| '6' -> 53
| '7' -> 54
| '8' -> 55
| '9' -> 56
| ':' -> 57
| ';' -> 58
| ',' -> 43
| '-' -> 44
| '.' -> 45
| '?' -> 62
| _other -> 0

let pick_num n =
    if n < 0 then None
    else Some (
            Sdl.Rect.make
                ~x:(((n mod 16) * 16) + 4)
                ~y:(((n / 16) * 16) + 4)
                ~w:(16 - 8)
                ~h:(16 - 8))

let pick_char c =
    pick_num (decode_char c)

let render_string font_tx x_start y_start str font_size =
    let length = String.length str in
    let m = font_size / 4 in
    for i = 0 to (length - 1) do
        let srect = pick_char str.[i] in
        let drect = Some (Sdl.Rect.make
            ~x:(x_start + (i * font_size * m))
            ~y:y_start
            ~w:(font_size * m)
            ~h:(font_size * m)) in
        Sdl.render_copy !rdr ~texture:font_tx ~srcrect:srect ~dstrect:drect;
    done

let render_table font_tx n =
    let srect = pick_num n in
    let out_size = if n == (-1) then 600 else 600 / 16 in
    let drect = Some (Sdl.Rect.make ~x:0 ~y:0 ~w:out_size ~h:out_size) in
    Sdl.render_copy !rdr ~texture:font_tx ~srcrect:srect ~dstrect:drect

let render_bottom combat_ui_tx =
    Sdl.render_copy !rdr
        ~texture:combat_ui_tx
        ~srcrect:None
        ~dstrect:(Some combat_ui_dstrect)

let render_view  view_ui_tx chateau_tx =

    Sdl.render_copy !rdr
        ~texture:chateau_tx
        ~srcrect:None
        ~dstrect:(Some view_img_dstrect);

    Sdl.render_copy !rdr
        ~texture:view_ui_tx
        ~srcrect:None
        ~dstrect:(Some view_ui_dstrect)




let make_box tx width height alpha =
    Sdl.set_render_target !rdr (Some tx);
    Sdl.set_render_draw_color !rdr ~r:0 ~g:0 ~b:0 ~a:alpha;
    Sdl.render_clear !rdr;
    Sdl.set_render_draw_color !rdr ~r:255 ~g:255 ~b:255 ~a:255;
    let up_rect = Sdl.Rect.make
        ~x:cartouche_line_width
        ~y:0
        ~w:(width - 2 * cartouche_line_width)
        ~h:cartouche_line_width in
    Sdl.render_fill_rect !rdr up_rect;
    let down_rect = Sdl.Rect.make
        ~x:cartouche_line_width
        ~y:(height - cartouche_line_width)
        ~w:(width - 2 * cartouche_line_width)
        ~h:cartouche_line_width in
    Sdl.render_fill_rect !rdr down_rect;
    let left_rect = Sdl.Rect.make
        ~x:0
        ~y:cartouche_line_width
        ~w:cartouche_line_width
        ~h:(height - cartouche_line_width * 2) in
    Sdl.render_fill_rect !rdr left_rect;
    let right_rect = Sdl.Rect.make
        ~x:(width - cartouche_line_width)
        ~y:cartouche_line_width
        ~w:cartouche_line_width
        ~h:(height - 2 * cartouche_line_width) in
    Sdl.render_fill_rect !rdr right_rect;
    let square_span = cartouche_line_width / 2 in
    let sqrt1_rect = Sdl.Rect.make
        ~x:square_span
        ~y:square_span
        ~w:cartouche_line_width
        ~h:cartouche_line_width in
    Sdl.render_fill_rect !rdr sqrt1_rect;
    let sqrt2_rect = Sdl.Rect.make
        ~x:(width - cartouche_line_width - square_span)
        ~y:square_span
        ~w:cartouche_line_width
        ~h:cartouche_line_width in
    Sdl.render_fill_rect !rdr sqrt2_rect;
    let sqrt3_rect = Sdl.Rect.make
        ~x:square_span
        ~y:(height - cartouche_line_width - square_span)
        ~w:cartouche_line_width
        ~h:cartouche_line_width in
    Sdl.render_fill_rect !rdr sqrt3_rect;
    let sqrt4_rect = Sdl.Rect.make
        ~x:(width - cartouche_line_width - square_span)
        ~y:(height - cartouche_line_width - square_span)
        ~w:cartouche_line_width
        ~h:cartouche_line_width in
    Sdl.render_fill_rect !rdr sqrt4_rect;
    Sdl.set_render_target !rdr None;
    Sdl.set_render_draw_color !rdr ~r:0 ~g:0 ~b:0 ~a:255

let make_view_ui tx =
    make_box tx view_ui_width view_ui_height 0

let make_combat_ui tx font_tx =
    make_box tx combat_ui_width combat_ui_height 125;

    Sdl.set_render_target !rdr (Some tx);
    let get_center_y fsize (rectv:Sdl.Rect.t) =
        let m = fsize / 4 in
        let span = (rectv.h - (fsize - m)) / 2 in
        rectv.y + span
    in

    Sdl.set_render_draw_color !rdr ~r:255 ~g:255 ~b:255 ~a:255;
    let str = "    G-PRI ........ 302/203  45 ....... ......." in
    render_string font_tx p1_rect.x (get_center_y 10 p1_rect)
    (String.cat "Gandalf" str) 11;
    render_string font_tx p2_rect.x (get_center_y 10 p2_rect)
    (String.cat "Legolas" str) 11;
    render_string font_tx p3_rect.x (get_center_y 10 p3_rect)
    (String.cat "Bilbo  " str) 11;
    render_string font_tx p4_rect.x (get_center_y 10 p4_rect)
    (String.cat "Roger  " str) 11;


    (*Sdl.set_render_draw_color rdr ~r:0 ~g:0 ~b:0 ~a:64;*)

    Sdl.set_render_target !rdr None;
    Sdl.set_render_draw_color !rdr ~r:0 ~g:0 ~b:0 ~a:255

let () =
    print_endline "Hello, World!";
    Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
    Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
    Sdl.set_hint "SDL_RENDER_VSYNC" "1";
    window := Sdl.create_window
        ~title:"rogue"
        ~x:`centered
        ~y:`centered
        ~width:screen_width
        ~height:screen_height
        ~flags:[ Sdl.WindowFlags.OpenGL ];

    rdr := Sdl.create_renderer
        ~win:!window
        ~index:(-1)
        ~flags:[
            Sdl.RendererFlags.Accelerated;
            Sdl.RendererFlags.TargetTexture;
            Sdl.RendererFlags.PresentVSync;];

    let fname = "/home/seb/src/rogue/assets/font2.png" in
    let font_tx = Img.load_texture !rdr ~filename:fname in
    Sdl.set_texture_scale_mode font_tx Sdl.ScaleMode.NEAREST;

    (*let fname = "/home/seb/src/rogue/assets/chateau.png" in*)
    let fname = "/home/seb/src/rogue/assets/The Last Blade.jpg" in
    let chateau_tx = Img.load_texture !rdr ~filename:fname in
    Sdl.set_texture_scale_mode chateau_tx Sdl.ScaleMode.NEAREST;

    (* bottom cartouche texture *)
    let combat_ui_tx =
        Sdl.create_texture !rdr
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:combat_ui_width
        ~height:combat_ui_height
    in
    (*Sdl.set_texture_blend_mode combat_ui_tx Sdl.BlendMode.BLEND;*)
    Sdl.set_texture_blend_mode combat_ui_tx Sdl.BlendMode.BLEND;

    let view_ui_tx =
        Sdl.create_texture !rdr
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:view_ui_width
        ~height:view_ui_height
    in
    (*Sdl.set_texture_blend_mode combat_ui_tx Sdl.BlendMode.BLEND;*)
    Sdl.set_texture_blend_mode view_ui_tx Sdl.BlendMode.BLEND;

    make_view_ui view_ui_tx;
    make_combat_ui combat_ui_tx font_tx;

    let rec loop n =
        Sdl.render_clear !rdr;
        if render_table_switch == true then render_table font_tx n;
        render_view view_ui_tx chateau_tx;
        render_bottom combat_ui_tx;
        Sdl.render_present !rdr;
        match Sdl.poll_event () with
        | Some Sdl.Event.SDL_QUIT _ -> ()
        | Some Sdl.Event.SDL_KEYDOWN e -> (
            match e.scancode with
            | Sdl.Scancode.ESCAPE -> ()
            | Sdl.Scancode.A ->
                print_int (-1); print_newline ();
                loop (-1)
            | Sdl.Scancode.RIGHT ->
                let n2 = if n == 256 then (-1) else (n + 1) in
                print_int n2; print_newline ();
                loop n2
            | Sdl.Scancode.LEFT ->
                let n2 = if n == (-1) then 256 else (n - 1) in
                print_int n2; print_newline ();
                loop n2
            | Sdl.Scancode.DOWN ->
                let n1 = if n != (-1) then n + 16 else 0 in
                let n2 = if n1 > 256 then (n1 - 256) else n1 in
                print_int n2; print_newline ();
                loop n2
            | Sdl.Scancode.UP ->
                let n1 = n - 16 in
                let n2 = if n1 < 0 then (n1 + 256) else n1 in
                print_int n2; print_newline ();
                loop n2

            | _ -> loop n
        )
        | _ -> loop n

    in

    loop (-1);

    Tk.hello ();

    Img.quit ();
    Sdl.destroy_texture font_tx;
    Sdl.destroy_texture view_ui_tx;
    Sdl.destroy_texture chateau_tx;
    Sdl.destroy_renderer !rdr;
    Sdl.destroy_window !window;
    rdr := Sdl.Renderer.null ();
    window := Sdl.Window.null ()

