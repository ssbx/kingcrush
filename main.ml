open CamlSDL2
open CamlSDL2_image


let screen_width  = 1240
let screen_height = 780
let figs_width = 177
let texs_margin = 10
let texs_width = figs_width * 8
let texs_dst_rect  = Sdl.Rect.make
    ~x:texs_margin
    ~y:texs_margin
    ~w:(780 - 2 * texs_margin)
    ~h:(780 - 2 * texs_margin)
let color_clear = Sdl.Color.make ~r:60 ~g:60 ~b:60 ~a:255
let color_black_square = Sdl.Color.make ~r:181 ~g:136 ~b:99 ~a:255
let color_white_square = Sdl.Color.make ~r:240 ~g:217 ~b:181 ~a:255
let data_path = "/home/seb/src/kingcrush/data"
let exit_keys = [Sdl.Keycode.Escape; Sdl.Keycode.Q]

type texs_t = {
    board  : Sdl.Texture.t;
    pieces : Sdl.Texture.t;
    hints  : Sdl.Texture.t;
    map    : Sdl.Texture.t;
    menu   : Sdl.Texture.t;
}

type figs_t = {
    bP : Sdl.Texture.t;
    wP : Sdl.Texture.t;
    bN : Sdl.Texture.t;
    wN : Sdl.Texture.t;
    bB : Sdl.Texture.t;
    wB : Sdl.Texture.t;
    bR : Sdl.Texture.t;
    wR : Sdl.Texture.t;
    bQ : Sdl.Texture.t;
    wQ : Sdl.Texture.t;
    bK : Sdl.Texture.t;
    wK : Sdl.Texture.t;
}

type input_t = {
    mutable changed               : bool;
    mutable left_button_pressed   : bool;
    mutable middle_button_pressed : bool;
    mutable right_button_pressed  : bool;
    mutable wheel                 : int;
    mutable pos_x                 : int;
    mutable pos_y                  : int;
}

let _print_input (m : input_t) =

    let bool_to_int_str = function v ->
        Bool.to_int v |> Int.to_string
    in

    print_endline (
        "input="   ^
        " left:"   ^ (bool_to_int_str m.left_button_pressed)   ^
        " middle:" ^ (bool_to_int_str m.middle_button_pressed) ^
        " right:"  ^ (bool_to_int_str m.right_button_pressed)  ^
        " pos_x:"  ^ (Int.to_string m.pos_x) ^
        " pos_y:"  ^ (Int.to_string m.pos_y) ^
        " wheel:"  ^ (Int.to_string m.wheel))

let () =

    Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
    Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
    Sdl.set_hint "SDL_RENDER_VSYNC" "1";

    let win = Sdl.create_window
        ~title:"KingCrush"
        ~x:`centered
        ~y:`centered
        ~width:screen_width
        ~height:screen_height
        ~flags:[ Sdl.WindowFlags.OpenGL ]
    in

    let rdr = Sdl.create_renderer
        ~win:win
        ~index:(-1)
        ~flags:[
            Sdl.RendererFlags.Accelerated;
            Sdl.RendererFlags.TargetTexture;
            Sdl.RendererFlags.PresentVSync ]
    in

    let load_fig = function fig ->
        let cat = Filename.concat in
        let fname = cat (cat (cat data_path "pieces") "default") fig in
        Img.load_texture rdr ~filename:fname
    in

    let figs = {
        bP = load_fig "bP.png";
        wP = load_fig "wP.png";
        bN = load_fig "bN.png";
        wN = load_fig "wN.png";
        bB = load_fig "bB.png";
        wB = load_fig "wB.png";
        bR = load_fig "bR.png";
        wR = load_fig "wR.png";
        bQ = load_fig "bQ.png";
        wQ = load_fig "wQ.png";
        bK = load_fig "bK.png";
        wK = load_fig "wK.png";
    }
    in

    let board_tex =
        Sdl.create_texture rdr
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:texs_width
        ~height:texs_width
    in

    Sdl.set_render_target rdr (Some board_tex);
    Sdl.set_render_draw_color2 rdr color_white_square;
    Sdl.render_clear rdr;
    Sdl.set_render_draw_color2 rdr color_black_square;

    let make_rect = fun ix iy ->
        Sdl.Rect.make
            ~x:(ix * figs_width)
            ~y:(iy * figs_width)
            ~w:figs_width
            ~h:figs_width
    in

    for i = 0 to 7 do
        for j = 0 to 7 do
            if Int.rem (i + j + 1) 2 = 0 then
                Sdl.render_fill_rect rdr (make_rect i j)
        done
    done;

    Sdl.set_render_target rdr None;

    let texs = {
        board  = board_tex;
        pieces = Sdl.Texture.null ();
        hints  = Sdl.Texture.null ();
        map    = Sdl.Texture.null ();
        menu   = Sdl.Texture.null ();
    } in

    let input = {
        changed               = false;
        left_button_pressed   = false;
        middle_button_pressed = false;
        right_button_pressed  = false;
        wheel                 = 0;
        pos_x                 = 0;
        pos_y                 = 0;
    } in

    let exit   = ref false in
    let _ticks = ref 0 in
    let _delta = ref 0 in

    while !exit != true do

        let rec process_events = function () ->
            match Sdl.poll_event () with
            | Some Sdl.Event.SDL_QUIT _ -> (
                exit := true
            )

            | Some Sdl.Event.SDL_KEYDOWN e -> (
                if (List.mem e.keycode exit_keys) then
                    exit := true
            )

            | Some Sdl.Event.SDL_MOUSEBUTTONDOWN mb -> (
                match mb.mb_button with
                | 1 -> (
                    input.left_button_pressed <- true;
                    input.changed <- true
                )
                | 2 -> (
                    input.middle_button_pressed <- true;
                    input.changed <- true
                )
                | 3 -> (
                    input.right_button_pressed  <- true;
                    input.changed <- true
                )
                | _ -> ();
                process_events ()
            )

            | Some Sdl.Event.SDL_MOUSEBUTTONUP mb -> (
                match mb.mb_button with
                | 1 -> (
                    input.left_button_pressed <- false;
                    input.changed <- true
                )
                | 2 -> (
                    input.middle_button_pressed <- false;
                    input.changed <- true
                )
                | 3 -> (
                    input.right_button_pressed <- false;
                    input.changed <- true
                )
                | _ -> ();
                process_events ()
            )

            | Some Sdl.Event.SDL_MOUSEWHEEL mw -> (
                input.wheel <- mw.mw_y;
                input.changed <- true;
                process_events ()
            )

            | Some Sdl.Event.SDL_MOUSEMOTION mm -> (
                input.pos_x <- mm.mm_x;
                input.pos_y <- mm.mm_y;
                input.changed <- true;
                process_events ()
            )

            | Some _ -> (
                process_events ()
            )

            | None -> ()
        in

        process_events ();

        if input.changed then (
            input.changed <- false;
            input.wheel <- 0;
        );

        Sdl.set_render_draw_color2 rdr color_clear;
        Sdl.render_clear rdr;
        Sdl.render_copy rdr
            ~texture:texs.board
            ~srcrect:None
            ~dstrect:(Some texs_dst_rect);

        Sdl.render_present rdr;

    done;

    let destroy_tex = function tx ->
        assert(tx <> (Sdl.Texture.null ()));
        Sdl.destroy_texture tx
    in

    destroy_tex figs.bP;
    destroy_tex figs.bN;
    destroy_tex figs.bB;
    destroy_tex figs.bR;
    destroy_tex figs.bQ;
    destroy_tex figs.bK;
    destroy_tex figs.wP;
    destroy_tex figs.wN;
    destroy_tex figs.wB;
    destroy_tex figs.wR;
    destroy_tex figs.wQ;
    destroy_tex figs.wK;

    destroy_tex texs.board;

    Sdl.destroy_renderer rdr;
    Sdl.destroy_window win;
    Sdl.quit ()

