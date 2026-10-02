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

type texs_t = {
    board  : Sdl.Texture.t;
    pieces : Sdl.Texture.t;
    hints  : Sdl.Texture.t;
    map    : Sdl.Texture.t;
    menu   : Sdl.Texture.t;
}

let texs_empty = {
    board  = Sdl.Texture.null ();
    pieces = Sdl.Texture.null ();
    hints  = Sdl.Texture.null ();
    map    = Sdl.Texture.null ();
    menu   = Sdl.Texture.null ();
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

let figs_empty = {
    bP = Sdl.Texture.null ();
    wP = Sdl.Texture.null ();
    bN = Sdl.Texture.null ();
    wN = Sdl.Texture.null ();
    bB = Sdl.Texture.null ();
    wB = Sdl.Texture.null ();
    bR = Sdl.Texture.null ();
    wR = Sdl.Texture.null ();
    bQ = Sdl.Texture.null ();
    wQ = Sdl.Texture.null ();
    bK = Sdl.Texture.null ();
    wK = Sdl.Texture.null ();
}

type input_t = {
    mutable changed               : bool;
    mutable left_button_pressed   : bool;
    mutable middle_button_pressed : bool;
    mutable right_button_pressed  : bool;
    mutable wheel : int;
    mutable pos_x : int;
    mutable pos_y : int;
}

let input_empty = {
    changed                 = false;
    left_button_pressed     = false;
    middle_button_pressed   = false;
    right_button_pressed    = false;
    wheel                   = 0;
    pos_x                   = 0;
    pos_y                   = 0;
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

type app_t = {
    mutable rdr     : Sdl.Renderer.t;
    mutable win     : Sdl.Window.t;
    mutable figs    : figs_t;
    mutable texs    : texs_t;
    mutable ticks   : int;
    mutable delta   : int;
    mutable exit    : bool;
    exit_keys       : Sdl.Keycode.t list;
    figs_w          : int;
    data_path       : string;
    scr_w           : int;
    scr_h           : int;
    clear_color     : Sdl.Color.t;
    input           : input_t;
}

let app : app_t = {
    rdr         = Sdl.Renderer.null ();
    win         = Sdl.Window.null ();
    figs        = figs_empty;
    texs        = texs_empty;
    figs_w      = figs_width;
    ticks       = 0;
    delta       = 0;
    exit        = false;
    exit_keys   = [Sdl.Keycode.Escape; Sdl.Keycode.Q];
    data_path   = "/home/seb/src/kingcrush/data";
    scr_w       = screen_width;
    scr_h       = screen_height;
    clear_color = color_clear;
    input       = input_empty;
}


let () =

    Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
    Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
    Sdl.set_hint "SDL_RENDER_VSYNC" "1";

    app.win <- Sdl.create_window
        ~title:"KingCrush"
        ~x:`centered
        ~y:`centered
        ~width:app.scr_w
        ~height:app.scr_h
        ~flags:[ Sdl.WindowFlags.OpenGL ];

    app.rdr <- Sdl.create_renderer
        ~win:app.win
        ~index:(-1)
        ~flags:[
            Sdl.RendererFlags.Accelerated;
            Sdl.RendererFlags.TargetTexture;
            Sdl.RendererFlags.PresentVSync ];


    let load_fig = function fig ->
        let cat = Filename.concat in
        let fname = cat (cat (cat app.data_path "pieces") "default") fig in
        Img.load_texture app.rdr ~filename:fname
    in

    app.figs <- {
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
    };


    let board_tex =
        Sdl.create_texture app.rdr
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:texs_width
        ~height:texs_width
    in

    Sdl.set_render_target app.rdr (Some board_tex);
    Sdl.set_render_draw_color2 app.rdr color_white_square;
    Sdl.render_clear app.rdr;
    Sdl.set_render_draw_color2 app.rdr color_black_square;

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
                Sdl.render_fill_rect app.rdr (make_rect i j)
        done
    done;

    Sdl.set_render_target app.rdr None;

    app.texs <- {texs_empty with board = board_tex};


    while app.exit != true do

        let rec consume_events = function () ->
            match Sdl.poll_event () with
            | Some Sdl.Event.SDL_QUIT _ ->
                app.exit <- true;

            | Some Sdl.Event.SDL_KEYDOWN e ->
                if (List.mem e.keycode app.exit_keys) then
                    app.exit <- true;

            | Some Sdl.Event.SDL_MOUSEBUTTONDOWN mb ->
                (match mb.mb_button with
                | 1 ->
                    app.input.left_button_pressed <- true;
                    app.input.changed <- true;
                | 2 ->
                    app.input.middle_button_pressed <- true;
                    app.input.changed <- true;
                | 3 ->
                    app.input.right_button_pressed  <- true;
                    app.input.changed <- true;
                | _ -> ());
                consume_events ()

            | Some Sdl.Event.SDL_MOUSEBUTTONUP mb ->
                (match mb.mb_button with
                | 1 ->
                    app.input.left_button_pressed <- false;
                    app.input.changed <- true;
                | 2 ->
                    app.input.middle_button_pressed <- false;
                    app.input.changed <- true;
                | 3 ->
                    app.input.right_button_pressed <- false;
                    app.input.changed <- true;
                | _ -> ());
                consume_events ()

            | Some Sdl.Event.SDL_MOUSEWHEEL mw ->
                app.input.wheel <- mw.mw_y;
                app.input.changed <- true;
                consume_events ()

            | Some Sdl.Event.SDL_MOUSEMOTION mm ->
                app.input.pos_x <- mm.mm_x;
                app.input.pos_y <- mm.mm_y;
                app.input.changed <- true;
                consume_events ()

            | Some _ ->
                consume_events ()

            | None -> ()
        in

        consume_events ();

        if app.input.changed then (
            app.input.changed <- false;
            app.input.wheel <- 0;
        );

        Sdl.set_render_draw_color2 app.rdr app.clear_color;
        Sdl.render_clear app.rdr;
        Sdl.render_copy app.rdr
            ~texture:app.texs.board
            ~srcrect:None
            ~dstrect:(Some texs_dst_rect);

        Sdl.render_present app.rdr;

    done;

    let destroy_tex = function tx ->
        assert(tx <> (Sdl.Texture.null ()));
        Sdl.destroy_texture tx
    in

    destroy_tex app.figs.bP;
    destroy_tex app.figs.bN;
    destroy_tex app.figs.bB;
    destroy_tex app.figs.bR;
    destroy_tex app.figs.bQ;
    destroy_tex app.figs.bK;
    destroy_tex app.figs.wP;
    destroy_tex app.figs.wN;
    destroy_tex app.figs.wB;
    destroy_tex app.figs.wR;
    destroy_tex app.figs.wQ;
    destroy_tex app.figs.wK;

    destroy_tex app.texs.board;

    Sdl.destroy_renderer app.rdr;
    Sdl.destroy_window app.win;
    Sdl.quit ()

