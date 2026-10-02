open CamlSDL2
open CamlSDL2_image

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
    print_endline (
        "input=" ^
        " left:"    ^ (Bool.to_int m.left_button_pressed |> Int.to_string)   ^
        " middle:"  ^ (Bool.to_int m.middle_button_pressed |> Int.to_string) ^
        " right:"   ^ (Bool.to_int m.right_button_pressed |> Int.to_string)  ^
        " pos_x:"   ^ (Int.to_string m.pos_x) ^
        " pos_y:"   ^ (Int.to_string m.pos_y) ^
        " wheel:"   ^ (Int.to_string m.wheel))

type app_t = {
    mutable rdr     : Sdl.Renderer.t;
    mutable win     : Sdl.Window.t;
    mutable figs    : figs_t;
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
    figs_w      = 177;
    ticks       = 0;
    delta       = 0;
    exit        = false;
    exit_keys   = [Sdl.Keycode.Escape; Sdl.Keycode.Q];
    data_path   = "/home/seb/src/kingcrush/data";
    scr_w       = 1240;
    scr_h       = 780;
    clear_color = Sdl.Color.make ~r:100 ~g:100 ~b:100 ~a:255;
    input       = input_empty;
}

let load_figs () =
    let load_tx = fun fig ->
        let cat = Filename.concat in
        let fname = cat (cat (cat app.data_path "pieces") "default") fig in
        Img.load_texture app.rdr ~filename:fname
    in
    {
        bP = load_tx "bP.png";
        wP = load_tx "wP.png";
        bN = load_tx "bN.png";
        wN = load_tx "wN.png";
        bB = load_tx "bB.png";
        wB = load_tx "wB.png";
        bR = load_tx "bR.png";
        wR = load_tx "wR.png";
        bQ = load_tx "bQ.png";
        wQ = load_tx "wQ.png";
        bK = load_tx "bK.png";
        wK = load_tx "wK.png";
    }

let release_figs () =
    let null_tx = Sdl.Texture.null () in
    let destroy = function tx -> assert(tx <> null_tx); Sdl.destroy_texture tx in
    destroy app.figs.bP;
    destroy app.figs.bN;
    destroy app.figs.bB;
    destroy app.figs.bR;
    destroy app.figs.bQ;
    destroy app.figs.bK;
    destroy app.figs.wP;
    destroy app.figs.wN;
    destroy app.figs.wB;
    destroy app.figs.wR;
    destroy app.figs.wQ;
    destroy app.figs.wK


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
        ~flags:[ Sdl.RendererFlags.Accelerated;
                 Sdl.RendererFlags.TargetTexture;
                 Sdl.RendererFlags.PresentVSync ];
    app.figs <- load_figs ();


    let rec consume_events = fun () ->
        (match Sdl.poll_event () with
        | None -> ()
        | Some Sdl.Event.SDL_QUIT _ ->
            app.exit <- true;
        | Some Sdl.Event.SDL_KEYDOWN e ->
            if (List.mem e.keycode app.exit_keys) then
                app.exit <- true;
        | Some Sdl.Event.SDL_MOUSEBUTTONDOWN mb ->
            (match mb.mb_button with
            | 1 -> app.input.left_button_pressed <- true; app.input.changed <- true;
            | 2 -> app.input.middle_button_pressed <- true; app.input.changed <- true;
            | 3 -> app.input.right_button_pressed <- true; app.input.changed <- true;
            | _ -> ());
            consume_events ()
        | Some Sdl.Event.SDL_MOUSEBUTTONUP mb ->
            (match mb.mb_button with
            | 1 -> app.input.left_button_pressed <- false; app.input.changed <- true;
            | 2 -> app.input.middle_button_pressed <- false; app.input.changed <- true;
            | 3 -> app.input.right_button_pressed <- false; app.input.changed <- true;
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
            consume_events ());
    in

    while app.exit != true do
        consume_events ();
        if app.input.changed then (
            _print_input app.input;
            app.input.changed <- false;
            app.input.wheel <- 0;
        );
        Sdl.set_render_draw_color2 app.rdr app.clear_color;
        Sdl.render_clear app.rdr;
        Sdl.render_present app.rdr;
    done;

    release_figs ();
    Sdl.destroy_renderer app.rdr;
    Sdl.destroy_window app.win

