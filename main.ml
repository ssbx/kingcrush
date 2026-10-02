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

type app_t = {
    mutable rdr     : Sdl.Renderer.t;
    mutable win     : Sdl.Window.t;
    mutable figs    : figs_t;
    mutable ticks   : int;
    mutable delta   : int;
    mutable exit    : bool;
    mutable refresh : bool;
    exit_keys       : Sdl.Scancode.t list;
    figs_w          : int;
    data_path       : string;
    scr_w           : int;
    scr_h           : int;
}
let app : app_t = {
    rdr = Sdl.Renderer.null ();
    win = Sdl.Window.null ();
    figs = figs_empty;
    figs_w = 177;
    ticks = 0;
    delta = 0;
    exit    = false;
    exit_keys = [Sdl.Scancode.ESCAPE; Sdl.Scancode.Q ];
    refresh = true;
    data_path = "/home/seb/src/kingcrush/data";
    scr_w = 1240;
    scr_h = 780;

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

    Sdl.set_render_draw_color app.rdr ~r:0 ~g:0 ~b:0 ~a:255;

    while app.exit != true do
        match Sdl.poll_event () with
        | Some Sdl.Event.SDL_QUIT _ ->
            app.exit <- true;
        | Some Sdl.Event.SDL_KEYDOWN e ->
            if (List.mem e.scancode exit_keys) then
                app.exit <- true;
        | _ -> ();
        if app.refresh then (
            Sdl.render_clear app.rdr;
            Sdl.render_present app.rdr;
            app.refresh <- false;
        )
    done;

    release_figs ();
    Sdl.destroy_renderer app.rdr;
    Sdl.destroy_window app.win;

    app.rdr  <- Sdl.Renderer.null ();
    app.win  <- Sdl.Window.null ();
    app.figs <- figs_empty;
    print_endline "hello"
