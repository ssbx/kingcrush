open CamlSDL2


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
    mutable rdr : Sdl.Renderer.t;
    mutable win : Sdl.Window.t;
    mutable figs : figs_t;
    mutable base_path : string;
    scr_w : int;
    scr_h : int;
}

let app : app_t = {
    rdr = Sdl.Renderer.null ();
    win = Sdl.Window.null ();
    figs = figs_empty;
    base_path = "";
    scr_w = 1240;
    scr_h = 780;
}




let () =
    Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
    Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
    Sdl.set_hint "SDL_RENDER_VSYNC" "1";
    app.base_path <- Sdl.get_base_path () |> Filename.dirname;
    print_endline "hh";
    print_endline app.base_path;
    print_endline "hh";
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
             Sdl.RendererFlags.PresentVSync;];


    Sdl.destroy_renderer app.rdr;
    Sdl.destroy_window app.win;
    print_endline "hello"
