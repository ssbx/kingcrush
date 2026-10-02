open CamlSDL2


type figs_t = {
    mutable bP : Sdl.texture;
    mutable wP : Sdl.texture;
    mutable bN : Sdl.texture;
    mutable wN : Sdl.texture;
    mutable bB : Sdl.texture;
    mutable wB : Sdl.texture;
    mutable bR : Sdl.texture;
    mutable wR : Sdl.texture;
    mutable bQ : Sdl.texture;
    mutable wQ : Sdl.texture;
    mutable bK : Sdl.texture;
    mutable wK : Sdl.texture;
}

type app_t = {
    mutable rdr : Sdl.Renderer.t;
    mutable win : Sdl.Window.t;
    scr_w : int;
    scr_h : int;
    img_figs : figs_t;
}

let app : app_t = {
    rdr = Sdl.Renderer.null ();
    win = Sdl.Window.null ();
    scr_w = 1240;
    scr_h = 780;
    img_figs = {
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
             Sdl.RendererFlags.PresentVSync;];


    Sdl.destroy_renderer app.rdr;
    Sdl.destroy_window app.win;
    print_endline "hello"
