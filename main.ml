open CamlSDL2
open CamlSDL2_image


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

type app_t = {
    in_changed               : bool;
    in_left_button_pressed   : bool;
    in_middle_button_pressed : bool;
    in_right_button_pressed  : bool;
    in_wheel                 : int;
    in_pos_x                 : int;
    in_pos_y                 : int;
    exit                     : bool;
    ticks                    : int;
    delta                    : int;
}

module Consts = struct
    module Scr = struct
        let w = 1240
        let h = 780
    end
    module Col = struct
        let clear = Sdl.Color.make ~r:60 ~g:60 ~b:60 ~a:255
        let black = Sdl.Color.make ~r:181 ~g:136 ~b:99 ~a:255
        let white = Sdl.Color.make ~r:240 ~g:217 ~b:181 ~a:255
    end

    module Ui = struct
        let figs_width = 177
        let board_margin = 10
        let board_width = figs_width * 8
        let board_dst_rect  = Sdl.Rect.make
            ~x:board_margin
            ~y:board_margin
            ~w:(780 - 2 * board_margin)
            ~h:(780 - 2 * board_margin)
        let drag_dst_width = (780 - 2 * board_margin) / 8
    end
    let data_path = "/home/seb/src/kingcrush/data"
    let exit_keys = [Sdl.Keycode.Escape; Sdl.Keycode.Q]
end
module C = Consts

let _print_input app =

    let bool_to_int_str = fun v -> Bool.to_int v |> Int.to_string in
    print_endline (
        "input="   ^
        " left:"   ^ (bool_to_int_str app.in_left_button_pressed)   ^
        " middle:" ^ (bool_to_int_str app.in_middle_button_pressed) ^
        " right:"  ^ (bool_to_int_str app.in_right_button_pressed)  ^
        " pos_x:"  ^ (Int.to_string app.in_pos_x) ^
        " pos_y:"  ^ (Int.to_string app.in_pos_y) ^
        " wheel:"  ^ (Int.to_string app.in_wheel))

let rec process_events (app : app_t) =
    match Sdl.poll_event () with
    | Some Sdl.Event.SDL_QUIT _ -> (
        {app with exit = true}
    )

    | Some Sdl.Event.SDL_KEYDOWN e -> (
        if (List.mem e.keycode Consts.exit_keys) then
            {app with exit = true}
        else
            process_events app
    )

    | Some Sdl.Event.SDL_MOUSEBUTTONDOWN mb -> (
        match mb.mb_button with
        | 1 -> (
            process_events
                {app with in_left_button_pressed = true; in_changed = true}
        )
        | 2 -> (
            process_events
                {app with in_middle_button_pressed = true; in_changed = true}
        )
        | 3 -> (
            process_events
                {app with in_right_button_pressed = true; in_changed = true}
        )
        | _ -> (
            process_events app
        )
    )

    | Some Sdl.Event.SDL_MOUSEBUTTONUP mb -> (
        match mb.mb_button with
        | 1 -> (
            process_events
                {app with in_left_button_pressed = false; in_changed = true}
        )
        | 2 -> (
            process_events
                {app with in_middle_button_pressed = false; in_changed = true}
        )
        | 3 -> (
            process_events
                {app with in_right_button_pressed = false; in_changed = true}
        )
        | _ -> (
            process_events app
        )
    )

    | Some Sdl.Event.SDL_MOUSEWHEEL mw -> (
        process_events
            {app with in_wheel = mw.mw_y; in_changed = true}
    )

    | Some Sdl.Event.SDL_MOUSEMOTION mm -> (
        process_events
            {app with in_pos_x = mm.mm_x; in_pos_y = mm.mm_y; in_changed = true}
    )
    | Some _ -> (
        process_events app
    )

    | None -> (
        app
    )


let () =

    Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
    Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
    Sdl.set_hint "SDL_RENDER_VSYNC" "1";

    let win = Sdl.create_window
        ~title:"KingCrush"
        ~x:`centered
        ~y:`centered
        ~width:C.Scr.w
        ~height:C.Scr.h
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

    let cat = Filename.concat in


    let make_tex_rect = fun ix iy ->
        Sdl.Rect.make
            ~x:(ix * C.Ui.figs_width)
            ~y:(iy * C.Ui.figs_width)
            ~w:C.Ui.figs_width
            ~h:C.Ui.figs_width
    in

    let make_drag_rect = fun x y ->
        Sdl.Rect.make
            ~x:(x - C.Ui.drag_dst_width / 2)
            ~y:(y - C.Ui.drag_dst_width / 3 * 2)
            ~w:C.Ui.drag_dst_width
            ~h:C.Ui.drag_dst_width
    in

    (* figs imgs *)
    let load_fig = fun fig ->
        let fname = cat (cat (cat C.data_path "pieces") "default") fig in
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

    (* board tex *)
    let board_tex =
        Sdl.create_texture rdr
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:C.Ui.board_width
        ~height:C.Ui.board_width
    in

    Sdl.set_render_target rdr (Some board_tex);
    Sdl.set_render_draw_color2 rdr C.Col.white;
    Sdl.render_clear rdr;
    Sdl.set_render_draw_color2 rdr C.Col.black;

    for i = 0 to 7 do
        for j = 0 to 7 do
            if Int.rem (i + j + 1) 2 = 0 then
                Sdl.render_fill_rect rdr (make_tex_rect i j)
        done
    done;

    Sdl.set_render_target rdr None;


    (* hints tex *)
    let hint_tex_fname = cat (cat C.data_path "images") "hint.png" in
    let hint_img = Img.load_texture rdr ~filename:hint_tex_fname in
    let hints_tex =
        Sdl.create_texture rdr
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:C.Ui.board_width
        ~height:C.Ui.board_width
    in

    Sdl.set_texture_blend_mode hints_tex Sdl.BlendMode.BLEND;
    Sdl.set_render_target rdr (Some hints_tex);
    Sdl.set_render_draw_color rdr ~r:0 ~g:0 ~b:0 ~a:0;
    Sdl.render_clear rdr;

    for i = 0 to 7 do
        for j = 0 to 7 do
            Sdl.render_copy rdr
                ~texture:hint_img
                ~srcrect:None
                ~dstrect:(Some (make_tex_rect i j));
        done
    done;

    Sdl.set_render_target rdr None;


    (* pieces tex *)
    let pieces_tex =
        Sdl.create_texture rdr
        ~fmt:Sdl.PixelFormat.RGBA8888
        ~access:Sdl.TextureAccess.Target
        ~width:C.Ui.board_width
        ~height:C.Ui.board_width
    in

    Sdl.set_texture_blend_mode pieces_tex Sdl.BlendMode.BLEND;
    Sdl.set_render_target rdr (Some pieces_tex);
    Sdl.set_render_draw_color rdr ~r:0 ~g:0 ~b:0 ~a:0;
    Sdl.render_clear rdr;

    Sdl.render_copy rdr
        ~texture:figs.wK
        ~srcrect:None
        ~dstrect:(Some (make_tex_rect 4 7));

    Sdl.render_copy rdr
        ~texture:figs.bK
        ~srcrect:None
        ~dstrect:(Some (make_tex_rect 4 0));

    Sdl.set_render_target rdr None;

    let texs = {
        board  = board_tex;
        pieces = pieces_tex;
        hints  = hints_tex;
        map    = Sdl.Texture.null ();
        menu   = Sdl.Texture.null ();
    } in

    let app = {
        in_changed               = false;
        in_left_button_pressed   = false;
        in_middle_button_pressed = false;
        in_right_button_pressed  = false;
        in_wheel                 = 0;
        in_pos_x                 = 0;
        in_pos_y                 = 0;
        exit                     = false;
        ticks                    = 0;
        delta                    = 0;
    } in

    let rec loop app =

        if app.exit = true then () else (

            let app2 = process_events app in

            if app2.in_changed = true then _print_input app2;

            Sdl.set_render_target rdr None;
            Sdl.set_render_draw_color2 rdr C.Col.clear;
            Sdl.render_clear rdr;
            Sdl.render_copy rdr
                ~texture:texs.board
                ~srcrect:None
                ~dstrect:(Some C.Ui.board_dst_rect);
            Sdl.render_copy rdr
                ~texture:texs.hints
                ~srcrect:None
                ~dstrect:(Some C.Ui.board_dst_rect);
            Sdl.render_copy rdr
                ~texture:texs.pieces
                ~srcrect:None
                ~dstrect:(Some C.Ui.board_dst_rect);

            (* drag test *)
            if app2.in_left_button_pressed then (
                Sdl.render_copy rdr
                    ~texture:figs.wP
                    ~srcrect:None
                    ~dstrect:(Some (make_drag_rect app2.in_pos_x app2.in_pos_y))
            );

            Sdl.render_present rdr;

            loop {app2 with in_changed = false; in_wheel = 0}
        )

    in

    loop app;

    let destroy_tex = fun tx ->
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
    destroy_tex texs.hints;
    destroy_tex texs.pieces;

    Sdl.destroy_renderer rdr;
    Sdl.destroy_window win;
    Sdl.quit ()

