open Tsdl
open Gamekit
open Gamekit.Anims
open Chess

let anim_time = 100
let anim_type = Easing.Quintic_out

type drag_t =
  | BDown of (Chess.piece_t * int * int)
  | BUp of (int * int)
  | BUpCancel

(* could divide theses states in submodules *)
type view_state_t = {
  (* renderer things *)
  mutable renderer : Sdl.renderer option;
  mutable pieces_text : Sdl.texture option;
  (* animation things *)
  mutable position : Chess.position_t;
  mutable anims_queue : Gamekit.Anims.anim_handle_t list;
  mutable anim_piece : Sdl.texture option;
  mutable anim_x : int;
  mutable anim_y : int;
  anim_rect : Sdl.rect;
  (* drag/drap things *)
  drag_rect : Sdl.rect;
  mutable cursor_x : int;
  mutable cursor_y : int;
  mutable drag_queue : drag_t list;
  mutable drag_from_rank : int;
  mutable drag_from_file : int;
  mutable drag_piece : Sdl.texture option;
  mutable drag_active : bool;
}

let view_state =
  {
    position = Chess.Utils.empty_position;
    renderer = None;
    drag_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0;
    anim_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0;
    pieces_text = None;
    cursor_x = 0;
    cursor_y = 0;
    anims_queue = [];
    anim_piece = None;
    anim_x = 0;
    anim_y = 0;
    drag_queue = [];
    drag_piece = None;
    drag_active = false;
    drag_from_file = 0;
    drag_from_rank = 0;
  }

(* ========================================================================= *)
(* utilities =============================================================== *)
(* ========================================================================= *)
let get_renderer () =
  match view_state.renderer with None -> failwith "norenderer" | Some r -> r

let get_pieces_text () =
  match view_state.pieces_text with
  | None -> failwith "no texture"
  | Some t -> t

let board_iter_xy func b =
  Array.iteri (fun y file -> Array.iteri (fun x ch -> func x y ch) file) b

(* ========================================================================= *)
(* renderer things ========================================================= *)
(* ========================================================================= *)
let update_board_texture board_pos =
  let rdr = get_renderer ()
  and tex = get_pieces_text ()
  and pw = Figures.piece_width in
  sdl_try (Sdl.set_render_target rdr (Some tex));
  sdl_try (Sdl.set_render_draw_color rdr 0 0 0 0);
  sdl_try (Sdl.render_clear rdr);
  board_iter_xy
    (fun x y ch ->
      match Figures.piece ch with
      | Some t ->
          let rect = Sdl.Rect.create ~x:(pw * x) ~y:(pw * y) ~w:pw ~h:pw in
          sdl_try (Sdl.render_copy ~dst:rect rdr t)
      | None -> ())
    board_pos;
  sdl_try (Sdl.set_render_target rdr None)

let init_pieces_texture () =
  let rdr = get_renderer () and bwidth = Figures.piece_width * 8 in
  let tex =
    sdl_get_ok
      (Sdl.create_texture rdr Sdl.Pixel.format_rgba8888 ~w:bwidth ~h:bwidth
         Sdl.Texture.access_target)
  in
  sdl_try (Sdl.set_texture_blend_mode tex Sdl.Blend.mode_blend);
  sdl_try (Sdl.set_render_target rdr (Some tex));
  sdl_try (Sdl.set_render_draw_color rdr 0 0 0 0);
  sdl_try (Sdl.render_clear rdr);
  sdl_try (Sdl.set_render_target rdr None);
  tex

(* ========================================================================= *)
(* position utils ========================================================== *)
(* ========================================================================= *)
let play_audio () =
  let curr_id = Streak_model.current_position_id () in
  let pos = Streak_model.position_at (curr_id - 1) in
  let mv = match pos.mv_next with Some v -> v | None -> assert false in
  if Chess.Utils.is_a_piece pos.board.(mv.to_x).(mv.to_y) then
    Audio.play Audio.Capture
  else Audio.play Audio.Move

let cleanup_view_state () =
  view_state.drag_active <- false;
  view_state.drag_piece <- None

let update_position () =
  cleanup_view_state ();
  view_state.position <- Streak_model.current_position ();
  update_board_texture view_state.position.board

let coords_to_square x y =
  let bw = Info.Screen.logical_board_width in
  let pw = bw / 8
  and bx = Sdl.Rect.x Info.Screen.board_rect
  and by = Sdl.Rect.y Info.Screen.board_rect in
  if x < bx || x > bx + bw || y < by || y > by + bw then None
  else
    let x_square = (x - bx) / pw and y_square = (y - by) / pw in
    Some (x_square, y_square)

let square_to_coords x y =
  let bw = Info.Screen.logical_board_width in
  let pw = bw / 8
  and bx = Sdl.Rect.x Info.Screen.board_rect
  and by = Sdl.Rect.y Info.Screen.board_rect in

  if x > 7 || y > 7 then failwith "should not !!!!"
  else
    let xc = bx + (x * pw) and yc = by + (y * pw) in
    (xc, yc)


(* ========================================================================= *)
(* moves animation ========================================================= *)
(* ========================================================================= *)
let update_anims () =
  if Option.is_none view_state.anim_piece then (
    match view_state.anims_queue with
    | [] -> update_position ()
    | anim :: tail ->
        view_state.anims_queue <- tail;
        Gamekit.Anims.start anim
  )

let create_anim ~x_src ~x_dst ~y_src ~y_dst ~board_start ~board_end ~piece ~fwd =
  let anim = Gamekit.Anims.create_v2
    ~pt1_start:x_src ~pt1_end:x_dst ~at1_update:(fun v -> view_state.anim_x <- v)
    ~pt2_start:y_src ~pt2_end:y_dst ~at2_update:(fun v -> view_state.anim_y <- v)
    ~span:anim_time
    ~at_start:(fun () ->
      view_state.anim_piece <- Figures.piece piece;
      update_board_texture board_start
    )
    ~at_end:(fun () ->
      view_state.anim_piece <- None;
      update_board_texture board_end;
      if fwd = true then play_audio ();
      update_anims ()) anim_type in
  view_state.anims_queue <- view_state.anims_queue @ [anim];
  update_anims ()


let anim_move from_pos_id to_pos_id =
  let get_mv m = match m with | Some v -> v | None -> failwith "nomove here!"
  and fwd      = from_pos_id < to_pos_id
  and from_pos = Streak_model.position_at from_pos_id
  and to_pos   = Streak_model.position_at to_pos_id in

  let x_src, y_src,
      x_dst, y_dst,
      board_start, board_end,
      piece =
    if fwd = true then (
      (* forward: startup position = from_pos minus mv.from piece *)
      let mv           = get_mv from_pos.mv_next in
      let piece        = from_pos.board.(mv.from_x).(mv.from_y)
      and x_src, y_src = square_to_coords mv.from_y mv.from_x
      and x_dst, y_dst = square_to_coords mv.to_y mv.to_x
      and board_end    = to_pos.board
      and board_start  = Chess.Utils.copy_board from_pos.board in
      board_start.(mv.from_x).(mv.from_y) <- '.';
      (x_src, y_src, x_dst, y_dst, board_start, board_end, piece)
    ) else (
      (* backard: startup position = bkward pos minus mv.from piece *)
      let mv           = get_mv from_pos.mv_last in
      let piece        = to_pos.board.(mv.from_x).(mv.from_y) in
      let x_src, y_src = square_to_coords mv.to_y mv.to_x
      and x_dst, y_dst = square_to_coords mv.from_y mv.from_x
      and board_end    = to_pos.board
      and board_start  = Chess.Utils.copy_board to_pos.board in
      board_start.(mv.from_x).(mv.from_y) <- '.';
      (x_src, y_src, x_dst, y_dst, board_start, board_end, piece)
    )
  in

  create_anim ~x_src ~x_dst ~y_src ~y_dst ~board_start ~board_end ~piece ~fwd


(* ========================================================================= *)
(* drag drop =============================================================== *)
(* ========================================================================= *)
(* todo use a drag / click things from gamekit, assez générique qui puisse
 être utilisé pour plein de basard *)
let drag_init piece rank file =
  let pos = Streak_model.current_position () in
  let board = Chess.Utils.copy_board pos.board in
  Board_hints.show pos rank file;
  board.(file).(rank) <- '.';
  view_state.drag_from_rank <- rank;
  view_state.drag_from_file <- file;
  view_state.drag_active <- true;
  view_state.drag_piece <- Figures.piece piece;
  update_board_texture board

let rec update_pick () =
  match view_state.drag_queue with
  | [] -> ()
  | BDown (piece, rank, file) :: t ->
      view_state.drag_queue <- t;
      drag_init piece rank file;
      update_pick ()
  | BUp (to_r, to_f) :: t ->
      if view_state.drag_active = true then (
        view_state.drag_active <- false;
        view_state.drag_piece <- None;
        view_state.drag_queue <- t;
        Board_hints.clear ();
        Streak_controller.player_move view_state.drag_from_rank
          view_state.drag_from_file to_r to_f);
      update_pick ()
  | BUpCancel :: t ->
      view_state.drag_active <- false;
      view_state.drag_piece <- None;
      view_state.drag_queue <- t;
      Board_hints.clear ();
      update_board_texture (Streak_model.current_position ()).board;
      update_pick ()

let queue_drag_event evt =
  view_state.drag_queue <- List.append view_state.drag_queue [ evt ]


(* ========================================================================= *)
(* input events ============================================================ *)
(* ========================================================================= *)

let handle_button1_down x y =
  match coords_to_square x y with
  | None -> ()
  | Some (rank, file) ->
      if Streak_model.player_turn () then
        let piece = view_state.position.board.(file).(rank) in
        if Streak_controller.can_pick_piece rank file then
          if Option.is_none view_state.anim_piece then drag_init piece rank file
          else queue_drag_event (BDown (piece, rank, file))

let handle_mouse_motion event =
  view_state.cursor_x <- Sdl.Event.(get event mouse_motion_x);
  view_state.cursor_y <- Sdl.Event.(get event mouse_motion_y)

let handle_mouse_wheel event =
  if Option.is_none view_state.anim_piece then
    match Sdl.Event.(get event mouse_wheel_y) with
    | 1 -> Streak_controller.move_backward ()
    | -1 -> Streak_controller.move_forward ()
    | _ -> ()

let handle_button1_up x y =
  Board_hints.clear ();
  match coords_to_square x y with
  | None ->
      view_state.drag_active <- false;
      view_state.drag_piece <- None;
      update_board_texture (Streak_model.current_position ()).board;
      update_pick ()
  | Some (to_rank, to_file) ->
      if view_state.drag_active then
        if Option.is_none view_state.anim_piece then (
          view_state.drag_active <- false;
          view_state.drag_piece <- None;
          Streak_controller.player_move view_state.drag_from_rank
            view_state.drag_from_file to_rank to_file)
        else queue_drag_event (BUp (to_rank, to_file))

let handle_sdl_event ~event =
  match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
  | `Mouse_motion -> handle_mouse_motion event
  | `Mouse_wheel -> handle_mouse_wheel event
  | `Mouse_button_up ->
      if Sdl.Event.(get event mouse_button_button) = 1 then
        let x = Sdl.Event.(get event mouse_button_x)
        and y = Sdl.Event.(get event mouse_button_y) in
        handle_button1_up x y
  | `Mouse_button_down ->
      if Sdl.Event.(get event mouse_button_button) = 1 then
        let x = Sdl.Event.(get event mouse_button_x)
        and y = Sdl.Event.(get event mouse_button_y) in
        handle_button1_down x y
  | _ -> ()

let handle_game_event = function
  | Streak_model.NewPuzzle -> update_position ()
  | Streak_model.PuzzleSolved ->
      update_position ();
      Audio.play Audio.PuzzleRushGood
  | Streak_model.OponentMove (from_pos, to_pos) ->
      if !Info.with_anims then anim_move from_pos to_pos
      else (
        play_audio ();
        update_position ())
  | Streak_model.PlayerMove (_, _) ->
      play_audio ();
      update_position ()
  | Streak_model.Update -> update_position ()
  | Streak_model.MoveForward (from_pos, to_pos) ->
      if !Info.with_anims then anim_move from_pos to_pos
      else (
        play_audio ();
        update_position ())
  | Streak_model.MoveBackward (from_pos, to_pos) ->
      if !Info.with_anims then anim_move from_pos to_pos
      else update_position ()
  | _ -> ()

(* ========================================================================= *)
(* game callbacks ========================================================== *)
(* ========================================================================= *)

let init ~renderer =
  view_state.renderer <- Some renderer;
  view_state.pieces_text <- Some (init_pieces_texture ());
  Sdl.Rect.set_w view_state.drag_rect Info.Screen.logical_square_width;
  Sdl.Rect.set_h view_state.drag_rect Info.Screen.logical_square_width;
  Sdl.Rect.set_w view_state.anim_rect Info.Screen.logical_square_width;
  Sdl.Rect.set_h view_state.anim_rect Info.Screen.logical_square_width;
  update_board_texture view_state.position.board

let draw ~renderer =
  let ptext = get_pieces_text () in
  sdl_try (Sdl.render_copy ~dst:Info.Screen.board_rect renderer ptext);
  (match view_state.anim_piece with
  | None -> ()
  | Some text -> (
    Sdl.Rect.set_x view_state.anim_rect view_state.anim_x;
    Sdl.Rect.set_y view_state.anim_rect view_state.anim_y;
    sdl_try (Sdl.render_copy ~dst:view_state.anim_rect renderer text)));
  (match (view_state.drag_active, view_state.drag_piece) with
  | false, _ -> ()
  | true, None -> failwith "drag active but no pieces to draw"
  | true, Some p ->
      let ps = Info.Screen.logical_square_width in
      let half_ps = ps / 2 in
      let x = view_state.cursor_x - half_ps
      and y = view_state.cursor_y - half_ps in
      Sdl.Rect.set_x view_state.drag_rect x;
      Sdl.Rect.set_y view_state.drag_rect y;
      sdl_try (Sdl.render_copy ~dst:view_state.drag_rect renderer p))

let update () = ()

let release () =
  Sdl.destroy_texture (get_pieces_text ());
  view_state.renderer <- None;
  view_state.pieces_text <- None
