open Tsdl
open Chesslibs
open Gamekit.Utils
open Gamekit.Anims
open Ressources

let anim_time = 100
let anim_type = Easing.Quintic_out

type anim_direction_t = Forward | Backward

type piece_anim_t = {
  from_position : Chess.position_t;
  to_position : Chess.position_t;
  piece : Chess.piece_t;
  animation : Easing.anim_t;
  direction : anim_direction_t;
  mutable x : int;
  mutable y : int;
}

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
  mutable anims_queue : piece_anim_t list;
  mutable anim_running : piece_anim_t option;
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
    position = Chess.empty_position;
    renderer = None;
    drag_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0;
    anim_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0;
    pieces_text = None;
    cursor_x = 0;
    cursor_y = 0;
    anims_queue = [];
    anim_running = None;
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
  and pw = Pieces.piece_width in
  sdl_try (Sdl.set_render_target rdr (Some tex));
  sdl_try (Sdl.set_render_draw_color rdr 0 0 0 0);
  sdl_try (Sdl.render_clear rdr);
  board_iter_xy
    (fun x y ch ->
      match Pieces.piece ch with
      | Some t ->
          let rect = Sdl.Rect.create ~x:(pw * x) ~y:(pw * y) ~w:pw ~h:pw in
          sdl_try (Sdl.render_copy ~dst:rect rdr t)
      | None -> ())
    board_pos;
  sdl_try (Sdl.set_render_target rdr None)

let init_pieces_texture () =
  let rdr = get_renderer () and bwidth = Pieces.piece_width * 8 in
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

let new_board_width () =
  match Sdl.get_renderer_output_size (get_renderer ()) with
  | Error (`Msg err) -> failwith err
  | Ok (w, h) -> Stdlib.min w h

let draw ~renderer =
  let ptext = get_pieces_text () in
  sdl_try (Sdl.render_copy ~dst:Game_state.Screen.board_rect renderer ptext);
  (match view_state.anim_running with
  | None -> ()
  | Some anim -> (
      match Pieces.piece anim.piece with
      | None -> failwith "no anim piece"
      | Some piece ->
          Sdl.Rect.set_x view_state.anim_rect anim.x;
          Sdl.Rect.set_y view_state.anim_rect anim.y;
          sdl_try (Sdl.render_copy ~dst:view_state.anim_rect renderer piece)));
  (match (view_state.drag_active, view_state.drag_piece) with
  | false, _ -> ()
  | true, None -> failwith "drag active but no pieces to draw"
  | true, Some p ->
      let ps = Game_state.Screen.logical_square_width in
      let half_ps = ps / 2 in
      let x = view_state.cursor_x - half_ps
      and y = view_state.cursor_y - half_ps in
      Sdl.Rect.set_x view_state.drag_rect x;
      Sdl.Rect.set_y view_state.drag_rect y;
      sdl_try (Sdl.render_copy ~dst:view_state.drag_rect renderer p))

(* ========================================================================= *)
(* position utils ========================================================== *)
(* ========================================================================= *)
let play_audio () =
  let curr_id = Model.current_position_id () in
  let pos = Model.position_at (curr_id - 1) in
  let mv = match pos.mv_next with Some v -> v | None -> assert false in
  if Chess.is_a_piece pos.board.(mv.to_x).(mv.to_y) then
    Audio.play Audio.Capture
  else Audio.play Audio.Move

let cleanup_view_state () =
  view_state.drag_active <- false;
  view_state.drag_piece <- None

let update_position () =
  cleanup_view_state ();
  view_state.position <- Model.current_position ();
  update_board_texture view_state.position.board

let coords_to_square x y =
  let bw = Game_state.Screen.logical_board_width in
  let pw = bw / 8
  and bx = Sdl.Rect.x Game_state.Screen.board_rect
  and by = Sdl.Rect.y Game_state.Screen.board_rect in
  if x < bx || x > bx + bw || y < by || y > by + bw then None
  else
    let x_square = (x - bx) / pw and y_square = (y - by) / pw in
    Some (x_square, y_square)

let square_to_coords x y =
  let bw = Game_state.Screen.logical_board_width in
  let pw = bw / 8
  and bx = Sdl.Rect.x Game_state.Screen.board_rect
  and by = Sdl.Rect.y Game_state.Screen.board_rect in

  if x > 7 || y > 7 then failwith "should not !!!!"
  else
    let xc = bx + (x * pw) and yc = by + (y * pw) in
    (xc, yc)

(* ========================================================================= *)
(* moves animation ========================================================= *)
(* ========================================================================= *)

let update_anims () =
  match view_state.anim_running with
  | Some _ -> ()
  | None -> (
      match view_state.anims_queue with
      | [] ->
          update_position ()
      | anim :: tail ->
          let new_anim =
            { anim with animation = Easing.start anim.animation }
          in
          view_state.anim_running <- Some new_anim;
          view_state.anims_queue <- tail;
          update_board_texture new_anim.from_position.board)


let fupdate x y = view_state.anim_x <- x; view_state.anim_y <- y
let fended () = view_state.anim_running <- None; update_anims ()

let queue_anim anim =
  view_state.anims_queue <- List.append view_state.anims_queue [ anim ];
  update_anims ()

let anim_forward from_pos_id to_pos_id =
  cleanup_view_state ();
  (* get positions from game *)
  let from_pos = Model.position_at from_pos_id
  and to_pos = Model.position_at to_pos_id in

  let mv =
    match from_pos.mv_next with Some m -> m | None -> failwith "no mv next"
  in

  (* XXX inverted board and opengl axises XXX *)
  let x1, y1 = square_to_coords mv.from_y mv.from_x in
  let x2, y2 = square_to_coords mv.to_y mv.to_x in
  let start_pt : Easing.point_t = { x = x1; y = y1 }
  and end_pt : Easing.point_t = { x = x2; y = y2 } in

  let new_from_pos = Chess.copy_position from_pos in
  let piece = new_from_pos.board.(mv.from_x).(mv.from_y) in
  (* we have to remove the piece *)
  new_from_pos.board.(mv.from_x).(mv.from_y) <- '.';

  let anim =
    {
      from_position = new_from_pos;
      to_position = to_pos;
      animation =
        Easing.create ~anim_type ~start_point:start_pt ~end_point:end_pt
          ~duration_ms:anim_time ~fun_start:fupdate ~fun_update:fupdate;
      direction = Forward;
      piece;
      x = start_pt.x;
      y = start_pt.y;
    }
  in
  queue_anim anim

let anim_backward from_pos_id to_pos_id =
  cleanup_view_state ();

  let _from_pos = Model.position_at from_pos_id
  and to_bk_pos = Model.position_at to_pos_id in

  let mv =
    match to_bk_pos.mv_next with
    | None -> failwith "no mv next anim backward"
    | Some m -> m
  in

  (* anim back *)
  let x1, y1 = square_to_coords mv.from_y mv.from_x in
  let x2, y2 = square_to_coords mv.to_y mv.to_x in
  let end_pt : Easing.point_t = { x = x1; y = y1 }
  and start_pt : Easing.point_t = { x = x2; y = y2 } in

  let new_to_bk_pos = Chess.copy_position to_bk_pos in
  let piece = new_to_bk_pos.board.(mv.from_x).(mv.from_y) in
  new_to_bk_pos.board.(mv.from_x).(mv.from_y) <- '.';

  let anim =
    {
      from_position = new_to_bk_pos;
      to_position = to_bk_pos;
      animation =
        Easing.create ~anim_type ~start_point:start_pt ~end_point:end_pt
          ~duration_ms:anim_time ~fun_start:fupdate ~fun_update:fupdate;
      direction = Backward;
      piece;
      x = start_pt.x;
      y = start_pt.y;
    }
  in
  queue_anim anim

(* ========================================================================= *)
(* inputs ================================================================== *)
(* ========================================================================= *)

let drag_init piece rank file =
  let pos = Model.current_position () in
  let board = Chess.copy_board pos.board in
  Brd_hints.show pos rank file;
  board.(file).(rank) <- '.';
  view_state.drag_from_rank <- rank;
  view_state.drag_from_file <- file;
  view_state.drag_active <- true;
  view_state.drag_piece <- Pieces.piece piece;
  update_board_texture board

let rec update_pick () =
  match view_state.drag_queue with
  | [] -> ()
  | BDown (piece, rank, file) :: t ->
      view_state.drag_queue <- t;
      drag_init piece rank file;
      update_pick ()
  | BUp (to_r, to_f) :: t ->
      if view_state.drag_active then (
        view_state.drag_active <- false;
        view_state.drag_piece <- None;
        view_state.drag_queue <- t;
        Brd_hints.clear ();
        Controller.player_move view_state.drag_from_rank
          view_state.drag_from_file to_r to_f);
      update_pick ()
  | BUpCancel :: t ->
      view_state.drag_active <- false;
      view_state.drag_piece <- None;
      view_state.drag_queue <- t;
      Brd_hints.clear ();
      update_board_texture (Model.current_position ()).board;
      update_pick ()

let queue_drag_event evt =
  view_state.drag_queue <- List.append view_state.drag_queue [ evt ]

let handle_button1_down x y =
  match coords_to_square x y with
  | None -> ()
  | Some (rank, file) ->
      if Model.player_turn () then
        let piece = view_state.position.board.(file).(rank) in
        if Controller.can_pick_piece rank file then
          if view_state.anim_running = None then drag_init piece rank file
          else queue_drag_event (BDown (piece, rank, file))

let handle_mouse_motion event =
  view_state.cursor_x <- Sdl.Event.(get event mouse_motion_x);
  view_state.cursor_y <- Sdl.Event.(get event mouse_motion_y)

let handle_mouse_wheel event =
  if view_state.anim_running = None then
    match Sdl.Event.(get event mouse_wheel_y) with
    | 1 -> Controller.move_backward ()
    | -1 -> Controller.move_forward ()
    | _ -> ()

let handle_button1_up x y =
  Brd_hints.clear ();
  match coords_to_square x y with
  | None ->
      view_state.drag_active <- false;
      view_state.drag_piece <- None;
      update_board_texture (Model.current_position ()).board;
      update_pick ()
  | Some (to_rank, to_file) ->
      if view_state.drag_active then
        if view_state.anim_running = None then (
          view_state.drag_active <- false;
          view_state.drag_piece <- None;
          Controller.player_move view_state.drag_from_rank
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

(* ========================================================================= *)
(* model events ============================================================ *)
(* ========================================================================= *)
let handle_game_event = function
  | Model.NewPuzzle -> update_position ()
  | Model.PuzzleSolved ->
      update_position ();
      Audio.play Audio.PuzzleRushGood
  | Model.OponentMove (from_pos, to_pos) ->
      if !Game_state.with_anims then anim_forward from_pos to_pos
      else (
        play_audio ();
        update_position ())
  | Model.PlayerMove (_, _) ->
      play_audio ();
      update_position ()
  | Model.Update -> update_position ()
  | Model.MoveForward (from_pos, to_pos) ->
      if !Game_state.with_anims then anim_forward from_pos to_pos
      else (
        play_audio ();
        update_position ())
  | Model.MoveBackward (from_pos, to_pos) ->
      if !Game_state.with_anims then anim_backward from_pos to_pos
      else update_position ()
  | _ -> ()

let update () =
  match view_state.anim_running with
  | None -> ()
  | Some anim -> (
      match Easing.animate anim.animation !Game_state.ticks with
      | Easing.AnimEnded (_x, _y) ->
          view_state.anim_running <- None;
          update_board_texture anim.to_position.board;
          if anim.direction = Forward then play_audio ();
          update_anims ();
          if view_state.anim_running = None then update_pick ()
      | Easing.AnimActive (x, y) ->
          anim.x <- x;
          anim.y <- y)


(* ========================================================================= *)
(* init/release ============================================================ *)
(* ========================================================================= *)
let init ~renderer =
  view_state.renderer <- Some renderer;
  view_state.pieces_text <- Some (init_pieces_texture ());
  Sdl.Rect.set_w view_state.drag_rect Game_state.Screen.logical_square_width;
  Sdl.Rect.set_h view_state.drag_rect Game_state.Screen.logical_square_width;
  Sdl.Rect.set_w view_state.anim_rect Game_state.Screen.logical_square_width;
  Sdl.Rect.set_h view_state.anim_rect Game_state.Screen.logical_square_width;
  update_board_texture view_state.position.board

let release () =
  Sdl.destroy_texture (get_pieces_text ());
  view_state.renderer <- None;
  view_state.pieces_text <- None
