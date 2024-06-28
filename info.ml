open Tsdl

type model_if = {
  mutable current_position_id : (unit -> int);
  mutable position_at : (int -> Chesslib.position_t);
  mutable current_position : (unit -> Chesslib.position_t);
  mutable player_turn : (unit -> bool);
}

let model : model_if = {
  current_position_id = (fun () -> 0);
  position_at = (fun _ -> Chesslib.empty_position);
  current_position = (fun () -> Chesslib.empty_position);
  player_turn = (fun () -> false);
}

let model_reset () =
  model.current_position_id <- (fun () -> 0);
  model.position_at <- (fun _ -> Chesslib.empty_position);
  model.current_position <- (fun () -> Chesslib.empty_position);
  model.player_turn <- (fun () -> false)

let model_set m =
  model.current_position_id <- m.current_position_id;
  model.position_at <- m.position_at;
  model.current_position <- m.current_position;
  model.player_turn <- m.player_turn

type ctrl_if = {
  mutable move_forward : (unit -> unit);
  mutable move_backward : (unit -> unit);
  mutable can_pick_piece : (int -> int -> bool);
  mutable player_move : (int -> int -> int -> int -> unit);
}

let ctrl : ctrl_if = {
  move_forward = (fun () -> ());
  move_backward = (fun () -> ());
  can_pick_piece = (fun _ _ -> false);
  player_move = (fun _ _ _ _ -> ());
}

let ctrl_reset () =
  ctrl.move_forward <- (fun () -> ());
  ctrl.move_backward <- (fun () -> ());
  ctrl.can_pick_piece <- (fun _ _ -> false);
  ctrl.player_move <- (fun _ _ _ _ -> ())

let ctrl_set i =
  ctrl.move_forward <- i.move_forward;
  ctrl.move_backward <- i.move_backward;
  ctrl.can_pick_piece <- i.can_pick_piece;
  ctrl.player_move <- i.player_move

let pref_dir : string ref = ref ""
let base_dir : string ref = ref ""

let wait_for_events : bool ref = ref false
let quit_loop : bool ref = ref false
let pause_redraw : bool ref = ref false
let needs_redraw : bool ref = ref true
let with_audio : bool ref = ref true
let game_len : int ref = ref 10

let streak_theme : Chesslib.Puzzles.theme_t ref = ref Chesslib.Puzzles.AnyTheme
let streak_rank : int ref = ref 1000

module Display = struct

  let logical_w : int = 1920
  let logical_h : int = 1080

  let padd_thin = 50
  let padd_large = 140

  let gen_board_rect thin_p large_p =
    let w = logical_h - 2 * thin_p in
    Sdl.Rect.create
      ~w:w ~h:w
      ~x:(logical_w - w - large_p)
      ~y:thin_p

  let board_rect : Sdl.rect = gen_board_rect padd_thin padd_large
  let gen_score_rect thin_p large_p =
    let w = logical_w - 2 * large_p - thin_p - (Sdl.Rect.w board_rect)
    and h = logical_h - 2 * thin_p in
    Sdl.Rect.create ~w ~h
      ~x:large_p
      ~y:thin_p

  let score_rect : Sdl.rect = gen_score_rect padd_thin padd_large

  let logical_board_width  : int = Sdl.Rect.w board_rect
  let logical_square_width : int = logical_board_width / 8

end
