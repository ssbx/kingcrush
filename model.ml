open Chesslibs

type game_event_t =
  | NewPuzzle
  | LevelComplete
  | GameOver
  | PuzzleSolved
  | OponentMove of (int * int)
  | PlayerMove of (int * int)
  | MoveForward of (int * int)
  | MoveBackward of (int * int)
  | Update

type game_state_t = {
  mutable views : (game_event_t -> unit) list;
  mutable puzzle : Puzzles.puzzle_t;
  mutable pos_id_max : int; (* the last move *)
  mutable pos_id_reached : int; (* the move the player must play *)
  mutable pos_id_visible : int;
  mutable solved : bool;
  mutable rush_ended : bool;
  mutable player_turn : bool;
  mutable streak : int;
  mutable ended : bool;
  mutable npuzzles : int;
}

let game_state =
  {
    views = [];
    puzzle = Puzzles.empty;
    pos_id_max = 0;
    pos_id_reached = 0;
    pos_id_visible = 0;
    solved = false;
    rush_ended = false;
    player_turn = false;
    streak = -1;
    ended = false;
    npuzzles = 0;
  }

let emit event = List.iter (fun f -> f event) game_state.views

let oponent_move () =
  let i = game_state.pos_id_reached in
  game_state.pos_id_reached <- i + 1;
  game_state.pos_id_visible <- game_state.pos_id_reached;
  game_state.player_turn <- true;
  emit (OponentMove (i, i + 1))

let load_next () =
  if game_state.npuzzles = 0 then (
    emit Update;
    emit LevelComplete)
  else (
    game_state.npuzzles <- game_state.npuzzles - 1;
    match Puzzles.next_puzzle () with
    | None -> print_endline "puzzle rush ended congrats!!"
    | Some puzzle ->
        game_state.puzzle <- puzzle;
        game_state.pos_id_reached <- 0;
        game_state.pos_id_visible <- 0;
        game_state.pos_id_max <- List.length puzzle.positions - 1;
        game_state.solved <- false;
        game_state.player_turn <- false;
        game_state.streak <- game_state.streak + 1;
        emit NewPuzzle;
        oponent_move ())

let refresh_views () = emit Update

let set_streak_ended () =
  emit Update;
  emit GameOver
(* ========================================================================= *)
(* various getters ========================================================= *)
(* ========================================================================= *)

let position_at n =
  if n >= 0 && n <= game_state.pos_id_max then
    List.nth game_state.puzzle.positions n
  else failwith "bad index arg for position_at"

let current_position () = position_at game_state.pos_id_visible
let current_position_id () = game_state.pos_id_visible
let player_turn () = game_state.player_turn
let get_solved () = game_state.solved
let get_rush_ended () = game_state.rush_ended
let get_streak () = game_state.streak
let get_puzzle () = game_state.puzzle
let get_rating () = game_state.puzzle.rating

let can_pick_piece rank file =
  if
    game_state.pos_id_visible = game_state.pos_id_reached
    && game_state.pos_id_reached != game_state.pos_id_max
  then
    let pos = current_position () in
    let p = pos.board.(file).(rank) in
    Chess.is_a_piece p && Chess.piece_color p = pos.active_player
  else false

(* ========================================================================= *)
(* navigate forward backward commands ====================================== *)
(* ========================================================================= *)
let move_bwd () =
  match game_state.pos_id_visible = 0 with
  | true -> ()
  | false ->
      let old_pos = game_state.pos_id_visible
      and new_pos = game_state.pos_id_visible - 1 in
      game_state.pos_id_visible <- new_pos;
      emit (MoveBackward (old_pos, new_pos))

let move_fwd () =
  match game_state.pos_id_visible < game_state.pos_id_reached with
  | false -> ()
  | true ->
      let old_pos = game_state.pos_id_visible
      and new_pos = game_state.pos_id_visible + 1 in
      game_state.pos_id_visible <- new_pos;
      emit (MoveForward (old_pos, new_pos))

(* ========================================================================= *)
(* actualy, puzzle logic is pretty small =================================== *)
(* ========================================================================= *)
let player_move_ok () =
  (* the position after the player move, oponent to play *)
  let pos = game_state.pos_id_reached in
  game_state.pos_id_reached <- pos + 1;
  game_state.pos_id_visible <- game_state.pos_id_reached;
  game_state.player_turn <- false;

  emit (PlayerMove (pos - 1, pos));

  (* maybe an oponent move next *)
  if game_state.pos_id_reached = game_state.pos_id_max then (
    game_state.solved <- true;
    emit PuzzleSolved;
    load_next ())
  else oponent_move ()

let player_move from_r from_f to_r to_f =
  if
    game_state.pos_id_visible = game_state.pos_id_reached
    && game_state.pos_id_reached <> game_state.pos_id_max
  then
    match (current_position ()).mv_next with
    | None -> assert false
    | Some mv ->
        if
          mv.from_x = from_f && mv.from_y = from_r && mv.to_x = to_f
          && mv.to_y = to_r
        then (
          player_move_ok ();
          true)
        else (
          emit Update;
          false)
  else (
    emit Update;
    false)

(* ========================================================================= *)
(* initialisation and close ================================================ *)
(* ========================================================================= *)

let start n =
  Puzzles.reset ~theme:Puzzles.AnyTheme ~start_rank:(-1);
  game_state.puzzle <- Puzzles.empty;
  game_state.pos_id_max <- 0;
  game_state.pos_id_reached <- 0;
  game_state.pos_id_visible <- 0;
  game_state.solved <- false;
  game_state.rush_ended <- false;
  game_state.streak <- -1;
  game_state.npuzzles <- n;
  load_next ()

let init () =
  let csv_file = Filename.concat (List.nth Data.Sites.data 0) "puzzles.csv" in
  Puzzles.init csv_file

let listen f = game_state.views <- game_state.views @ [ f ]
let release () = Puzzles.release ()
