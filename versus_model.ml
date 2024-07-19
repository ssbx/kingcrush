open Chesslib

type game_state_t =
  { mutable views : (Events.t -> unit) list
  ; mutable puzzle : Puzzles.puzzle_t
  ; mutable pos_id_max : int (* the last move *)
  ; mutable pos_id_reached : int (* the move the player must play *)
  ; mutable pos_id_visible : int
  ; mutable solved : bool
  ; mutable rush_ended : bool
  ; mutable player_turn : bool
  ; mutable streak : int
  ; mutable ended : bool
  ; mutable npuzzles : int
  }

let game_state =
  { views = []
  ; puzzle = Puzzles.empty
  ; pos_id_max = 0
  ; pos_id_reached = 0
  ; pos_id_visible = 0
  ; solved = false
  ; rush_ended = false
  ; player_turn = false
  ; streak = -1
  ; ended = false
  ; npuzzles = 0
  }
;;

let emit event = List.iter (fun f -> f event) game_state.views

let oponent_move () =
  let i = game_state.pos_id_reached in
  game_state.pos_id_reached <- i + 1;
  game_state.pos_id_visible <- game_state.pos_id_reached;
  game_state.player_turn <- true;
  emit (Events.OponentMove (i, i + 1))
;;

let load_next () =
  if game_state.npuzzles = 0
  then (
    emit Events.Update;
    emit Events.LevelComplete)
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
      emit Events.NewPuzzle;
      oponent_move ())
;;

let refresh_views () = emit Events.Update

let set_streak_ended () =
  emit Events.Update;
  emit Events.GameOver
;;

(* ========================================================================= *)
(* various getters ========================================================= *)
(* ========================================================================= *)

let position_at n =
  if n >= 0 && n <= game_state.pos_id_max
  then List.nth game_state.puzzle.positions n
  else failwith "bad index arg for position_at"
;;

let current_position () = position_at game_state.pos_id_visible
let current_position_id () = game_state.pos_id_visible
let player_turn () = game_state.player_turn
let get_solved () = game_state.solved
let get_rush_ended () = game_state.rush_ended
let get_streak () = game_state.streak
let get_puzzle () = game_state.puzzle
let get_rating () = game_state.puzzle.rating

let can_pick_piece rank file =
  if game_state.pos_id_visible = game_state.pos_id_reached
     && game_state.pos_id_reached != game_state.pos_id_max
  then (
    let pos = current_position () in
    let p = pos.board.(file).(rank) in
    Chesslib.Utils.is_a_piece p && Chesslib.Utils.piece_color p = pos.active_player)
  else false
;;

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
    emit (Events.MoveBackward (old_pos, new_pos))
;;

let move_fwd () =
  match game_state.pos_id_visible < game_state.pos_id_reached with
  | false -> ()
  | true ->
    let old_pos = game_state.pos_id_visible
    and new_pos = game_state.pos_id_visible + 1 in
    game_state.pos_id_visible <- new_pos;
    emit (Events.MoveForward (old_pos, new_pos))
;;

(* ========================================================================= *)
(* actualy, puzzle logic is pretty small =================================== *)
(* ========================================================================= *)
let player_move_ok () =
  (* the position after the player move, oponent to play *)
  let pos = game_state.pos_id_reached in
  game_state.pos_id_reached <- pos + 1;
  game_state.pos_id_visible <- game_state.pos_id_reached;
  game_state.player_turn <- false;
  emit (Events.PlayerMove (pos - 1, pos));
  (* maybe an oponent move next *)
  if game_state.pos_id_reached = game_state.pos_id_max
  then (
    game_state.solved <- true;
    emit Events.PuzzleSolved;
    load_next ())
  else oponent_move ()
;;

let player_move from_r from_f to_r to_f =
  if game_state.pos_id_visible = game_state.pos_id_reached
     && game_state.pos_id_reached <> game_state.pos_id_max
  then (
    match (current_position ()).mv_next with
    | None -> assert false
    | Some mv ->
      if mv.from_x = from_f && mv.from_y = from_r && mv.to_x = to_f && mv.to_y = to_r
      then (
        player_move_ok ();
        true)
      else (
        emit Events.Update;
        false))
  else (
    emit Events.Update;
    false)
;;

(* ========================================================================= *)
(* initialisation and close ================================================ *)
(* ========================================================================= *)

(*
   let start ~rank ~theme n = ()
   Puzzles.reset ~theme ~start_rank:rank;
   game_state.puzzle <- Puzzles.empty;
   game_state.pos_id_max <- 0;
   game_state.pos_id_reached <- 0;
   game_state.pos_id_visible <- 0;
   game_state.solved <- false;
   game_state.rush_ended <- false;
   game_state.streak <- -1;
   game_state.npuzzles <- n;
   load_next ()
*)
let start () = ()

let init () =
  let csv_file = Filename.concat !Info.base_dir "puzzles.csv" in
  Puzzles.init csv_file
;;

let release () = Puzzles.release ()
let register_callback f = game_state.views <- game_state.views @ [ f ]
let clear_callback () = game_state.views <- []

let generate_themes ~themes_file ~theme_groups_file =
  init ();
  let themes_chan = open_out themes_file
  and theme_groups_chan = open_out theme_groups_file
  and themes, theme_groups = Puzzles.themes_info () in
  List.iter (fun (t, n) -> Printf.fprintf themes_chan "%-15i %s\n" n t) themes;
  close_out themes_chan;
  List.iter (fun (t, n) -> Printf.fprintf theme_groups_chan "%-15i %s\n" n t) theme_groups;
  close_out theme_groups_chan;
  release ()
;;

let interface : Info.model_if =
  { current_position_id; position_at; current_position; player_turn }
;;
