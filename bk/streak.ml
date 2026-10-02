open Tsdl
open Utils
open Chess
open Ressources

module Model = struct

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
    else assert false

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

  let start ~rank ~theme n =
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

  let init () =
    let csv_file = Filename.concat !Conf.base_dir "puzzles.csv" in
    Puzzles.init csv_file

  let release () = Puzzles.release ()

  let register_callback f = game_state.views <- game_state.views @ [ f ]
  let clear_callback () = game_state.views <- []

  let generate_themes ~themes_file ~theme_groups_file =
    init ();
    let themes_chan = open_out themes_file
    and theme_groups_chan = open_out theme_groups_file
    and themes, theme_groups = Puzzles.themes_info () in
    List.iter (fun (t,n) ->
      Printf.fprintf themes_chan "%-15i %s\n" n t) themes;
    close_out themes_chan;
    List.iter (fun (t,n) ->
      Printf.fprintf theme_groups_chan "%-15i %s\n" n t) theme_groups;
    close_out theme_groups_chan;
    release ()

  let interface : Conf.model_if = {
    current_position_id;
    position_at;
    current_position;
    player_turn;
  }

end

module Hud = struct

  let rdr : Sdl.renderer option ref = ref None
  let bg_tex : Sdl.texture option ref = ref None
  let score_tex : Sdl.texture option ref = ref None
  let rating_tex : Sdl.texture option ref = ref None
  let score_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let rating_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  let get_rdr () = match !rdr with Some v -> v | None -> assert false
  let get_tex () = match !bg_tex with Some v -> v | None -> assert false

  let get_score_tex () =
    match !score_tex with Some v -> v | None -> assert false

  let get_rating_tex () =
    match !rating_tex with Some v -> v | None -> assert false

  let generate_msg ~renderer ~score ~rating =
    if Option.is_some !score_tex then (
      Sdl.destroy_texture (get_score_tex ());
      score_tex := None);
    if Option.is_some !rating_tex then (
      Sdl.destroy_texture (get_rating_tex ());
      score_tex := None);
    let score_texture =
      let surf = Fonts.get_surface score in
      let w, h = Sdl.get_surface_size surf in
      Sdl.Rect.set_w score_rect w;
      Sdl.Rect.set_h score_rect h;
      match Sdl.create_texture_from_surface renderer surf with
      | Error (`Msg _) -> assert false
      | Ok t ->
          Sdl.free_surface surf;
          t
    in
    let rating_texture =
      let surf = Fonts.get_surface rating in
      let w, h = Sdl.get_surface_size surf in
      Sdl.Rect.set_w rating_rect w;
      Sdl.Rect.set_h rating_rect h;
      match Sdl.create_texture_from_surface renderer surf with
      | Error (`Msg _) -> assert false
      | Ok t ->
          Sdl.free_surface surf;
          t
    in
    score_tex := Some score_texture;
    rating_tex := Some rating_texture

  let init ~renderer =
    let texture =
      sdl_get_ok
        (Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 ~w:100 ~h:100
           Sdl.Texture.access_target)
    in
    sdl_try (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
    sdl_try (Sdl.set_render_target renderer (Some texture));
    sdl_try (Sdl.set_render_draw_color renderer 100 100 100 155);
    sdl_try (Sdl.render_clear renderer);
    sdl_try (Sdl.set_render_target renderer None);
    bg_tex := Some texture;
    rdr := Some renderer

  let release () =
    if Option.is_some !score_tex then Sdl.destroy_texture (get_score_tex ());
    if Option.is_some !bg_tex then Sdl.destroy_texture (get_tex ());
    bg_tex := None;
    score_tex := None

  let draw ~renderer =
    sdl_try (Sdl.render_copy ~dst:Conf.Display.score_rect renderer (get_tex ()));

    Sdl.Rect.set_x score_rect (Sdl.Rect.x Conf.Display.score_rect + 10);
    Sdl.Rect.set_y score_rect (Sdl.Rect.y Conf.Display.score_rect + 10);
    sdl_try (Sdl.render_copy ~dst:score_rect renderer (get_score_tex ()));
    Sdl.Rect.set_x rating_rect (Sdl.Rect.x Conf.Display.score_rect + 10);
    Sdl.Rect.set_y rating_rect
      (Sdl.Rect.y Conf.Display.score_rect + 10 + Sdl.Rect.h score_rect);
    sdl_try (Sdl.render_copy ~dst:rating_rect renderer (get_rating_tex ()))

  let handle_game_event = function
    | Model.NewPuzzle ->
        let rating = Printf.sprintf "rating: %s" (Model.get_rating ())
        and score = Printf.sprintf "streak: %i" (Model.get_streak ()) in
        generate_msg ~renderer:(get_rdr ()) ~score ~rating
    | _ -> ()
end

module Controller = struct

  let move_forward () = Model.move_fwd ()
  let move_backward () = Model.move_bwd ()
  let can_pick_piece rank file = Model.can_pick_piece rank file
  let quit () = Conf.quit_loop := true

  let new_game n =
    Model.start ~rank:!Conf.streak_rank ~theme:!Conf.streak_theme n

  let player_move from_x from_y to_x to_y =
    if Move.is_valid from_x from_y to_x to_y (Model.current_position ()) then (
      if Model.player_move from_x from_y to_x to_y <> true then
        Model.set_streak_ended ())
    else Model.refresh_views ()


  let interface : Conf.controller_if = {
    move_forward;
    move_backward;
    can_pick_piece;
    new_game;
    player_move;
  }

end
