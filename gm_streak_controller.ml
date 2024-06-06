open Chesslibs

let move_forward () = Gm_streak_model.move_fwd ()
let move_backward () = Gm_streak_model.move_bwd ()
let can_pick_piece rank file = Gm_streak_model.can_pick_piece rank file
let quit () = Game_info.quit_requested := true

let new_game n =
  Gm_streak_model.start n

let player_move from_x from_y to_x to_y =
  if Move.is_valid from_x from_y to_x to_y (Gm_streak_model.current_position ()) then (
    if Gm_streak_model.player_move from_x from_y to_x to_y <> true then
      Gm_streak_model.set_streak_ended ())
  else Gm_streak_model.refresh_views ()
