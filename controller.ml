open Chesslibs

let move_forward () = Model.move_fwd ()
let move_backward () = Model.move_bwd ()
let can_pick_piece rank file = Model.can_pick_piece rank file
let quit () = Game_state.quit_requested := true

let new_game n =
  Model.start n

let player_move from_x from_y to_x to_y =
  if Move.is_valid from_x from_y to_x to_y (Model.current_position ()) then (
    if Model.player_move from_x from_y to_x to_y <> true then
      Model.set_streak_ended ())
  else Model.refresh_views ()
