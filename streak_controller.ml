open Chess

let move_forward () = Streak_model.move_fwd ()
let move_backward () = Streak_model.move_bwd ()
let can_pick_piece rank file = Streak_model.can_pick_piece rank file
let quit () = Info.quit_loop := true

let new_game n =
  Streak_model.start ~rank:!Info.streak_rank ~theme:!Info.streak_theme n

let player_move from_x from_y to_x to_y =
  if Move.is_valid from_x from_y to_x to_y (Streak_model.current_position ()) then (
    if Streak_model.player_move from_x from_y to_x to_y <> true then
      Streak_model.set_streak_ended ())
  else Streak_model.refresh_views ()
