open Chesslib

let move_forward () = Versus_model.move_fwd ()
let move_backward () = Versus_model.move_bwd ()
let can_pick_piece rank file = Versus_model.can_pick_piece rank file
let quit () = Info.quit_loop := true
let new_game () = Versus_model.start ()

let player_move from_x from_y to_x to_y =
  if Move.is_valid from_x from_y to_x to_y (Versus_model.current_position ())
  then (
    if Versus_model.player_move from_x from_y to_x to_y <> true
    then Versus_model.set_streak_ended ())
  else Versus_model.refresh_views ()
;;

let interface : Info.ctrl_if =
  { move_forward; move_backward; can_pick_piece; player_move }
;;
