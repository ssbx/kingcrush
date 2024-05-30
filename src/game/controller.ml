open Chesslibs

let move_forward () = Model.move_fwd ()
let move_backward () = Model.move_bwd ()
let can_pick_piece rank file = Model.can_pick_piece rank file
let quit () = State.quit_requested := true

let new_game ?(timeout = true) n =
  (*Level_details.anim_out ();*)
  Audio.music_stop ();
  let start_fun = (fun () ->
    Sounds.play Audio.LevelStart;
    Model.start n
  ) in
  if timeout then
    Timer.fire_in 1000 start_fun
  else
    start_fun ()

let player_move from_x from_y to_x to_y =
  if Move.is_valid from_x from_y to_x to_y (Model.current_position ()) then (
    if Model.player_move from_x from_y to_x to_y <> true then
      Model.set_streak_ended ())
  else Model.refresh_views ()
