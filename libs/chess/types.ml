type color_t = Black | White
type board_t = char array array

type move_t = {
  from_x : int;
  from_y : int;
  to_x : int;
  to_y : int;
  promote_to : char;
}

type piece_t = char

type position_t = {
  board : board_t;
  mv_last : move_t option;
  mv_next : move_t option;
  active_player : color_t;
}
