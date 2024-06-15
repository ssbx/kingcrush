open Tsdl

let wait_for_events : bool ref = ref false
let quit_loop : bool ref = ref false
let pause_redraw : bool ref = ref false
let needs_redraw : bool ref = ref true
let with_audio : bool ref = ref true
let with_anims : bool ref = ref true
let game_len : int ref = ref 10

let streak_theme : Chess.Puzzles.theme_t ref = ref Chess.Puzzles.AnyTheme
let streak_rank : int ref = ref 1000

module Screen = struct
  let logical_w : int = 1600
  let logical_h : int = 1200
  let square_size : int = 177
  let board_size : int = 177 * 8
  let board_size_f : float = 177. *. 8.
  let logical_board_width : int = Float.to_int (Float.of_int logical_w *. 0.6)
  let logical_square_width : int =
    Float.to_int (Float.of_int logical_w *. 0.6 /. 8.)

  let board_rect : Sdl.rect =
    Sdl.Rect.create
      ~x:((logical_w / 2) - (logical_board_width / 2))
      ~y:((logical_h / 2) - (logical_board_width / 2))
      ~w:logical_board_width ~h:logical_board_width

  let score_rect : Sdl.rect = Sdl.Rect.create ~x:10 ~y:10 ~w:800 ~h:1000
  let board_ratio : float ref = ref (4. /. 3.)
  let board_x : int ref = ref 0
  let board_y : int ref = ref 0
  let board_padding : int ref = ref 10
end
