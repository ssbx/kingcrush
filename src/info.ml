open Tsdl

let pref_dir : string ref = ref ""
let base_dir : string ref = ref ""
let data_dir : string ref = ref ""

let wait_for_events : bool ref = ref false
let quit_loop : bool ref = ref false
let pause_redraw : bool ref = ref false
let needs_redraw : bool ref = ref true
let with_audio : bool ref = ref true
let with_anims : bool ref = ref true
let game_len : int ref = ref 10

let streak_theme : Chess.Puzzles.theme_t ref = ref Chess.Puzzles.AnyTheme
let streak_rank : int ref = ref 1000

module Display = struct

  let logical_w : int = 1920
  let logical_h : int = 1080

  let padd_thin = 50
  let padd_large = 140

  let gen_board_rect thin_p large_p =
    let w = logical_h - 2 * thin_p in
    Sdl.Rect.create
      ~w:w ~h:w
      ~x:(logical_w - w - large_p)
      ~y:thin_p

  let board_rect : Sdl.rect = gen_board_rect padd_thin padd_large
  let gen_score_rect thin_p large_p =
    let w = logical_w - 2 * large_p - thin_p - (Sdl.Rect.w board_rect)
    and h = logical_h - 2 * thin_p in
    Sdl.Rect.create ~w ~h
      ~x:large_p
      ~y:thin_p

  let score_rect : Sdl.rect = gen_score_rect padd_thin padd_large

  let logical_board_width  : int = Sdl.Rect.w board_rect
  let logical_square_width : int = logical_board_width / 8

end
