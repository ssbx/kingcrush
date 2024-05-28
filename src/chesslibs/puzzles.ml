open Fen

type puzzle_t = {
  lichess_id : string;
  rating : string;
  player_color : Chess.color_t;
  positions : Chess.position_t list;
}

let empty =
  {
    lichess_id = "none";
    rating = "0";
    player_color = Chess.White;
    positions = [ Chess.empty_position ];
  }

type theme_t = AnyTheme | Theme of string

let current_theme : theme_t ref = ref AnyTheme

(* ========================================================================= *)
(* init csv file =========================================================== *)
(* ========================================================================= *)

let csv_file = Filename.concat (List.nth Assets.Sites.data 0) "puzzles.csv"
let csv_chan = Stdlib.open_in csv_file
let csv_size = Stdlib.in_channel_length csv_chan
let random_step = 25000

(* ========================================================================= *)
(* csv file searchs ======================================================== *)
(* ========================================================================= *)

let rec go_nl ch = if Stdlib.input_char ch <> '\n' then go_nl ch

let rec seek_rank ~rank ~pos ~window =
  go_nl csv_chan;
  let line = Stdlib.input_line csv_chan in
  let splited = String.split_on_char ',' line in
  let rank_str = List.nth splited 3 in
  let rank_i = Stdlib.int_of_string rank_str in
  let nwin = window / 2 in
  if rank_i < rank then (
    let npos = pos + nwin in
    Stdlib.seek_in csv_chan npos;
    if nwin > 0 then seek_rank ~rank ~pos:npos ~window:nwin)
  else if rank_i > rank then (
    let npos = pos - nwin in
    Stdlib.seek_in csv_chan npos;
    if nwin > 0 then seek_rank ~rank ~pos:npos ~window:nwin)
  else ()

let init () = Random.self_init ()

let reset ~theme ~start_rank =
  current_theme := theme;
  if start_rank > 0 then (
    Stdlib.seek_in csv_chan (csv_size / 2);
    seek_rank ~rank:start_rank ~pos:(csv_size / 2) ~window:(csv_size / 2))
  else Stdlib.seek_in csv_chan 0

let filter ~theme = current_theme := theme
let release () = Stdlib.close_in csv_chan

let list_themes () =
  Stdlib.seek_in csv_chan 0;
  let rec search_th l =
    match Stdlib.input_line csv_chan with
    | exception End_of_file -> l
    | line ->
        let splited = String.split_on_char ',' line in
        let themes_str = List.nth splited 7 in
        let themes = String.split_on_char ' ' themes_str in
        let add_theses =
          List.filter
            (fun in_kw ->
              List.exists (fun kw -> String.equal in_kw kw) l = false)
            themes
        in
        search_th (l @ add_theses)
  in
  Stdlib.seek_in csv_chan 0;
  search_th []

(* ========================================================================= *)
(* get puzzles ============================================================= *)
(* ========================================================================= *)
let rec next_nth_line chan i =
  match i with
  | 0 -> (
      let line = Stdlib.input_line chan in
      match !current_theme with
      | AnyTheme -> line
      | Theme th ->
          let themes =
            String.split_on_char ' '
              (List.nth (String.split_on_char ',' line) 7)
          in
          if List.exists (fun kw -> String.equal th kw) themes then line
          else next_nth_line chan 0)
  | _ ->
      let _ = Stdlib.input_line chan in
      next_nth_line chan (i - 1)

let next_puzzle () =
  try
    let line = next_nth_line csv_chan (Random.int random_step) in
    Printf.printf "loading: %s%!\n" line;
    match String.split_on_char ',' line with
    | id :: fen :: mvs :: rating :: _ ->
        let fen_pos = Fen.read fen in
        let player_color, positions =
          match fen_pos.to_play with
          | "w" ->
              ( Chess.Black,
                Chess.reverse_positions
                  (Chess.positions_from_mvs fen_pos mvs Chess.White) )
          | _ -> (Chess.White, Chess.positions_from_mvs fen_pos mvs Chess.Black)
        in
        Some { lichess_id = id; rating; player_color; positions }
    | _ -> failwith "not a valid csv file"
  with End_of_file -> None
