open Fen

type puzzle_t = {
  lichess_id : string;
  rating : string;
  player_color : Types.color_t;
  positions : Types.position_t list;
}

let empty =
  {
    lichess_id = "none";
    rating = "0";
    player_color = Types.White;
    positions = [ Utils.empty_position ];
  }

type theme_t = AnyTheme | Theme of string

let current_theme : theme_t ref = ref AnyTheme

(* ========================================================================= *)
(* init csv file =========================================================== *)
(* ========================================================================= *)

let csv_chan : Stdlib.in_channel ref = ref Stdlib.stdin
let csv_size : int ref = ref 0
let random_step = 25000

(* ========================================================================= *)
(* csv file searchs ======================================================== *)
(* ========================================================================= *)

let rec go_nl ch = if Stdlib.input_char ch <> '\n' then go_nl ch

let rec seek_rank ~rank ~pos ~window =
  go_nl !csv_chan;
  let line = Stdlib.input_line !csv_chan in
  let splited = String.split_on_char ',' line in
  let rank_str = List.nth splited 3 in
  let rank_i = Stdlib.int_of_string rank_str in
  let nwin = window / 2 in
  if rank_i < rank then (
    let npos = pos + nwin in
    Stdlib.seek_in !csv_chan npos;
    if nwin > 0 then seek_rank ~rank ~pos:npos ~window:nwin)
  else if rank_i > rank then (
    let npos = pos - nwin in
    Stdlib.seek_in !csv_chan npos;
    if nwin > 0 then seek_rank ~rank ~pos:npos ~window:nwin)
  else ()

let init fname =
  Random.self_init ();
  csv_chan := Stdlib.open_in fname;
  csv_size := Stdlib.in_channel_length !csv_chan

let reset ~theme ~start_rank =
  current_theme := theme;
  if start_rank > 0 then (
    Stdlib.seek_in !csv_chan (!csv_size / 2);
    seek_rank ~rank:start_rank ~pos:(!csv_size / 2) ~window:(!csv_size / 2))
  else Stdlib.seek_in !csv_chan 0

let filter ~theme = current_theme := theme
let release () = Stdlib.close_in !csv_chan

let rec list_count_themes chan themes_acc theme_groups_acc prog =
  let prog_n = if prog = 1000 then ( Printf.printf ".%!"; 0) else (prog + 1) in
  match Stdlib.input_line !csv_chan with
  | exception End_of_file -> (themes_acc, theme_groups_acc)
  | line -> (
      let new_theme_groups = List.nth (String.split_on_char ',' line) 7 in
      let new_theme_groups_acc =
        match List.assoc_opt new_theme_groups theme_groups_acc with
        | None -> (new_theme_groups, 1) :: theme_groups_acc
        | Some n -> (new_theme_groups, n + 1) ::
          (List.remove_assoc new_theme_groups theme_groups_acc)
      in

      let new_themes = String.split_on_char ' ' new_theme_groups in
      let new_themes_acc = List.fold_left (fun acc kw ->
        match List.assoc_opt kw acc with
        | None -> (kw, 1) :: acc
        | Some n -> (kw, n + 1) :: (List.remove_assoc kw acc)
      ) themes_acc new_themes in
      list_count_themes chan new_themes_acc new_theme_groups_acc prog_n
  )

let themes_info () =
  Stdlib.seek_in !csv_chan 0;
  Printf.printf "themes_info: Collecting themes and theme groups: ";
  let (t, tg) = list_count_themes !csv_chan [] [] 0 in
  Printf.printf "\n";
  Printf.printf "themes_info: Sorting themes...\n%!";
  let t_sorted = List.sort (fun (_, x) (_, y) -> Int.compare y x) t in
  Printf.printf "themes_info: Sorting theme groups...\n%!";
  let tg_sorted = List.sort (fun (_, x) (_, y) -> Int.compare y x) tg in
  Stdlib.seek_in !csv_chan 0;
  Printf.printf "themes_info: Done!\n%!";
  (t_sorted, tg_sorted)


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
    let line = next_nth_line !csv_chan (Random.int random_step) in
    Printf.printf "loading: %s%!\n" line;
    match String.split_on_char ',' line with
    | id :: fen :: mvs :: rating :: _ ->
        let fen_pos = Fen.read fen in
        let player_color, positions =
          match fen_pos.to_play with
          | "w" ->
              ( Types.Black,
                Utils.reverse_positions
                  (Utils.positions_from_mvs fen_pos mvs Types.White) )
          | _ -> (Types.White, Utils.positions_from_mvs fen_pos mvs Types.Black)
        in
        Some { lichess_id = id; rating; player_color; positions }
    | _ -> failwith "not a valid csv file"
  with End_of_file -> None
