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

let rec list_count_themes chan tht ght prog n =
  let prog_n = if prog = 1000 then (Printf.printf ".%!"; 0) else ( prog + 1 ) in
  match Stdlib.input_line !csv_chan with
  | exception End_of_file -> ()
  | line -> (
      let new_tg = List.nth (String.split_on_char ',' line) 7 in
      begin match Hashtbl.find_opt ght new_tg with
        | None -> Hashtbl.add ght new_tg 1
        | Some n ->
            Hashtbl.remove ght new_tg;
            Hashtbl.add ght new_tg (n + 1)
      end;
      let new_t = String.split_on_char ' ' new_tg in
      List.iter (fun kw ->
        match Hashtbl.find_opt tht kw with
        | None -> Hashtbl.add tht kw 1
        | Some n ->
          Hashtbl.remove tht kw;
          Hashtbl.add tht kw (n + 1)
      ) new_t;
      list_count_themes chan tht ght prog_n (n + 1)
  )

let themes_info () =
  Stdlib.seek_in !csv_chan 0;
  Printf.printf "themes_info: Collecting themes and theme groups: \n";
  let t_h = Hashtbl.create 1000
  and tg_h = Hashtbl.create 1000 in
  list_count_themes !csv_chan t_h tg_h 0 0;
  let t_l = List.of_seq (Hashtbl.to_seq t_h)
  and tg_l = List.of_seq (Hashtbl.to_seq tg_h) in
  Printf.printf "\n";
  Printf.printf "themes_info: Sorting themes...\n%!";
  let t_l_s = List.sort (fun (_, x) (_, y) -> Int.compare y x) t_l in
  Printf.printf "themes_info: Sorting theme groups...\n%!";
  let tg_l_s = List.sort (fun (_, x) (_, y) -> Int.compare y x) tg_l in
  Stdlib.seek_in !csv_chan 0;
  Printf.printf "themes_info: Done!\n%!";
  (t_l_s, tg_l_s)


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
