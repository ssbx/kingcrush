
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

module Fen = struct
  type fen_pos_t = {
    orig_str : string;
    fen_board : char array array;
    to_play : string;
    castling : string;
    en_passant : string;
    half_move : string;
    full_move : string;
  }

  let decode_char = function
    | '1' -> `Num 1
    | '2' -> `Num 2
    | '3' -> `Num 3
    | '4' -> `Num 4
    | '5' -> `Num 5
    | '6' -> `Num 6
    | '7' -> `Num 7
    | '8' -> `Num 8
    | ch -> `Char ch

  let rec fill_board y board rows =
    match rows with
    | [] -> board
    | row :: tail ->
        let x = ref 0 in
        String.iter
          (fun ch ->
            match decode_char ch with
            | `Num n -> x := !x + n
            | `Char _ ->
                board.(y).(!x) <- ch;
                x := !x + 1)
          row;
        fill_board (y + 1) board tail

  let gen_board str =
    fill_board 0 (Array.make_matrix 8 8 '.') (String.split_on_char '/' str)

  let print fen =
    Printf.printf "orig_str: %s\n" fen.orig_str;
    Printf.printf "to_play: %s\n" fen.to_play;
    Printf.printf "castling: %s\n" fen.castling;
    Printf.printf "en_passant: %s\n" fen.en_passant;
    Printf.printf "half_move: %s\n" fen.half_move;
    Printf.printf "full_move: %s\n" fen.full_move;
    Printf.printf "   y 0  1  2  3  4  5  6  7\n";
    Printf.printf " x ________________________\n";
    let fnc file_idx file =
      Printf.printf " %i |" file_idx;
      Array.iter (fun c -> Printf.printf " %c " c) file;
      print_endline ""
    in
    Array.iteri fnc fen.fen_board

  let read str =
    match String.split_on_char ' ' str with
    | [ pos; to_play; castling; en_passant; half_move; full_move ] ->
        {
          orig_str = str;
          fen_board = gen_board pos;
          to_play;
          castling;
          en_passant;
          half_move;
          full_move;
        }
    | _ -> failwith "not a valid fen position"
end

let empty_position : position_t =
  {
    board =
      [|
        [| 'r'; 'n'; 'b'; 'q'; 'k'; 'b'; 'n'; 'r' |];
        [| 'p'; 'p'; 'p'; 'p'; 'p'; 'p'; 'p'; 'p' |];
        [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
        [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
        [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
        [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
        [| 'P'; 'P'; 'P'; 'P'; 'P'; 'P'; 'P'; 'P' |];
        [| 'R'; 'N'; 'B'; 'Q'; 'K'; 'B'; 'N'; 'R' |];
      |];
    mv_last = None;
    mv_next = None;
    active_player = White;
  }

let copy_position pos =
  {
    board = Array.map (fun l -> Array.copy l) pos.board;
    mv_last = pos.mv_last;
    mv_next = pos.mv_next;
    active_player = pos.active_player;
  }

let is_a_piece = function '.' -> false | _ -> true

let piece_color = function
  | 'r' | 'n' | 'b' | 'q' | 'k' | 'p' -> Black
  | 'R' | 'N' | 'B' | 'Q' | 'K' | 'P' -> White
  | _ -> failwith "not a piece"

let check_color from_p to_p =
  if is_a_piece from_p = false then assert false;
  if is_a_piece to_p then (
    if piece_color from_p = piece_color to_p then
      Printf.printf "can not take my piece\n%!";
    false)
  else true

let check_rook_move from_x from_y to_x to_y pos =
  Printf.printf "check rook %!\n";
  if from_x <> to_x && from_y <> to_y then
    Printf.printf "rooks only on lines!%!\n"
  else if from_x = to_x then
    (* check move on x axis *)
    let incr = if from_y < to_y then 1 else -1 in
    let rec check_path ypath =
      if ypath = to_y then true
      else if pos.board.(ypath).(from_x) <> '.' then false
      else check_path (ypath + incr)
    in
    let valid = check_path (from_y + incr) in
    if valid then Printf.printf "valid rook move\n%!"
    else Printf.printf "not valid rook move\n%!"
  else
    (* check move on y axis *)
    let incr = if from_x < to_x then 1 else -1 in
    let rec check_path xpath =
      if xpath = to_x then true
      else if pos.board.(from_y).(xpath) <> '.' then false
      else check_path (xpath + incr)
    in
    let valid = check_path (from_x + incr) in
    if valid then Printf.printf "valid rook move\n%!"
    else Printf.printf "not valid rook move\n%!"

let is_valid_move from_x from_y to_x to_y pos =
  if from_x = to_x && from_y = to_y then Printf.printf "no move to self\n%!"
  else
    let from_p = pos.board.(from_y).(from_x)
    and to_p = pos.board.(to_y).(to_x) in
    if check_color from_p to_p then
      match from_p with
      | 'r' | 'R' -> () (*check_rook_move from_x from_y to_x to_y pos*)
      | _ -> ()

let get_i = function
  | 'a' | '8' -> 0
  | 'b' | '7' -> 1
  | 'c' | '6' -> 2
  | 'd' | '5' -> 3
  | 'e' | '4' -> 4
  | 'f' | '3' -> 5
  | 'g' | '2' -> 6
  | 'h' | '1' -> 7
  | _ -> failwith "bad move def"

let swap col = match col with White -> Black | Black -> White

let copy_board mat =
  [|
    Array.copy mat.(0);
    Array.copy mat.(1);
    Array.copy mat.(2);
    Array.copy mat.(3);
    Array.copy mat.(4);
    Array.copy mat.(5);
    Array.copy mat.(6);
    Array.copy mat.(7);
  |]

let get_p board mv =
  if mv.promote_to = '.' then board.(mv.from_x).(mv.from_y) else mv.promote_to

let rec gen_pos_list n acc board color last_mv moves =
  match moves with
  | [] ->
      let c =
        { board; mv_next = None; mv_last = last_mv; active_player = color }
      in
      List.append acc [ c ]
  | mv :: tail ->
      let c =
        { board; mv_next = Some mv; mv_last = last_mv; active_player = color }
      in
      let next_board = copy_board board in
      next_board.(mv.to_x).(mv.to_y) <- get_p next_board mv;
      next_board.(mv.from_x).(mv.from_y) <- '.';
      gen_pos_list (n + 1) (List.append acc [ c ]) next_board (swap color)
        (Some mv) tail

let print_pos pos =
  (match pos.active_player with
  | White -> Printf.printf "White to play\n"
  | Black -> Printf.printf "Black to play\n");

  (match pos.mv_next with
  | Some m ->
      Printf.printf "expect fromx:%i fromy:%i tox:%i toy:%i\n" m.from_x m.from_y
        m.to_x m.to_y
  | None -> ());

  (match pos.mv_last with
  | Some m ->
      Printf.printf "last mv fromx:%i fromy:%i tox:%i toy:%i\n" m.from_x
        m.from_y m.to_x m.to_y
  | None -> ());

  Array.iter
    (fun a ->
      Array.iter (fun ch -> Printf.printf " %c " ch) a;
      print_endline "")
    pos.board

let rec print_positions ls =
  match ls with
  | [] -> ()
  | p :: tail ->
      print_pos p;
      print_positions tail

let get_promotion str col =
  if String.length str <> 5 then '.'
  else
    let ch = String.get str 4 in
    match ch with
    | 'r' | 'R' -> if col = White then 'R' else 'r'
    | 'n' | 'N' -> if col = White then 'N' else 'n'
    | 'b' | 'B' -> if col = White then 'B' else 'b'
    | 'q' | 'Q' -> if col = White then 'Q' else 'q'
    | 'k' | 'K' -> if col = White then 'K' else 'k'
    | 'p' | 'P' -> if col = White then 'P' else 'p'
    | _ -> failwith "not a piece"

let positions_from_mvs (fen : Fen.fen_pos_t)  mvs_str start_col =
  let moves, _ =
    List.fold_left
      (fun (mvs, c) mv ->
        let move =
          {
            from_y = get_i (String.get mv 0);
            from_x = get_i (String.get mv 1);
            to_y = get_i (String.get mv 2);
            to_x = get_i (String.get mv 3);
            promote_to = get_promotion mv c;
          }
        in
        (mvs @ [ move ], if c = Black then White else Black))
      ([], start_col)
      (String.split_on_char ' ' mvs_str)
  in

  let initial_color = if String.equal fen.to_play "w" then White else Black in
  gen_pos_list 0 [] fen.fen_board initial_color None moves

let reverse_position p =
  let nboard =
    Array.fold_left
      (fun acc l ->
        let nl =
          Array.fold_left (fun acc ch -> Array.append [| ch |] acc) [||] l
        in
        Array.append [| nl |] acc)
      [||] p.board
  in
  let nmv_last =
    match p.mv_last with
    | None -> None
    | Some m ->
        Some
          {
            from_x = 7 - m.from_x;
            from_y = 7 - m.from_y;
            to_x = 7 - m.to_x;
            to_y = 7 - m.to_y;
            promote_to = m.promote_to;
          }
  in
  let nmv_next =
    match p.mv_next with
    | None -> None
    | Some m ->
        Some
          {
            from_x = 7 - m.from_x;
            from_y = 7 - m.from_y;
            to_x = 7 - m.to_x;
            to_y = 7 - m.to_y;
            promote_to = m.promote_to;
          }
  in
  {
    board = nboard;
    mv_last = nmv_last;
    mv_next = nmv_next;
    active_player = p.active_player;
  }

let reverse_positions p = List.map reverse_position p

module Board = struct
  exception Wrong_chessboard_coordinate of string

  type color_t   = Black | White
  type figure_t  = R | N | B | K | Q | P
  type piece_t   = (figure_t * color_t)
  type square_t  = piece_t option
  type row_id    = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  type column_id = CA | CB | CC | CD | CE | CF | CG | CH
  type orient_t  = WhiteBottom | BlackBottom

  type t = {
    orient : orient_t;
    position :
           (* 8        7        6        5        4        3        2        1    *)
  (* a *) ((square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
  (* b *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
  (* c *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
  (* d *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
  (* e *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
  (* f *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
  (* g *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
  (* h *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t))
  }

  let empty_position = {
    orient = WhiteBottom;
    position = (
      (None,None,None,None,None,None,None,None),
      (None,None,None,None,None,None,None,None),
      (None,None,None,None,None,None,None,None),
      (None,None,None,None,None,None,None,None),
      (None,None,None,None,None,None,None,None),
      (None,None,None,None,None,None,None,None),
      (None,None,None,None,None,None,None,None),
      (None,None,None,None,None,None,None,None)
    )
  }

  let default_position = {
    orient = WhiteBottom;
    position = (
      (Some(R,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(R,White)),
      (Some(B,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(B,White)),
      (Some(N,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(N,White)),
      (Some(Q,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(Q,White)),
      (Some(K,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(K,White)),
      (Some(N,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(N,White)),
      (Some(B,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(B,White)),
      (Some(R,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(R,White))
    )
  }

  let row_of_int = function
    | 0 -> R8 | 1 -> R7 | 2 -> R6 | 3 -> R5
    | 4 -> R4 | 5 -> R3 | 6 -> R2 | 7 -> R1
    | n -> raise (Wrong_chessboard_coordinate
            (Printf.sprintf "request row %i but expect an int in range 0 to 7" n))

  let column_of_int = function
    | 0 -> CA | 1 -> CB | 2 -> CC | 3 -> CD
    | 4 -> CE | 5 -> CF | 6 -> CG | 7 -> CH
    | n -> raise (Wrong_chessboard_coordinate
            (Printf.sprintf "request column %i but expect an int in range 0 to 7" n))

  let get_column column position =
    match column, position with
    | CA, (c,_,_,_,_,_,_,_) -> c
    | CB, (_,c,_,_,_,_,_,_) -> c
    | CC, (_,_,c,_,_,_,_,_) -> c
    | CD, (_,_,_,c,_,_,_,_) -> c
    | CE, (_,_,_,_,c,_,_,_) -> c
    | CF, (_,_,_,_,_,c,_,_) -> c
    | CG, (_,_,_,_,_,_,c,_) -> c
    | CH, (_,_,_,_,_,_,_,c) -> c

  let get_square row column =
    match row, column with
    | R1, (_,_,_,_,_,_,_,s) -> s
    | R2, (_,_,_,_,_,_,s,_) -> s
    | R3, (_,_,_,_,_,s,_,_) -> s
    | R4, (_,_,_,_,s,_,_,_) -> s
    | R5, (_,_,_,s,_,_,_,_) -> s
    | R6, (_,_,s,_,_,_,_,_) -> s
    | R7, (_,s,_,_,_,_,_,_) -> s
    | R8, (s,_,_,_,_,_,_,_) -> s

  (* Axises follows SDL2 direction:
      x left -> right (a -> b), y top -> bottom (8 -> 1) *)
  let square_at b x y =
    let (x, y) = if b.orient = WhiteBottom then (x, y) else (7 - x, 7 - y) in
    get_square (row_of_int y) (get_column (column_of_int x) b.position)

end

module Puzzles = struct
  open Fen

  type puzzle_t = {
    lichess_id : string;
    rating : string;
    player_color : color_t;
    positions : position_t list;
  }

  let empty =
    {
      lichess_id = "none";
      rating = "0";
      player_color = White;
      positions = [ empty_position ];
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
                ( Black,
                  reverse_positions
                    (positions_from_mvs fen_pos mvs White) )
            | _ -> (White, positions_from_mvs fen_pos mvs Black)
          in
          Some { lichess_id = id; rating; player_color; positions }
      | _ -> failwith "not a valid csv file"
    with End_of_file -> None
end


module Uci = struct
  open Unix
  open Printf

  let in_buffer = Bytes.create 4096
  let in_index = ref 0
  let fd_to_engine   : Unix.file_descr ref = ref Unix.stdout
  let fd_from_engine : Unix.file_descr ref = ref Unix.stdin
  let engine_chans : (in_channel * out_channel) ref = ref (Stdlib.stdin, Stdlib.stdout)

  let get_line () =
    let rec match_newline = (fun () ->
      try (
        match (Unix.read !fd_from_engine in_buffer !in_index 1) with
        | 0 -> None
        | 1 when (Bytes.get in_buffer !in_index) = '\n'->
            let len = !in_index in in_index := 0;
            Some (Bytes.sub_string in_buffer 0 len)
        | 1 -> in_index := !in_index + 1; match_newline ()
        | _ -> failwith "wwwhath"
      ) with
        | Unix_error (Unix.EAGAIN, _, _) -> None
    ) in
    match_newline ()

  let send str =
    let msg = Bytes.of_string str in
    let _ = Unix.write !fd_to_engine msg 0 (Bytes.length msg) in ()

  let quit () =
    let pid = process_pid !engine_chans in
    send "quit\n";
    match waitpid [] pid with
    | (p, (WEXITED   0)) when p = pid ->
      Unix.close !fd_to_engine; Unix.close !fd_from_engine; exit 0
    | _ -> eprintf "wrong exit!"

  let init () =
    engine_chans := open_process_args "gnuchess" [|"gnuchess"; "--uci"|];
    let from_e, to_e = !engine_chans in
    fd_from_engine   := Unix.descr_of_in_channel from_e;
    fd_to_engine     := Unix.descr_of_out_channel to_e;
    Unix.set_nonblock !fd_from_engine;
    send "uci\n";
    Unix.sleepf 0.2;
    let rec consume = (fun () ->
      match get_line () with
      | Some l when (String.equal l "uciok") -> ()
      | Some l -> printf ".. %s\n%!" l; consume ()
      | None -> failwith "can not reach uciok"
    ) in
    consume ()
end

module Move = struct

  exception Wrong_move of string
  type check_move_t = {
    src_x : int;
    src_y : int;
    src_piece : piece_t;
    dst_x : int;
    dst_y : int;
    dst_piece : piece_t;
    position : position_t;
  }

  let pp = Printf.printf
  let p v = Printf.printf "%s\n%!" v
  let pm m =
    Array.iter (fun a ->
      Array.iter (fun v ->
        if v then (pp " o ") else (pp " . ")
      ) a;
      pp "\n"
    ) m;
    p "end"

  let rook_mat = [|
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true |];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;true ;false;false;false;false;false;false;false|];
  |]

  let bishop_mat = [|
  [|true ;false;false;false;false;false;false;false;false;false;false;false;false;false;true |];
  [|false;true ;false;false;false;false;false;false;false;false;false;false;false;true ;false|];
  [|false;false;true ;false;false;false;false;false;false;false;false;false;true ;false;false|];
  [|false;false;false;true ;false;false;false;false;false;false;false;true ;false;false;false|];
  [|false;false;false;false;true ;false;false;false;false;false;true ;false;false;false;false|];
  [|false;false;false;false;false;true ;false;false;false;true ;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;false;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;false;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;true ;false;false;false;true ;false;false;false;false;false|];
  [|false;false;false;false;true ;false;false;false;false;false;true ;false;false;false;false|];
  [|false;false;false;true ;false;false;false;false;false;false;false;true ;false;false;false|];
  [|false;false;true ;false;false;false;false;false;false;false;false;false;true ;false;false|];
  [|false;true ;false;false;false;false;false;false;false;false;false;false;false;true ;false|];
  [|true ;false;false;false;false;false;false;false;false;false;false;false;false;false;true |];
  |]

  let queen_mat = [|
  [|true ;false;false;false;false;false;false;true ;false;false;false;false;false;false;true |];
  [|false;true ;false;false;false;false;false;true ;false;false;false;false;false;true ;false|];
  [|false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false|];
  [|false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false|];
  [|false;false;false;false;true ;false;false;true ;false;false;true ;false;false;false;false|];
  [|false;false;false;false;false;true ;false;true ;false;true ;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;true ;true ;false;false;false;false;false;false|];
  [|true ;true ;true ;true ;true ;true ;true ;false;true ;true ;true ;true ;true ;true ;true |];
  [|false;false;false;false;false;false;true ;true ;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;true ;false;true ;false;true ;false;false;false;false;false|];
  [|false;false;false;false;true ;false;false;true ;false;false;true ;false;false;false;false|];
  [|false;false;false;true ;false;false;false;true ;false;false;false;true ;false;false;false|];
  [|false;false;true ;false;false;false;false;true ;false;false;false;false;true ;false;false|];
  [|false;true ;false;false;false;false;false;true ;false;false;false;false;false;true ;false|];
  [|true ;false;false;false;false;false;false;true ;false;false;false;false;false;false;true |];
  |]

  let knight_mat = [|
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;false;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;true ;false;false;false;true ;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;true ;false;false;false;true ;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;false;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  |]

  let king_mat = [|
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;true ;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;false;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;true ;true ;true ;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|];
  |]

  let gen_move_matrix mv_mat src_x src_y =
    let mat = Array.make_matrix 8 8 false
    and move_orig_x = 7 - src_x
    and move_orig_y = 7 - src_y in
    Array.iteri
      (fun i v -> Array.blit mv_mat.(move_orig_y + i) move_orig_x v 0 8)
      mat;
    mat

  let filter_path moves board player_color src_x src_y incr_fun =
    let rec filter_p (x, y) ~clear =
      if x = 8 || x = -1 || y = 8 || y = -1 then ()
      else if clear then (
        moves.(y).(x) <- false;
        filter_p (incr_fun (x, y)) ~clear)
      else
        match board.(y).(x) with
        | '.' -> filter_p (incr_fun (x, y)) ~clear
        | p ->
            if piece_color p = player_color then (
              moves.(y).(x) <- false;
              filter_p (incr_fun (x, y)) ~clear:true)
            else filter_p (incr_fun (x, y)) ~clear:true
    in
    filter_p (incr_fun (src_x, src_y)) ~clear:false

  let get_valid_moves board col src_x src_y = function
    | 'R' | 'r' ->
        let moves = gen_move_matrix rook_mat src_x src_y in
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y));
        filter_path moves board col src_x src_y (fun (x, y) -> (x, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x, y - 1));
        moves
    | 'B' | 'b' ->
        let moves = gen_move_matrix bishop_mat src_x src_y in
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y - 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y - 1));
        moves
    | 'Q' | 'q' ->
        let moves = gen_move_matrix queen_mat src_x src_y in
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y));
        filter_path moves board col src_x src_y (fun (x, y) -> (x, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x, y - 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y - 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y - 1));
        moves
    | 'K' | 'k' ->
        let moves = gen_move_matrix king_mat src_x src_y in
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y));
        filter_path moves board col src_x src_y (fun (x, y) -> (x, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x, y - 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x + 1, y - 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y + 1));
        filter_path moves board col src_x src_y (fun (x, y) -> (x - 1, y - 1));
        moves
    | 'N' | 'n' ->
        let moves = gen_move_matrix knight_mat src_x src_y in
        Array.iteri
          (fun y a ->
            Array.iteri
              (fun x b ->
                match b with
                | '.' -> ()
                | p -> if piece_color p = col then moves.(y).(x) <- false)
              a)
          board;
        moves
    | 'P' | 'p' ->
        let moves = Array.make_matrix 8 8 false in
        (* front *)
        if board.(src_y - 1).(src_x) = '.' then (
          moves.(src_y - 1).(src_x) <- true;
          (* starting square *)
          if src_y = 6 then
            if board.(src_y - 2).(src_x) = '.' then
              moves.(src_y - 2).(src_x) <- true);
        (* front left *)
        (if src_x > 0 then
           let sl = board.(src_y - 1).(src_x - 1) in
           if sl <> '.' then
             if piece_color sl <> col then
               moves.(src_y - 1).(src_x - 1) <- true);
        (* front right *)
        (if src_x < 7 then
           let sr = board.(src_y - 1).(src_x + 1) in
           if sr <> '.' then
             if piece_color sr <> col then
               moves.(src_y - 1).(src_x + 1) <- true);

        (* en passant todo *)
        moves
    | _ -> failwith "not implemented"

  let get_move_matrix board col src_x src_y =
    get_valid_moves board col src_x src_y board.(src_y).(src_x)

  let check_dest_color c =
    if c.dst_piece <> '.' then
      if piece_color c.src_piece = piece_color c.dst_piece then
        raise (Wrong_move "src and dst colo are the same")

  let check_active_player c =
    if piece_color c.src_piece <> c.position.active_player then
      raise (Wrong_move "not an active player color piece")

  let check_king_check _ = ()

  let check_piece_move c =
    let mvs =
      get_valid_moves c.position.board c.position.active_player c.src_x c.src_y
        c.src_piece
    in
    if mvs.(c.dst_y).(c.dst_x) <> true then
      match c.src_piece with
      | 'r' | 'R' -> raise (Wrong_move "rook")
      | 'b' | 'B' -> raise (Wrong_move "bishop")
      | 'q' | 'Q' -> raise (Wrong_move "queen")
      | 'n' | 'N' -> raise (Wrong_move "night")
      | 'k' | 'K' -> raise (Wrong_move "king")
      | 'p' | 'P' -> raise (Wrong_move "pawn")
      | _ -> failwith "nnnnot a piece"

  let is_valid from_x from_y to_x to_y pos =
    match
      let c =
        {
          src_x = from_x;
          src_y = from_y;
          src_piece = pos.board.(from_y).(from_x);
          dst_x = to_x;
          dst_y = to_y;
          dst_piece = pos.board.(to_y).(to_x);
          position = pos;
        }
      in
      check_dest_color c;
      check_active_player c;
      check_piece_move c;
      check_king_check c
    with
    | exception Wrong_move v ->
        pp "exception Wrong_move: %s%!\n" v;
        false
    | _ -> true

end

