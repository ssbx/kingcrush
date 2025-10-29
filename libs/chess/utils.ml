open Types
open Fen

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

let positions_from_mvs fen mvs_str start_col =
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

