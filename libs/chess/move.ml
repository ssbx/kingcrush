open Types
open Utils

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
              | p -> if Utils.piece_color p = col then moves.(y).(x) <- false)
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
           if Utils.piece_color sl <> col then
             moves.(src_y - 1).(src_x - 1) <- true);
      (* front right *)
      (if src_x < 7 then
         let sr = board.(src_y - 1).(src_x + 1) in
         if sr <> '.' then
           if Utils.piece_color sr <> col then
             moves.(src_y - 1).(src_x + 1) <- true);

      (* en passant todo *)
      moves
  | _ -> failwith "not implemented"

let get_move_matrix board col src_x src_y =
  get_valid_moves board col src_x src_y board.(src_y).(src_x)

let check_dest_color c =
  if c.dst_piece <> '.' then
    if Utils.piece_color c.src_piece = Utils.piece_color c.dst_piece then
      raise (Wrong_move "src and dst colo are the same")

let check_active_player c =
  if Utils.piece_color c.src_piece <> c.position.active_player then
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

