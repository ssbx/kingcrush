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
