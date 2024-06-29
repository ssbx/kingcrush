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
  Printf.printf "<<<< sending %s\n" str;
  let msg = Bytes.of_string str in
  let _ = Unix.write !fd_to_engine msg 0 (Bytes.length msg) in ()


let test_quit : bool ref = ref false
let test_fen_send : bool ref = ref false

let handle_line line =
  if String.equal line "uciok" then (
    Printf.printf "--> %s\n" line;
    send "isready"
  ) else if String.equal line "readyok" then (
    if !test_fen_send then (
      Printf.printf "--> %s\n" line; test_quit := true
    ) else (
      let (fen,_) = Puzzles.next_fen () in
      test_fen_send := true;
      send fen; send "isready"
    )
  ) else (
    Printf.printf "--> %s\n" line
  )

let rec consume_buffer () =
  match get_line () with
  | None -> Printf.printf "next\n"
  | Some l -> (
    handle_line l;
    consume_buffer ()
  )

let update () =
  consume_buffer ()

let rec loop () =  (* should use the game loop *)
  if !test_quit <> true then (
    update ();
    Unix.sleepf 0.2;
    loop ()
  )

let open_gnuchess () = open_process_args "gnuchess" [|"gnuchess"; "--uci"|]
let open_stockfish () = open_process_args "stockfish" [|"stockfish"|]
let open_toga2 () = open_process_args "toga2" [|"toga2"|]

let init () =
  engine_chans := open_gnuchess ();
  let from_e, to_e = !engine_chans in
  fd_from_engine   := Unix.descr_of_in_channel from_e;
  fd_to_engine     := Unix.descr_of_out_channel to_e;
  Unix.set_nonblock !fd_from_engine;
  send "uci\n";
  loop ()

let quit () =
  let pid = process_pid !engine_chans in
  send "quit\n";
  match waitpid [] pid with
  | (p, (WEXITED   0)) when p = pid ->
    Printf.printf "exit ok\n";
    Unix.close !fd_to_engine; Unix.close !fd_from_engine;
    exit 0
  | _ -> eprintf "wrong exit!"

let test () =
  Puzzles.init "data/puzzles.csv";
  init ();
  let (fen,_) = Puzzles.next_fen () in
  print_endline fen;

  Puzzles.release ();
  quit ()
