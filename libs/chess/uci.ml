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
