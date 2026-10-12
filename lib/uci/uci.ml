
type state_t = Init | Ready

type t = {
  pid           : int;
  stdout_fd     : Unix.file_descr;
  stdout_ch     : in_channel;
  stderr_ch     : in_channel;
  stderr_fd     : Unix.file_descr;
  stdin_ch      : out_channel;
  mutable state : state_t;
}

let send ch str =
  Out_channel.output_string ch str;
  Out_channel.flush ch


let start () =
  let (out_ch, in_ch, err_ch) =
    Unix.open_process_args_full "stockfish" [||] [||]
  in
  let out_fd = Unix.descr_of_in_channel out_ch in
  let err_fd = Unix.descr_of_in_channel err_ch in
  Unix.set_nonblock out_fd;
  Unix.set_nonblock err_fd;
  let pid = Unix.process_full_pid (out_ch, in_ch, err_ch) in
  send in_ch "uci\n";
  {
    pid       = pid;
    stdout_fd = out_fd;
    stdout_ch = out_ch;
    stdin_ch  = in_ch;
    stderr_ch = err_ch;
    stderr_fd = err_fd;
    state     = Init;
  }

let close s =
  send s.stdin_ch "quit\n";
  try
    ignore (Unix.waitpid [] s.pid)
  with
  | _ -> ();
    ignore (Unix.close_process_full
              (s.stdout_ch, s.stdin_ch, s.stderr_ch))

let read_line ch fd =
  let buff = Bytes.create 100 in
  try
    let n = Unix.read fd buff 0 100 in
    if (n > 0) then
      Some (Bytes.sub buff 0 n |> Bytes.to_string)
    else
      None
  with
  | Unix.Unix_error (EAGAIN,_,_)
  | Unix.Unix_error (EWOULDBLOCK,_,_) -> None

let read s =
  Option.iter print_endline (read_line s.stderr_ch s.stderr_fd);
  read_line s.stdout_ch s.stdout_fd

let print s =
  Printf.eprintf "pid:%i\n" s.pid

