module Unix = UnixLabels

module Exec = struct
  type t = {
    pid         : int;
    to_prog     : Unix.file_descr;
    from_prog   : Unix.file_descr;
    mutable buf : bytes;
    mutable pos : int;
    len         : int;
  }

  let buf_len = 300

  let send fd str =
    Unix.write
      ~buf:(Bytes.of_string str)
      ~pos:0
      ~len:(String.length str) fd |> ignore;
    Unix.out_channel_of_descr fd |> flush


  let start () =
    let (read_out, write_out),
        (read_in,  write_in)  =
      Unix.pipe ~cloexec:true (),
      Unix.pipe ~cloexec:true ()
    in
    Unix.set_nonblock write_in;
    Unix.set_nonblock read_out;
    Unix.set_close_on_exec read_out;
    Unix.set_close_on_exec read_in;
    send write_in "uci\n\n";
    let pid = Unix.create_process
        ~prog:"stockfish"
        ~args:[||]
        ~stdin:read_in
        ~stdout:write_out
        ~stderr:Unix.stderr
    in
    {
      pid       = pid;
      to_prog   = write_in;
      from_prog = read_out;
      buf       = Bytes.make buf_len '\x00';
      pos       = 0;
      len       = buf_len;
    }

  let close s =
    send s.to_prog "quit\n";
    try Unix.waitpid ~mode:[] s.pid |> ignore with _ -> ();
    Unix.kill ~pid:s.pid ~signal:Sys.sigterm

  let match_line s =
    if (Bytes.contains s.buf '\n') then (
      let index = Bytes.index s.buf '\n' in
      let line  = Bytes.sub s.buf 0 index |> Bytes.to_string in
      let new_buf = Bytes.make buf_len '.' in
      Bytes.blit s.buf (index + 1) new_buf 0 (s.len - index - 1);
      s.buf <- new_buf;
      s.pos <- s.pos - index - 1;
      Some line
    ) else (
      if (s.pos = s.len) then failwith "bad input";
      None
    )

  let read_line s =
    try
      let n = Unix.read
          ~buf:s.buf
          ~pos:s.pos
          ~len:(s.len - s.pos)
          s.from_prog in
      s.pos <- s.pos + n;
      match_line s;
    with
    | Unix.Unix_error (EAGAIN,_,_)
    | Unix.Unix_error (EWOULDBLOCK,_,_) ->
      match_line s



  let print s =
    Printf.eprintf "pid:%i\n" s.pid

end


let start () = Exec.start ()
let close s = Exec.close s

let update s =
  let more = ref true in
  while !more do
    match (Exec.read_line s) with
    | None -> more := false
    | Some v -> print_endline v
  done

