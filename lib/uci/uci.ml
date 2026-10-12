module Unix = UnixLabels

module Proc = struct
  type t = {
    pid         : int;
    to_prog     : Unix.file_descr;
    from_prog   : Unix.file_descr;
    mutable buf : bytes;
    mutable pos : int;
    len         : int;
  }

  let buf_len = 300


  let send s str =
    print_endline ("-- send: " ^ str);
    Unix.write
      ~buf:(Bytes.of_string (str ^ "\n"))
      ~pos:0
      ~len:((String.length str) + 1) s.to_prog |> ignore;
    Unix.out_channel_of_descr s.to_prog |> flush


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

type t = {
  proc              : Proc.t;
  mutable id_name   : string;
  mutable id_author : string;
  mutable options   : string list;
  mutable uciok     : bool;
  mutable isready   : bool;
  mutable running   : bool;
  mutable ingame    : bool;
}

let debug s =
  print_endline ("-- uciok: " ^ (Bool.to_string s.uciok));
  print_endline ("-- id:     " ^ s.id_name);
  print_endline ("-- author: " ^ s.id_author);
  print_endline ("-- isready: " ^ (Bool.to_string s.isready));
  print_endline ("-- running: " ^ (Bool.to_string s.running));
  print_endline ("-- ingame: " ^ (Bool.to_string s.ingame));
  List.iter (fun v -> print_endline ("-- opt: " ^ v)) s.options


let start () =
  let proc = Proc.start() in
  Proc.send proc "uci";
  {
    proc      = proc;
    id_name   = "";
    id_author = "";
    options   = [];
    uciok     = false;
    isready   = false;
    running   = false;
    ingame    = false;
  }

let close s =
  debug s;
  Proc.send s.proc "stop";
  Proc.send s.proc "quit";
  Unix.sleepf 0.1;
  Proc.close s.proc

type msg_type = UciOpt | UciInfo of string | UciOk | UciReadyOk |
                UciNone | UciBestMove of string

let handle_line s v =
  let drop s1 s2 = String.drop_first (String.length s2) s1 |> String.trim in
  if (String.starts_with ~prefix:"id name" v) then (
    s.id_name <- drop v "id name"; UciOpt
  ) else if (String.starts_with ~prefix:"id author" v) then (
    s.id_author <- drop v "id author"; UciOpt
  ) else if (String.starts_with ~prefix:"option name" v) then (
    s.options <- (drop v "option name") :: s.options; UciOpt
  ) else if (String.equal v "uciok") then (
    s.uciok <- true; UciOk
  ) else if (String.equal v "readyok") then (
    s.isready <- true; UciReadyOk
  ) else if (String.starts_with ~prefix:"info " v) then (
    UciInfo v
  ) else if (String.starts_with ~prefix:"bestmove " v) then (

    UciBestMove v
  ) else (
    print_endline ("ERR handle_line unknown: " ^ v);
    UciNone
  )


let update s =
  let more = ref true in
  while !more do
    match (Proc.read_line s.proc) with
    | None -> more := false
    | Some v ->
      (match (handle_line s v) with
      | UciOk -> Proc.send s.proc "isready"
      | UciReadyOk -> (
        Proc.send s.proc "ucinewgame";
        Proc.send s.proc "go";
        s.running <- true
      )
      | UciInfo v -> print_endline (" -- infffff: " ^ v)
      | _ -> ()
      );
      print_endline v
  done

let stop s =
  Proc.send s.proc "stop"
