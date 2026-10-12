let readable fd =
  match Unix.select [fd] [] [] 0. with
  | [], _, _ -> false
  | _ -> true

let  _a () =
  let (_stdout_ch, _stdin_ch, _stderr_ch) = Unix.open_process_args_full
      "tt"
      [|"tt";"2"|]
      [||]
  in
    (*
    let _pid = Unix.process_full_pid (_out_ch, _in_ch, _err_ch) in
    Printf.eprintf "wait pid: %i\n" pid;
    let _ = Unix.waitpid [] pid in*)
  Printf.eprintf "---\n";
  flush stderr;

  let _stdout_fd = Unix.descr_of_in_channel _stdout_ch in
  let a = ref 0 in
  let rec r () =

    if (readable _stdout_fd) then
      try (
        match (In_channel.input_line _stdout_ch) with
        | None -> (
            Printf.eprintf "no data\n";
            flush stderr;
          )
        | Some str -> (
            Printf.eprintf "%s (%i)\n" str !a;
            flush stderr;
            a := 0;
            r ()
          )
      ) with
      | End_of_file -> ();
    else begin
      a := !a + 1;
      r ()
    end

  in

  r ();
  Printf.eprintf "hello uci \n";
  flush stderr

