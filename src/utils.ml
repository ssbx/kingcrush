
let sdl_try = function | Ok _ -> () | Error (`Msg e) -> failwith e
let sdl_get_ok = function | Ok v -> v | Error (`Msg e) -> failwith e
let sdl_ignore _ = ()
let log msg = Printf.printf msg

let some_or_fail = function | Some v -> v | None -> failwith "error some or fail"
let sdl_get_ticks () = Int32.to_int (Tsdl.Sdl.get_ticks ())
