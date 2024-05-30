let init () = if !State.with_audio then Audio.init ()
let play v = if !State.with_audio then Audio.play v
let release () = if !State.with_audio then Audio.release ()
