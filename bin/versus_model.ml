
type versus_event_t =
  | Start
  | End

let listeners : (versus_event_t -> unit) list ref = ref []

let init () = ()

let register_callback cb = listeners := cb :: !listeners
let clear_callbacks () = listeners := []
