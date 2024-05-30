open Utils

type job_t = {
  at : int;
  fn  : unit -> unit;
}

let jobs : job_t list ref = ref []

let fire_at ms f =
  jobs := {at = ms; fn = f} :: !jobs

let fire_in ms f =
  fire_at ((sdl_get_ticks ()) + ms) f

let rec update_all ticks = function
  | [] -> ()
  | j :: tail ->
      if ticks > j.at then (
        j.fn ();
        jobs := List.filter (fun v -> v != j) !jobs
      );
      update_all ticks tail

let length () = List.length !jobs

let update ticks =
  update_all ticks !jobs
