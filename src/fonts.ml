open Tsdl
open Tsdl_ttf
open Gamekit

let fonts_dir : string = List.nth Data.Sites.fonts 0
let f500 : Ttf.font option ref = ref None
let fg_color = Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255

let get_f500 () =
  match !f500 with Some v -> v | None -> failwith "f500 not loaded"

let get_surface v =
  match Ttf.render_text_solid (get_f500 ()) v fg_color with
  | Error (`Msg e) -> failwith e
  | Ok s -> s

let init () =
  sdl_try (Ttf.init ());
  match Ttf.open_font (Filename.concat fonts_dir "f500.ttf") 32 with
  | Ok f -> f500 := Some f
  | Error (`Msg e) -> failwith e
