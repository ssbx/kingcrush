open Tsdl
open Tsdl_image
open Utils

let piece_width = 177
let images_dir = Filename.concat (List.nth Assets.Sites.pieces 0) "default"
let get_path v = Filename.concat images_dir v

type p_type = {
  bP : Sdl.texture;
  wP : Sdl.texture;
  bN : Sdl.texture;
  wN : Sdl.texture;
  bB : Sdl.texture;
  wB : Sdl.texture;
  bR : Sdl.texture;
  wR : Sdl.texture;
  bQ : Sdl.texture;
  wQ : Sdl.texture;
  bK : Sdl.texture;
  wK : Sdl.texture;
}

let db : p_type option ref = ref None

let open_texture renderer filename =
  sdl_get_ok (Image.load_texture renderer filename)

let init ~renderer =
  let flags = Image.Init.png in
  assert (Image.init flags = flags);
  db :=
    Some
      {
        bP = open_texture renderer (get_path "bP.png");
        bN = open_texture renderer (get_path "bN.png");
        bB = open_texture renderer (get_path "bB.png");
        bR = open_texture renderer (get_path "bR.png");
        bQ = open_texture renderer (get_path "bQ.png");
        bK = open_texture renderer (get_path "bK.png");
        wP = open_texture renderer (get_path "wP.png");
        wN = open_texture renderer (get_path "wN.png");
        wB = open_texture renderer (get_path "wB.png");
        wR = open_texture renderer (get_path "wR.png");
        wQ = open_texture renderer (get_path "wQ.png");
        wK = open_texture renderer (get_path "wK.png");
      }

let release () =
  match !db with
  | Some v ->
      Sdl.destroy_texture v.bP;
      Sdl.destroy_texture v.bN;
      Sdl.destroy_texture v.bB;
      Sdl.destroy_texture v.bR;
      Sdl.destroy_texture v.bQ;
      Sdl.destroy_texture v.bK;
      Sdl.destroy_texture v.wP;
      Sdl.destroy_texture v.wN;
      Sdl.destroy_texture v.wB;
      Sdl.destroy_texture v.wR;
      Sdl.destroy_texture v.wQ;
      Sdl.destroy_texture v.wK;
      db := None
  | None -> ()

let piece ch =
  match !db with
  | Some v -> (
      match ch with
      | 'p' -> Some v.bP
      | 'n' -> Some v.bN
      | 'b' -> Some v.bB
      | 'r' -> Some v.bR
      | 'q' -> Some v.bQ
      | 'k' -> Some v.bK
      | 'P' -> Some v.wP
      | 'N' -> Some v.wN
      | 'B' -> Some v.wB
      | 'R' -> Some v.wR
      | 'Q' -> Some v.wQ
      | 'K' -> Some v.wK
      | _ -> None)
  | _ -> failwith "textures not loaded"
