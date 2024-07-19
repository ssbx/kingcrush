open CamlSDL2
open CamlSDL2_image

let piece_width = 177

(*let images_dir = Filename.concat (List.nth Data.Sites.pieces 0) "default"*)
let images_dir : string ref = ref ""
let get_path v = Filename.concat !images_dir v

type p_type =
  { bP : Sdl.Texture.t
  ; wP : Sdl.Texture.t
  ; bN : Sdl.Texture.t
  ; wN : Sdl.Texture.t
  ; bB : Sdl.Texture.t
  ; wB : Sdl.Texture.t
  ; bR : Sdl.Texture.t
  ; wR : Sdl.Texture.t
  ; bQ : Sdl.Texture.t
  ; wQ : Sdl.Texture.t
  ; bK : Sdl.Texture.t
  ; wK : Sdl.Texture.t
  }

let db : p_type option ref = ref None

let init ~renderer =
  (images_dir := Filename.(concat (concat !Info.base_dir "pieces") "default"));
  Img.init [ `PNG ];
  db
  := Some
       { bP = Img.load_texture renderer ~filename:(get_path "bP.png")
       ; bN = Img.load_texture renderer ~filename:(get_path "bN.png")
       ; bB = Img.load_texture renderer ~filename:(get_path "bB.png")
       ; bR = Img.load_texture renderer ~filename:(get_path "bR.png")
       ; bQ = Img.load_texture renderer ~filename:(get_path "bQ.png")
       ; bK = Img.load_texture renderer ~filename:(get_path "bK.png")
       ; wP = Img.load_texture renderer ~filename:(get_path "wP.png")
       ; wN = Img.load_texture renderer ~filename:(get_path "wN.png")
       ; wB = Img.load_texture renderer ~filename:(get_path "wB.png")
       ; wR = Img.load_texture renderer ~filename:(get_path "wR.png")
       ; wQ = Img.load_texture renderer ~filename:(get_path "wQ.png")
       ; wK = Img.load_texture renderer ~filename:(get_path "wK.png")
       }
;;

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
;;

let piece ch =
  match !db with
  | Some v ->
    (match ch with
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
;;
