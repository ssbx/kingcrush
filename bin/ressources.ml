open Tsdl
open Tsdl_mixer
open Tsdl_image
open Tsdl_ttf
open Gamekit


module Audio = struct
  let audio_dir : string ref = ref ""
  let get_path f = Filename.concat !audio_dir f
  let enabled : bool ref = ref true

  let music_chan : int option ref = ref None

  type sound_t =
    | Capture
    | Move
    | PuzzleRushGood
    | PuzzleRushEnd
    | GameOver
    | LevelStart
    | LevelComplete
    | MusicGroove
    | MusicCalm

  type sounds_t = {
    capture : Mixer.chunk;
    move : Mixer.chunk;
    puzzle_rush_good : Mixer.chunk;
    puzzle_rush_end : Mixer.chunk;
    game_over : Mixer.chunk;
    level_start : Mixer.chunk;
    level_complete : Mixer.chunk;
    music_groove : Mixer.chunk;
    music_calm : Mixer.chunk;
  }

  let chan_finished_callback n =
    match !music_chan with | Some v when v = n -> music_chan := None | _ -> ()

  let sounds : sounds_t option ref = ref None

  let get_sound t =
    match !sounds with
    | Some s -> (
        match t with
        | Move -> s.move
        | Capture -> s.capture
        | PuzzleRushGood -> s.puzzle_rush_good
        | LevelStart -> s.level_start
        | LevelComplete -> s.level_complete
        | GameOver -> s.game_over
        | PuzzleRushEnd -> s.puzzle_rush_end
        | MusicGroove -> s.music_groove
        | MusicCalm -> s.music_calm)
    | None ->
            failwith "error: audio.ml get_sound None"

  let play s =
    if !enabled then (
      match Mixer.play_channel (-1) (get_sound s) 0 with
      | Ok _ -> ()
      | Error (`Msg m) ->
              failwith ("audio.ml play error:" ^ m)
    )

  let music_play mus =
    if !enabled then (
      match !music_chan with
      | Some v ->  let _ = Mixer.fade_out_channel v 100 in music_chan := None
      | _ -> ();
      match Mixer.play_channel (-1) (get_sound mus) 0 with
      | Ok ch -> music_chan := Some ch
      | Error (`Msg m) -> failwith ("audio music_play " ^ m)
    )



  let music_fade_out n =
    if !enabled then (
      match !music_chan with
      | Some v -> let _ = Mixer.fade_out_channel v n in music_chan := None
      | None -> ()
    )

  let music_stop () =
    music_fade_out 500

  let init () =
    audio_dir := Filename.concat !Conf.base_dir "sounds";
    if !enabled then (
      Mixer.channel_finished chan_finished_callback;
      sounds :=
        Some
          {
            capture = sdl_get_ok (Mixer.load_wav (get_path "Capture.wav"));
            move = sdl_get_ok (Mixer.load_wav (get_path "Move.wav"));
            puzzle_rush_good =
              sdl_get_ok (Mixer.load_wav (get_path "PuzzleRushGood.wav"));
            puzzle_rush_end =
              sdl_get_ok (Mixer.load_wav (get_path "PuzzleRushEnd.wav"));
            game_over = sdl_get_ok (Mixer.load_wav (get_path "GameOver.wav"));
            level_start = sdl_get_ok (Mixer.load_wav (get_path "LevelStart.wav"));
            level_complete =
              sdl_get_ok (Mixer.load_wav (get_path "LevelComplete.wav"));
            music_groove =
              sdl_get_ok (Mixer.load_wav (get_path "MusicGroove.wav"));
            music_calm =
              sdl_get_ok (Mixer.load_wav (get_path "MusicCalm2.wav"));
          }
    )

  let release () =
    if !enabled then (
      Mixer.free_chunk (get_sound Move);
      Mixer.free_chunk (get_sound Capture);
      Mixer.free_chunk (get_sound PuzzleRushGood);
      Mixer.free_chunk (get_sound LevelStart);
      Mixer.free_chunk (get_sound LevelComplete);
      Mixer.free_chunk (get_sound GameOver);
      Mixer.free_chunk (get_sound PuzzleRushEnd);
      Mixer.free_chunk (get_sound MusicGroove);
      Mixer.free_chunk (get_sound MusicCalm);
    )
end

module Figures = struct

  let piece_width = 177
  (*let images_dir = Filename.concat (List.nth Data.Sites.pieces 0) "default"*)
  let images_dir : string ref = ref ""
  let get_path v = Filename.concat !images_dir v

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
    images_dir := Filename.(concat (concat !Conf.base_dir "pieces") "default");
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
end

module Fonts = struct

  let fonts_dir : string ref = ref ""

  let f500 : Ttf.font option ref = ref None
  let fg_color = Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255

  let get_f500 () =
    match !f500 with Some v -> v | None -> failwith "f500 not loaded"

  let get_surface v =
    match Ttf.render_text_solid (get_f500 ()) v fg_color with
    | Error (`Msg e) -> failwith e
    | Ok s -> s

  let init () =
    fonts_dir := Filename.concat !Conf.base_dir "fonts";
    sdl_try (Ttf.init ());
    match Ttf.open_font (Filename.concat !fonts_dir "OpenSans-Regular.ttf") 32 with
    | Ok f -> f500 := Some f
    | Error (`Msg e) -> failwith e
end
