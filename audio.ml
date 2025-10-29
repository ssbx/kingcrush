open Tsdl_mixer
open Gamekit

#include "log.cppo"

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
  | None -> LOG_CRASH()

let play s =
  if !enabled then (
    match Mixer.play_channel (-1) (get_sound s) 0 with
    | Ok _ -> ()
    | Error (`Msg m) -> LOG_FAILWITH(m)
  )

let music_play mus =
  if !enabled then (
    match !music_chan with
    | Some v ->  let _ = Mixer.fade_out_channel v 100 in music_chan := None
    | _ -> ();
    match Mixer.play_channel (-1) (get_sound mus) 0 with
    | Ok ch -> music_chan := Some ch
    | Error (`Msg m) -> LOG_FAILWITH(m)
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
  audio_dir := Filename.concat !Info.base_dir "sounds";
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
