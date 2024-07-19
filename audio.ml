open CamlSDL2_mixer

#include "log.cppo"

let audio_dir : string ref = ref ""
let get_path f = Filename.concat !audio_dir f
let enabled : bool ref = ref true

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
  capture : Mix.Chunk.t;
  move : Mix.Chunk.t;
  puzzle_rush_good : Mix.Chunk.t;
  puzzle_rush_end : Mix.Chunk.t;
  game_over : Mix.Chunk.t;
  level_start : Mix.Chunk.t;
  level_complete : Mix.Chunk.t;
  music_groove : Mix.Chunk.t;
  music_calm : Mix.Chunk.t;
}

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
    Mix.play_channel
      ~channel:(-1)
      ~chunk:(get_sound s)
      ~loops:0
  )

let music_play _ = ()
  (*
let music_play mus =
  if !enabled then (
    match !music_chan with
    | Some v ->  let _ = Mix.fade_out_channel v 100 in music_chan := None
    | _ -> ();
    match Mix.play_channel (-1) (get_sound mus) 0 with
    | Ok ch -> music_chan := Some ch
    | Error (`Msg m) -> LOG_FAILWITH(m)
  )

*)

let music_fade_out _ = ()
let music_stop () = ()

let init () =
  audio_dir := Filename.concat !Info.base_dir "sounds";
  if !enabled then (
    sounds :=
      Some
        {
          capture = Mix.load_wav (get_path "Capture.wav");
          move = Mix.load_wav (get_path "Move.wav");
          puzzle_rush_good =
            Mix.load_wav (get_path "PuzzleRushGood.wav");
          puzzle_rush_end =
            Mix.load_wav (get_path "PuzzleRushEnd.wav");
          game_over = Mix.load_wav (get_path "GameOver.wav");
          level_start = Mix.load_wav (get_path "LevelStart.wav");
          level_complete =
            Mix.load_wav (get_path "LevelComplete.wav");
          music_groove =
            Mix.load_wav (get_path "MusicGroove.wav");
          music_calm =
            Mix.load_wav (get_path "MusicCalm2.wav");
        }
  )

let release () =
  if !enabled then (
    Mix.free_chunk (get_sound Move);
    Mix.free_chunk (get_sound Capture);
    Mix.free_chunk (get_sound PuzzleRushGood);
    Mix.free_chunk (get_sound LevelStart);
    Mix.free_chunk (get_sound LevelComplete);
    Mix.free_chunk (get_sound GameOver);
    Mix.free_chunk (get_sound PuzzleRushEnd);
    Mix.free_chunk (get_sound MusicGroove);
    Mix.free_chunk (get_sound MusicCalm);
  )
