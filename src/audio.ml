open Tsdl_mixer
open Utils

let audio_dir : string = List.nth Assets.Sites.sounds 0
let get_path f = Filename.concat audio_dir f

let groove_music : Mixer.music option ref = ref None
let calm_music : Mixer.music option ref = ref None

type music_t = Groove | Calm

type sound_t =
  | Capture
  | Move
  | PuzzleRushGood
  | PuzzleRushEnd
  | GameOver
  | LevelStart
  | LevelComplete

type sounds_t = {
  capture : Mixer.chunk;
  move : Mixer.chunk;
  puzzle_rush_good : Mixer.chunk;
  puzzle_rush_end : Mixer.chunk;
  game_over : Mixer.chunk;
  level_start : Mixer.chunk;
  level_complete : Mixer.chunk;
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
      | PuzzleRushEnd -> s.puzzle_rush_end)
  | None -> assert false

let play s =
  match Mixer.play_channel (-1) (get_sound s) 0 with
  | Ok _ -> ()
  | Error (`Msg m) -> Printf.eprintf "playing error %s\n%!" m

let music_play m =
  match m with
  | Groove ->
      sdl_try (Mixer.play_music (some_or_fail !groove_music) 0)
  | Calm ->
      sdl_try (Mixer.play_music (some_or_fail !calm_music) 0)

let music_stop () =
  sdl_ignore (Mixer.fade_out_music Mixer.default_frequency)

let init () =
  groove_music := Some (sdl_get_ok (Mixer.load_mus (get_path "MusicGroove.wav")));
  calm_music := Some (sdl_get_ok (Mixer.load_mus (get_path "MusicCalm2.wav")));
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
      }

let release () =
  Mixer.free_chunk (get_sound Move);
  Mixer.free_chunk (get_sound Capture);
  Mixer.free_chunk (get_sound PuzzleRushGood);
  Mixer.free_chunk (get_sound LevelStart);
  Mixer.free_chunk (get_sound LevelComplete);
  Mixer.free_chunk (get_sound GameOver);
  Mixer.free_chunk (get_sound PuzzleRushEnd);
  Mixer.free_music (some_or_fail !groove_music);
  Mixer.free_music (some_or_fail !calm_music)



