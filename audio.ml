open CamlSDL2_mixer

let audio_dir : string ref = ref ""
let get_path f = Filename.concat !audio_dir f

module Sample = struct
  type t =
    | Capture
    | Move
    | PuzzleRushGood
    | PuzzleRushEnd
    | GameOver
    | LevelStart
    | LevelComplete

  type samples_t =
    { capture : Mix.Chunk.t
    ; move : Mix.Chunk.t
    ; puzzle_rush_good : Mix.Chunk.t
    ; puzzle_rush_end : Mix.Chunk.t
    ; game_over : Mix.Chunk.t
    ; level_start : Mix.Chunk.t
    ; level_complete : Mix.Chunk.t
    }

  let samples : samples_t option ref = ref None

  let get_sample t =
    match !samples with
    | Some s ->
      (match t with
       | Move -> s.move
       | Capture -> s.capture
       | PuzzleRushGood -> s.puzzle_rush_good
       | LevelStart -> s.level_start
       | LevelComplete -> s.level_complete
       | GameOver -> s.game_over
       | PuzzleRushEnd -> s.puzzle_rush_end)
    | None -> failwith "no sounds"
  ;;

  let play smp = Mix.play_channel ~channel:(-1) ~chunk:(get_sample smp) ~loops:0

  let init () =
    samples
    := Some
         { capture = Mix.load_wav (get_path "Capture.wav")
         ; move = Mix.load_wav (get_path "Move.wav")
         ; puzzle_rush_good = Mix.load_wav (get_path "PuzzleRushGood.wav")
         ; puzzle_rush_end = Mix.load_wav (get_path "PuzzleRushEnd.wav")
         ; game_over = Mix.load_wav (get_path "GameOver.wav")
         ; level_start = Mix.load_wav (get_path "LevelStart.wav")
         ; level_complete = Mix.load_wav (get_path "LevelComplete.wav")
         }
  ;;

  let release () =
    Mix.free_chunk (get_sample Move);
    Mix.free_chunk (get_sample Capture);
    Mix.free_chunk (get_sample PuzzleRushGood);
    Mix.free_chunk (get_sample LevelStart);
    Mix.free_chunk (get_sample LevelComplete);
    Mix.free_chunk (get_sample GameOver);
    Mix.free_chunk (get_sample PuzzleRushEnd)
  ;;
end

module Music = struct
  type t =
    | Groove
    | Calm

  type musics_t =
    { groove : Mix.Chunk.t
    ; calm : Mix.Chunk.t
    }

  let music_chan : int option ref = ref None
  let musics : musics_t option ref = ref None

  let get_music (mt : t) =
    match !musics with
    | None -> failwith "no music"
    | Some mus ->
      (match mt with
       | Groove -> mus.groove
       | Calm -> mus.calm)
  ;;

  let fade_out ~ms =
    match !music_chan with
    | Some chan ->
      Mix.fade_out_channel ~channel:chan ~ms;
      music_chan := None
    | None -> ()
  ;;

  let stop () = fade_out ~ms:100

  let play mus =
    stop ();
    music_chan := Some (Mix.play_channel_ ~channel:(-1) ~chunk:(get_music mus) ~loops:0)
  ;;

  let init () =
    musics
    := Some
         { groove = Mix.load_wav (get_path "MusicGroove.wav")
         ; calm = Mix.load_wav (get_path "MusicCalm2.wav")
         }
  ;;

  let release () =
    Mix.free_chunk (get_music Groove);
    Mix.free_chunk (get_music Calm)
  ;;
end

let init () =
  audio_dir := Filename.concat !Info.base_dir "sounds";
  Sample.init ();
  Music.init ()
;;

let release () =
  Sample.release ();
  Music.release ()
;;
