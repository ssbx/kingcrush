open CamlSDL2_mixer

let audio_dir : string ref = ref ""
let music_dir : string ref = ref ""
let get_path f = Filename.concat !audio_dir f
let get_mus_path f = Filename.concat !music_dir f

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
    { groove : Mix.Music.t
    ; calm : Mix.Music.t
    }

  let musics : musics_t option ref = ref None

  let get_music (mt : t) =
    match !musics with
    | None -> failwith "no music"
    | Some mus ->
      (match mt with
       | Groove -> mus.groove
       | Calm   -> mus.calm)

  ;;

  let play mt =
    if Mix.playing_music () then Mix.halt_music ();
    let mus = get_music mt in
    Mix.fade_in_music mus ~ms:100 ~loops:0

  ;;
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


  let fade_out ~ms =
    if Mix.playing_music () then (
      Mix.fade_out_music ~ms
    )
    (*
    if Mix.playing_music () then (
      if (Mix.fading_music () <> Mix.FadingMusic.FADING_OUT) then (
      )
    )
    *)

  ;;

  let stop () =
    print_endline "mus stop";
    fade_out ~ms:100

  ;;

  let init () =
    Printf.printf "path %s\n" (get_mus_path "Groove.wav");
    musics
    := Some
         { groove = Mix.load_mus (get_mus_path "Groove.ogg")
         ; calm = Mix.load_mus (get_mus_path "Calm2.ogg")
         }

  ;;

  let release () =
    Mix.free_music (get_music Groove);
    Mix.free_music (get_music Calm)
  ;;
end

let init () =
  audio_dir := Filename.concat !Info.base_dir "sounds";
  music_dir := Filename.concat !Info.base_dir "musics";
  Sample.init ();
  Music.init ()
;;

let release () =
  Sample.release ();
  Music.release ()
;;
