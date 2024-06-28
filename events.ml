type t =
  | NewPuzzle
  | LevelComplete
  | GameOver
  | PuzzleSolved
  | OponentMove of (int * int)
  | PlayerMove of (int * int)
  | MoveForward of (int * int)
  | MoveBackward of (int * int)
  | Update
