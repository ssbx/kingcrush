(* This module seams over killing, BUT, I have to many index access errors on
   arrays and lists at runtime. This should constraint theses. Idealy, the lib
   should only offer access to board trought defined types.*)

exception Wrong_chessboard_coordinate of string

type color_t   = Black | White
type figure_t  = R | N | B | K | Q | P
type piece_t   = (figure_t * color_t)
type square_t  = piece_t option
type row_id    = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
type column_id = CA | CB | CC | CD | CE | CF | CG | CH
type orient_t  = WhiteBottom | BlackBottom

type t = {
  orient : orient_t;
  position :
         (* 8        7        6        5        4        3        2        1    *)
(* a *) ((square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
(* b *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
(* c *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
(* d *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
(* e *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
(* f *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
(* g *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t) *
(* h *)  (square_t*square_t*square_t*square_t*square_t*square_t*square_t*square_t))
}

let empty_position = {
  orient = WhiteBottom;
  position = (
    (None,None,None,None,None,None,None,None),
    (None,None,None,None,None,None,None,None),
    (None,None,None,None,None,None,None,None),
    (None,None,None,None,None,None,None,None),
    (None,None,None,None,None,None,None,None),
    (None,None,None,None,None,None,None,None),
    (None,None,None,None,None,None,None,None),
    (None,None,None,None,None,None,None,None)
  )
}

let default_position = {
  orient = WhiteBottom;
  position = (
    (Some(R,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(R,White)),
    (Some(B,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(B,White)),
    (Some(N,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(N,White)),
    (Some(Q,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(Q,White)),
    (Some(K,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(K,White)),
    (Some(N,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(N,White)),
    (Some(B,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(B,White)),
    (Some(R,Black),Some(P,Black),None,None,None,None,Some(P,White),Some(R,White))
  )
}

let row_of_int = function
  | 0 -> R8 | 1 -> R7 | 2 -> R6 | 3 -> R5
  | 4 -> R4 | 5 -> R3 | 6 -> R2 | 7 -> R1
  | n -> raise (Wrong_chessboard_coordinate
          (Printf.sprintf "request row %i but expect an int in range 0 to 7" n))

let column_of_int = function
  | 0 -> CA | 1 -> CB | 2 -> CC | 3 -> CD
  | 4 -> CE | 5 -> CF | 6 -> CG | 7 -> CH
  | n -> raise (Wrong_chessboard_coordinate
          (Printf.sprintf "request column %i but expect an int in range 0 to 7" n))

let get_column column position =
  match column, position with
  | CA, (c,_,_,_,_,_,_,_) -> c
  | CB, (_,c,_,_,_,_,_,_) -> c
  | CC, (_,_,c,_,_,_,_,_) -> c
  | CD, (_,_,_,c,_,_,_,_) -> c
  | CE, (_,_,_,_,c,_,_,_) -> c
  | CF, (_,_,_,_,_,c,_,_) -> c
  | CG, (_,_,_,_,_,_,c,_) -> c
  | CH, (_,_,_,_,_,_,_,c) -> c

let get_square row column =
  match row, column with
  | R1, (_,_,_,_,_,_,_,s) -> s
  | R2, (_,_,_,_,_,_,s,_) -> s
  | R3, (_,_,_,_,_,s,_,_) -> s
  | R4, (_,_,_,_,s,_,_,_) -> s
  | R5, (_,_,_,s,_,_,_,_) -> s
  | R6, (_,_,s,_,_,_,_,_) -> s
  | R7, (_,s,_,_,_,_,_,_) -> s
  | R8, (s,_,_,_,_,_,_,_) -> s

(* Axises follows SDL2 direction:
    x left -> right (a -> b), y top -> bottom (8 -> 1) *)
let square_at b x y =
  let (x, y) = if b.orient = WhiteBottom then (x, y) else (7 - x, 7 - y) in
  get_square (row_of_int y) (get_column (column_of_int x) b.position)

