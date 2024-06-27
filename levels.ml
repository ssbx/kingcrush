type level = {
  completed : bool;
  rank      : int;
  themes    : string list;
  npuzzles  : int;
  doc       : string;
}


let levels = {
  completed = false;
  rank = 1000;
  doc = "docdoc";
  themes = ["endgame";"long"];
  npuzzles = 10;
}

let init () = ()
