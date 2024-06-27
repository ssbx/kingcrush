type level = {
  complete : bool;
  rank     : int;
  themes   : string list;
  npuzzles : int;
  unlocks  : level list;
}


let all = {
  complete = false;
  rank = 1000;
  themes = ["endgame";"long"];
  npuzzles = 10;
  unlocks = [];
}

let init () = ()
