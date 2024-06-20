open Sexplib

let cheat = "data/anims/explosion1.sc"

type sprite_cheat = {
  sprite_width : int;
  sprite_height : int;
  cheat_width : int;
  cheat_height : int;
}

let p2 = function
  | Sexp.List [
      Sexp.List [
        Atom a;
        Sexp.List [Atom b; Atom c ];
        Sexp.List [Atom d; Atom e ]];
      Sexp.List [
        Atom f;
        Sexp.List [Atom g; Atom h ];
        Sexp.List [Atom i; Atom j ]]] when
          String.equal a "sprite" &&
          String.equal f "cheat"  &&
          String.equal b "width"  &&
          String.equal d "height" &&
          String.equal g "width"  &&
          String.equal i "height"
          ->
            let spr = {
              sprite_width = int_of_string c;
              sprite_height = int_of_string e;
              cheat_width = int_of_string h;
              cheat_height = int_of_string j;
            } in
            Printf.printf "sprite w:%i h:%i cheat w:%i h:%i\n" spr.sprite_width spr.sprite_height spr.cheat_width spr.cheat_height
  | _ -> print_endline "nomatch"

let () =

(* Build an Sexp from: (This (is an) (s expression)) *)
  let exp1 = Sexp.(List [
    Atom "This";
    List [Atom "is"; Atom "an"];
    List [Atom "s"; Atom "expression"]
  ]) in
  (* Serialize an Sexp object into a string *)
  print_endline (Sexp.to_string exp1);
  (* Parse a string and produce a Sexp object  *)
  let exp2 = Sexp.of_string "(This (is an) (s expression))" in
  (* Ensure we parsed what we expected. *)
  assert (Sexp.compare exp1 exp2 = 0);


  print_endline "";
  let c = open_in cheat in
  let l = in_channel_length c in
  let str = really_input_string c l in
  close_in c;
  let exp = Sexp.of_string str in
  p2 exp;
  print_endline str;

