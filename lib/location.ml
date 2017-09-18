type t = {
    startPos : Lexing.position;
    endPos : Lexing.position;
  }

let dummy = {
    startPos = Lexing.dummy_pos;
    endPos = Lexing.dummy_pos;
  }

type 'a loc = {
    item : 'a;
    loc : t;
  }

let mk startPos endPos =
  { startPos; endPos }

let mkloc item loc =
  { item; loc }

let mkdummy item =
  { item; loc = dummy }

let to_string a : string =
  Printf.sprintf "start_pos: %i end_pos: %i\n" a.startPos.Lexing.pos_lnum a.endPos.Lexing.pos_lnum
