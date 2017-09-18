type t = { startPos : Lexing.position; endPos : Lexing.position; }

val dummy : t

type 'a loc = { item : 'a; loc : t; }

val mk : Lexing.position -> Lexing.position -> t
val mkloc : 'a -> t -> 'a loc
val mkdummy : 'a -> 'a loc

val to_string : t -> string
