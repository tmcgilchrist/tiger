type t = {
    startPos : Lexing.position;
    endPos : Lexing.position;
  }

val dummy : t

type 'a loc = { item : 'a; loc : t; }

val sexp_of_loc : ('a -> Sexplib0.Sexp.t) -> 'a loc -> Sexplib0.Sexp.t
val to_string : t -> string

val mk : Lexing.position -> Lexing.position -> t
val mkloc : 'a -> t -> 'a loc
val mkdummy : 'a -> 'a loc
