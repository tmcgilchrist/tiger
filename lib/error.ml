open Core_kernel

type t =
  | LexingError
  | SyntaxError

(* TODO use with sexp *)
exception Error of t * Location.t * string

let name_of_error = function
  | SyntaxError -> "Syntax error"
  | LexingError -> "Lexing error"

let raise_error err loc msg =
  raise @@ Error (err, loc, msg)

let syntax_error = raise_error SyntaxError
let lexing_error = raise_error LexingError

let msg_of_error e loc msg =
  let open Location in
  let open Lexing in
  let startpos, endpos = loc.startPos, loc.endPos in
  printf "%s:\n\tFrom line %d, column %d to line %d, column %d\n\t%s"
    (name_of_error e)
    startpos.pos_lnum (startpos.pos_cnum - startpos.pos_bol)
    endpos.pos_lnum (endpos.pos_cnum - endpos.pos_bol)
    msg
