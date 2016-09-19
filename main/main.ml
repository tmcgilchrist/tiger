open Core.Std

open Syntax

(* the name of the file which contains the expressions *)
let filename = Sys.argv.(1)

let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    Printf.printf "%s\n" (Syntax.Print.pp_expr (Parser.prog Lexer.token filebuf))
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf);
  In_channel.close input

let _ = main ()
