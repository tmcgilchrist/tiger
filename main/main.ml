open Core_kernel

(* the name of the file which contains the expressions *)
let filename = Sys.argv.(1)

let main () =
  let input = In_channel.create filename in
  let filebuf = Lexing.from_channel input in
  try
    Printf.printf "%s\n" (Syntax.Pretty.print_to_string (Parser.prog Lexer.lexer filebuf))
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf);
  In_channel.close input

let _ = main ()
