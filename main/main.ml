
(* the name of the file which contains the expressions *)
let filename = Sys.argv.(1)

let main () =
  let input = In_channel.open_text filename in
  let filebuf = Lexing.from_channel input in
  try
    (* TODO Convert Pretty module to PPrint library and then fix this printing. *)
    let _ = Parser.prog Lexer.lexer filebuf in
    Printf.printf "%s%!" "yes"
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf);
  In_channel.close input

let _ = main ()
