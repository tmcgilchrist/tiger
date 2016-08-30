open Core.Std

(* the name of the file which contains the expressions *)
let filename = Sys.argv.(1)

let print_int_list l =
  let s = List.to_string ~f:(fun(x) -> Int.to_string x) l in
  printf "%s\n" s

let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    print_int_list (Parser.main Lexer.token filebuf)
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf);
  In_channel.close input

let _ = main ()
