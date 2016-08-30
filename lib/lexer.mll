{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
(* add the semicolon as a new token *)
| ';'
    { SEMICOLON }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
