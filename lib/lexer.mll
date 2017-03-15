{
  open Lexing
  open Error
  open Parser
  open Printf

  exception Error of string

  let special_char = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | '\\' -> '\\'
    | '"' -> '"'
    | _ -> failwith "Lexer.special_char"
}

let newline = '\r' | '\n' | "\r\n"
let digits = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let special_char = ['n' 't' '\\' '"']
let white = ['\t' ' ']

rule lexer = parse
  | "/*" {comment 1 lexbuf}
  | '"' { string (Buffer.create 16) lexbuf }
  | eof { EOF }
  | newline { new_line lexbuf; lexer lexbuf }
  | white+ { lexer lexbuf }
  | "while"{ WHILE }
  | "for" { FOR }
  | "to" { TO }
  | "break" { BREAK }
  | "let" { LET }
  | "in" { IN }
  | "end" { END }
  | "function" { FUNCTION }
  | "var" { VAR }
  | "type" { TYPE }
  | "array" { ARRAY }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "do" { DO }
  | "of" { OF }
  | "nil" { NIL }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | '.' { DOT }
  | '&' { AMPER }
  | '|' { PIPE }
  | ":=" { COLONEQ }
  | '=' { EQ }
  | "<>" { NEQ }
  | '<' { LT }
  | "<=" { LTEQ }
  | '>' { GT }
  | ">=" { GTEQ }
  | (digits | '_')+ as n { INT (int_of_string n) }
  | alpha (alpha | digits | '_')* as v { IDENT v }
and string buf = parse
  | '"' {
      STRING (Buffer.contents buf)
    }
  | '\n' {
      new_line lexbuf;
      Buffer.add_char buf '\n';
      string buf lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      string buf lexbuf
    }
  | '\\' (special_char as c) {
    Buffer.add_char buf (special_char c);
    string buf lexbuf
    }
  | '\\' (digits digits digits) as code {
    Buffer.add_char buf (Char.chr (int_of_string code));
    string buf lexbuf
    }
  | '\\' (white | newline) {
    string_ignore buf lexbuf
    }
  | eof {
      lexing_error
        (Location.mk (lexeme_start_p lexbuf) (lexeme_start_p lexbuf))
        "Non closed string"
    }

and string_ignore buf = parse
  | '\\' { string buf lexbuf }
  | newline { new_line lexbuf; string buf lexbuf }
  | white { string_ignore buf lexbuf }
  | _ as c {
    lexing_error
      (Location.mk (lexeme_start_p lexbuf) (lexeme_start_p lexbuf))
      (sprintf "Illegal character %c in a formatting sequence" c)
    }

and comment depth = parse
  | "/*" { comment (depth + 1) lexbuf }
  | "*/" {
    if depth = 1 then
      lexer lexbuf
    else
      comment (depth - 1) lexbuf
    }
  | eof {
      lexing_error
        (Location.mk (lexeme_start_p lexbuf) (lexeme_start_p lexbuf))
        "Non closed commentary"
    }
  | newline { new_line lexbuf; comment depth lexbuf }
  | _ { comment depth lexbuf }
