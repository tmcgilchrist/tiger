{
  open Parser

  exception Error of string
}

let newline = '\r' | '\n' | "\r\n"
let digits = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| "while"
    { WHILE }
| "for"
    { FOR }
| "to"
    { TO }
| "break"
    { BREAK }
| "let"
    { LET }
| "in"
    { IN }
| "end"
    { END }
| "function"
    { FUNCTION }
| "var"
    { VAR }
| "type"
    { TYPE }
| "array"
    { ARRAY }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "do"
    { DO }
| "of"
    { OF }
| "nil"
    { NIL }
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
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| '['
    { LBRACK }
| ']'
    { RBRACK }
| ','
    { COMMA }
| ';'
    { SEMICOLON }
| ':'
    { COLON }
| '.'
    { DOT }
| '&'
    { AMPER }
| '|'
    { PIPE }
| ":="
    { COLONEQ }
| '='
    { EQ }
| "<>"
    { NEQ }
| '<'
    { LT }
| "<="
    { LTEQ }
| '>'
    { GT }
| ">="
    { GTEQ }
| eof
    { EOF }
| (digits | '_')+ as n { INT (int_of_string n) }
| alpha (alpha | digits | '_')* as v { ID v }
