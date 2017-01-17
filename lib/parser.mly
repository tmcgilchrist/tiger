%{
  open Lexing
  module S = Syntax
  module L = Location
%}

%token WHILE FOR TO BREAK LET IN END FUNCTION VAR
%token TYPE ARRAY IF THEN ELSE DO OF NIL
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token COMMA SEMICOLON COLON DOT AMPER PIPE COLONEQ
%token EQ NEQ LT LTEQ GT GTEQ
%token EOF

%token <int> INT
%token <string> STRING
%token <string> IDENT

%start <Syntax.expr Location.loc> prog

%%

(* Tag with location *)
%inline loc(X) :
| x = X { L.mkloc x (L.mk $startpos $endpos) }

prog:
  | e = loc(expr) EOF { e }
  | error  {Error.syntax_error (L.mk $startpos $endpos) ""}

expr:
| i = loc(INT) { S.Int i }
| e1 = loc(expr) o = loc(op) e2 = loc(expr)
    { S.Op (o, e1, e2) }
| unit = loc(NIL) { S.Nil unit }

%inline op :
| PLUS { S.Plus }
| MINUS { S.Minus }
| TIMES { S.Times }
| DIV { S.Div }
| EQ { S.Eq }
| NEQ { S.NEq }
| GT { S.Gt }
| GTEQ { S.GtEq }
| LT { S.Lt }
| LTEQ { S.LtEq }
