%{
  open Lexing
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
%token <string> ID

%start <Syntax.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
| i = INT
    { Syntax.Int i }
| e1 = expr PLUS e2 = expr
    { (Syntax.Op (Syntax.PLUS, e1, e2)) }
| e1 = expr MINUS e2 = expr
    { (Syntax.Op (Syntax.MINUS, e1, e2)) }
| e1 = expr TIMES e2 = expr
    { (Syntax.Op (Syntax.TIMES, e1, e2)) }
