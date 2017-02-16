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

%right FUNCTION TYPE
%nonassoc LOOP
%right ELSE
%nonassoc COLONEQ
%right LBRACKET
%left PIPE
%left AMPER
%nonassoc EQ NEQ GT GE LT LE
%left PLUS MINUS
%left TIMES DIVIDE
%left UNARYMINUS

%start <Syntax.expr Location.loc> prog

%%

(* Tag with location *)
%inline loc(X) :
| x = X { L.mkloc x (L.mk $startpos $endpos) }

symbol :
| x = loc(IDENT) { x }

prog:
  | e = loc(expr) EOF { e }
  | error  {Error.syntax_error (L.mk $startpos $endpos) ""}

expr:
  | v = loc(lvalue)
    { S.Var v }
  | u = loc(NIL)
    { S.Nil u }
  | LPAREN seq = separated_list(SEMICOLON, loc(expr)) RPAREN
    { S.Seq seq }
  | i = loc(INT)
    { S.Int i }
  | _minus = MINUS e = loc (expr) {
    S.Op (
      L.mkloc S.Minus (L.mk $startpos(_minus) $endpos(_minus)),
      L.mkdummy (S.Int (L.mkdummy 0)),
      e
    )
  } %prec UNARYMINUS
  | e1 = loc(expr) o = loc(op) e2 = loc(expr)
    { S.Op (o, e1, e2) }
  | e1 = loc(expr) AMPER e2 = loc(expr) {
    S.If (
      e1,
      e2,
      Some (L.mkdummy (S.Int (L.mkdummy 0)))
    )
  } %prec AMPER
  | e1 = loc(expr) PIPE e2 = loc(expr) {
    S.If (
      e1,
      L.mkdummy (S.Int (L.mkdummy 1)),
      Some e2
    )
  }
  | e1 = loc(expr) PIPE e2 = loc(expr) {
    S.If (
      e1,
      L.mkdummy (S.Int (L.mkdummy 1)),
      Some e2
    )
  }
  | s = loc(STRING)
    { S.String s }

lvalue :
  | x = symbol { S.SimpleVar x }

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
