%{
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
%right LBRACK
%left PIPE
%left AMPER
%nonassoc EQ NEQ GT GTEQ LT LTEQ
%left PLUS MINUS
%left TIMES DIV
%nonassoc UNARYMINUS
%start <Syntax.expr Location.loc> prog

%%

(* Tag with location *)
%inline loc(X) :
| x = X { L.mkloc x (L.mk $startpos $endpos) }

symbol :
| x = loc(IDENT) { L.mkloc (Symbol.symbol x.L.item) x.L.loc }

prog:
  | e = loc(expr) EOF { e }

  | error {Error.syntax_error (L.mk $startpos $endpos) ""}

expr:
  | var = loc(lvalue) { S.Var var }
  | unit = loc(NIL) { S.Nil unit }
  | LPAREN seq = separated_list(SEMICOLON, loc(expr)) RPAREN
    { S.Seq seq }
  | i = loc(INT)
    { S.Int i }
  | _minus = MINUS e = loc (expr) {
    S.UnaryOp (
      L.mkloc S.Minus (L.mk $startpos(_minus) $endpos(_minus)),
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
  }
  | e1 = loc(expr) PIPE e2 = loc(expr) {
    S.If (
      e1,
      L.mkdummy (S.Int (L.mkdummy 1)),
      Some e2
    )
  }
  | s = loc(STRING) { S.String s }
  | fn = symbol LPAREN formals = separated_list(COMMA, loc(expr)) RPAREN {
    S.Call (
      fn,
      formals
    )
   }
  | typ = symbol LBRACE fields = separated_list(COMMA, field_assign) RBRACE {
    S.Record (
      typ,
      fields
    )
   }
  | typ = symbol LBRACK size = loc(expr) RBRACK OF init = loc(expr) {
    S.Array (
      typ,
      size,
      init
    )
  } %prec LBRACK
  | var = loc(lvalue) COLONEQ exp = loc(expr) {
    S.Assign (
      var,
      exp
    )
  }
  | IF cond = loc(expr) THEN if_true = loc(expr) {
    S.If (
      cond,
      if_true,
      None
    )
  } %prec ELSE
  | IF cond = loc(expr) THEN if_true = loc(expr) ELSE if_false = loc(expr) {
    S.If (
      cond,
      if_true,
      Some if_false
    )
  }
  | WHILE cond = loc(expr) DO body = loc(expr) {
    S.While (
      cond,
      body
    )
  } %prec LOOP
  | FOR i = symbol COLONEQ from = loc(expr) TO toe = loc(expr) DO body = loc(expr) {
    S.For (
      i.L.item,
      ref true,
      from,
      toe,
      body
    )
  } %prec LOOP
  | unit = loc(BREAK) { S.Break unit }
  | LET decs = decs IN body = loc(separated_list(SEMICOLON, loc(expr))) END {
    S.Let (
      decs,
      body
    )
  }
  | unit = loc(EOF) {S.Nil unit}

lvalue :
  | x = symbol { S.SimpleVar x }
  | var = loc(lvalue) DOT field = symbol {
    S.FieldVar (
      var,
      field
    )
  }
  | var = symbol LBRACK subscript = loc(expr) RBRACK {
    (* Redundant but needed to solve a conflict *)
    S.SubscriptVar (
      L.mkloc (S.SimpleVar var) var.L.loc,
      subscript
    )
  }
  | var = loc(lvalue) LBRACK subscript = loc(expr) RBRACK {
    S.SubscriptVar (
      var,
      subscript
    )
  }

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

field_assign :
  | name = symbol EQ exp = loc(expr) { (name, exp) }

decs :
  | l = dec* { l }

dec :
  | t = loc(tydec)+ { S.TypeDec t }
  | v = loc(vardec) { S.VarDec v }
  | f = loc(fundec)+ { S.FunctionDec f }


%inline tydec :
  | TYPE type_name = symbol EQ typ = ty { S.{ type_name; typ } }

ty :
  | t = symbol { S.NameTy t }
  | LBRACE fields = tyfields RBRACE { S.RecordTy fields }
  | ARRAY OF ty = symbol { S.ArrayTy ty }

tyfields :
  | fields = separated_list(COMMA, tyfield) { fields }

tyfield :
  | name = symbol COLON typ = symbol { S.{ name; typ; escape = ref true  } }

vardec :
| VAR var_name = symbol COLONEQ init = loc(expr) {
    S.{
      var_name;
      escape = ref true;
      var_typ = None;
      init;
    }
  }
| VAR var_name = symbol COLON var_typ = symbol COLONEQ init = loc(expr) {
    S.{
      var_name;
      escape = ref true;
      var_typ = Some var_typ;
      init;
    }
  }

%inline fundec :
  | FUNCTION fun_name = symbol LPAREN params = tyfields RPAREN EQ body = loc(expr) {
    S.{
      fun_name;
      params;
      result_typ = None;
      body
    }
  }
  | FUNCTION fun_name = symbol LPAREN params = tyfields RPAREN
    COLON result_typ = symbol EQ body = loc(expr) {
    S.{
      fun_name;
      params;
      result_typ = Some result_typ;
      body
    }
  }
