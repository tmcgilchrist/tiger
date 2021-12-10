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
  | x = X { Location.mkloc x (Location.mk $startpos $endpos) }

symbol :
  | x = loc(IDENT) { Location.mkloc (Symbol.symbol x.Location.item) x.Location.loc }

prog:
  | e = loc(expr) EOF { e }

  | error {Error.syntax_error (Location.mk $startpos $endpos) ""}

expr:
  | var = loc(lvalue) { Syntax.Var var }
  | unit = loc(NIL) { Syntax.Nil unit }
  | LPAREN seq = separated_list(SEMICOLON, loc(expr)) RPAREN
    { Syntax.Seq seq }
  | i = loc(INT)
    { Syntax.Int i }
  | _minus = MINUS e = loc (expr) {
                             Syntax.UnaryOp (
                                 Location.mkloc Syntax.Minus (Location.mk $startpos(_minus) $endpos(_minus)),
                                 e
                               )
  } %prec UNARYMINUS
  | e1 = loc(expr) o = loc(op) e2 = loc(expr)
    { Syntax.Op (o, e1, e2) }
  | e1 = loc(expr) AMPER e2 = loc(expr) {
                                   Syntax.If (
      e1,
      e2,
      Some (Location.mkdummy (Syntax.Int (Location.mkdummy 0)))
    )
  }
  | e1 = loc(expr) PIPE e2 = loc(expr) {
                                  Syntax.If (
      e1,
      Location.mkdummy (Syntax.Int (Location.mkdummy 1)),
      Some e2
    )
  }
  | s = loc(STRING) { Syntax.String s }
  | fn = symbol LPAREN formals = separated_list(COMMA, loc(expr)) RPAREN {
                                                 Syntax.Call (
                                                     fn,
                                                     formals
                                                   )
                                               }
  | typ = symbol LBRACE fields = separated_list(COMMA, field_assign) RBRACE {
                                                 Syntax.Record (
                                                     typ,
                                                     fields
    )
   }
  | typ = symbol LBRACK size = loc(expr) RBRACK OF init = loc(expr) {
                                                               Syntax.Array (
      typ,
      size,
      init
    )
  } %prec LBRACK
  | var = loc(lvalue) COLONEQ exp = loc(expr) {
                                         Syntax.Assign (
      var,
      exp
    )
  }
  | IF cond = loc(expr) THEN if_true = loc(expr) {
                                            Syntax.If (
      cond,
      if_true,
      None
    )
  } %prec ELSE
  | IF cond = loc(expr) THEN if_true = loc(expr) ELSE if_false = loc(expr) {
                                                                      Syntax.If (
      cond,
      if_true,
      Some if_false
    )
  }
  | WHILE cond = loc(expr) DO body = loc(expr) {
                                          Syntax.While (
      cond,
      body
    )
  } %prec LOOP
  | FOR i = symbol COLONEQ from = loc(expr) TO toe = loc(expr) DO body = loc(expr) {
                                                                              Syntax.For (
        i.Location.item,
      ref true,
      from,
      toe,
      body
    )
  } %prec LOOP
  | unit = loc(BREAK) { Syntax.Break unit }
  | LET decs = decs IN body = loc(separated_list(SEMICOLON, loc(expr))) END {
                                   Syntax.Let (
      decs,
      body
    )
  }
  | unit = loc(EOF) {Syntax.Nil unit}

lvalue :
  | x = symbol { Syntax.SimpleVar x }
  | var = loc(lvalue) DOT field = symbol {
                                      Syntax.FieldVar (
      var,
      field
    )
  }
  | var = symbol LBRACK subscript = loc(expr) RBRACK {
    (* Redundant but needed to solve a conflict *)
                                         Syntax.SubscriptVar (
                                             Location.mkloc (Syntax.SimpleVar var) var.Location.loc,
        subscript
    )
  }
  | var = loc(lvalue) LBRACK subscript = loc(expr) RBRACK {
                                              Syntax.SubscriptVar (
      var,
      subscript
    )
  }

%inline op :
  | PLUS { Syntax.Plus }
  | MINUS { Syntax.Minus }
  | TIMES { Syntax.Times }
  | DIV { Syntax.Div }
  | EQ { Syntax.Eq }
  | NEQ { Syntax.NEq }
  | GT { Syntax.Gt }
  | GTEQ { Syntax.GtEq }
  | LT { Syntax.Lt }
  | LTEQ { Syntax.LtEq }

field_assign :
  | name = symbol EQ exp = loc(expr) { (name, exp) }

decs :
  | l = dec* { l }

dec :
  | t = loc(tydec)+ { Syntax.TypeDec t }
  | v = loc(vardec) { Syntax.VarDec v }
  | f = loc(fundec)+ { Syntax.FunctionDec f }


%inline tydec :
  | TYPE type_name = symbol EQ typ = ty { Syntax.{ type_name; typ } }

ty :
  | t = symbol { Syntax.NameTy t }
  | LBRACE fields = tyfields RBRACE { Syntax.RecordTy fields }
  | ARRAY OF ty = symbol { Syntax.ArrayTy ty }

tyfields :
  | fields = separated_list(COMMA, tyfield) { fields }

tyfield :
  | name = symbol COLON typ = symbol { Syntax.{ name; typ; escape = ref true  } }

vardec :
| VAR var_name = symbol COLONEQ init = loc(expr) {
                                            Syntax.{
      var_name;
      escape = ref true;
      var_typ = None;
      init;
    }
  }
| VAR var_name = symbol COLON var_typ = symbol COLONEQ init = loc(expr) {
                                                                   {
                                                                     Syntax.var_name = var_name;
                                                                     escape = ref true;
                                                                     var_typ = Some var_typ;
                                                                     init;
                                                                   }
                                                                 }

%inline fundec :
  | FUNCTION fun_name = symbol LPAREN params = tyfields RPAREN EQ body = loc(expr) {
                                                                              Syntax.{
      fun_name;
      params;
      result_typ = None;
      body
    }
  }
  | FUNCTION fun_name = symbol LPAREN params = tyfields RPAREN
    COLON result_typ = symbol EQ body = loc(expr) {
                                             Syntax.{
      fun_name;
      params;
      result_typ = Some result_typ;
      body
    }
  }
