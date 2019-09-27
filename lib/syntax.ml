(* This module defines the initial AST structure *)
open Core_kernel

(* module L = Location *)
module S = Symbol

type op =
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | NEq
  | Lt
  | LtEq
  | Gt
  | GtEq
  [@@deriving sexp_of]

type field = {
  name : S.t Location.loc;
  escape : bool ref;
  typ : S.t Location.loc;
  }
  [@@deriving sexp_of]
type ty =
  | NameTy of S.t Location.loc
  | RecordTy of field list
  | ArrayTy of S.t Location.loc
  [@@deriving sexp_of]

type expr =
  | Var of var Location.loc
  | Nil of unit Location.loc
  | Int of int Location.loc
  | String of string Location.loc
  | Call of S.t Location.loc * (* function name *)
            expr Location.loc list
  | UnaryOp of op Location.loc * expr Location.loc
  | Op of op Location.loc * expr Location.loc * expr Location.loc
  | Record of S.t Location.loc * (* type name *)
              (S.t Location.loc * expr Location.loc) list (* elements *)
  | Seq of expr Location.loc list
  | Assign of var Location.loc * expr Location.loc
  | If of expr Location.loc * (* condition *)
          expr Location.loc * (* then *)
          expr Location.loc option  (* else *)
  | While of expr Location.loc * (* condition *)
             expr Location.loc    (* body *)
  | For of S.t * (* indice symbol *)
           bool ref * (* escapes *)
           expr Location.loc * (* from *)
           expr Location.loc * (* to *)
           expr Location.loc (* body *)
  | Break of unit Location.loc
  | Let of dec list *
           expr Location.loc list Location.loc
  | Array of S.t Location.loc * (* type *)
             expr Location.loc * (* size *)
             expr Location.loc (* init *)
  [@@deriving sexp_of]
and var =
  | SimpleVar of S.t Location.loc
  | FieldVar of var Location.loc *
                S.t Location.loc
  | SubscriptVar of var Location.loc *
                    expr Location.loc
  [@@deriving sexp_of]
and dec =
  | FunctionDec of fundec Location.loc list
  | VarDec of vardec Location.loc
  | TypeDec of typedec Location.loc list
  [@@deriving sexp_of]
and fundec = {
  fun_name : S.t Location.loc;
  params : field list;
  result_typ : S.t Location.loc option;
  body : expr Location.loc;
}
  [@@deriving sexp_of]
and vardec = {
  var_name : S.t Location.loc;
  escape : bool ref;
  var_typ : S.t Location.loc option;
  init : expr Location.loc;
}
  [@@deriving sexp_of]
and typedec = {
  type_name : S.t Location.loc;
  typ : ty;
}
  [@@deriving sexp_of]

module Pretty = struct
  open SmartPrint

  let pp_sym s : SmartPrint.t = string @@ snd @@ s.Location.item

  let pp_sym' s : SmartPrint.t = string @@ snd @@ s

  let pp_op o : SmartPrint.t =
    string @@ match o.Location.item with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Eq -> "="
    | NEq -> "<>"
    | Lt -> "<"
    | LtEq -> "<="
    | Gt -> ">"
    | GtEq -> ">="

  let comma_space : SmartPrint.t = string ", "
  let equals : SmartPrint.t = string "="
  let colon_equals : SmartPrint.t = string ":="

  let pp_field (d : field) : SmartPrint.t =
    pp_sym d.name ^^ string ":" ^^ pp_sym d.typ

  let pp_ty (d : ty) : SmartPrint.t =
    match d with
    | NameTy s -> pp_sym s
    | RecordTy fs -> braces @@ separate comma_space @@ List.map ~f:pp_field fs
    | ArrayTy s -> string "array of" ^^ pp_sym s

  let pp_typ_dec (d : typedec Location.loc) : SmartPrint.t =
    string "type" ^^ pp_sym d.Location.item.type_name ^^
      equals ^^ pp_ty d.Location.item.typ

  let rec pp e : SmartPrint.t =
    match e.Location.item with
    | Int {Location.item=n; _} -> string @@ string_of_int @@ n
    | Var v -> pp_var @@ v
    | UnaryOp (o, a) -> pp_op o ^-^ pp a
    | Op (o, a, b) -> pp a ^^ pp_op o ^^ pp b
    | Nil (_) -> string "nil"
    | String {Location.item=s; _} -> double_quotes @@ string s
    | Call (s, e) ->
       let a = separate comma_space @@ List.map ~f:pp e in
       pp_sym s ^^ parens a
    | Seq b ->
       let bs = List.map ~f:pp b in
       parens @@ separate (string "; ") bs
    | Record (name, e) ->
       let b (s,x) = pp_sym s ^^ string "=" ^^ pp x in
       let a = separate comma_space @@ List.map ~f:b e in
       pp_sym name ^^ braces a
    | Assign (s, e) -> pp_var s ^^ colon_equals ^^ pp e
    | If (c, t, e) ->
       let e' = match e with
         | None -> string ""
         | (Some ex) -> newline ^^ string "else" ^^ pp ex in
       string "if" ^^ pp c ^^ newline ^^
       string "then" ^^ pp t ^^ e'
    | While (c, e) -> string "while" ^^ pp c ^^ string "do" ^^ newline ^^
                        indent @@ pp e
    | For (s, _, fr, si, ini) ->
       string "for" ^^ pp_sym' s ^^ colon_equals ^^
         pp fr ^^ string "to" ^^ pp si ^^ string "do" ^^ newline ^^
           indent @@ pp ini
    | Break _ -> string "break"
    | Let (d, b) ->
       let e = separate (string ";" ^^ newline) @@ List.map ~f:pp b.Location.item in
       string "let" ^^ newline ^^
         indent (pp_decs d ^^ newline) ^^
         string "in" ^^ newline ^^
         indent e ^^ newline ^^
         string "end"

    | Array (t, s, i) -> pp_sym t ^^ (brakets @@ pp s) ^^ string "of" ^^ pp i

  and pp_fun_dec (d: fundec Location.loc) : SmartPrint.t =
      let f = d.Location.item in
      let p = separate comma_space @@ List.map ~f:pp_field f.params in
      string "function" ^^ pp_sym f.fun_name ^^ parens p ^^
        Option.value_map ~f:(fun x -> string ":" ^^ pp_sym x) ~default:(string "") f.result_typ ^^
          string "=" ^^ newline ^^
            indent (pp f.body)

  and pp_dec (d: dec) : SmartPrint.t =
    match d with
    | FunctionDec a -> separate newline @@ List.map ~f:pp_fun_dec a
    | VarDec a -> pp_var_dec a
    | TypeDec td -> separate newline @@ List.map ~f:pp_typ_dec td

  and pp_decs (d : dec list) : SmartPrint.t =
    separate (newline) @@ List.map ~f:pp_dec d

  and pp_var_dec (d : vardec Location.loc) : SmartPrint.t =
    let a = Option.value_map ~f:(fun x -> string ":" ^-^ pp_sym x)
                             ~default:(string "")
                             d.Location.item.var_typ in
    string "var" ^^ pp_sym d.Location.item.var_name ^-^ a ^^ colon_equals ^^ pp d.Location.item.init

  and pp_var v : SmartPrint.t =
    match v.Location.item with
    | SimpleVar {Location.item=sv; _} -> pp_sym' sv
    | FieldVar (fv, e) -> pp_var fv ^-^ string "." ^-^ pp_sym e
    | SubscriptVar (v, e) -> pp_var v ^^ pp e

  let print_to_string prog : string = to_string 80 4 @@ pp prog
end

let strip_loc (a : expr Location.loc) : expr = a
(* let rec strip_loc = function
 *   | Var {item = v; _} -> Var (Location.mkdummy @@ strip_loc_var v)
 *   | Nil {item = u; _} -> Nil (Location.mkdummy u)
 *   | Int {item = i; _} -> Int (Location.mkdummy i)
 *   | String {item = str; _} -> String (Location.mkdummy str)
 *   (\* | Call [((t, _loc), (expr, _loc))] ->  *\)
 *   | UnaryOp ({item = op; _}, {item = expr; _}) ->  UnaryOp (Location.mkdummy op, Location.mkdummy @@ strip_loc expr)
 *   | Op ({item = op; _}, {item = lhs; _}, {item = rhs; _}) -> Op (Location.mkdummy op, Location.mkdummy @@ strip_loc lhs, Location.mkdummy @@ strip_loc rhs)
 *   | Record of S.t Location.loc * (\* type name *\)
 *               (S.t Location.loc * expr Location.loc) list (\* elements *\)
 *   | Seq of expr Location.loc list
 *   | Assign of var Location.loc * expr Location.loc
 *   | If of expr Location.loc * (\* condition *\)
 *           expr Location.loc * (\* then *\)
 *           expr Location.loc option  (\* else *\)
 *   | While of expr Location.loc * (\* condition *\)
 *              expr Location.loc    (\* body *\)
 *   | For of S.t * (\* indice symbol *\)
 *            bool ref * (\* escapes *\)
 *            expr Location.loc * (\* from *\)
 *            expr Location.loc * (\* to *\)
 *            expr Location.loc (\* body *\)
 *   | Break of unit Location.loc
 *   | Let of dec list *
 *            expr Location.loc list Location.loc
 *   | Array of S.t Location.loc * (\* type *\)
 *              expr Location.loc * (\* size *\)
 *              expr Location.loc (\* init *\)
 * and strip_loc_var = function
 *   | SimpleVar {item = t; _} -> SimpleVar {item = t; loc = Location.dummy}
 *   | FieldVar ({item = var; _}, {item = t; _}) -> FieldVar ({item = var; loc = Location.dummy}, {item = t; loc = Location.dummy})
 *   | SubscriptVar ({item = var; _}, {item = expr; _}) -> SubscriptVar ({item = var; loc = Location.dummy}, {item = expr; loc = Location.dummy}) *)
