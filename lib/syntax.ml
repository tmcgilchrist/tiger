(* This module defines the initial AST structure *)
open Core_kernel

module L = Location
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

type field = {
  name : S.t L.loc;
  escape : bool ref;
  typ : S.t L.loc;
}

type ty =
  | NameTy of S.t L.loc
  | RecordTy of field list
  | ArrayTy of S.t L.loc

type expr =
  | Var of var L.loc
  | Nil of unit L.loc
  | Int of int L.loc
  | String of string L.loc
  | Call of S.t L.loc * (* function name *)
            expr L.loc list
  | UnaryOp of op L.loc * expr L.loc
  | Op of op L.loc * expr L.loc * expr L.loc
  | Record of S.t L.loc * (* type name *)
              (S.t L.loc * expr L.loc) list (* elements *)
  | Seq of expr L.loc list
  | Assign of var L.loc * expr L.loc
  | If of expr L.loc * (* condition *)
          expr L.loc * (* then *)
          expr L.loc option  (* else *)
  | While of expr L.loc * (* condition *)
             expr L.loc    (* body *)
  | For of S.t * (* indice symbol *)
           bool ref * (* escapes *)
           expr L.loc * (* from *)
           expr L.loc * (* to *)
           expr L.loc (* body *)
  | Break of unit L.loc
  | Let of dec list *
           expr L.loc list L.loc
  | Array of S.t L.loc * (* type *)
             expr L.loc * (* size *)
             expr L.loc (* init *)
and var =
  | SimpleVar of S.t L.loc
  | FieldVar of var L.loc *
                S.t L.loc
  | SubscriptVar of var L.loc *
                    expr L.loc
and dec =
  | FunctionDec of fundec L.loc list
  | VarDec of vardec L.loc
  | TypeDec of typedec L.loc list

and fundec = {
  fun_name : S.t L.loc;
  params : field list;
  result_typ : S.t L.loc option;
  body : expr L.loc;
}

and vardec = {
  var_name : S.t L.loc;
  escape : bool ref;
  var_typ : S.t L.loc option;
  init : expr L.loc;
}

and typedec = {
  type_name : S.t L.loc;
  typ : ty;
}




module Pretty = struct
  open SmartPrint

  let pp_sym s : SmartPrint.t = string @@ snd @@ s.L.item

  let pp_sym' s : SmartPrint.t = string @@ snd @@ s

  let pp_op o : SmartPrint.t =
    string @@ match o.L.item with
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

  let pp_typ_dec (d : typedec L.loc) : SmartPrint.t =
    string "type" ^^ pp_sym d.L.item.type_name ^^
      equals ^^ pp_ty d.L.item.typ

  let rec pp e : SmartPrint.t =
    match e.L.item with
    | Int {L.item=n; _} -> string @@ string_of_int @@ n
    | Var v -> pp_var @@ v
    | UnaryOp (o, a) -> pp_op o ^-^ pp a
    | Op (o, a, b) -> pp a ^^ pp_op o ^^ pp b
    | Nil (_) -> string "nil"
    | String {L.item=s; _} -> double_quotes @@ string s
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
       let e = separate (string ";" ^^ newline) @@ List.map ~f:pp b.L.item in
       string "let" ^^ newline ^^
         indent (pp_decs d ^^ newline) ^^
         string "in" ^^ newline ^^
         indent e ^^ newline ^^
         string "end"

    | Array (t, s, i) -> pp_sym t ^^ (brakets @@ pp s) ^^ string "of" ^^ pp i

  and pp_fun_dec (d: fundec L.loc) : SmartPrint.t =
      let f = d.L.item in
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

  and pp_var_dec (d : vardec L.loc) : SmartPrint.t =
    let a = Option.value_map ~f:(fun x -> string ":" ^-^ pp_sym x)
                             ~default:(string "")
                             d.L.item.var_typ in
    string "var" ^^ pp_sym d.L.item.var_name ^-^ a ^^ colon_equals ^^ pp d.L.item.init

  and pp_var v : SmartPrint.t =
    match v.L.item with
    | SimpleVar {L.item=sv; _} -> pp_sym' sv
    | FieldVar (fv, e) -> pp_var fv ^-^ string "." ^-^ pp_sym e
    | SubscriptVar (v, e) -> pp_var v ^^ pp e

  let print_to_string prog : string = to_string 80 4 @@ pp prog
end
