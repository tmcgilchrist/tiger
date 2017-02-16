(* This module defines the initial AST structure *)
open Core_kernel.Std

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
  | For of Symbol.t * (* indice symbol *)
           bool ref * (* escapes *)
           expr L.loc * (* from *)
           expr L.loc * (* to *)
           expr L.loc (* body *)
  | Break of unit L.loc
  | Let of dec list *
           expr L.loc
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

  let pp_sym s : SmartPrint.t = string s.L.item

  let rec pp_var v : SmartPrint.t =
    match v.L.item with
    | SimpleVar {L.item=sv; _} -> string sv
    | FieldVar (fv, e) -> pp_var fv
    | SubscriptVar (_, _) -> string "error : subscript var"

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

  let rec pp e : SmartPrint.t =
    match e.L.item with
    | Int {L.item=n; _} -> string @@ string_of_int @@ n
    | Var v -> pp_var @@ v
    | Op (o, a, b) -> pp a ^^ pp_op o ^^ pp b
    | Nil (_) -> string "nil"
    | String {L.item=s; _} -> string s
    | Call (s, e) -> pp_sym s ^^ (List.fold_left ~f:(fun a b -> a ^^ pp b) ~init:empty e)
    | Seq b ->
       let bs = List.map ~f:pp b in
       parens @@ separate (string "; ") bs
    | Record (_, _) -> string "error : record"
    | Assign (_, _) -> string "error : assign"
    | If (_, _, _) -> string "error : if"
    | While (_, _) -> string "error : while"
    | For (_, _, _, _, _) -> string "error : for"
    | Break _ -> string "error : break"
    | Let (_, _) -> string "error : let"
    | Array (_, _, _) -> string "error : array"

  let print_to_string prog : string = to_string 60 60 @@ pp prog
end
