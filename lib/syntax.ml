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

type expr =
  | Var of var L.loc
  | Op of op L.loc * expr L.loc * expr L.loc
  | Int of int L.loc
  | Nil of unit L.loc
and var =
  | SimpleVar of S.t L.loc

module Print = struct

  let pp_oper op = match op.L.item with
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

(*   let pp_decl = function *)
(*     | _ -> "" *)
(* (\* VarDec (a, b, c, d) -> "var " ^ ident ^ " " *\) *)

(*   let pp_decls d = List.fold_left d ~init:"" ~f:(fun b a -> b ^ pp_decl a) *)


  let pp_var v = match v.L.item with
    | SimpleVar {L.item=sv; _} -> "var " ^ sv ^ " "

  let rec pp_expr exp = match exp.L.item with
    | Int {L.item=n; _} -> string_of_int n
    | Var i -> pp_var i
    | Op (o, a, b) -> pp_expr a ^ " " ^ pp_oper o ^ " " ^ pp_expr b
    | Nil (_) -> "nil"

  let pp_prog prog =  pp_expr prog
end
