(* This module defines the initial AST structure *)
open Core.Std

type op =
  | PLUS
  | MINUS
  | TIMES
  | DIV

type expr =
  | Int of int
  | Op of op * expr * expr

module Print = struct

  let pp_oper = function
    | PLUS -> "+"
    | MINUS -> "-"
    | TIMES -> "*"
    | DIV -> "/"

  let rec pp_expr = function
    | Int i -> string_of_int i
    | Op (o, a, b) -> pp_expr a ^ " " ^ pp_oper o ^ " " ^ pp_expr b
end
