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

module Print :
  sig
    val pp_oper : op L.loc -> string
    val pp_expr : expr L.loc -> string
    val pp_prog : expr L.loc -> string
  end
