module T = Types

type access

type entry =
  | VarEntry of T.t
  | FunEntry of T.t list * T.t

let base_venv =
  Symbol.Table.empty

let base_tenv =
  Symbol.Table.empty
  |> Symbol.Table.add (Symbol.symbol "int") T.Int
  |> Symbol.Table.add (Symbol.symbol "string") T.String
