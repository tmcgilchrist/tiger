module T = Types

type access

type entry =
  | VarEntry of T.t
  | FunEntry of T.t list * T.t

let base_venv =
  Symbol.Table.empty

let base_tenv =
  Symbol.Table.empty
  |> Symbol.Table.add_exn ~key:(Symbol.symbol "int") ~data:T.Int
  |> Symbol.Table.add_exn ~key:(Symbol.symbol "string") ~data:T.String
