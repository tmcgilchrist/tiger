module T = Types
module S = Symbol

type access

type entry =
  | VarEntry of T.t
  | FunEntry of T.t list * T.t

let base_venv =
  S.Table.empty

let base_tenv =
  S.Table.empty
  |> S.Table.add ~key:(S.symbol "int") ~data:T.Int
  |> S.Table.add ~key:(S.symbol "string") ~data:T.String
