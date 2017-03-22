type venv = Env.entry Symbol.Table.t
type tenv = Types.t Symbol.Table.t

type expty = {
  exp : Translate.exp;
  ty : Types.t;
}

let trans_exp v t e = {exp = Translate.(); ty = Types.Nil}

let trans_dec v t s = (v, t)

let trans_ty t s = Types.Nil
