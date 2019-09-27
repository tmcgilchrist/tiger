open Core_kernel

type venv = Env.entry Symbol.Table.t
type tenv = Types.t Symbol.Table.t

type expty = {
  exp : Translate.exp;
  ty : Types.t;
}

type semant_error =
  | Semant (* Placeholder for errors from the type checker *)
  | UndefinedVariable
  | ExpectedVariable of Symbol.t * Location.t
  | NotImplemented (* Temporary error for unimplemented features *)

val trans_exp : venv -> tenv -> Syntax.expr Location.loc -> (expty, semant_error) Result.t

val trans_dec : venv -> tenv -> Syntax.dec -> (venv * tenv, semant_error) Result.t

val trans_ty : tenv -> Syntax.ty -> (Types.t, semant_error) Result.t
