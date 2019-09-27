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

module S = Syntax
module L = Location
module T = Types

let check_int (expr : expty) =
  match expr.ty with
  | T.Int -> Ok T.Int
  | _ -> Error Semant

(* TODO translate v into actual type, it could be a NAME
   and should really be a concrete type.
*)
let actual_ty (ty : Types.t) : Types.t option =
  match ty with
  | T.Int -> Some T.Int
  | T.String -> Some T.String
  | T.Unit -> Some T.Unit
  | T.Nil -> Some T.Nil (* Not sure if this is correct *)
  | T.Record (_) -> None (* Fill in types for any names *)
  | T.Array (_) -> None (* Check each thing in an array is the same type *)
  | T.Name (_,_) -> None (* Follow names through venv (?) to get the actual types *)


let rec trans_exp venv tenv exp =
  match exp.L.item with
  | S.Op (op, left, right) ->
     (match op.L.item with
     | S.Plus | S.Minus | S.Times | S.Div ->
        (* Arithmetic operations require integer arguments *)
        let open Result.Monad_infix in
        trans_exp venv tenv left >>= fun tyleft ->
        trans_exp venv tenv right >>= fun tyright ->
        check_int tyleft >>= fun _ ->
        check_int tyright >>= fun _ ->
        Ok {exp = Translate.(); ty = T.Int}
     | _ -> Error NotImplemented)
  | S.Var v ->
     trvar venv v
  | _ -> Error NotImplemented
  and trvar (venv : venv) var = match var.L.item with
    | S.SimpleVar var  ->
       (match Symbol.Table.find venv var.L.item with
       | Some (Env.VarEntry v) ->
          (match actual_ty v with
          | Some t -> Ok {exp = Translate.(); ty = t}
          | None -> Error NotImplemented)
       | Some (Env.FunEntry _) ->
          Error (ExpectedVariable (var.Location.item, var.Location.loc))
       | None ->
          Error UndefinedVariable)
    | _ -> Error NotImplemented



let trans_dec v t _s = Ok (v, t)

let trans_ty _t _s = Ok Types.Nil

(* TODO

 1. Fill in trans_exp starting with a match on expr and follow the types
    into what should occur for each branch.
 2. Build up semant_error to cover the types of errors occuring and
    giving line/position numbers.

*)
