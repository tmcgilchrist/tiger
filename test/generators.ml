open Core_kernel
open Quickcheck

let gen_op =
  let module S = Syntax in
  Generator.of_list [
    S.Plus;
    S.Minus;
    S.Times;
    S.Div;
    S.Eq;
    S.NEq;
    S.Lt;
    S.LtEq;
    S.Gt;
    S.GtEq;
  ]

let gen_loc x =
  Location.mkloc x Location.dummy

let gen_int =
 Generator.(
    small_positive_int >>=
      fun i -> return @@ Syntax.Int (gen_loc i))

let gen_arith =
  Generator.(
    gen_int >>= fun a ->
    gen_int >>= fun b ->
    gen_op >>= fun op ->
    return @@ gen_loc @@ Syntax.Op (gen_loc op, gen_loc a, gen_loc b)
  )
