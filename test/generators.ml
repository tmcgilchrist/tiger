let gen_op =
  let module S = Syntax in

  let other a =
    match (a mod 4) with
    | 0 -> S.Plus
    | 1 -> S.Minus
    | 2 -> S.Times
    | _ -> S.Div
    (* | 4 -> S.Eq *)
    (* | 5 -> S.NEq *)
    (* | 6 -> S.Lt *)
    (* | 7 -> S.LtEq *)
    (* | 8 -> S.Gt *)
    (* | _ -> S.GtEq *)
  in
  QCheck.Gen.(sized @@ fix (fun self n -> map other nat))

let gen_loc x =
  let module L = Location in
  L.mkloc x L.dummy

let arbitrary_op =
  QCheck.make gen_op

let gen_int =
  QCheck.Gen.(
    int >>=
      fun i -> return @@ Syntax.Int (gen_loc i))

let gen_arith =
  QCheck.Gen.(
    gen_int >>= fun a ->
    gen_int >>= fun b ->
    gen_op >>= fun op ->
    return @@ gen_loc @@ Syntax.Op (gen_loc op, gen_loc a, gen_loc b)
  )

let arbitrary_arith : 'a QCheck.arbitrary =
  let exp_to_string ae = Syntax.Pretty.print_to_string ae in
  QCheck.make ~print:exp_to_string (gen_arith)
