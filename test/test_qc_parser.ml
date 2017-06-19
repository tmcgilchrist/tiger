let gen_op =
  let module S = Syntax in

  let other a =
    match (a mod 10) with
    | 0 -> S.Plus
    | 1 -> S.Minus
    | 2 -> S.Times
    | 3 -> S.Div
    | 4 -> S.Eq
    | 5 -> S.NEq
    | 6 -> S.Lt
    | 7 -> S.LtEq
    | 8 -> S.Gt
    | _ -> S.GtEq
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
    return @@ Syntax.Op (gen_loc op, gen_loc a, gen_loc b)
  )

let arbitrary_arith =
  QCheck.make gen_arith

let quick =
  QCheck.Test.make ~count:1000
    ~name:"parse arith"
    QCheck.(arbitrary_arith) (fun l ->
      let a = Parser.prog Lexer.lexer @@ Lexing.from_string @@ SmartPrint.to_string 80 4 @@
        Syntax.Pretty.pp @@
          gen_loc @@ l in
      a = a
    (* TODO we want to print out the counter example here. *)
    )

let passing =
  QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list small_nat)
    (fun l -> List.rev (List.rev l) = l)

let failing =
  QCheck.Test.make ~count:10
    ~name:"fail_sort_id"
    QCheck.(list small_nat)
    (fun l -> l = List.sort compare l)

let _ =
  let open OUnit in
  run_test_tt_main
    ("quickcheck tests" >:::
       List.map QCheck_runner.to_ounit_test [passing; quick])
