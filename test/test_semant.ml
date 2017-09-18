let trans_op =
  QCheck.Test.make ~count:1000
    ~name:"semant check op with ints"
    QCheck.(small_nat)
    (fun l ->
      let a = Parser.prog Lexer.lexer @@ Lexing.from_string @@ SmartPrint.to_string 80 4 @@
        Syntax.Pretty.pp @@
          Generators.gen_loc @@ l in
      match a = l with
      | true -> true
      | false -> let _ = Core_kernel.(Out_channel.printf "l: %s\n" (Syntax.Pretty.print_to_string l);
                                      Out_channel.printf "a: %s\n" (Syntax.Pretty.print_to_string a);)
                in false
    )


let _ =
  let open OUnit in
  run_test_tt_main
    ("quickcheck semant tests" >:::
       List.map QCheck_runner.to_ounit_test [passing; quick])
