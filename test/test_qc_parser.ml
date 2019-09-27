open OUnit
open Core_kernel
open Quickcheck

let quick _ =
  test Generators.gen_arith
      ~f:(fun l ->
        let
          result = (Parser.prog Lexer.lexer @@ Lexing.from_string @@ Syntax.Pretty.print_to_string @@ l)
        in
        assert_equal ~msg:"lex and parse roundtrip failed"
          ~printer:(fun a -> Sexp.to_string_hum @@ Syntax.sexp_of_expr a.Location.item)
          (Syntax.strip_loc result)
          l)

let test_cases = [
    "lex and parse" >:: quick
  ]
