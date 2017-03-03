open OUnit

let parse_n_print a = a |> Lexing.from_string |> (fun x -> Parser.prog Lexer.token x)

let test_nil c =
  let x = "nil" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_arith_mult c =
  let x = "1 * 3" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_arith_add c =
  let x = "1 + 3" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_arith_minus c =
  let x = "2 - 3" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_arith_div c =
  let x = "1 / 3" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_eq c =
  let x = "1 = 3" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_neq c =
  let x = "1 <> 3" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_simple_var c =
  let x = "simple" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("msg: " ^ b)

let test_list c =
  let x = "(0; true)" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("expected: '" ^ x ^ "' got: '" ^ b ^ "'")

let test_list_fail c =
  let x = "(0 true)" in
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("expected: '" ^ x ^ "' got: '" ^ b ^ "'")

let test_cases = [
    "parse_arith_mult" >:: test_arith_mult;
    "parse_arith_add" >:: test_arith_add;
    "parse_arith_minus" >:: test_arith_minus;
    "parse_arith_div" >:: test_arith_div;
    "parse_nil" >:: test_nil;
    "parse_eq" >:: test_eq;
    "parse_neq" >:: test_neq;
    "parse_simple_var" >:: test_simple_var;
    "parse_list" >:: test_list;
    "parse_list_fail" >:: test_list_fail;
  ]

let _ =
  let suite = "Parser" >::: test_cases in
  run_test_tt_main suite
