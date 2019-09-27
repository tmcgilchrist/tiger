open OUnit
open Core_kernel

let parse_n_print a : Syntax.expr Location.loc =
  a |> Lexing.from_string
    |> Parser.prog Lexer.lexer

let test_string x =
  let b = x |> parse_n_print |> Syntax.Pretty.print_to_string in
  assert_equal x b ~msg:("expected: '" ^ x ^ "' \ngot: '" ^ b ^ "'")

let test_nil _ =
  test_string "nil"

let test_arith_mult _ =
 test_string "1 * 3"

let test_arith_add _ =
  test_string "1 + 3"

let test_arith_minus _ =
  test_string "2 - 3"

let test_arith_div _ =
  test_string "1 / 3"

let test_minus_num _ =
  let ex = "-1752185014444534876 + 922652609614397967" in
  test_string ex

let test_eq _ =
  test_string "1 = 3"

let test_neq_1 _ =
  test_string "1 <> 3"

let test_simple_var _ =
  test_string "simple"

let test_var_dec _ =
  test_string @@
"let
    var row := intArray [N] of 0
in
    row ()
end"

let test_var_array_dec _ =
  test_string @@
"let
    type arrtype = array of int
    var arr1:arrtype := arrtype [10] of 0
in
    arr1
end"

let test_list _ =
 test_string "(0; true)"

let test_quoted_string _ =
  test_string "\"string\""

let test_call _ =
  test_string "double (2, 0)"

let test_assign _ =
  test_string "foo {a = 0, b = 1}"

let test_type _ =
  test_string "intArray [N] of 0"

let test_types _ =
  test_string @@
"let
    type intArray = array of int
in
    try (0)
end"

let test_neq_2 _ =
  test_string "v := 0"

let test_if _ =
  test_string @@
"if true
then 0"

let test_if_else _ =
  test_string @@
"if true
then 0
else 1"

let test_while _ =
  test_string @@
"while true do
    false"

let test_for _ =
  test_string @@
"for j := 0 to 10 do
    print (j)"

let test_function _ =
  test_string @@
"let
    function zero (a : int) =
        0
in
    zero ()
end"

let test_records _ =
  test_string @@ "let
    type rectype = {name : string, age : int}
    var rec1:rectype := rectype {name = \"Nobody\", age = 1000}
in
    rec1.name := \"Somebody\";
    rec1
end"

let test_recursive_function _ =
  test_string @@
"let
    function nfactor (n : int) : int =
        if n = 0
        then 1
        else n * nfactor (n - 1)
in
    nfactor (10)
end"

let test_cases = [
    "parse_arith_mult" >:: test_arith_mult;
    "parse_arith_add" >:: test_arith_add;
    "parse_arith_minus" >:: test_arith_minus;
    "parse_arith_div" >:: test_arith_div;
    "parse_test_minus" >:: test_minus_num;
    "parse_nil" >:: test_nil;
    "parse_eq" >:: test_eq;
    "parse_neq" >:: test_neq_1;
    "parse_simple_var" >:: test_simple_var;
    "parse_var_dec" >:: test_var_dec;
    "parse_var_array_dec" >:: test_var_array_dec;
    "parse_list" >:: test_list;
    "parse_string" >:: test_quoted_string;
    "parse_call" >:: test_call;
    "parse_assign" >:: test_assign;
    "parse_type" >:: test_type;
    "parse_types" >:: test_types;
    "parse_neq" >:: test_neq_2;
    "parse_if" >:: test_if;
    "parse_if_else" >:: test_if_else;
    "parse_while" >:: test_while;
    "parse_for" >:: test_for;
    "parse_function" >:: test_function;
    "parse_function_recursive" >:: test_recursive_function;
    "parse_records" >:: test_records;
  ]

let _ =
  let suite = "Parser" >::: test_cases in
  run_test_tt_main suite
