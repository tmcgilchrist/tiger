open OUnit

let _ =
  let suite = "Tests"
              >::: Test_qc_parser.test_cases
              @ Test_parser.test_cases
  in run_test_tt_main suite
