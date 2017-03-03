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
       List.map QCheck_runner.to_ounit_test [passing])
