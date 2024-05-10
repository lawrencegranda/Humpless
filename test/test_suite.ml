open OUnit2

let suite = "Test Suite" >::: [ Test_table.suite; Test_tasks.suite ]
let () = run_test_tt_main suite
