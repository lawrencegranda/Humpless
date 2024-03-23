open OUnit2
open Final_project.Table_ops

let tests =
  [
    ("empty csv" >:: fun _ -> assert_equal ([], []) (from_csv "empty.csv"));
    ("not empty csv" >:: fun _ -> assert_equal (from_csv "test2.csv"));
  ]
