open OUnit2

let tests =
  [
    ("empty csv" >:: fun _ -> assert_equal [] (from_csv "empty.csv"));
    ( "not empty csv" >:: fun _ ->
      assert_equal (List.length > 0) (from_csv "test2.csv") );
  ]
