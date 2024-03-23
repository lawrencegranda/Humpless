open OUnit2
open Final_project

let _ =
  let open Table_ops in
  [
    ("empty csv" >:: fun _ -> assert_equal ([], []) (from_csv "empty.csv"));
    ( "not empty csv" >:: fun _ ->
      assert_equal true (List.length (snd (from_csv "test2.csv")) > 0) );
  ]
