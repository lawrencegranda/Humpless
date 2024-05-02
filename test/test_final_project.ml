open OUnit2
open Final_project.Table_ops

let headers, rows = from_csv "test2.csv"

let remove_spaces =
  String.fold_left
    (fun acc c -> if c = ' ' || c = '\t' then acc else acc ^ String.make 1 c)
    ""

let tests =
  [
    ("empty csv" >:: fun _ -> assert_equal ([], []) (from_csv "empty.csv"));
    ( "not empty csv" >:: fun _ ->
      assert_equal true (List.length (snd (from_csv "test2.csv")) > 0) );
    ("empty list as string" >:: fun _ -> assert_equal "" (list_as_string []));
    ( "single elt list as string" >:: fun _ ->
      assert_equal "abc" (list_as_string [ "abc" ]) );
    ( "multiple elts list as string" >:: fun _ ->
      assert_equal "abc"
        (remove_spaces (list_as_string [ "a"; "b"; "c" ]))
        ~printer:(fun x -> x) );
    ("empty row as string" >:: fun _ -> assert_equal "" (row_as_string [ [] ] 0));
    ( "test2.csv row as string" >:: fun _ ->
      assert_equal "Mathhomework2024-03-2516:00:00Collegetodo"
        (remove_spaces (row_as_string rows 0))
        ~printer:(fun x -> x) );
    ( "test2.csv header as string" >:: fun _ ->
      assert_equal "NameDescriptionDateTimeCategoryProgress"
        (remove_spaces (list_as_string headers))
        ~printer:(fun x -> x) );
    ( "empty rows as string" >:: fun _ ->
      assert_equal ""
        (remove_spaces (rows_as_string [ [] ]))
        ~printer:(fun x -> x) );
    ( "test2.csv rows as string" >:: fun _ ->
      assert_equal
        "Mathhomework2024-03-2516:00:00Collegetodo\n\
         Getgasforthecar2024-03-2320:00:00Personaltodo\n\
         3110prelimlectures9-182024-03-2211:30:00Testsdone\n\
         3110MS2typereport2024-03-2823:59:59Collegein-progress"
        (remove_spaces (rows_as_string rows))
        ~printer:(fun x -> x) );
  ]

let test_suite = "Table_ops test suite" >::: tests
let _ = run_test_tt_main test_suite
