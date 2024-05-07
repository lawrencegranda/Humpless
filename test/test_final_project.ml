open OUnit2
open Final_project

let assign_indices matrix =
  List.mapi
    (fun i row ->
      [
        string_of_int i;
        List.nth row 1;
        List.nth row 2;
        List.nth row 3;
        List.nth row 4;
        List.nth row 5;
        List.nth row 6;
      ])
    matrix

let sort_by_name matrix =
  assign_indices
    (List.sort (fun a b -> String.compare (List.nth a 1) (List.nth b 1)) matrix)
(* Sort by name (2nd element) *)

let sort_by_description matrix =
  assign_indices
    (List.sort (fun a b -> String.compare (List.nth a 2) (List.nth b 2)) matrix)
(* Sort by description (3rd element) *)

let sort_by_date matrix =
  assign_indices
    (List.sort (fun a b -> String.compare (List.nth a 3) (List.nth b 3)) matrix)
(* Sort by due date (4th element) *)

let sort_by_time matrix =
  assign_indices
    (List.sort (fun a b -> String.compare (List.nth a 4) (List.nth b 4)) matrix)
(* Sort by time (5th element) *)

let sort_by_category matrix =
  assign_indices
    (List.sort (fun a b -> String.compare (List.nth a 5) (List.nth b 5)) matrix)
(* Sort by category (6th element) *)

let sort_by_progress matrix =
  assign_indices
    (List.sort (fun a b -> String.compare (List.nth a 6) (List.nth b 6)) matrix)
(* Sort by progress (7th element) *)

let print_matrix matrix =
  List.fold_left
    (fun acc list ->
      acc ^ "\n" ^ List.fold_left (fun str a -> str ^ " " ^ a) "" list)
    "" matrix

let initial_table () =
  let table = Table.make_table ~autosave:false "" in
  Table.add_task table "Task 1" "Description 1" "2024-05-10" "10:00:00"
    "Category A" "todo";
  Table.add_task table "Task 2" "Description 2" "2024-05-08" "11:30:00"
    "Category B" "in-progress";
  Table.add_task table "Task 3" "Description 3" "2024-05-07" "14:00:00"
    "Category C" "done";
  table

let initial_matrix =
  [
    [
      "0";
      "Task 1";
      "Description 1";
      "2024-05-10";
      "10:00:00";
      "Category A";
      "todo";
    ];
    [
      "1";
      "Task 2";
      "Description 2";
      "2024-05-08";
      "11:30:00";
      "Category B";
      "in-progress";
    ];
    [
      "2";
      "Task 3";
      "Description 3";
      "2024-05-07";
      "14:00:00";
      "Category C";
      "done";
    ];
  ]

let assert_matrix_equal real expected =
  assert_equal (Table.matrix_from_table real) expected ~printer:print_matrix

let test_creation _ =
  let table = initial_table () in
  let expected_matrix = sort_by_name initial_matrix in

  assert_matrix_equal table expected_matrix

let test_add_task _ =
  let table = initial_table () in
  Table.add_task table "Task 4" "Description 4" "2024-05-09" "12:00:00"
    "Category D" "todo";
  let expected_matrix =
    sort_by_name
      (initial_matrix
      @ [
          [
            "3";
            "Task 4";
            "Description 4";
            "2024-05-09";
            "12:00:00";
            "Category D";
            "todo";
          ];
        ])
  in
  assert_matrix_equal table expected_matrix;
  Table.remove_task table 3

let test_remove_task _ =
  let table = initial_table () in
  let expected_matrix = sort_by_name (List.tl initial_matrix) in
  Table.remove_task table 0;
  assert_matrix_equal table expected_matrix

let test_set_name _ =
  let table = initial_table () in
  Table.set_name table 1 "Updated Task 2";
  let expected_matrix =
    sort_by_name
      (List.map
         (fun row ->
           if List.hd row = "1" then
             [
               "1";
               "Updated Task 2";
               List.nth row 2;
               List.nth row 3;
               List.nth row 4;
               List.nth row 5;
               List.nth row 6;
             ]
           else row)
         initial_matrix)
  in
  assert_matrix_equal table expected_matrix

let test_set_description _ =
  let table = initial_table () in
  Table.set_description table 0 "Updated Description 1";
  let expected_matrix =
    sort_by_name
      (List.map
         (fun row ->
           if List.hd row = "0" then
             [
               "0";
               List.nth row 1;
               "Updated Description 1";
               List.nth row 3;
               List.nth row 4;
               List.nth row 5;
               List.nth row 6;
             ]
           else row)
         initial_matrix)
  in
  assert_matrix_equal table expected_matrix

let test_set_due_date _ =
  let table = initial_table () in
  Table.set_due_date table 2 "2024-05-11";
  let expected_matrix =
    sort_by_name
      (List.map
         (fun row ->
           if List.hd row = "2" then
             [
               "2";
               List.nth row 1;
               List.nth row 2;
               "2024-05-11";
               List.nth row 4;
               List.nth row 5;
               List.nth row 6;
             ]
           else row)
         initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  (* Try setting an invalid date *)
  try
    Table.set_due_date table 0 "invalid_date";
    failwith "InvalidDateFormat exception not raised"
  with Table.InvalidDateFormat -> ()

let test_set_time _ =
  let table = initial_table () in
  Table.set_time table 1 "15:00:00";
  let expected_matrix =
    sort_by_name
      (List.map
         (fun row ->
           if List.hd row = "1" then
             [
               "1";
               List.nth row 1;
               List.nth row 2;
               List.nth row 3;
               "15:00:00";
               List.nth row 5;
               List.nth row 6;
             ]
           else row)
         initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  (* Try setting an invalid time *)
  try
    Table.set_time table 0 "invalid_time";
    failwith "Table.InvalidTimeFormat exception not raised"
  with Table.InvalidTimeFormat -> ()

let basic_suite =
  "Basic Table Tests"
  >::: [
         "test_creation" >:: test_creation;
         "test_Table.add_task" >:: test_add_task;
         "test_Table.remove_task" >:: test_remove_task;
         "test_set_name" >:: test_set_name;
         "test_set_description" >:: test_set_description;
         "test_set_due_date" >:: test_set_due_date;
         "test_set_time" >:: test_set_time;
       ]

let test_reset_filter _ =
  let table = initial_table () in
  Table.filter_by_name table "Task 1";
  Table.reset_filter table;
  let expected_matrix = sort_by_name initial_matrix in
  assert_matrix_equal table expected_matrix

let test_filter_by_name _ =
  let table = initial_table () in
  let name = "Task 1" in
  Table.filter_by_name table name;
  let expected_matrix =
    List.filter
      (fun task -> List.nth task 1 = name)
      (sort_by_name initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  Table.reset_filter table

let test_filter_by_description _ =
  let table = initial_table () in
  let description = "Description 1" in
  Table.filter_by_description table description;
  let expected_matrix =
    List.filter
      (fun task -> List.nth task 2 = description)
      (sort_by_name initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  Table.reset_filter table

let test_filter_due_after _ =
  let table = initial_table () in
  let date = "2024-05-08" in
  Table.filter_due_after table date;
  let expected_matrix =
    List.filter
      (fun task -> List.nth task 3 >= date)
      (sort_by_name initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  Table.reset_filter table

let test_filter_due_before _ =
  let table = initial_table () in
  let date = "2024-05-09" in
  Table.filter_due_before table date;
  let expected_matrix =
    List.filter
      (fun task -> List.nth task 3 <= date)
      (sort_by_name initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  Table.reset_filter table

let test_filter_by_category _ =
  let table = initial_table () in
  let category = "Category A" in
  Table.filter_by_category table category;
  let expected_matrix =
    List.filter
      (fun task -> List.nth task 5 = category)
      (sort_by_name initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  Table.reset_filter table

let test_filter_by_progress _ =
  let table = initial_table () in
  let progress = "done" in
  Table.filter_by_progress table progress;
  let expected_matrix =
    List.filter
      (fun task -> List.nth task 6 = progress)
      (sort_by_name initial_matrix)
  in
  assert_matrix_equal table expected_matrix;
  Table.reset_filter table

let filter_suite =
  "Filter Tests"
  >::: [
         "test_reset_filter" >:: test_reset_filter;
         "test_filter_by_name" >:: test_filter_by_name;
         "test_filter_by_description" >:: test_filter_by_description;
         "test_filter_due_after" >:: test_filter_due_after;
         "test_filter_due_before" >:: test_filter_due_before;
         "test_filter_by_category" >:: test_filter_by_category;
         "test_filter_by_progress" >:: test_filter_by_progress;
       ]

let sorting_suite = "Sorting Tests" >::: []
let suite = "Table Test Suite" >::: [ basic_suite; filter_suite; sorting_suite ]
let () = run_test_tt_main suite
