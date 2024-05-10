open Final_project.Table
open OUnit2

let flow_test _ =
  (* [print_table table] prints the tasks in [table] to the terminal. *)
  let print_table table = print_endline (string_from_table table) in

  (* [print_my_table table] prints the tasks in [table] and adds an empty
     line. *)
  let print_my_table table =
    print_table table;
    print_endline ""
  in

  (* Create a new table for testing. *)
  let table = make_table "test1.csv" ~autosave:false in

  (* Print the initial state of the table. *)
  print_my_table table;

  (* Remove tasks from the table. *)
  remove_task table 0;
  remove_task table 0;
  remove_task table 0;
  remove_task table 0;
  remove_task table 0;
  print_my_table table;

  (* Add new tasks to the table. *)
  add_task table "3110 Meeting" "Meet with Sophia" "2024-04-10" "00:00:00"
    "College" "TODO";

  add_task table "3110 MS2" "type report" "2024-03-28" "" "College"
    "in-progress";

  add_task table "3110 prelim" "lectures 9-18" "2024-03-22" "11:30:00" "Tests"
    "done";
  add_task table "Get gas" "for the car" "2024-03-23" "20:00:00" "Personal"
    "todo";
  add_task table "Math homework" "" "2024-03-25" "16:00:00" "College" "TODO";
  print_my_table table;

  (* Filter tasks by name, description, due date, category, and progress. *)
  filter_by_name table "3110";
  print_my_table table;
  filter_by_description table "car ";
  print_my_table table;
  filter_due_after table "2024-03-28";
  print_my_table table;
  filter_due_before table "2024-03-28";
  print_my_table table;
  filter_by_category table "college";
  print_my_table table;
  filter_by_progress table "TODO";
  print_my_table table;
  exclude_progress table "DONE";
  print_my_table table;

  (* Add a new task and reset filters. *)
  add_task table "1110 Essay" "final fws essay" "2024-05-17" "" "College" "TODO";
  reset_filter table;
  print_my_table table;

  (* Sort tasks by name, description, date, time, category, and progress. *)
  sort_by_name table;
  print_my_table table;
  sort_by_description table;
  print_my_table table;
  sort_by_date table;
  print_my_table table;
  sort_by_time table;
  print_my_table table;
  sort_by_category table;
  print_my_table table;
  sort_by_progress table;
  print_my_table table;
  sort_by_name table;

  (* Remove a task and print the final state of the table. *)
  remove_task table 0;
  print_my_table table;

  (* Assert that the test passes. *)
  assert_equal true true

(** Define the test suite. *)
let suite = "Integration Test" >::: [ "Flow Test" >:: flow_test ]
