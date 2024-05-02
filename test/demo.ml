open Final_project.Table

let print_my_table table =
  print_table table;
  print_endline ""

(* Tests do not have write access. *)
let table = make_table "test1.csv" ~autosave:false;;

print_my_table table;;
filter_by_name table "3110";;
print_my_table table;;
filter_by_description table "car ";;
print_my_table table;;
filter_due_after table "2024-03-28";;
print_my_table table;;
filter_due_before table "2024-03-28";;
print_my_table table;;
filter_by_category table "college";;
print_my_table table;;
filter_by_progress table "TODO";;
print_my_table table;;
exclude_progress table "DONE";;
print_my_table table;;
add_task table "1110 Essay" "final fws essay" "2024-05-17" "" "College" "TODO";;
reset_filter table;;
print_my_table table;;
sort_by_name table;;
print_my_table table;;
sort_by_description table;;
print_my_table table;;
sort_by_date table;;
print_my_table table;;
sort_by_time table;;
print_my_table table;;
sort_by_category table;;
print_my_table table;;
sort_by_progress table;;
print_my_table table;;
sort_by_name table;;
remove_task table 0;;
print_my_table table
