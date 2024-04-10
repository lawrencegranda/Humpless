open Final_project

let print_styles output style1 style2 =
  ANSITerminal.print_string [ ANSITerminal.on_default; style1; style2 ] output

let print_question output =
  print_styles (output ^ "\t") ANSITerminal.Reset ANSITerminal.cyan

(* Print [message] using the [print_question] format and return the input. *)
let get_input message =
  let () = print_question message in
  let output = read_line () in
  String.trim output

let print_csv path =
  let open Table_ops in
  let headers, rows = from_csv path in
  print_endline (list_as_string headers);
  print_endline (row_as_string rows 0);
  print_endline (rows_as_string rows)

let path =
  "data/" ^ if Array.length Sys.argv > 1 then Sys.argv.(1) else "test.csv"

let () =
  print_csv path;
  print_endline "We are going to add a new task."

let task_name = get_input "Task Name:"
let description = get_input "Description:"
let date = get_input "Due Date [YYYY-MM-DD]:"
let category = get_input "Task Category:"

let () =
  Table_ops.add_task task_name description date category path;
  print_csv path
