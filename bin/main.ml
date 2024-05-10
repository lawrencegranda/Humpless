open Final_project
open Task
open Table

let print_styles output style1 style2 =
  ANSITerminal.print_string [ ANSITerminal.on_default; style1; style2 ] output

(**[print_color output style color] prints [output] according to the specified
   [style] and [color]*)
let print_color output style color = print_styles output style color

(**[print_table table] prints [table] with its headers in a blue font and its
   data in the default font. Adds a newline before and after the table.*)
let print_table tab =
  let string_lst = String.split_on_char '\n' (string_from_table tab) in
  print_color
    ("\n" ^ List.hd string_lst ^ "\n")
    ANSITerminal.Bold ANSITerminal.blue;
  print_endline (String.concat "\n" (List.tl string_lst));
  print_newline ()

(**[print_help_message table] prints a list of commands the user can run on
   [table]*)
let print_help_message tab =
  print_color
    ("Here are the operations you can conduct on " ^ tab)
    ANSITerminal.Reset ANSITerminal.cyan;
  print_color ": \n- add/edit/delete" ANSITerminal.Bold ANSITerminal.cyan;
  print_color " a task\n" ANSITerminal.Reset ANSITerminal.cyan;
  print_color "- filter " ANSITerminal.Bold ANSITerminal.cyan;
  print_color "the table \n" ANSITerminal.Reset ANSITerminal.cyan

(* Prints [message] using and returns the user input to stdin*)
let get_input message =
  let () = print_color message ANSITerminal.Reset ANSITerminal.cyan in
  let output = read_line () in
  String.trim output

(**[get_valid input table] prompts the user for [input] until a valid input is
   received. Requires [input] is one of ["ID"], ["column"], ["due_date"],
   ["time"], or ["progress"].*)
let rec get_valid input table =
  let retry () =
    let _ =
      print_color "Invalid input. Please try again\n" ANSITerminal.Bold
        ANSITerminal.cyan
    in
    get_valid input table
  in
  match input with
  | "ID" ->
      let id = get_input "ID of task: " in
      if is_valid_id table (int_of_string id) then id else retry ()
  | "column" ->
      let col = get_input "Column: " in
      if is_valid_column col then col else retry ()
  | "due_date" ->
      let dd = get_input "Due Date [YYYY-MM-DD]: " in
      if is_valid_date dd then dd else retry ()
  | "time" ->
      let t = get_input "Time [HH:MM:SS]: " in
      if is_valid_time t then t else retry ()
  | "progress" ->
      let p = get_input "Progress [todo/done/in-progress]: " in
      if is_valid_progress p then p else retry ()
  | _ -> failwith "Invalid input"

(**[add table args] adds the parameters specified in [args] to [table].*)
let add tab =
  let name = get_input "Task Name: " in
  let description = get_input "Description: " in
  let due_date = get_valid "due_date" tab in
  let time = get_valid "time" tab in
  let category = get_input "Task Category: " in
  let progress = get_valid "progress" tab in
  add_task tab name description due_date time category progress

(**[edit table args] edits [table] with the details specified in [args].*)
let edit tab =
  let id = int_of_string (get_valid "ID" tab) in
  let column = get_valid "column" tab in
  match String.uppercase_ascii column with
  | "NAME" -> set_name tab id (get_input "Updated value: ")
  | "DESCRIPTION" -> set_description tab id (get_input "Updated value: ")
  | "DATE" -> set_due_date tab id (get_valid "due_date" tab)
  | "TIME" -> set_time tab id (get_valid "time" tab)
  | "CATEGORY" -> set_category tab id (get_input "Updated value: ")
  | "PROGRESS" -> set_progress tab id (get_valid "progress" tab)
  | _ -> failwith "Invalid input"

(**[delete table args] deletes the task specified in [args] from [table].*)
let delete tab =
  let id = int_of_string (get_valid "ID" tab) in
  remove_task tab id

(**[filter table args] filters [table] according to the details in [args].*)
let filter tab =
  let column = get_input "Column: " in
  match String.uppercase_ascii column with
  | "NAME" -> filter_by_name tab (get_input "Name of task: ")
  | "DESCRIPTION" -> filter_by_description tab (get_input "Description: ")
  | "DATE" -> (
      let order = get_input "Before or after? (B/A)" in
      match order with
      | "A" -> filter_due_after tab (get_valid "due_date" tab)
      | "B" -> filter_due_before tab (get_valid "due_date" tab)
      | _ ->
          print_color "Filtering after." ANSITerminal.Reset ANSITerminal.cyan;
          filter_due_after tab (get_valid "due_date" tab))
  | "CATEGORY" -> filter_by_category tab (get_input "Category: ")
  | "PROGRESS" -> filter_by_progress tab (get_valid "progress" tab)
  | "RM" -> reset_filter tab
  | _ ->
      print_color "Cannot filter by that column" ANSITerminal.Reset
        ANSITerminal.cyan

(**[execute_command command table] executes [command] on [table] and prints out
   [command_description command] if [command] is invalid.*)
let execute_command command tab =
  let arg_list = String.split_on_char ' ' command in
  match List.hd arg_list with
  | "exit" -> exit 0
  | "help" -> print_help_message (get_path tab)
  | "add" -> add tab
  | "edit" -> edit tab
  | "delete" -> delete tab
  | "filter" -> filter tab
  | _ ->
      print_color "Sorry, that's an invalid command. \n" ANSITerminal.Reset
        ANSITerminal.cyan;
      print_help_message (get_path tab)

(**[run_command] continuously takes in user input from [stdin] and applies them
   to the table designated by the user. If an invalid table is provided, the
   user is prompted to enter a valid input. Exits on ctrl + c or 'exit'.*)
let run_command () =
  try
    let tab_name = ref Sys.argv.(1) in
    let tab = ref (make_table ("data/" ^ !tab_name)) in

    (*handles repetitive (recusive) manipulations to tab*)
    let process_table_ops () =
      let rec read_command _ =
        let%lwt command = Lwt_io.read_line_opt Lwt_io.stdin in
        match command with
        | None ->
            print_endline "Sorry, no command detected";
            exit 0
        | Some c ->
            let _ = execute_command c !tab in
            let _ = print_table !tab in
            read_command ()
      in
      let _ = read_command () in
      let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
      never_resolved
    in
    Lwt_main.run (process_table_ops ())
  with _ ->
    print_endline
      "Please provide a table located in the data folder you'd like to work \
       with."

let _ = run_command ()
