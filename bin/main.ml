open Final_project
open Table

(*main operations: - add task - edit task - remove task - filter task *)

let print_styles output style1 style2 =
  ANSITerminal.print_string [ ANSITerminal.on_default; style1; style2 ] output

let print_color output style color = print_styles output style color

let print_table tab =
  let string_lst = String.split_on_char '\n' (string_from_table tab) in
  print_color
    ("\n" ^ List.hd string_lst ^ "\n")
    ANSITerminal.Bold ANSITerminal.blue;
  print_endline (String.concat "\n" (List.tl string_lst));
  print_newline ()

let print_help_message tab =
  let _ =
    print_color
      ("Here are the operations you can conduct on " ^ tab ^ ": ")
      ANSITerminal.Bold ANSITerminal.cyan
  in
  print_color
    "\n- add a task\n- edit a task\n- remove a task \n- filter the table \n"
    ANSITerminal.Reset ANSITerminal.cyan

let command_description command =
  let instructions =
    "Did you mean " ^ command ^ "?\nThe format for " ^ command ^ " is: "
  in
  let description =
    match command with
    | "add" ->
        "add [name] [description] [due date] [time] [category] [progress]"
    | "edit" -> "edit [ID] [category] [updated_value]"
    | "delete" -> "delete [ID]"
    | "filter" -> "filter [category or 'rm']"
    | _ -> failwith "Invalid Command"
  in
  let _ = print_color instructions ANSITerminal.Bold ANSITerminal.cyan in
  print_color (description ^ "\n") ANSITerminal.Reset ANSITerminal.cyan

(* ALLOW FOR MULTI WORD ENTRIES OML WTF BRUH DO NOT FORGET*)
let add tab args =
  try
    if Array.length args <> 7 then command_description "add"
    else add_task tab args.(1) args.(2) args.(3) args.(4) args.(5) args.(6)
  with _ -> command_description "add"

let edit tab args =
  try
    if Array.length args <> 4 then command_description "edit"
    else
      match args.(2) with
      | "name" -> set_name tab (int_of_string args.(1)) args.(3)
      | "description" -> set_description tab (int_of_string args.(1)) args.(3)
      | "date" -> set_due_date tab (int_of_string args.(1)) args.(3)
      | "time" -> set_time tab (int_of_string args.(1)) args.(3)
      | "category" -> set_category tab (int_of_string args.(1)) args.(3)
      | "progress" -> set_progress tab (int_of_string args.(1)) args.(3)
      | _ -> failwith "Invalid input"
  with _ -> command_description "edit"

let delete tab args =
  try
    if Array.length args <> 2 then command_description "delete"
    else remove_task tab (int_of_string args.(1))
  with _ -> command_description "delete"

let filter tab args =
  try
    if args.(1) = "rm" then reset_filter tab
    else if Array.length args <> 3 then command_description "filter"
    else
      match args.(1) with
      | "name" -> filter_by_name tab args.(2)
      | "description" -> filter_by_description tab args.(2)
      | "date" -> filter_due_after tab args.(2)
      | "category" -> filter_by_category tab args.(2)
      | "progress" -> filter_by_progress tab args.(2)
      | _ -> failwith "Invalid input"
  with _ -> command_description "filter"

let execute_command command tab =
  let arg_list = String.split_on_char ' ' command in
  let args = Array.of_list arg_list in
  match List.hd arg_list with
  | "exit" -> exit 0
  | "help" -> print_help_message (get_path tab)
  | "add" -> add tab args
  | "edit" -> edit tab args
  | "delete" -> delete tab args
  | "filter" -> filter tab args
  | _ ->
      print_color "Sorry, that's an invalid command. \n" ANSITerminal.Reset
        ANSITerminal.cyan;
      print_help_message (get_path tab)

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
