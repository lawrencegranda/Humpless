open Final_project
open Table

(*main operations: - add task - edit task - remove task - filter task *)

let print_help_message tab =
  print_endline
    ("Here are the operations you can conduct on " ^ tab
   ^ ": \n- add a task\n- edit a task\n- remove a task \n- filter the table \n"
    )

let tab = make_table "data/myinput.csv"

let print_styles output style1 style2 =
  ANSITerminal.print_string [ ANSITerminal.on_default; style1; style2 ] output

let print_color output color = print_styles output ANSITerminal.Reset color

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
    | "filter" -> "filter [category]"
    | _ -> failwith "Invalid Command"
  in
  let _ = print_color instructions ANSITerminal.cyan in
  print_color (description ^ "\n") ANSITerminal.red

let execute_command command tab =
  let arg_list = String.split_on_char ' ' command in
  match List.hd arg_list with
  | "exit" -> exit 0
  | "help" -> print_help_message (get_path tab)
  | "add" -> if List.length arg_list <> 8 then command_description "add"
  | "edit" -> failwith "Not impl"
  | "delete" -> failwith "Not impl"
  | "filter" -> failwith "Not impl"
  | _ ->
      print_endline "Sorry, that's an invalid command.";
      print_help_message (get_path tab)

let run_command () =
  try
    let tab_name = ref Sys.argv.(1) in
    let tab = ref (make_table ("data/" ^ !tab_name)) in
    let _ = print_endline (get_path !tab) in
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
            let _ = print_endline (string_from_table !tab) in
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
