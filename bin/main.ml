open Final_project
open Table

(*main operations: - add task - edit task - remove task - filter task *)

let print_help_message tab =
  print_endline
    ("Here are the operations you can conduct on " ^ tab
   ^ ": \n- add a task\n- edit a task\n- remove a task \n- filter the table \n"
    )

(* let execute_command command tab = let arg_list = String.split_on_char ' '
   command in let _ = print_endline (List.hd arg_list) in match List.hd arg_list
   with | "add" -> failwith "Not impl" | "edit" -> failwith "Not impl" |
   "delete" -> failwith "Not impl" | "filter" -> failwith "Not impl" | _ ->
   print_endline "Sorry, that's an invalid command."; print_help_message tab *)

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
            if c = "exit" then exit 0;
            if c = "help" then print_help_message !tab_name;
            (* let _ = execute_command c in *)
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
