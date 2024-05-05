open Final_project
open Table

let path =
  "data/" ^ if Array.length Sys.argv > 1 then Sys.argv.(1) else "test.csv"
(* let table = make_table path let () = print_table table *)

(*main operations: - add task - edit task - remove task - filter task *)

let run_command () =
  try
    let tab = Sys.argv.(1) in
    (*handles repetitive (recusive) manipulations to tab*)
    let process_table_ops () =
      let rec read_command _ =
        let%lwt message = Lwt_io.read_line_opt Lwt_io.stdin in
        match message with
        | None ->
            print_endline "Sorry, no command detected";
            exit 0
        | Some m ->
            if m = "exit" then exit 0;
            let%lwt () = Lwt_io.printl ("Command " ^ m ^ " received") in
            read_command ()
      in
      let _ = read_command () in
      let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
      never_resolved
    in

    Lwt_main.run (process_table_ops ())
  with _ -> print_endline "Please provide a table you'd like to work with."

let _ = run_command ()
