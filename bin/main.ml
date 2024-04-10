open Final_project

(**[csv] is the csv the user passes in. If they omit a file, the default
   test.csv will be used*)
let csv = if Array.length Sys.argv > 1 then Sys.argv.(1) else "test.csv"

let () =
  let open Table_ops in
  let headers, rows = from_csv ("data/" ^ csv) in
  print_endline (list_as_string headers);
  print_endline (row_as_string rows 0);
  print_endline (rows_as_string rows)
