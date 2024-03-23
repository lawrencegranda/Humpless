open Final_project

let () =
  let open Table_ops in
  let headers, rows = from_csv "test.csv" in
  print_endline (list_as_string headers);
  print_endline (row_as_string rows 0);
  print_endline (rows_as_string rows)
