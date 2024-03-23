let from_csv path =
  let lines = Csv.load path in
  let headers, rows =
    match lines with
    | [] -> ([], [])
    | h :: t -> (h, t)
  in
  (headers, rows)

let headers, rows = from_csv "test.csv"

let list_as_string headers =
  String.trim (List.fold_left (fun acc h -> acc ^ "\t" ^ h) "" headers)

let row_as_string rows i = list_as_string (List.nth rows i)

let rows_as_string rows =
  String.trim
    (List.fold_left (fun acc row -> acc ^ "\n" ^ list_as_string row) "" rows)

let _ = print_endline (list_as_string headers)
let _ = print_endline (row_as_string rows 0)
let _ = print_endline (rows_as_string rows)
