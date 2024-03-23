let from_csv path =
  let lines = Csv.load path in
  let headers, rows =
    match lines with
    | [] -> ([], [])
    | h :: t -> (h, t)
  in
  (headers, rows)

let headers, rows = from_csv "test.csv"

(** [max_width] is the maximum number of characters to be printed for a value *)
let max_width = 15

(** [truncate_str str] is [str] with [max_width] characters and appends ["..."]
    if [str] is longer (replaces last three chars with ["..."]). *)
let truncate_str str =
  if String.length str <= max_width then str
  else String.sub str 0 (max_width - 3) ^ "..."

let format_cell str = Printf.sprintf "%-*s  " max_width (truncate_str str)

let list_as_string headers =
  String.trim
    (List.fold_left (fun acc h -> acc ^ "\t" ^ format_cell h) "" headers)

let row_as_string rows i = list_as_string (List.nth rows i)

let rows_as_string rows =
  String.trim
    (List.fold_left (fun acc row -> acc ^ "\n" ^ list_as_string row) "" rows)

let _ = print_endline (list_as_string headers)
let _ = print_endline (row_as_string rows 0)
let _ = print_endline (rows_as_string rows)
