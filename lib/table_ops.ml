let from_csv path =
  let lines = Csv.load path in
  let headers, rows =
    match lines with
    | [] -> ([], [])
    | h :: t -> (h, t)
  in
  (headers, rows)

(** [max_width] is the maximum number of characters to be printed for a value *)
let max_width = 15

(** [truncate_str str] is [str] with [max_width] characters and appends ["..."]
    if [str] is longer (replaces last three chars with ["..."]). *)
let truncate_str str =
  if String.length str <= max_width then str
  else String.sub str 0 (max_width - 3) ^ "..."

(** [format_cell str] is [str] truncated by the max_width*)
let format_cell str = Printf.sprintf "%-*s  " max_width (truncate_str str)

let list_as_string lst =
  String.trim (List.fold_left (fun acc e -> acc ^ "\t" ^ format_cell e) "" lst)

let row_as_string rows i = list_as_string (List.nth rows i)

let rows_as_string rows =
  String.trim
    (List.fold_left (fun acc row -> acc ^ "\n" ^ list_as_string row) "" rows)

let add_task name description date category path =
  let line =
    name ^ "," ^ description ^ "," ^ date ^ "," ^ "00:00:00" ^ "," ^ category
    ^ "," ^ "Not started"
  in
  let oc = open_out_gen [ Open_append; Open_creat ] 0o666 path in
  Printf.fprintf oc "\n%s\n" line;
  close_out oc
