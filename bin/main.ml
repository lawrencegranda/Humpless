let from_csv path =
  let lines = Csv.load path in
  let headers, rows =
    match lines with
    | [] -> ([], [])
    | h :: t -> (h, t)
  in
  (headers, rows)

let _ = from_csv "test.csv"
