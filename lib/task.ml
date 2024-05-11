type t = {
  mutable name : string;
  mutable description : string;
  mutable due_date : string;
  mutable time : string;
  mutable category : string;
  mutable progress : string; (* "done" / "in-progress" / "todo" *)
}
(**AF: The record
   [{name = n; description = d; due_date = date; time = t; category = c; progress = p}]
   represents a task with name [n], description [d], due_date [date], etc. RI:
   The fields in [due_date], [time], and [progress] must be formatted
   (YYYY-MM-DD), (HH:MM:SS), and be one of ["done"; "in-progress"; "todo"]*)

exception InvalidDateFormat
exception InvalidTimeFormat
exception InvalidProgress

(**[is_valid_progress p] is [true] when [p] is a valid progress status. Ex:
   [is_valid_progress p] is [true] if [p] is ["in-progress"]*)
let is_valid_progress progress =
  let progress = String.trim (String.lowercase_ascii progress) in
  List.mem progress [ "done"; "in-progress"; "todo" ]

(**[is_valid_date d] is [true] if [d] is a valid date in the (YYYY-MM-DD)
   format. *)
let is_valid_date date_str =
  let date_str = String.trim date_str in
  let len = String.length date_str in
  if len <> 10 then false
  else
    try
      let year = int_of_string (String.sub date_str 0 4) in
      let month = int_of_string (String.sub date_str 5 2) in
      let day = int_of_string (String.sub date_str 8 2) in
      year >= 1000 && 1 <= month && month <= 12 && 1 <= day && day <= 31
    with _ -> false

(**[is_valid_time t] is [true] if [t] is a valid time in the (HH:MM:SS) format. *)
let is_valid_time time_str =
  let time_str = String.trim time_str in
  let len = String.length time_str in
  if len <> 8 || time_str.[2] <> ':' || time_str.[5] <> ':' then false
  else
    try
      let hours = int_of_string (String.sub time_str 0 2) in
      let minutes = int_of_string (String.sub time_str 3 2) in
      let seconds = int_of_string (String.sub time_str 6 2) in
      0 <= hours && hours <= 23 && 0 <= minutes && minutes <= 59 && 0 <= seconds
      && seconds <= 59
    with _ -> false

(* Getter functions *)
let name task = task.name
let description task = task.description
let due_date task = task.due_date
let time task = task.time
let category task = task.category
let progress task = task.progress

(* Setter functions *)
let set_name task new_name = task.name <- String.trim new_name

let set_description task new_description =
  task.description <- String.trim new_description

let set_due_date task new_due_date =
  if is_valid_date new_due_date then task.due_date <- String.trim new_due_date
  else raise InvalidDateFormat

let set_time task new_time =
  if String.trim new_time = "" then task.time <- "23:59:59"
  else if is_valid_time new_time then task.time <- String.trim new_time
  else raise InvalidTimeFormat

let set_category task new_category = task.category <- String.trim new_category

let set_progress task new_progress =
  if is_valid_progress new_progress then
    task.progress <- String.lowercase_ascii (String.trim new_progress)
  else raise InvalidProgress

let create_task name description due_date time category progress =
  let new_task =
    {
      name = "";
      description = "";
      due_date = "";
      time = "";
      category = "";
      progress = "";
    }
  in
  (* Set the fields using setters *)
  set_name new_task name;
  set_description new_task description;
  set_due_date new_task due_date;
  set_time new_task time;
  set_category new_task category;
  set_progress new_task progress;
  new_task (* Return the created task *)
