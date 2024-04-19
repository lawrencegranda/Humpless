exception InvalidDateFormat
exception InvalidTimeFormat
exception InvalidProgress

(* Function to check if progress is valid *)
let is_valid_progress progress =
  List.mem progress [ "done"; "in-progress"; "todo" ]

(** Function to validate date format (YYYY-MM-DD) *)
let is_valid_date date_str =
  let date_str = String.trim date_str in
  let len = String.length date_str in
  if len <> 10 then raise InvalidDateFormat
  else
    try
      let year = int_of_string (String.sub date_str 0 4) in
      let month = int_of_string (String.sub date_str 5 2) in
      let day = int_of_string (String.sub date_str 8 2) in
      year >= 1000 && 1 <= month && month <= 12 && 1 <= day && day <= 31
    with _ -> raise InvalidDateFormat

(** Function to validate time format (HH:MM:SS) *)
let is_valid_time time_str =
  let time_str = String.trim time_str in
  let len = String.length time_str in
  if len <> 8 then raise InvalidTimeFormat
  else
    try
      let hours = int_of_string (String.sub time_str 0 2) in
      let minutes = int_of_string (String.sub time_str 3 2) in
      let seconds = int_of_string (String.sub time_str 6 2) in
      0 <= hours && hours <= 23 && 0 <= minutes && minutes <= 59 && 0 <= seconds
      && seconds <= 59
    with _ -> raise InvalidTimeFormat

type t = {
  mutable name : string;
  mutable description : string;
  mutable due_date : string;
      (* format needs to be defined based on your needs *)
  mutable time : string; (* format needs to be defined based on your needs *)
  mutable category : string;
  mutable progress : string; (* "done" / "in-progress" / "todo" *)
}

(* Getter functions *)
let get_name task = task.name
let get_description task = task.description
let get_due_date task = task.due_date
let get_time task = task.time
let get_category task = task.category
let get_progress task = task.progress

(* Setter functions *)
let set_name task new_name = task.name <- String.trim new_name

let set_description task new_description =
  task.description <- String.trim new_description

let set_due_date task new_due_date =
  if is_valid_date new_due_date then task.due_date <- String.trim new_due_date
  else raise InvalidDateFormat

let set_time task new_time =
  if is_valid_time new_time then task.time <- String.trim new_time
  else raise InvalidTimeFormat

let set_category task new_category = task.category <- String.trim new_category

let set_progress task new_progress =
  if is_valid_progress new_progress then
    task.progress <- String.trim new_progress
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
