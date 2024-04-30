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

(* - table operations: *)
(* - filtering *)
(* - delete tasks *)
(* - check a task (use built-in task function, to be coded) *)
(* - modify specific task fields *)
(* - encapsulate table *)
(* - comment *)

type data = {
  idx : int;
  task : Task.t;
}

type table = {
  mutable path : string option;
  data : data list ref;
}

exception InvalidTaskIndex

(** Function to read tasks from a CSV file *)
let load_tasks path =
  let lines = Csv.load path in
  let headers, rows =
    match lines with
    | [] -> ([], [])
    | h :: t -> (h, t)
  in
  if
    headers <> [ "Name"; "Description"; "Date"; "Time"; "Category"; "Progress" ]
  then failwith "Invalid CSV header format"
  else
    List.map
      (fun row ->
        Task.create_task (List.nth row 0) (List.nth row 1) (List.nth row 2)
          (List.nth row 3) (List.nth row 4) (List.nth row 5))
      rows

(** Function to write tasks to a CSV file *)
let save_tasks table =
  match table.path with
  | None -> ()
  | Some path ->
      let data =
        List.map
          (fun data ->
            let task = data.task in
            [
              Task.name task;
              Task.description task;
              Task.due_date task;
              Task.time task;
              Task.category task;
              Task.progress task;
            ])
          !(table.data)
      in
      Csv.save path
        ([ "Name"; "Description"; "Date"; "Time"; "Category"; "Progress" ]
        :: data)

let make_table path =
  let tasks = ref [] in
  (try tasks := load_tasks path with _ -> ());
  let data = ref (List.mapi (fun idx task -> { idx; task }) !tasks) in
  { path = Some path; data }

(** Function to add a new task *)
let add_task table new_task =
  table.data := new_task :: !(table.data);
  save_tasks table

(** Function to remove a task by index *)
let remove_task table index =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Create a new list to hold the data without the removed element *)
    let new_data = ref [] in
    (* Add elements before the target index *)
    List.iteri
      (fun i task -> if i < index then new_data := task :: !new_data)
      !(table.data);
    (* Add elements after the target index *)
    List.iteri
      (fun i task -> if i > index then new_data := task :: !new_data)
      !(table.data);
    (* Update the table reference with the new list *)
    table.data := !new_data;
    save_tasks table

(** Function to edit a task's data by index *)
let edit_task table index edited_task =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Create a new list to hold the updated data *)
    let new_data = ref [] in
    (* Iterate through the original data *)
    List.iteri
      (fun i task ->
        if i = index then
          (* Add the edited task at the specified index *)
          new_data := edited_task :: !new_data
        else
          (* Add the original task for other indices *)
          new_data := task :: !new_data)
      !(table.data);
    (* Update the table reference with the new list *)
    table.data := !new_data;
    save_tasks table

(** Function to set a task's progress *)
let set_progress table index progress =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new progress at the target index *)
          Task.set_progress task progress)
      !(table.data);
  save_tasks table

(** Function to filter tasks based on a predicate *)
let filter_tasks table predicate =
  let data = List.filter predicate !(table.data) in
  { path = None; data = ref data }

(** Function to sort tasks based on a comparison function *)
let sort_tasks table compare =
  let data = List.sort compare !(table.data) in
  let data = List.mapi (fun idx d -> { idx; task = d.task }) data in
  table.data := data;
  save_tasks table

let sort_by_index table =
  let compare d1 d2 = d1.idx - d2.idx in
  sort_tasks table compare

let sort_by_name table =
  let compare d1 d2 =
    let task1 = d1.task in
    let name1 = Task.name task1 in
    let task2 = d2.task in
    let name2 = Task.name task2 in
    String.compare (String.uppercase_ascii name1) (String.uppercase_ascii name2)
  in
  sort_tasks table compare

let sort_by_description table =
  let compare d1 d2 =
    let task1 = d1.task in
    let description1 = Task.description task1 in
    let task2 = d2.task in
    let description2 = Task.description task2 in
    String.compare
      (String.uppercase_ascii description1)
      (String.uppercase_ascii description2)
  in
  sort_tasks table compare

let sort_by_date table =
  let compare d1 d2 =
    let task1 = d1.task in
    let duedate1 = Task.due_date task1 in
    let task2 = d2.task in
    let duedate2 = Task.due_date task2 in
    String.compare
      (String.uppercase_ascii duedate1)
      (String.uppercase_ascii duedate2)
  in
  sort_tasks table compare

let sort_by_time table =
  let compare d1 d2 =
    let task1 = d1.task in
    let time1 = Task.time task1 in
    let task2 = d2.task in
    let time2 = Task.time task2 in
    String.compare (String.uppercase_ascii time1) (String.uppercase_ascii time2)
  in
  sort_tasks table compare

let sort_by_category table =
  let compare d1 d2 =
    let task1 = d1.task in
    let category1 = Task.category task1 in
    let task2 = d2.task in
    let category2 = Task.category task2 in
    String.compare
      (String.uppercase_ascii category1)
      (String.uppercase_ascii category2)
  in
  sort_tasks table compare

let sort_by_progress table =
  let compare d1 d2 =
    let task1 = d1.task in
    let progress1 = Task.progress task1 in
    let task2 = d2.task in
    let progress2 = Task.progress task2 in
    String.compare
      (String.uppercase_ascii progress1)
      (String.uppercase_ascii progress2)
  in
  sort_tasks table compare

let print_table table =
  let data =
    List.map
      (fun data ->
        let task = data.task in
        [
          string_of_int data.idx;
          Task.name task;
          Task.description task;
          Task.due_date task;
          Task.time task;
          Task.category task;
          Task.progress task;
        ])
      !(table.data)
  in
  let str =
    rows_as_string
      ([ "Id"; "Name"; "Description"; "Date"; "Time"; "Category"; "Progress" ]
      :: data)
  in
  print_endline str
