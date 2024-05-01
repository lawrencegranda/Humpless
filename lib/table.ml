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

type comparator = data -> data -> int

type sorting =
  | Name
  | Description
  | Date
  | Time
  | Category
  | Progress
  | Other of comparator

type table = {
  mutable path : string option;
  data : data list ref;
  mutable sorting : sorting;
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

(** Function to filter tasks based on a predicate *)
let filter_tasks table predicate =
  let data = List.filter predicate !(table.data) in
  { path = None; data = ref data; sorting = Name }

(** Function to sort tasks based on a comparison function *)
let sort_table_with_comparator table (compare : comparator) =
  let data = List.sort compare !(table.data) in
  let data = List.mapi (fun idx d -> { idx; task = d.task }) data in
  table.data := data;
  save_tasks table

let sort_table table (compare : comparator) =
  sort_table_with_comparator table compare;
  table.sorting <- Other compare

let sort_by_index table =
  let compare d1 d2 = d1.idx - d2.idx in
  sort_table_with_comparator table compare

let sort_by_name table =
  let compare d1 d2 =
    let task1 = d1.task in
    let name1 = Task.name task1 in
    let task2 = d2.task in
    let name2 = Task.name task2 in
    String.compare (String.uppercase_ascii name1) (String.uppercase_ascii name2)
  in
  sort_table_with_comparator table compare;
  table.sorting <- Name

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
  sort_table_with_comparator table compare;
  table.sorting <- Description

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
  sort_table_with_comparator table compare;
  table.sorting <- Date

let sort_by_time table =
  let compare d1 d2 =
    let task1 = d1.task in
    let time1 = Task.time task1 in
    let task2 = d2.task in
    let time2 = Task.time task2 in
    String.compare (String.uppercase_ascii time1) (String.uppercase_ascii time2)
  in
  sort_table_with_comparator table compare;
  table.sorting <- Time

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
  sort_table_with_comparator table compare;
  table.sorting <- Category

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
  sort_table_with_comparator table compare;
  table.sorting <- Progress

let sort_default table =
  match table.sorting with
  | Name -> sort_by_name table
  | Description -> sort_by_description table
  | Date -> sort_by_date table
  | Time -> sort_by_time table
  | Category -> sort_by_category table
  | Progress -> sort_by_progress table
  | Other compare -> sort_table table compare

let make_table path =
  let tasks = ref [] in
  (try tasks := load_tasks path with _ -> ());
  let data = ref (List.mapi (fun idx task -> { idx; task }) !tasks) in
  let table = { path = Some path; data; sorting = Name } in
  sort_default table;
  table

(** Function to push a new task to the data list *)
let push_task table new_task =
  sort_by_index table;
  table.data :=
    { idx = List.length !(table.data); task = new_task } :: !(table.data);
  sort_default table

(** Function to add a new task to the table *)
let add_task table name description due_date time category progress =
  let task =
    Task.create_task name description due_date time category progress
  in
  push_task table task

(** Function to remove a task by index *)
let remove_task table index =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Create a new list to hold the data without the removed element *)
    let new_data = ref [] in
    (* Add elements before the target index *)
    List.iter
      (fun task -> if task.idx < index then new_data := task :: !new_data)
      !(table.data);
    (* Add elements after the target index *)
    List.iter
      (fun task -> if task.idx > index then new_data := task :: !new_data)
      !(table.data);
    (* Update the table reference with the new list *)
    table.data := !new_data;
    sort_default table

(** Function to set a task's name *)
let set_name table index name =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new name at the target index *)
          Task.set_name task name)
      !(table.data);
  save_tasks table

(** Function to set a task's description *)
let set_description table index description =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new description at the target index *)
          Task.set_description task description)
      !(table.data);
  save_tasks table

(** Function to set a task's due date *)
let set_due_date table index due_date =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new due date at the target index *)
          Task.set_due_date task due_date)
      !(table.data);
  save_tasks table

(** Function to set a task's time *)
let set_time table index time =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new time at the target index *)
          Task.set_time task time)
      !(table.data);
  save_tasks table

(** Function to set a task's category *)
let set_category table index category =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new category at the target index *)
          Task.set_category task category)
      !(table.data);
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

let print_table table =
  sort_default table;
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
