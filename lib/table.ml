type data = {
  idx : int; (* Index of the task. *)
  task : Task.t; (* Details of the task. *)
}
(** Represents a single task [Task.t] along with its index [idx] in the table. *)

type comparator = data -> data -> int
(** A function that compares two [data] values and returns an [int]. *)

type predicate = data -> bool
(** A function that takes a [data] value and returns a [bool]. *)

(** Represents the criteria by which tasks can be sorted. *)
type sorting =
  | Name (* Sort by name. *)
  | Description (* Sort by description. *)
  | Date (* Sort by date. *)
  | Time  (** Sort by time. *)
  | Category (* Sort by category. *)
  | Progress (* Sort by progress. *)

type t = {
  path : string option; (* Path to store the table as a csv *)
  data : data list ref; (* List of the tasks *)
  mutable sorting : sorting; (* Sorting mode *)
  mutable filter : predicate; (* Filter predicate *)
}
(** Abstraction Function: A table [{path; data; sorting; filter}] has the
    following properties:
    - The [Table.t] represents a collection of [data] tasks.
    - Each task in the table is represented by a data record containing an index
      (idx) and a [Task.t] record.
    - The table can be sorted and filtered based on different criteria.
    - The [sorting] and [filter] criteria can be modified.

    Representation Invariant:
    - The index (idx) of each data record is unique within the table and must be
      non-negative and smaller than the total number of tasks.
    - Any time the table is given to the client, the table's data list is sorted
      based on the current sorting criteria.
    - The [filter] predicate should return [true] for data records that should
      be included in the table represneation and [false] for those that should
      be excluded.
    - The path should be a valid file [path] or [None]. *)

exception InvalidTaskIndex
exception InvalidDateFormat
exception InvalidTimeFormat
exception InvalidProgress
exception InvalidPermissions of string
exception InvalidHeaders

(** [max_width] is the maximum number of characters to be printed for a value. *)
let max_width = 22

(** [truncate_str str] is [str] with [max_width] characters and appends ["..."]
    if [str] is longer (replaces last three chars with ["..."]). *)
let truncate_str str =
  if String.length str <= max_width then str
  else String.sub str 0 (max_width - 3) ^ "..."

(** [format_cell str] is [str] truncated by the [max_width]. *)
let format_cell str = Printf.sprintf "%-*s" max_width (truncate_str str)

(** [list_as_string lst] is a [string] representation of a 1-dimensional [list]
    or row. *)
let list_as_string lst =
  String.trim (List.fold_left (fun acc e -> acc ^ "   " ^ format_cell e) "" lst)

(** [rows_as_string rows] is a [string] representation of a 2-dimensional
    [list]. *)
let rows_as_string rows =
  String.trim
    (List.fold_left (fun acc row -> acc ^ "\n" ^ list_as_string row) "" rows)

(** [load_tasks path] is a list of [Task.t] tasks from the csv file located at
    [path]. If the [path] is invalid or the data is wrongly formatted, it will
    raise an exception and abort. *)
let load_tasks path =
  let lines = Csv.load path in
  if lines = [] then []
  else
    let headers, rows =
      match lines with
      | [] -> ([], [])
      | h :: t -> (h, t)
    in
    if
      headers
      <> [ "Name"; "Description"; "Date"; "Time"; "Category"; "Progress" ]
    then raise InvalidHeaders
    else
      List.map
        (fun row ->
          try
            Task.create_task (List.nth row 0) (List.nth row 1) (List.nth row 2)
              (List.nth row 3) (List.nth row 4) (List.nth row 5)
          with
          | Task.InvalidDateFormat -> raise InvalidDateFormat
          | Task.InvalidTimeFormat -> raise InvalidTimeFormat
          | Task.InvalidProgress -> raise InvalidProgress)
        rows

(** Function to write tasks to a CSV file. Raises *)
let save_tasks table =
  match table.path with
  | None -> ()
  | Some path -> (
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
      try
        Csv.save path
          ([ "Name"; "Description"; "Date"; "Time"; "Category"; "Progress" ]
          :: data)
      with Sys_error m -> raise (InvalidPermissions m))

(** Function to filter tasks based on a predicate *)
let filter_tasks_with_predicate table (predicate : predicate) =
  table.filter <- predicate

let reset_filter table =
  let all _ = true in
  filter_tasks_with_predicate table all

let filter_by_name table name =
  let name = String.trim (String.lowercase_ascii name) in
  let predicate data =
    let task = data.task in
    let task_name = String.trim (String.lowercase_ascii (Task.name task)) in
    Base.String.is_substring task_name ~substring:name
  in
  filter_tasks_with_predicate table predicate

let filter_by_description table description =
  let description = String.trim (String.lowercase_ascii description) in
  let predicate data =
    let task = data.task in
    let task_description =
      String.trim (String.lowercase_ascii (Task.description task))
    in
    Base.String.is_substring task_description ~substring:description
  in
  filter_tasks_with_predicate table predicate

let filter_by_date table date =
  let date = String.trim (String.lowercase_ascii date) in
  let predicate data =
    let task = data.task in
    let task_date = String.trim (String.lowercase_ascii (Task.due_date task)) in
    Base.String.is_substring task_date ~substring:date
  in
  filter_tasks_with_predicate table predicate

let filter_due_after table date =
  let date = String.trim (String.lowercase_ascii date) in
  let predicate data =
    let task = data.task in
    let task_date = String.trim (String.lowercase_ascii (Task.due_date task)) in
    String.compare date task_date <= 0
  in
  filter_tasks_with_predicate table predicate

let filter_due_before table date =
  let date = String.trim (String.lowercase_ascii date) in
  let predicate data =
    let task = data.task in
    let task_date = String.trim (String.lowercase_ascii (Task.due_date task)) in
    String.compare date task_date >= 0
  in
  filter_tasks_with_predicate table predicate

let filter_by_category table category =
  let category = String.trim (String.lowercase_ascii category) in
  let predicate data =
    let task = data.task in
    let task_category =
      String.trim (String.lowercase_ascii (Task.category task))
    in
    Base.String.is_substring task_category ~substring:category
  in
  filter_tasks_with_predicate table predicate

let filter_by_progress table progress =
  let progress = String.trim (String.lowercase_ascii progress) in
  let predicate data =
    let task = data.task in
    let task_progress =
      String.trim (String.lowercase_ascii (Task.progress task))
    in
    Base.String.is_substring task_progress ~substring:progress
  in
  filter_tasks_with_predicate table predicate

let exclude_progress table progress =
  let progress = String.trim (String.lowercase_ascii progress) in
  let predicate data =
    let task = data.task in
    let task_progress =
      String.trim (String.lowercase_ascii (Task.progress task))
    in
    not (Base.String.is_substring task_progress ~substring:progress)
  in
  filter_tasks_with_predicate table predicate

(** [filter_default table] is a list of [data] where each [data] included
    respects the default filter of the [table.filter] *)
let filter_default table =
  let predicate = table.filter in
  List.filter predicate !(table.data)

(** Function to sort tasks based on a comparison function *)
let sort_table_with_comparator table (compare : comparator) =
  let data = List.sort compare !(table.data) in
  let data = List.mapi (fun idx d -> { idx; task = d.task }) data in
  table.data := data;
  save_tasks table

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
    let progress1 =
      match Task.progress task1 with
      | "todo" -> 0
      | "in-progress" -> 1
      | "done" -> 2
      | _ -> 3
    in
    let task2 = d2.task in
    let progress2 =
      match Task.progress task2 with
      | "todo" -> 0
      | "in-progress" -> 1
      | "done" -> 2
      | _ -> 3
    in
    progress1 - progress2
  in
  sort_table_with_comparator table compare;
  table.sorting <- Progress

(** [sort_default table] resorts the table according to the default
    [table.filter]. *)
let sort_default table =
  match table.sorting with
  | Name -> sort_by_name table
  | Description -> sort_by_description table
  | Date -> sort_by_date table
  | Time -> sort_by_time table
  | Category -> sort_by_category table
  | Progress -> sort_by_progress table

let make_table ?(autosave = true) path =
  let tasks = ref [] in
  (try tasks := load_tasks path with
  | InvalidDateFormat -> raise InvalidDateFormat
  | InvalidTimeFormat -> raise InvalidTimeFormat
  | InvalidProgress -> raise InvalidProgress
  | InvalidHeaders -> raise InvalidHeaders
  | _ -> ());
  let data = ref (List.mapi (fun idx task -> { idx; task }) !tasks) in
  let table =
    {
      path = (if autosave then Some path else None);
      data;
      sorting = Name;
      filter = (fun _ -> true);
    }
  in
  sort_default table;
  table

(** Function to push a new task to the data list *)
let push_task table new_task =
  sort_default table;
  table.data :=
    { idx = List.length !(table.data); task = new_task } :: !(table.data);
  sort_default table

let add_task table name description due_date time category progress =
  let task =
    try Task.create_task name description due_date time category progress with
    | Task.InvalidDateFormat -> raise InvalidDateFormat
    | Task.InvalidTimeFormat -> raise InvalidTimeFormat
    | Task.InvalidProgress -> raise InvalidProgress
  in

  push_task table task

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

let set_due_date table index due_date =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new due date at the target index *)
          try Task.set_due_date task due_date
          with Task.InvalidDateFormat -> raise InvalidDateFormat)
      !(table.data);
  save_tasks table

let set_time table index time =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new time at the target index *)
          try Task.set_time task time
          with Task.InvalidTimeFormat -> raise InvalidTimeFormat)
      !(table.data);
  save_tasks table

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

let set_progress table index progress =
  if index < 0 || index >= List.length !(table.data) then raise InvalidTaskIndex
  else
    (* Iterate through the original data *)
    List.iter
      (fun data ->
        let task = data.task in
        if data.idx = index then
          (* Update the task with the new progress at the target index *)
          try Task.set_progress task progress
          with Task.InvalidProgress -> raise InvalidProgress)
      !(table.data);
  save_tasks table

let matrix_from_table table =
  sort_default table;
  let data = filter_default table in
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
    data

let string_from_table table =
  let data = matrix_from_table table in
  rows_as_string
    ([ "Id"; "Name"; "Description"; "Date"; "Time"; "Category"; "Progress" ]
    :: data)

let get_path t =
  match t.path with
  | None -> ""
  | Some p -> p

let is_valid_id table index =
  if index < 0 || index >= List.length !(table.data) then false else true

let is_valid_column col =
  let valid_cols =
    [ "ID"; "NAME"; "DESCRIPTION"; "DATE"; "TIME"; "CATEGORY"; "PROGRESS" ]
  in
  List.mem (String.uppercase_ascii col) valid_cols
