type table = {
  mutable path : string;
  data : Task.t list ref;
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
  let data =
    List.map
      (fun task ->
        [
          Task.get_name task;
          Task.get_description task;
          Task.get_due_date task;
          Task.get_time task;
          Task.get_category task;
          Task.get_progress task;
        ])
      !(table.data)
  in
  Csv.save table.path
    ([ "Name"; "Description"; "Date"; "Time"; "Category"; "Progress" ] :: data)

let make_table path =
  let data = ref [] in
  (try data := load_tasks path with _ -> ());
  { path; data }

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
    (* Create a new list to hold the updated data *)
    let new_data = ref [] in
    (* Iterate through the original data *)
    List.iteri
      (fun i task ->
        if i = index then
          (* Update the task with the new progress at the target index *)
          Task.set_progress task progress
        else
          (* Add the original task for other indices *)
          new_data := task :: !new_data)
      !(table.data);
    (* Update the table reference with the new list *)
    table.data := !new_data;
    save_tasks table

(** Function to filter tasks based on a predicate *)
let filter_tasks table predicate = List.filter predicate !(table.data)

(** Function to sort tasks based on a comparison function *)
let sort_tasks table compare = List.sort compare !(table.data)
