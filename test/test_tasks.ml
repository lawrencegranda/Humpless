open OUnit2
open Final_project

let assert_name task name = assert_equal (String.trim name) (Task.name task)

let assert_description task description =
  assert_equal (String.trim description) (Task.description task)

let assert_date task date = assert_equal (String.trim date) (Task.due_date task)
let assert_time task time = assert_equal (String.trim time) (Task.time task)

let assert_category task category =
  assert_equal (String.trim category) (Task.category task)

let assert_progress task progress =
  assert_equal
    (String.lowercase_ascii (String.trim progress))
    (Task.progress task)

let assert_task task name description date time category progress =
  assert_name task name;
  assert_description task description;
  assert_date task date;
  assert_time task time;
  assert_category task category;
  assert_progress task progress

let create_task name description date time category progress =
  let task = Task.create_task name description date time category progress in
  assert_task task name description date time category progress;
  task

let basic_task () =
  let name = "name" in
  let description = "decription" in
  let date = "2024-05-10" in
  let time = "10:00:00" in
  let category = "category" in
  let progress = "todo" in
  create_task name description date time category progress

let test_create_task _ =
  let _ = basic_task () in
  ()

let test_set_name _ =
  let task = basic_task () in
  let name = "New name" in
  Task.set_name task name;
  assert_name task name

let test_set_description _ =
  let task = basic_task () in
  let desc = "New description" in
  Task.set_description task desc;
  assert_description task desc

let test_set_date _ =
  let task = basic_task () in
  let date = "2020-01-01" in
  Task.set_due_date task date;
  assert_date task date

let test_set_time _ =
  let task = basic_task () in
  let time = "00:00:00" in
  Task.set_time task time;
  assert_time task time

let test_set_category _ =
  let task = basic_task () in
  let category = "New category" in
  Task.set_category task category;
  assert_category task category

let test_set_progress _ =
  let task = basic_task () in
  let progress = "done" in
  Task.set_progress task progress;
  assert_progress task progress

let invalid_dates =
  [
    ("string date", "invalid_date");
    ("not enough digits year", "999-01-01");
    ("invalid month", "2000-13-01");
    ("invalid day", "2000-12-32");
    ("one number", "2");
  ]

let test_set_invalid_dates_list =
  let test_set_invalid_date date =
    let task = basic_task () in
    assert_raises Task.InvalidDateFormat (fun () -> Task.set_due_date task date)
  in
  List.map
    (fun (title, date) -> title >:: fun _ -> test_set_invalid_date date)
    invalid_dates

let test_create_invalid_dates_list =
  let test_create_invalid_date date =
    let name = "name" in
    let description = "decription" in
    let date = date in
    let time = "10:00:00" in
    let category = "category" in
    let progress = "todo" in
    assert_raises Task.InvalidDateFormat (fun () ->
        create_task name description date time category progress)
  in
  List.map
    (fun (title, date) -> title >:: fun _ -> test_create_invalid_date date)
    invalid_dates

let invalid_times =
  [
    ("string date", "invalid_time");
    ("HH:MM:SS literally", "HH:MM:SS");
    ("out of bounds hours", "24:00:00");
    ("negative hours", "-1:00:00");
    ("not enough hours digits", "1:20:30");
    ("out of bounds minutes", "00:60:00");
    ("out of bounds seconds", "00:59:60");
    ("one number", "2");
  ]

let test_set_invalid_times_list =
  let test_set_invalid_time time =
    let task = basic_task () in
    assert_raises Task.InvalidTimeFormat (fun () -> Task.set_time task time)
  in
  List.map
    (fun (title, time) -> title >:: fun _ -> test_set_invalid_time time)
    invalid_times

let test_create_invalid_times_list =
  let test_create_invalid_time time =
    let name = "name" in
    let description = "decription" in
    let date = "2024-05-10" in
    let time = time in
    let category = "category" in
    let progress = "todo" in
    assert_raises Task.InvalidTimeFormat (fun () ->
        create_task name description date time category progress)
  in
  List.map
    (fun (title, time) -> title >:: fun _ -> test_create_invalid_time time)
    invalid_times

let test_set_invalid_progress _ =
  let task = basic_task () in
  assert_raises Task.InvalidProgress (fun () ->
      Task.set_progress task "invalid_progress")

let test_create_invalid_progress _ =
  let name = "name" in
  let description = "decription" in
  let date = "2024-05-10" in
  let time = "10:00:00" in
  let category = "category" in
  let progress = "invalid_progress" in
  assert_raises Task.InvalidProgress (fun () ->
      create_task name description date time category progress)

let suite =
  "Task Test Suite"
  >::: [
         "test_create_task" >:: test_create_task;
         "test_set_name" >:: test_set_name;
         "test_set_description" >:: test_set_description;
         "test_set_date" >:: test_set_date;
         "test_set_time" >:: test_set_time;
         "test_set_category" >:: test_set_category;
         "test_set_progress" >:: test_set_progress;
         "test_set_invalid_dates" >::: test_set_invalid_dates_list;
         "test_set_invalid_times" >::: test_set_invalid_times_list;
         "test_set_invalid_progress" >:: test_set_invalid_progress;
         "test_create_invalid_dates" >::: test_create_invalid_dates_list;
         "test_create_invalid_times" >::: test_create_invalid_times_list;
         "test_create_invalid_progress" >:: test_create_invalid_progress;
       ]
