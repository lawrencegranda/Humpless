type t
(** Represents a table of tasks.
    - Each task in the table has a name, description, due date, time, category,
      and progress.
    - Tasks can be filtered, sorted, added, removed, and updated in the table. *)

exception InvalidTaskIndex
(** Exception raised when an invalid task index is provided (out-of-bounds). *)

exception InvalidDateFormat
(** Exception raised when an invalid date format is encountered. Valid if it
    follows ["YYYY-MM-DD"] formatting.*)

exception InvalidTimeFormat
(** Exception raised when an invalid time format is encountered. Valid if it
    follows ["HH:MM:SS"] formatting.*)

exception InvalidProgress
(** Exception raised when an invalid progress value is provided. Valid if it is
    one of [["done"; "in-progress"; "todo"]] *)

exception InvalidPermissions of string
(** Exception raised when invalid permissions are detected when trying to save a
    file. *)

exception InvalidHeaders
(** Exception raised when the csv trying to be loaded does not contain the
    correct headers:
    [["Name"; "Description"; "Date"; "Time"; "Category"; "Progress"]] *)

val reset_filter : t -> unit
(** [reset_filter table] removes any applied filters on [table]. *)

val filter_by_name : t -> string -> unit
(** [filter_by_name table name] filters tasks in [table] by [name]. *)

val filter_by_description : t -> string -> unit
(** [filter_by_description table description] filters tasks in [table] by
    [description]. *)

val filter_due_after : t -> string -> unit
(** [filter_due_after table date] filters tasks in [table] with due dates after
    [date]. *)

val filter_due_before : t -> string -> unit
(** [filter_due_before table date] filters tasks in [table] with due dates
    before [date]. *)

val filter_by_category : t -> string -> unit
(** [filter_by_category table category] filters tasks in [table] by [category]. *)

val filter_by_progress : t -> string -> unit
(** [filter_by_progress table progress] filters tasks in [table] by [progress]. *)

val exclude_progress : t -> string -> unit
(** [exclude_progress table progress] excludes tasks in [table] with [progress]. *)

val sort_by_name : t -> unit
(** [sort_by_name table] sorts tasks in [table] by name. *)

val sort_by_description : t -> unit
(** [sort_by_description table] sorts tasks in [table] by description. *)

val sort_by_date : t -> unit
(** [sort_by_date table] sorts tasks in [table] by date. *)

val sort_by_time : t -> unit
(** [sort_by_time table] sorts tasks in [table] by time. *)

val sort_by_category : t -> unit
(** [sort_by_category table] sorts tasks in [table] by category. *)

val sort_by_progress : t -> unit
(** [sort_by_progress table] sorts tasks in [table] by progress. *)

val make_table : ?autosave:bool -> string -> t
(** [make_table path ?autosave] creates a new table with tasks loaded from
    [path]. [autosave] is by default [true]. If [true], after any operation, the
    table will be automatically saved into the [path] as a CSV file. Any
    operation might raise [InvalidPermissions] is it encounters problems while
    saving. *)

val add_task :
  t -> string -> string -> string -> string -> string -> string -> unit
(** [add_task table name description due_date time category progress] adds a new
    task to [table]. Raises [InvalidTaskIndex] if it is an invalid [index].
    Raises [InvalidDateFormat], [InvalidTimeFormat], or[InvalidProgress] if the
    correspondign fields are wrongly formatted. *)

val remove_task : t -> int -> unit
(** [remove_task table index] removes the task at [index] from [table]. Raises
    [InvalidTaskIndex] if it is an invalid [index]. *)

val set_name : t -> int -> string -> unit
(** [set_name table index name] sets the name of the task at [index] in [table]
    to [name]. Raises [InvalidTaskIndex] if it is an invalid [index]. *)

val set_description : t -> int -> string -> unit
(** [set_description table index description] sets the description of the task
    at [index] in [table] to [description].Raises [InvalidTaskIndex] if it is an
    invalid [index]. *)

val set_due_date : t -> int -> string -> unit
(** [set_due_date table index due_date] sets the due date of the task at [index]
    in [table] to [due_date]. Raises [InvalidTaskIndex] if it is an invalid
    [index]. Raises [InvalidDateFormat] if the correspondign field is wrongly
    formatted. *)

val set_time : t -> int -> string -> unit
(** [set_time table index time] sets the time of the task at [index] in [table]
    to [time]. Raises [InvalidTaskIndex] if it is an invalid [index]. Raises
    [InvalidTimeFormat] if the correspondign field is wrongly formatted. *)

val set_category : t -> int -> string -> unit
(** [set_category table index category] sets the category of the task at [index]
    in [table] to [category]. Raises [InvalidTaskIndex] if it is an invalid
    [index]. *)

val set_progress : t -> int -> string -> unit
(** [set_progress table index progress] sets the progress of the task at [index]
    in [table] to [progress]. Raises [InvalidTaskIndex] if it is an invalid
    [index]. Raises [InvalidProgress] if the correspondign field is wrongly
    formatted. *)

val string_from_table : t -> string
(** [string_from_table table] is a string representation of the [table] in a
    nice format. *)

val get_path : t -> string
(** [get_path table] is the path of the csv. Returns an empty string if the path
    is invalid*)
