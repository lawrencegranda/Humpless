type table

exception InvalidTaskIndex
exception InvalidDateFormat
exception InvalidTimeFormat
exception InvalidProgress

val reset_filter : table -> unit
(** [reset_filter table] removes any applied filters on [table]. *)

val filter_by_name : table -> string -> unit
(** [filter_by_name table name] filters tasks in [table] by [name]. *)

val filter_by_description : table -> string -> unit
(** [filter_by_description table description] filters tasks in [table] by
    [description]. *)

val filter_due_after : table -> string -> unit
(** [filter_due_after table date] filters tasks in [table] with due dates after
    [date]. *)

val filter_due_before : table -> string -> unit
(** [filter_due_before table date] filters tasks in [table] with due dates
    before [date]. *)

val filter_by_category : table -> string -> unit
(** [filter_by_category table category] filters tasks in [table] by [category]. *)

val filter_by_progress : table -> string -> unit
(** [filter_by_progress table progress] filters tasks in [table] by [progress]. *)

val exclude_progress : table -> string -> unit
(** [exclude_progress table progress] excludes tasks in [table] with [progress]. *)

val sort_by_name : table -> unit
(** [sort_by_name table] sorts tasks in [table] by name. *)

val sort_by_description : table -> unit
(** [sort_by_description table] sorts tasks in [table] by description. *)

val sort_by_date : table -> unit
(** [sort_by_date table] sorts tasks in [table] by date. *)

val sort_by_time : table -> unit
(** [sort_by_time table] sorts tasks in [table] by time. *)

val sort_by_category : table -> unit
(** [sort_by_category table] sorts tasks in [table] by category. *)

val sort_by_progress : table -> unit
(** [sort_by_progress table] sorts tasks in [table] by progress. *)

val make_table : string -> table
(** [make_table path] creates a new table with tasks loaded from [path]. *)

val add_task :
  table -> string -> string -> string -> string -> string -> string -> unit
(** [add_task table name description due_date time category progress] adds a new
    task to [table]. Raises [InvalidTaskIndex] if it is an invalid index. *)

val remove_task : table -> int -> unit
(** [remove_task table index] removes the task at [index] from [table]. Raises
    [InvalidTaskIndex] if it is an invalid index. *)

val set_name : table -> int -> string -> unit
(** [set_name table index name] sets the name of the task at [index] in [table]
    to [name]. Raises [InvalidTaskIndex] if it is an invalid index. *)

val set_description : table -> int -> string -> unit
(** [set_description table index description] sets the description of the task
    at [index] in [table] to [description].Raises [InvalidTaskIndex] if it is an
    invalid index. *)

val set_due_date : table -> int -> string -> unit
(** [set_due_date table index due_date] sets the due date of the task at [index]
    in [table] to [due_date]. Raises [InvalidTaskIndex] if it is an invalid
    index. *)

val set_time : table -> int -> string -> unit
(** [set_time table index time] sets the time of the task at [index] in [table]
    to [time]. Raises [InvalidTaskIndex] if it is an invalid index. *)

val set_category : table -> int -> string -> unit
(** [set_category table index category] sets the category of the task at [index]
    in [table] to [category]. Raises [InvalidTaskIndex] if it is an invalid
    index. *)

val set_progress : table -> int -> string -> unit
(** [set_progress table index progress] sets the progress of the task at [index]
    in [table] to [progress]. Raises [InvalidTaskIndex] if it is an invalid
    index. *)

val print_table : table -> unit
(** [print_table table] prints the tasks in [table]. *)
