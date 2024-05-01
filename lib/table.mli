type table

exception InvalidTaskIndex

val reset_filter : table -> unit
val filter_by_name : table -> string -> unit
val filter_by_description : table -> string -> unit
val filter_due_after : table -> string -> unit
val filter_due_before : table -> string -> unit
val filter_by_category : table -> string -> unit
val filter_by_progress : table -> string -> unit
val exclude_progress : table -> string -> unit
val sort_by_name : table -> unit
val sort_by_description : table -> unit
val sort_by_date : table -> unit
val sort_by_time : table -> unit
val sort_by_category : table -> unit
val sort_by_progress : table -> unit
val make_table : string -> table

val add_task :
  table -> string -> string -> string -> string -> string -> string -> unit

val remove_task : table -> int -> unit
val set_name : table -> int -> string -> unit
val set_description : table -> int -> string -> unit
val set_due_date : table -> int -> string -> unit
val set_time : table -> int -> string -> unit
val set_category : table -> int -> string -> unit
val set_progress : table -> int -> string -> unit
val print_table : table -> unit
