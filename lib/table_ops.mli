val from_csv : string -> string list * string list list
(** [from_csv path] is a [string list list] of the contents of [path],
with each comma separated value representing a unique element]*)

val list_as_string : string list -> string
(** [list_as_string lst] is the elements of [lst] concatenated with a tab and
    formatted*)

val row_as_string : string list list -> int -> string
(** [row_as_string rows i] is the row at index [i] as a string*)

val rows_as_string : string list list -> string
(** [rows_as_string rows] is each row of [rows] concactenated with a new line*)

val add_task : string -> string -> string -> string -> string -> unit
(**[add_task name descrip date category path] adds a new task with name: [name],
   description [descrip], etc to [path]*)
