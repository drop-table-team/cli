open Types

val create_search_json : string -> string list -> string

val get : ?headers:(string * string) list -> string -> string -> string Lwt.t

val post : ?headers:(string * string) list -> string -> string -> string Lwt.t

val get_modules : string -> modules list

val upload_file : string -> string -> modules list -> string Lwt.t
