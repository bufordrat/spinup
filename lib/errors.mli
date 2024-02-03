type t = [
  | `DirAlreadyExists of string
  ]

val already_exists : string -> (unit, string) result

val already_exists' : string -> (unit, t) result

val wrap_in_argv0 : string -> string
