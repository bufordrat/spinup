type t = { pname : string ;
           context : (string * string) list ; }

val default_paths : string list

val get_config :
  string -> string list -> (t, string) result
