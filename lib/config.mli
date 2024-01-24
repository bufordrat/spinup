type t = { pname : string ;
           context : (string * string) list ; }

val get_config :
  string -> string -> (t, string) result
