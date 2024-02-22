module Which : sig
  type t = Default | FromAFile of string
end

type t =
  { pname : string;
    context : (string * string) list;
    which : Which.t
  }

val default_paths : string list
val is_default : t -> bool
val get_config : string -> string list -> (t, string) result

val get_config :
  string -> string list -> (t, Config_error.t) result

val print_crunch : string -> unit
