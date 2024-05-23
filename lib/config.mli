module DataSource : module type of Action_error.DataSource

type t =
  { pname : string;
    context : (string * string) list;
    datasource : DataSource.t
  }

val default_paths : string list
val is_default : t -> bool

val get_config :
  string -> string list -> (t, Global_error_intf.t) result

module FromCrunch : sig
  val get_config :
    string -> string -> (t, Global_error.t) result
end
(* val print_crunch : string -> unit *)

val crunch_path : string
