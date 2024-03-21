type dir =
  { dir : string; actions : t list; config : Config.t }

and t =
  | Write of Template.Processed.t
  | Print of string
  | Run of Command.t
  | WithCD of dir

val run : t -> unit
val dry_run : t -> unit
val write : Template.Processed.t -> t
val directory_actions : Config.t -> (t list, string) result
val main_action : string -> (t, string) result

module Files : sig
  val files : Config.t -> Template.Unprocessed.t list
end
