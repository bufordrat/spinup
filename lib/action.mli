type dir = { dir : string ;
             actions : t list ; }

and t =
  | Write of Template.Processed.t
  | Run of Command.t
  | WithCD of dir

val run : t -> unit

val dry_run : t -> unit

val write : Template.Processed.t -> t

val directory_actions : string -> (t list, string) result

val main_action : string -> (t, string) result

module Files : sig
  val files : string -> Template.Unprocessed.t list
end