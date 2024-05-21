type dir =
  { dir : string; actions : t list; config : Config.t }

and t =
  | Write of Template.Processed.t
  | Print of string
  | Run of Command.t
  | WithCD of dir

module Main : sig 
  val run : t -> unit
  val dry_run : t -> unit
  val write : Template.Processed.t -> t

  val directory_actions :
    Config.t -> (t list, Global_error_intf.t) result

  val main_action : string -> (t, Global_error_intf.t) result

  module Files : sig
    val files : Config.t -> Template.Unprocessed.t list
  end
end
