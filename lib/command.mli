type verbosity = Quiet | Loud

type t =
  { args : string list;
    cmessage : string;
    verbosity : verbosity
  }

val make : string list -> string -> verbosity -> t
val run : t -> unit
