type verbosity = Quiet | Loud

type t =
  { args : string list;
    cmessage : string;
    verbosity : verbosity
  }

val make : string list -> string -> verbosity -> t
val runargs : string list -> unit
val run : t -> unit
