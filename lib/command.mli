type verbosity = Quiet | Loud

type t =
  { args : string list;
    cmessage : string;
    verbosity : verbosity
  }

val run : t -> unit
