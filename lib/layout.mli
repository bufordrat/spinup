type block =
  | Argv
  | Argv0
  | Block of int * string list
  | Blank
  | Grep of { path : string; line : int; col : int option }

type t = block list

module Smart : sig
  val argv : block
  val argv0 : block
  val block : int -> string list -> block
  val blank : block
  val grep : ?col:int -> string -> int -> block
end

val to_string : block list -> string
