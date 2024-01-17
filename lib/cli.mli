module Arguments : sig
  val dry_run : bool Cmdliner.Term.t
  val project_name : string Cmdliner.Term.t
end

module Command : sig
  val to_exe :
    ('a -> 'b -> unit) ->
    'a Cmdliner.Term.t ->
    'b Cmdliner.Term.t ->
    'c
end
