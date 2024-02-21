module Arguments : sig
  val print_config : bool Cmdliner.Term.t

  val dry_run : bool Cmdliner.Term.t

  val project_name : string option Cmdliner.Term.t
end

module Command : sig
  val to_exe :
    ('a -> 'b -> 'c -> unit) ->
    'a Cmdliner.Term.t ->
    'b Cmdliner.Term.t ->
    'c Cmdliner.Term.t ->
    'd
end
