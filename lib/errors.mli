val already_exists : string -> (unit, string) result

val process :
  Template.Unprocessed.t ->
  (Template.Processed.t, string) result
