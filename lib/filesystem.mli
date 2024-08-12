val already_exists : string -> (unit, Global_error.t) result

val validate_project_name :
  string -> (string, Global_error.t) result
