val already_exists : string -> (unit, Global_error.t) result

val validate_project_name :
  string -> (unit, Global_error.t) result

val prereqs : string list

val check_prereqs :
  string list -> (unit, Global_error.t) result
