let option_to_result path = function
  | Some contents -> Ok contents
  | None ->
     Error ("filesystem crunching error: " ^ path)
