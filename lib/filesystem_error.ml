type dir_or_file = Dir | File

type t =
  [ `AlreadyExists of string * dir_or_file * string
  | `BadFilename of string ]

module Smart = struct
  let already_exists cur dir_or_file path =
    `AlreadyExists (cur, dir_or_file, path)

  let is_already_exists = function
    | `AlreadyExists _ -> true
    | _ -> false

  let bad_filename project_name = `BadFilename project_name

  let is_bad_filename = function
    | `BadFilename _ -> true
    | _ -> false
end
