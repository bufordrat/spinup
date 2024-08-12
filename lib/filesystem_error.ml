type dir_or_file = Dir | File

type t =
  [ `AlreadyExists of string * dir_or_file * string
  | `BadProjectName of string ]

module Smart = struct
  let already_exists cur dir_or_file path =
    `AlreadyExists (cur, dir_or_file, path)

  let is_already_exists = function
    | `AlreadyExists _ -> true
    | _ -> false

  let bad_project_name pname = `BadProjectName pname

  let is_bad_project_name = function
    | `BadProjectName _ -> true
    | _ -> false
end
