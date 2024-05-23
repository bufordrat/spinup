type dir_or_file = Dir | File
type t = [`AlreadyExists of string * dir_or_file * string]

module Smart = struct
  let already_exists cur dir_or_file path =
    `AlreadyExists (cur, dir_or_file, path)
end
