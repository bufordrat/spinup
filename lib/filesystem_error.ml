type t = [`DirAlreadyExists of string]

module Smart = struct
  let dir_already_exists s = `DirAlreadyExists s
end
