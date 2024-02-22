type t =
  [ `ReferParsing of int * string
  | `ConfigCrunch of string
  | `FileReadError of string ]

module Smart = struct
  let refer_parsing (i, s) = `ReferParsing (i, s)
  let config_crunch path = `ConfigCrunch path
  let file_read_error path = `FileReadError path
end
