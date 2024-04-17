type t =
  [ `ReferParsing of int * string
  | `CrunchPath of string
  | `FileReadError of string ]

module Smart = struct
  let refer_parsing (i, s) = `ReferParsing (i, s)
  let crunch_path path = `CrunchPath path
  let file_read_error path = `FileReadError path
end
