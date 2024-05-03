type t =
  [ `ReferParsing of Action_error.DataSource.t * int * string
  | `CrunchPath of string * Lineinfo.t
  | `FileReadError of string ]

module Smart = struct
  let refer_parsing d (i, s) = `ReferParsing (d, i, s)

  let crunch_path path lineinfo =
    `CrunchPath (path, lineinfo)

  let file_read_error path = `FileReadError path
end
