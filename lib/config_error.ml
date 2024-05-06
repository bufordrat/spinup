type t =
  [ `ReferCrunch of int * string * Lineinfo.t
  | `ReferFile of int * string * string
  | `CrunchPath of string * Lineinfo.t
  | `FileReadError of string ]

module Smart = struct
  let refer_crunch (i, s) lineinfo =
    `ReferCrunch (i, s, lineinfo)

  let refer_file (i, s) path = `ReferFile (i, s, path)

  let crunch_path path lineinfo =
    `CrunchPath (path, lineinfo)

  let file_read_error path = `FileReadError path
end
