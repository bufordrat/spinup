type t =
  [ `ReferCrunch of int * string * string
  | `ReferFile of int * string * string
  | `ConfigCrunchPath of string * Lineinfo.t
  | `FileReadError of string ]

module Smart = struct
  let refer_crunch (i, s) path = `ReferCrunch (i, s, path)
  let refer_file (i, s) path = `ReferFile (i, s, path)

  let config_crunch_path path lineinfo =
    `ConfigCrunchPath (path, lineinfo)

  let file_read_error msg_from_stdlib = `FileReadError msg_from_stdlib
end
